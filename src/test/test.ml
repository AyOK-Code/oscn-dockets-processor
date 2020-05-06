open! Core_kernel

open Liboscn

type mismatch =
| Changed of (Yojson.Safe.t * Yojson.Safe.t)
| Added of Yojson.Safe.t
| Deleted of Yojson.Safe.t

let color s = function
| `Yellow -> sprintf "\x1b[0;33m%s\x1b[m" s
| `Green -> sprintf "\x1b[0;32m%s\x1b[m" s
| `Red -> sprintf "\x1b[0;31m%s\x1b[m" s

let jdiff left right =
  let errors = Queue.create () in
  let serialize_path path = List.rev path |> String.concat in
  let mismatch path m =
    let s = begin match m with
    | Changed (x, y) -> sprintf "%s: %s %s %s"
        (color (sprintf "+/- %s" (serialize_path path)) `Yellow)
        (Yojson.Safe.to_string x)
        (color "!=" `Yellow)
        (Yojson.Safe.to_string y)
    | Added x -> sprintf "%s: %s"
        (color (sprintf "+++ %s" (serialize_path path)) `Green)
        (Yojson.Safe.to_string x)
    | Deleted x -> sprintf "%s: %s"
        (color (sprintf "--- %s" (serialize_path path)) `Red)
        (Yojson.Safe.to_string x)
    end
    in
    Queue.enqueue errors s
  in
  let rec loop path = function
  | `Null, `Null -> ()
  | (`Bool x), (`Bool y) when Bool.(x = y) -> ()
  | (`Bool _ as x), (`Bool _ as y) -> mismatch path (Changed (x, y))
  | (`Float x), (`Float y) when Float.(x = y) -> ()
  | (`Float _ as x), (`Float _ as y) -> mismatch path (Changed (x, y))
  | (`String x), (`String y)
  | (`Intlit x), (`Intlit y) when String.(x = y) -> ()
  | (`String _ as x), (`String _ as y)
  | (`Intlit _ as x), (`Intlit _ as y) -> mismatch path (Changed (x, y))
  | (`Int x), (`Int y) when Int.(x = y) -> ()
  | (`Int _ as x), (`Int _ as y) -> mismatch path (Changed (x, y))
  | (`List llx), (`List lly)
  | (`Tuple llx), (`Tuple lly) ->
    let zipped, rest = List.zip_with_remainder llx lly in
    List.iteri zipped ~f:(fun i pair -> loop ((sprintf "[%d]" i)::path) pair);
    begin match rest with
    | Some (First ll) -> List.iter ll ~f:(fun x -> mismatch path (Deleted x))
    | Some (Second ll) -> List.iter ll ~f:(fun x -> mismatch path (Added x))
    | None -> ()
    end
  | (`Assoc llx), (`Assoc lly) ->
    let to_table ll = begin match String.Table.of_alist ll with
    | `Ok x -> x
    | `Duplicate_key key -> failwithf "Duplicate key: '%s' at %s" key (serialize_path path) ()
    end
    in
    let _merged = String.Table.merge (to_table llx) (to_table lly) ~f:(fun ~key -> function
      | `Left x -> mismatch ((sprintf ".%s" key)::path) (Deleted x); None
      | `Right x -> mismatch ((sprintf ".%s" key)::path) (Added x); None
      | `Both pair -> loop ((sprintf ".%s" key)::path) pair; None
      )
    in
    ()
  | (`Variant _), (`Variant _) -> failwithf "Invalid variant at %s" (serialize_path path) ()
  | x, y -> mismatch path (Changed (x, y))
  in
  loop [] (left, right);
  begin match Queue.length errors with
  | 0 -> ()
  | n ->
    printf "%s:\n%s\n" (color "Actual" `Green) (Yojson.Safe.pretty_to_string left);
    eprintf "JSON Mismatch (%d errors):\n%s\n" n (Queue.to_array errors |> String.concat_array ~sep:"\n");
    failwith "JSON Mismatch"
  end

let basic ~last_name ?first_name ?middle_name filename query () =
  let%lwt raw_case = Lwt_io.chars_of_file (sprintf "../../../../fixtures/%s.html" filename) |> Lwt_stream.to_string in
  let%lwt raw_expected = Lwt_io.chars_of_file (sprintf "../../../../fixtures/%s.json" filename) |> Lwt_stream.to_string in

  let uri = sprintf "https://www.oscn.net/dockets/GetCaseInformation.aspx%s" query |> Uri.of_string in
  let case_data = Case.process ~last_name ?first_name ?middle_name uri raw_case in
  let expected = Yojson.Safe.from_string raw_expected in

  jdiff (Oscn.prepare_data ~last_name ?first_name ?middle_name [case_data]) expected;

  Lwt.return_unit

let () =
  Lwt_main.run @@ Alcotest_lwt.run "Case processing" [
    "Case fixtures", [
      "case1.html", `Quick, basic ~last_name:"johnson" "case1" "?db=wagoner&number=CF-2015-00470&cmid=11288";
      "case2.html", `Quick, basic ~last_name:"johnson" "case2" "?db=mcclain&number=TR-2005-01955&cmid=127475";
      "case3.html", `Quick, basic ~last_name:"johnson" "case3" "?db=mcclain&number=CJ-2014-00181&cmid=16110";
      "case4.html", `Quick, basic ~last_name:"ward" "case4" "?db=oklahoma&number=TR-2014-1119&cmid=3080009";
      "case5.html", `Quick, basic ~last_name:"Laughlin" "case5" "?db=oklahoma&number=SC-2019-23057&cmid=3833620";
      "case6.html", `Quick, basic ~last_name:"Laughlin" "case6" "?db=oklahoma&number=CM-2010-1584&cmid=2596069";
      "case7.html", `Quick, basic ~last_name:"Laughlin" "case7" "?db=oklahoma&number=CF-2016-5438&cmid=3420850";
      "case8.html", `Quick, basic ~last_name:"johnson" "case8" "?db=oklahoma&number=TR-1991-3731&cmid=180879";
    ];
  ]
