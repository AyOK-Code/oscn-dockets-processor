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

let flatten (json : Yojson.Safe.t) =
  let rec loop json prefix acc =
    begin match json with
    | `Assoc pairs ->
      String.Table.add_exn acc ~key:prefix ~data:(`Assoc []);
      List.iter pairs ~f:(fun (key, value) ->
        loop value (sprintf "%s.%s" prefix key) acc
      )
    | `List values ->
      String.Table.add_exn acc ~key:prefix ~data:(`List []);
      List.iteri values ~f:(fun key value ->
        loop value (sprintf "%s[%d]" prefix key) acc
      )
    | x -> String.Table.add_exn acc ~key:prefix ~data:x
    end in
  begin match json with
  | `Assoc _ ->
    let acc = String.Table.create () in
    loop json "" acc;
    acc
  | x -> failwithf "Cannot flatten a non-object: %s" (Yojson.Safe.to_string x) ()
  end

let json_diff left right =
  let errors = Queue.create () in
  let mismatch s = Queue.enqueue errors s; None in
  let stringify = Yojson.Safe.to_string in
  let _merged = String.Table.merge (flatten left) (flatten right) ~f:(fun ~key -> function
    | `Left x -> sprintf "%s: %s" (color (sprintf "+++ %s" key) `Green) (stringify x) |> mismatch
    | `Right y -> sprintf "%s: %s" (color (sprintf "--- %s" key) `Red) (stringify y) |> mismatch
    | `Both (x, y) when Yojson.Safe.equal x y -> None
    | `Both (x, y) ->
      sprintf "%s: %s %s %s" (color (sprintf "+/- %s" key) `Yellow) (stringify x) (color "!=" `Yellow) (stringify y)
      |> mismatch
    )
  in
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
  let request = S.{ last_name; first_name; middle_name; dob_before = None; dob_after = None; } in
  let name_matcher = Oscn.make_name_matcher request in
  let case_data = Case.process ~name_matcher uri raw_case in
  let expected = Yojson.Safe.from_string raw_expected in

  json_diff (Oscn.prepare_data ~name_matcher [case_data]) expected;

  Lwt.return_unit

let () =
  Lwt_main.run @@ Alcotest_lwt.run "Case processing" [
    "Case fixtures", [
      "case1.html", `Quick, basic ~last_name:"johnson" "case1" "?db=wagoner&number=CF-2015-00470&cmid=11288";
      "case2.html", `Quick, basic ~last_name:"johnson" "case2" "?db=mcclain&number=TR-2005-01955&cmid=127475";
      "case3.html", `Quick, basic ~last_name:"johnson" "case3" "?db=mcclain&number=CJ-2014-00181&cmid=16110";
      "case4.html", `Quick, basic ~last_name:"ward" "case4" "?db=oklahoma&number=TR-2014-1119&cmid=3080009";
      "case5.html", `Quick, basic ~last_name:"ward" "case5" "?db=oklahoma&number=CF-2015-2603&cmid=3251429";
      "case6.html", `Quick, basic ~last_name:"Laughlin" "case6" "?db=oklahoma&number=CM-2010-1584&cmid=2596069";
      "case7.html", `Quick, basic ~last_name:"Laughlin" "case7" "?db=oklahoma&number=CF-2016-5438&cmid=3420850";
      "case8.html", `Quick, basic ~last_name:"johnson" "case8" "?db=oklahoma&number=TR-1991-3731&cmid=180879";
    ];
  ]
