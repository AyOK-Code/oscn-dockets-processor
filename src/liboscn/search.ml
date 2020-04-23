open! Core_kernel
open Cohttp
open Cohttp_lwt_unix
module Body = Cohttp_lwt.Body

let pool = Lwt_pool.create 2 (fun () -> Lwt.return_unit)

let real_fetch uri =
  Lwt_pool.use pool (fun () ->
    let%lwt () = Lwt_unix.sleep 0.5 in
    let debug_uri = Uri.to_string uri in
    let%lwt () = Lwt_io.printlf "Calling %s" debug_uri in
    let%lwt res, body = Client.post uri in
    let status = Response.status res in
    let%lwt () = Lwt_io.printlf "Got %s from %s" (Code.string_of_status status) debug_uri in
    begin match Code.code_of_status status with
    | 200 -> Body.to_string body
    | _ -> failwithf "The OSCN Dockets website failed with HTTP %s" (Code.string_of_status status) ()
    end
  )

let fake_fetch () =
  Lwt_io.chars_of_file "results1.html" |> Lwt_stream.to_string

let fetch_more_results href =
  let uri =
    Uri.of_string href
    |> (fun u -> Uri.with_scheme u (Some "https"))
    |> (fun u -> Uri.with_host u (Some "www.oscn.net"))
  in
  real_fetch uri

let yojson_of_date date = `String (Date.to_string date)

type case = {
  case_number: string;
  date_filed: Date.t [@to_yojson yojson_of_date];
  title: string;
  found_party: string;
} [@@deriving to_yojson]

type processed =
| Success of case
| Skipped of case
| Failed of Error.t

let valid_codes = [| "CF"; "CFTU"; "CM"; "CMTU"; "CRF"; "CRM"; "F"; "M"; "TR"; "TRTU" |]

let extract_cell cell =
  let open Soup in
  begin match leaf_text cell with
  | None | Some "" -> failwith "Empty cell"
  | Some x -> x
  end

let process_row row =
  let open Soup in
  try
    begin match row $$ "td" |> to_list with
    | [td1; td2; td3; td4] ->
      let case = {
        case_number = extract_cell td1;
        date_filed = extract_cell td2 |> Date.of_string;
        title = extract_cell td3;
        found_party = extract_cell td4;
      }
      in
      let code = String.take_while case.case_number ~f:(Char.(<>) '-') |> String.uppercase in
      if Array.mem valid_codes code ~equal:String.(=)
      then Success case
      else Skipped case
    | _ -> Failed (Error.of_string "Invalid row")
    end
  with exn -> Failed (Error.of_string (Libutils.Exception.human exn))

let rec process_page raw results =
  let open Soup in
  let html = parse raw in

  html $$ "table tbody" |> to_list |> Lwt_list.iter_p (fun table ->
    children table |> elements |> to_list |> Lwt_list.iter_p (fun tchild ->
      begin match name tchild with
      | "caption" -> Lwt.return_unit
      | "tr" ->
        let classes = Option.value ~default:"" (attribute "class" tchild) |> String.split ~on:' ' in
        begin match classes with
        | ll when List.mem ll "resultTableRow" ~equal:String.equal ->
          (* It's a result row *)
          begin match process_row tchild with
          | Success ({ case_number; _ } as case) -> String.Table.update results case_number ~f:(fun _ -> case)
          | Skipped _ | Failed _ -> ()
          end;
          Lwt.return_unit
        | ll when List.mem ll "resultTableHeaders" ~equal:String.equal -> Lwt.return_unit
        | [""] when tchild $? "td.moreResults a" |> Option.is_some ->
          (* It's a 'more results' link *)
          begin match Option.bind (tchild $? "td.moreResults a") ~f:(attribute "href") with
          | None | Some "" -> failwith "Could not find 'more results' link"
          | Some href ->
            let%lwt page = fetch_more_results href in
            process_page page results
          end
        | _ ->
          failwithf "Unexpected row class '%s' %s" (String.concat ~sep:" " classes) (Soup.to_string tchild) ()
        end
      | s -> failwithf "Unexpected result row element of type <%s>" s ()
      end
    )
  )

let scrape ?last_name ?first_name ?middle_name ?dob_before ?dob_after () =
  let query = [
    "db", ["all"];
    "lname", (Option.value last_name ~default:"" |> List.return);
    "fname", (Option.value first_name ~default:"" |> List.return);
    "mname", (Option.value middle_name ~default:"" |> List.return);
    "DoBMin", (Option.value_map dob_before ~default:"" ~f:Date.to_string |> List.return);
    "DoBMax", (Option.value_map dob_after ~default:"" ~f:Date.to_string |> List.return);
  ]
  in
  let _uri = Uri.make ~scheme:"https" ~host:"www.oscn.net" ~path:"/dockets/Results.aspx" ~query () in

  let%lwt page = fake_fetch ()in

  let results = String.Table.create () in
  let%lwt () = process_page page results in
  let j = `Assoc (String.Table.fold results ~init:[] ~f:(fun ~key ~data acc ->
      (key, (case_to_yojson data))::acc
    ))
  in
  Lwt_io.printl (Yojson.Safe.pretty_to_string j)
