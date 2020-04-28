open! Core_kernel

let fake_fetch () = Lwt_io.chars_of_file "results1.html" |> Lwt_stream.to_string

let yojson_of_date date = `String (Date.to_string date)
let yojson_of_uri uri = `String (Uri.to_string uri)

type search_result = {
  uri: Uri.t [@to_yojson yojson_of_uri];
  case_number: Text.t;
  date_filed: Date.t [@to_yojson yojson_of_date];
  title: Text.t;
  found_party: Text.t;
} [@@deriving to_yojson]

type processed =
| Success of search_result
| Skipped of search_result
| Failed of Error.t

let valid_codes = [| "CF"; "CFTU"; "CM"; "CMTU"; "CRF"; "CRM"; "F"; "M"; "TR"; "TRTU" |]

let extract_cell cell =
  let open Soup in
  begin match leaf_text cell with
  | None | Some "" -> failwith "Empty cell"
  | Some x -> Text.clean x
  end

let process_row row =
  let open Soup in
  try
    begin match row $$ "td" |> to_list with
    | [td1; td2; td3; td4] ->
      let link = begin match Option.bind (row $? "a") ~f:(attribute "href") with
      | None | Some "" -> failwith "Could not find the link to the case"
      | Some href -> href
      end
      in
      let case = {
        uri = Oscn.make_uri_from_href link;
        case_number = extract_cell td1;
        date_filed = extract_cell td2 |> Text.to_string |> Date.of_string;
        title = extract_cell td3;
        found_party = extract_cell td4;
      }
      in
      let code = String.take_while (Text.to_string case.case_number) ~f:(Char.(<>) '-') |> String.uppercase in
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
        (* let classes = Option.value ~default:"" (attribute "class" tchild) |> String.split ~on:' ' in *)
        (* TODO: validate classes *)
        let classes = classes tchild in
        begin match classes with
        | ll when List.mem ll "resultTableRow" ~equal:String.equal ->
          (* It's a 'resultTableRow' *)
          begin match process_row tchild with
          | Success ({ case_number; _ } as case) -> String.Table.update results (Text.to_string case_number) ~f:(fun _ -> case)
          | Skipped _ | Failed _ -> ()
          end;
          Lwt.return_unit
        | ll when List.mem ll "resultTableHeaders" ~equal:String.equal -> Lwt.return_unit
        | [""] when tchild $? "td.moreResults a" |> Option.is_some ->
          (* It contains a 'moreResults' link *)
          begin match Option.bind (tchild $? "td.moreResults a") ~f:(attribute "href") with
          | None | Some "" -> failwith "Could not find 'more results' link"
          | Some href ->
            let%lwt page = Oscn.(href |> make_uri_from_href |> real_fetch) in
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
  Lwt.return results
