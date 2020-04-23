open! Core_kernel

let yojson_of_date date = `String (Date.to_string date)
let yojson_of_uri uri = `String (Uri.to_string uri)

type payment = {
  amount: int;
}

type role =
| Defendant
| Plaintiff
| Arresting_agency
| Arresting_officer
[@@deriving to_yojson]

type name =
| Full of {
    first_name: Html.text;
    last_name: Html.text;
  }
| Other_name of Html.text
[@@deriving to_yojson]

type party = {
  name: name;
  role: role;
  (* dob: Date.t; *)
} [@@deriving to_yojson]

type event = {
  datetime: Time.t;
  description: Html.text;
}

type count = {
  notes: Html.text; (* TODO: Process this further *)
}

let parse_role = function
| "Defendant" -> Defendant
| "Plaintiff" -> Plaintiff
| "ARRESTING OFFICER" -> Arresting_officer
| "ARRESTING AGENCY" -> Arresting_agency
| s -> failwithf "Invalid role: '%s'" s ()

type case = {
  uri: Uri.t [@to_yojson yojson_of_uri];
  title: Html.text;
  parties: party list;
  (* Parties lookup *)
  (* is_defendant: bool; *)
  case_number: Html.text;
  date_filed: Date.t [@to_yojson yojson_of_date];
  judge: Html.text;
  (* Parties lookup *)
  (* arresting_agency: Html.text; *)
  (* court_dates: Date.t list; *)
  (* charges: Html.text list; *)
  (* payments: payment list; *)
} [@@deriving to_yojson]

let find_prefixed ~prefix strs =
  begin match List.find_map strs ~f:(String.chop_prefix ~prefix) with
  | None -> failwithf "Unable to find '%s'" prefix ()
  | Some s -> s
  end


let fake_fetch () = Lwt_io.chars_of_file "case3.html" |> Lwt_stream.to_string
let list_regex = Re2.create_exn ", "

let scrape uri =
  let open Soup in

  (* let%lwt raw = Oscn.real_fetch uri in *)
  let%lwt raw = fake_fetch () in
  let html = parse raw in

  (* Process header *)
  let title, case_number, date_filed, judge =
    begin match html $$ "table.caseStyle tr td" |> to_list with
    | [left; right] ->
      let title = (trimmed_texts left |> String.concat ~sep:" " |> Html.clean) in
      let fragments = trimmed_texts right in
      let case_number = find_prefixed ~prefix:"No. " fragments |> Html.clean in
      let date_filed = find_prefixed ~prefix:"Filed: " fragments |> Html.clean |> Html.text_to_string |> Date.of_string in
      let judge = find_prefixed ~prefix:"Judge: " fragments |> Html.clean in
      (title, case_number, date_filed, judge)
    | _ -> failwith "Invalid page structure, could not find case header info"
    end
  in

  (* Process parties *)
  let parties =
    begin match html $? "div#oscn-content > div.sized > p:not([class])" with
    | None -> failwith "Invalid page structure, could not find parties section"
    | Some p ->
      begin match previous_element p with
      | Some h2 when String.(name h2 = "h2") && List.mem (classes h2) "party" ~equal:String.equal ->
        let entries = Queue.create ~capacity:12 () in
        let buf = Buffer.create 32 in
        descendants p |> iter (fun child ->
          begin match element child with
          | Some el when String.(name el = "br") ->
            let entry = Buffer.contents buf |> Html.clean in
            Queue.enqueue entries entry;
            Buffer.clear buf
          | Some _el -> ()
          | None -> Buffer.add_string buf (to_string child)
          end
        );
        Queue.to_list entries |> List.map ~f:(fun entry ->
          begin match Re2.split list_regex (Html.text_to_string entry) |> List.to_array with
          | [|first; last; role|] ->
            {
              name = Full { first_name = Html.clean first; last_name = Html.clean last };
              role = parse_role role;
            }
          | [|other; role|] ->
            {
              name = Other_name (Html.clean other);
              role = parse_role role;
            }
          | _ -> failwithf "Invalid parties entry: '%s'" (Html.text_to_string entry) ()
          end
        )
      | None | Some _ -> failwith "Invalid page structure, could not find parties header"
      end
    end
  in


  let case = {
    uri;
    title;
    parties;
    case_number;
    date_filed;
    judge;
  }
  in
  print_endline (Yojson.Safe.pretty_to_string (case_to_yojson case));

  Lwt.return_unit
