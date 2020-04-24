open! Core_kernel

open S

let find_prefixed ~prefix strs =
  begin match List.find_map strs ~f:(String.chop_prefix ~prefix) with
  | None -> failwithf "Unable to find '%s'" prefix ()
  | Some s -> s
  end


let fake_fetch () = Lwt_io.chars_of_file "case2.html" |> Lwt_stream.to_string
let list_regex = Re2.create_exn ", "

let scrape ?last_name ?first_name ?middle_name uri =
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
    begin match html $? "h2.section.party" with
    | None -> failwith "Invalid page structure, could not find parties header"
    | Some h2 ->
      begin match next_element h2 with
      | Some p when String.(name p = "p") && List.is_empty (classes p) ->
        let groups = descendants p |> to_list |> List.group ~break:(fun _left right ->
            Option.value_map (element right) ~default:false ~f:(fun el -> String.((name el) = "br"))
          )
        in
        List.filter_map groups ~f:(fun group ->
          let buf = Buffer.create 16 in
          let person_href = List.fold group ~init:None ~f:(fun acc node ->
              begin match element node with
              | None ->
                Buffer.add_string buf (to_string node);
                acc
              | Some el when String.(name el = "a") ->
                Option.filter (attribute "href" el) ~f:(fun s -> not (String.is_empty s))
              | Some _ -> None
              end
            )
          in
          Case_sections.process_party (Buffer.contents buf |> Html.clean) (Option.map person_href ~f:Oscn.make_uri_from_href)
        )
      | None | Some _ -> failwith "Invalid page structure, could not find parties header"
      end
    end
  in

  (* Parties lookups *)
  let needle_opt = begin match last_name, first_name, middle_name with
  | (Some ln), (Some fn), (Some mn) -> Some (sprintf "%s, %s %s." ln fn mn |> String.uppercase)
  | (Some ln), (Some fn), None -> Some (sprintf "%s, %s" ln fn |> String.uppercase)
  | (Some ln), None, None -> Some (sprintf "%s," ln |> String.uppercase)
  | _ -> None
  end
  in
  let%lwt () = Lwt_io.printlf "PREFIX: %s" (Option.value ~default:"--" needle_opt) in
  let name_matcher left right =
    String.is_prefix ~prefix:left right || String.is_prefix ~prefix:right left
  in
  let is_defendant = Option.value_map needle_opt ~default:false ~f:(fun needle ->
      List.exists parties ~f:(function
      | { role = Defendant; name = Other_name s; uri = _ } when name_matcher needle (Html.text_to_string s) -> true
      | { role = Defendant; name = Full { first_name; last_name; }; uri = _ } ->
        name_matcher needle (sprintf "%s, %s" (Html.text_to_string last_name) (Html.text_to_string first_name))
      | _ -> false
      )
    )
  in
  let arresting_agency = List.find_map parties ~f:(function
    | { name; role = Arresting_agency; uri = _ } -> Some (name_to_text name)
    | _ -> None
    )
  in

  let case = {
    uri;
    title;
    parties;
    is_defendant;
    case_number;
    date_filed;
    judge;
    arresting_agency;
  }
  in
  print_endline (Yojson.Safe.pretty_to_string (case_to_yojson case));

  Lwt.return_unit
