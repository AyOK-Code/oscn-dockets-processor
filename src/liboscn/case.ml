open! Core_kernel

open S

let find_prefixed ~prefix strs =
  begin match List.find_map strs ~f:(String.chop_prefix ~prefix) with
  | None -> failwithf "Unable to find '%s'" prefix ()
  | Some s -> s
  end

let find_section ~html ~section_classes ~el_type =
  let open Soup in
  let section_class = List.map section_classes ~f:(fun s -> sprintf ".%s" s) |> String.concat in
  begin match html $? (sprintf "h2%s" section_class) with
  | None -> failwithf "Invalid page structure, could not find '%s' header" section_class ()
  | Some h2 ->
    begin match next_element h2 with
    | Some el when String.(name el = el_type) -> el
    | (None as next) | (Some _ as next) ->
      failwithf "Invalid page structure, could not find '%s' data (was %s instead of '%s')"
        section_class (Option.value_map next ~default:"not found" ~f:(fun x -> sprintf "'%s'" (name x))) el_type ()
    end
  end

let list_regex = Re2.create_exn ", "

let process_party text uri =
  begin match Re2.split list_regex (Text.to_string text) |> List.to_array with
  | [|last; first; role|] -> Some {
      name = Full {
          first_name = Text.clean (String.uppercase first);
          last_name = Text.clean (String.uppercase last);
        };
      role = Oscn.parse_role role;
      uri;
    }
  | [|other; role|] -> Some {
      name = Other_name (Text.clean (String.uppercase other));
      role = Oscn.parse_role role;
      uri;
    }
  | [| "" |] -> None
  | _ -> failwithf "Invalid parties entry: '%s'" (Text.to_string text) ()
  end

let fake_fetch () = Lwt_io.chars_of_file "case1.html" |> Lwt_stream.to_string

let scrape ?last_name ?first_name ?middle_name uri =
  let open Soup in

  (* let%lwt raw = Oscn.real_fetch uri in *)
  let%lwt raw = fake_fetch () in
  let html = parse raw in

  (* Process header *)
  let title, case_number, date_filed, judge =
    let table = find_section ~html ~section_classes:["styletop"] ~el_type:"table" in
    begin match table $$ "tbody tr td" |> to_list with
    | [left; right] ->
      let title = (texts left |> String.concat ~sep:" " |> Text.clean) in
      let fragments = trimmed_texts right in
      let case_number = find_prefixed ~prefix:"No. " fragments |> Text.clean in
      let date_filed = find_prefixed ~prefix:"Filed: " fragments |> Text.clean |> Text.to_string |> Date.of_string in
      let judge = find_prefixed ~prefix:"Judge: " fragments |> Text.clean in
      (title, case_number, date_filed, judge)
    | _ -> failwith "Invalid page structure, could not find case header info"
    end
  in

  (* Process parties *)
  let parties =
    let p = find_section ~html ~section_classes:["section"; "party"] ~el_type:"p" in
    let groups = descendants p |> to_list |> List.group ~break:(fun _left right ->
        Option.value_map (element right) ~default:false ~f:(fun el -> String.((name el) = "br"))
      )
    in
    let queue = Queue.create () in
    List.iter groups ~f:(fun group ->
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
      process_party (Buffer.contents buf |> Text.clean) (Option.map person_href ~f:Oscn.make_uri_from_href)
      |> Option.iter ~f:(Queue.enqueue queue)
    );
    Queue.to_array queue
  in

  (* Parties lookups *)
  let needle_opt = begin match last_name, first_name, middle_name with
  | (Some ln), (Some fn), (Some mn) -> Some (sprintf "%s, %s %s." ln fn mn |> String.uppercase)
  | (Some ln), (Some fn), None -> Some (sprintf "%s, %s" ln fn |> String.uppercase)
  | (Some ln), None, None -> Some (sprintf "%s," ln |> String.uppercase)
  | _ -> None
  end
  in
  let name_matcher left right =
    String.is_prefix ~prefix:left right || String.is_prefix ~prefix:right left
  in
  let is_defendant = Option.value_map needle_opt ~default:false ~f:(fun needle ->
      Array.exists parties ~f:(function
      | { role = Defendant; name = Other_name s; uri = _ } when name_matcher needle (Text.to_string s) -> true
      | { role = Defendant; name = Full { first_name; last_name; }; uri = _ } ->
        name_matcher needle (sprintf "%s, %s" (Text.to_string last_name) (Text.to_string first_name))
      | _ -> false
      )
    )
  in
  let arresting_agency = Array.find_map parties ~f:(function
    | { name; role = Arresting_agency; uri = _ } -> Some (Oscn.name_to_text name)
    | _ -> None
    )
  in

  (* Process events *)
  let events =
    let table = find_section ~html ~section_classes:["section"; "events"] ~el_type:"table" in
    table $$ "tbody tr" |> to_list |> Array.of_list_map ~f:(fun row ->
      begin match row $$ "td" |> to_list with
      | [td1; _td2; _td3; _td4] ->
        begin match trimmed_texts td1 with
        | hd::rest -> {
            datetime = Oscn.parse_datetime ~section:"event" (Text.clean hd);
            description = String.concat ~sep:" " rest |> String.uppercase |> Text.clean;
          }
        | [] -> failwith "Invalid events row structure"
        end
      | _ -> failwith "Invalid events table structure"
      end
    )
  in

  (* Events lookups *)
  let court_dates = Array.filter_map events ~f:(fun { datetime; description } ->
      Option.some_if
        (String.(=) (Text.to_string description) "COURT APPEARANCE")
        datetime
    )
  in

  (* Process dockets *)
  let dockets =
    let table = find_section ~html ~section_classes:["section"; "dockets"] ~el_type:"table" in
    table $$ "tbody tr" |> to_list |> Array.of_list_map ~f:(fun row ->
      let rows = row $$ "td" |> to_list |> Array.of_list_map ~f:(fun td ->
          texts td |> String.concat ~sep:" " |> Text.clean
        )
      in
      begin match rows with
      | [|td1; td2; td3; td4; td5; td6|] -> {
          date = td1 |> Text.require |> Option.map ~f:(Oscn.parse_date ~section:"docket");
          code = td2 |> Text.require;
          description = td3;
          count = td4 |> Text.require;
          party = td5 |> Text.require;
          amount = td6 |> Text.require |> Option.map ~f:(fun x -> Text.to_string x |> Money.amount);
        }
      | _ -> failwith "Invalid dockets table structure"
      end
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
    events;
    court_dates;
    dockets;
  }
  in
  print_endline (Yojson.Safe.pretty_to_string (case_to_yojson case));

  Lwt.return_unit
