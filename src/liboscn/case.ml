open! Core_kernel

open S

exception Not_present of string

let find_prefixed ~prefix strs =
  begin match Array.find_map strs ~f:(String.chop_prefix ~prefix) with
  | None -> failwithf "Unable to find '%s'" prefix ()
  | Some s -> s
  end

let find_section ~html ~section_classes ~el_type ?empty_type () =
  let open Soup in
  let section_class = List.map section_classes ~f:(fun s -> sprintf ".%s" s) |> String.concat in
  begin match html $? (sprintf "h2%s" section_class) with
  | None -> raise (Not_present (sprintf "Invalid page structure, could not find '%s' header" section_class))
  | Some h2 ->
    begin match next_element h2, empty_type with
    | Some el, _ when String.(name el = el_type) -> el
    | Some el, Some empty when String.(name el = empty) -> create_element el_type
    | (None as next), _
    | (Some _ as next), _ ->
      raise (Not_present (
          sprintf "Invalid page structure, could not find '%s' data (was %s instead of '%s')"
            section_class (Option.value_map next ~default:"not found" ~f:(fun x -> sprintf "'%s'" (name x))) el_type
        )
      )
    end
  end

let td_party td = Soup.texts td |> String.concat ~sep:" " |> String.uppercase |> Text.clean |> Text.require

let list_regex = Re2.create_exn ", "

let process_party text uri =
  begin match Re2.split list_regex (Text.to_string text) |> List.to_array with
  | [|last; first; role|] -> Some {
      name = Full_name {
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

let rec process_counts_open acc el =
  let open Soup in
  let p = next_element el |> Option.filter ~f:(fun x -> String.(name x = "p")) in
  begin match Option.bind p ~f:(fun x -> texts x |> List.last) with
  | None -> acc
  | Some s ->
    let p_el = Option.value_exn ~message:"Faulty assumption in counts processing" p in
    begin match Text.clean s |> Text.require with
    | None ->
      process_counts_open acc p_el
    | Some description ->
      process_counts_open ({ description }::acc) p_el
    end
  end

let process_counts_closed div =
  let open Soup in
  let first, second = begin match div $$ "table" |> to_list with
  | [first; second] when List.mem (classes first) "Counts" ~equal:String.equal -> first, second
  | _ -> failwith "Invalid page structure, could not locate distinct count lines"
  end
  in
  (* First table *)
  let count_as_filed, date_of_offense, violation_of1 = begin match first $$ "tbody tr td" |> to_list with
  | [number; description] ->
    begin match texts number with
    | s::_ when String.is_prefix s ~prefix:"Count #" -> ()
    | s::_ -> failwithf "Invalid first cell value for count first line: '%s'" s ()
    | _ -> failwith "Empty first cell value for count first line"
    end;
    let fragments = trimmed_texts description |> List.to_array in
    let count_as_filed =
      find_prefixed fragments ~prefix:"Count as Filed:"
      |> Text.clean
      |> Text.to_string
      |> (fun s -> String.chop_suffix s ~suffix:", in violation of" |> Option.value ~default:s)
      |> Text.clean
    in
    let date_of_offense = find_prefixed fragments ~prefix:"Date of Offense: " |> Date.of_string in
    let violation_of = description $? "a" |> Option.map ~f:(fun el -> texts el |> String.concat ~sep:" " |> Text.clean) in
    count_as_filed, date_of_offense, violation_of
  | _ -> failwith "Invalid page structure, could not locate distinct count columns for the first line"
  end
  in
  (* Second table *)
  second $$ "tbody tr" |> to_list |> List.map ~f:(fun tr ->
    let disposition, count_as_disposed, violation_of2, party = begin match tr $$ "td" |> to_list with
    | [_td1; td2; td3] ->
      let party = td_party td2 in
      begin match trimmed_texts td3 with
      | td1::td2::rest ->
        let disposition = begin match String.chop_prefix td1 ~prefix:"Disposed:" with
        | Some s -> Some (Text.clean s)
        | None -> failwith "Could not find disposition information"
        end
        in
        let count_as_disposed = begin match String.chop_prefix td2 ~prefix:"Count as Disposed:" with
        | Some s -> Some (Text.clean s)
        | None -> failwith "Could not find count as disposed information"
        end
        in
        let violation_of = begin match rest with
        | ["Violation of"; td4] -> Some (Text.clean td4)
        | [] -> None
        | _ -> failwith "Invalid page structure, end of fragments of second count column for the second line"
        end
        in
        disposition, count_as_disposed, violation_of, party
      | [] ->
        None, None, None, party
      | _ -> failwith "Invalid page structure, fragments of second count column for the second line"
      end
    | _ -> failwith "Invalid page structure, could not locate distinct count columns for the second line"
    end
    in
    let violation_of = begin match violation_of1, violation_of2 with
    | (Some xx as x), (Some yy) when Text.(xx = yy) -> x
    | (Some _), (Some _ as y) -> y
    | (Some _ as x), None -> x
    | None, (Some _ as y) -> y
    | None, None -> None
    end
    in
    {
      party;
      count_as_filed;
      date_of_offense;
      disposition;
      count_as_disposed;
      violation_of;
    }
  )

let process ~name_matcher uri raw =
  let open Soup in
  let html = parse raw in

  (* Process header *)
  let title, case_number, date_filed, date_closed, judge =
    let table = find_section ~html ~section_classes:["styletop"] ~el_type:"table" () in
    begin match table $$ "tbody tr td" |> to_list with
    | [left; right] ->
      let title = (texts left |> String.concat ~sep:" " |> Text.clean) in
      let fragments = trimmed_texts right |> List.to_array in
      let case_number = find_prefixed ~prefix:"No. " fragments |> Text.clean in
      let date_filed = find_prefixed ~prefix:"Filed: " fragments |> Text.clean |> Text.to_string |> Date.of_string in
      let date_closed =
        Array.find_map fragments ~f:(String.chop_prefix ~prefix:"Closed: ")
        |> Option.map ~f:(fun x -> Text.clean x |> Text.to_string |> Date.of_string)
      in
      let judge = find_prefixed ~prefix:"Judge: " fragments |> Text.clean in
      (title, case_number, date_filed, date_closed, judge)
    | _ -> failwith "Invalid page structure, could not find case header info"
    end
  in
  let status = if Option.is_some date_closed then Completed else Open in

  (* Process parties *)
  let parties =
    let p = find_section ~html ~section_classes:["section"; "party"] ~el_type:"p" () in
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
  let is_defendant =
    Array.exists parties ~f:(function
    | { role = Defendant; name; _ } when name_matcher (Oscn.name_to_text name) -> true
    | _ -> false
    )
  in
  let arresting_agency = Array.find_map parties ~f:(function
    | { name; role = Arresting_agency; uri = _ } -> Some (Oscn.name_to_text name)
    | _ -> None
    )
  in

  (* Process events *)
  let events =
    let table = find_section ~html ~section_classes:["section"; "events"] ~el_type:"table" ~empty_type:"p" () in
    table $$ "tbody tr" |> to_list |> Array.of_list_map ~f:(fun row ->
      begin match row $$ "td" |> to_list with
      | [td1; td2; td3; _td4] ->
        let party = td_party td2 in
        begin match trimmed_texts td1 with
        | hd::rest -> {
            party;
            datetime = Oscn.parse_datetime ~section:"event" (Text.clean hd);
            description = String.concat ~sep:" " rest |> String.uppercase |> Text.clean;
            judge = texts td3 |> String.concat ~sep:" " |> Text.clean |> Text.require;
          }
        | [] -> failwith "Invalid events row structure"
        end
      | _ -> failwith "Invalid events table structure"
      end
    )
  in

  (* Process counts *)
  let open_counts = begin try
    begin match html $? "h2.section.counts" with
    | None -> raise (Not_present "Did not find a counts section")
    | Some h2 ->
      process_counts_open [] h2 |> Array.of_list_rev
    end
  with Not_present _ -> [||] end
  in

  let completed_counts = begin match (html $$ "div.CountsContainer" |> to_list), status with
  | [], Open -> [||]
  | [], Completed -> failwith "Invalid page structure, did not find a counts section on a Closed case."
  | containers, _ ->
    let queue = Queue.create () in
    List.iter containers ~f:(fun container ->
      Queue.enqueue_all queue (process_counts_closed container)
    );
    Queue.to_array queue
  end
  in

  (* Process dockets *)
  let dockets =
    let table = find_section ~html ~section_classes:["section"; "dockets"] ~el_type:"table" () in
    table $$ "tbody tr" |> to_list |> Array.of_list_map ~f:(fun row ->
      let rows = row $$ "td" |> to_list in
      begin match rows with
      | [td1; td2; td3; td4; td5; td6] ->
        {
          date = texts td1 |> String.concat ~sep:" " |> Text.clean |> Text.require |> Option.map ~f:(Oscn.parse_date ~section:"docket");
          code = texts td2 |> String.concat ~sep:" " |> Text.clean |> Text.require;
          description = Option.value_map (child_element td3) ~default:"" ~f:(fun x -> texts x |> String.concat ~sep:" ") |> Text.clean;
          links = td3 $$ "a[href]" |> to_list |> List.filter_map ~f:(fun link ->
              attribute "href" link
              |> Option.map ~f:(fun x -> Oscn.make_uri_from_href x |> Uri.to_string)
            );
          count = texts td4 |> String.concat ~sep:" " |> Text.clean |> Text.require;
          party = texts td5 |> String.concat ~sep:" " |> Text.clean |> Text.uppercase |> Text.require;
          amount = texts td6 |> String.concat ~sep:" " |> Text.clean |> Text.require |> Option.map ~f:(fun x -> Text.to_string x |> Money.amount);
        }
      | _ -> failwith "Invalid dockets table structure"
      end
    )
  in

  (* Dockets lookups *)
  let transactions = Array.filter dockets ~f:(fun { amount; _ } -> Option.is_some amount) in
  {
    status;
    uri;
    title;
    parties;
    is_defendant;
    case_number;
    date_filed;
    date_closed;
    judge;
    arresting_agency;
    events;
    transactions;
    open_counts;
    completed_counts;
  }
