open! Core_kernel
open S

exception Not_present of string

let find_prefixed ~prefix strs =
  match Array.find_map strs ~f:(String.chop_prefix ~prefix) with
  | None -> failwithf "Unable to find '%s'" prefix ()
  | Some s -> s

let find_section ~html ~section_classes ~el_type ?empty_type () =
  let open Soup.Infix in
  let section_class = List.map section_classes ~f:(fun s -> sprintf ".%s" s) |> String.concat in
  match html $? sprintf "h2%s" section_class with
  | None ->
    raise (Not_present (sprintf "Invalid page structure, could not find '%s' header" section_class))
  | Some h2 -> (
    match Soup.next_element h2, empty_type with
    | Some el, _ when String.(Soup.name el = el_type) -> el
    | Some el, Some empty when String.(Soup.name el = empty) -> Soup.create_element el_type
    | (None as next), _
     |(Some _ as next), _ ->
      raise
        (Not_present
           (sprintf "Invalid page structure, could not find '%s' data (was %s instead of '%s')"
              section_class
              (Option.value_map next ~default:"not found" ~f:(fun x -> sprintf "'%s'" (Soup.name x)))
              el_type))
  )

let td_party ?(f = Fn.id) td = Soup.texts td |> String.concat ~sep:" " |> f |> Text.clean |> Text.require

let list_regex = Re2.create_exn ", "

let process_party text uri =
  match Re2.split list_regex (Text.to_string text) |> List.to_array with
  | [| last; first; role |] ->
    Some
      {
        name =
          Full_name
            {
              first_name = Text.clean (String.uppercase first);
              last_name = Text.clean (String.uppercase last);
            };
        role = Oscn.parse_role role;
        uri;
      }
  | [| other; role |] ->
    Some { name = Other_name (Text.clean (String.uppercase other)); role = Oscn.parse_role role; uri }
  | [| "" |] -> None
  | _ -> failwithf "Invalid parties entry: '%s'" (Text.to_string text) ()

let rec process_counts_open acc el =
  let p = Soup.next_element el |> Option.filter ~f:(fun x -> String.(Soup.name x = "p")) in
  match Option.bind p ~f:(fun x -> Soup.texts x |> List.last) with
  | None -> acc
  | Some s -> (
    let p_el = Option.value_exn ~message:"Faulty assumption in counts processing" p in
    match Text.clean s |> Text.require with
    | None -> process_counts_open acc p_el
    | Some description -> process_counts_open ({ description } :: acc) p_el
  )

let process_counts_closed div =
  let open Soup.Infix in
  let first, second =
    match div $$ "table" |> Soup.to_list with
    | [ first; second ] when List.mem (Soup.classes first) "Counts" ~equal:String.equal -> first, second
    | _ -> failwith "Invalid page structure, could not locate distinct count lines"
  in
  (* First table *)
  let count_as_filed, date_of_offense, violation_of1 =
    match first $$ "tbody tr td" |> Soup.to_list with
    | [ number; description ] ->
      begin
        match Soup.texts number with
        | s :: _ when String.is_prefix s ~prefix:"Count #" -> ()
        | s :: _ -> failwithf "Invalid first cell value for count first line: '%s'" s ()
        | _ -> failwith "Empty first cell value for count first line"
      end;
      let fragments = Soup.trimmed_texts description |> List.to_array in
      let count_as_filed =
        find_prefixed fragments ~prefix:"Count as Filed:"
        |> Text.clean
        |> Text.to_string
        |> (fun s -> String.chop_suffix s ~suffix:", in violation of" |> Option.value ~default:s)
        |> Text.clean
      in
      let date_of_offense = find_prefixed fragments ~prefix:"Date of Offense: " |> Date.of_string in
      let violation_of =
        description
        $? "a"
        |> Option.map ~f:(fun el -> Soup.texts el |> String.concat ~sep:" " |> Text.clean)
      in
      count_as_filed, date_of_offense, violation_of
    | _ -> failwith "Invalid page structure, could not locate distinct count columns for the first line"
  in
  (* Second table *)
  second
  $$ "tbody tr"
  |> Soup.to_list
  |> List.map ~f:(fun tr ->
         let disposition, count_as_disposed, violation_of2, party =
           match tr $$ "td" |> Soup.to_list with
           | [ _td1; td2; td3 ] -> (
             let party = td_party ~f:String.uppercase td2 in
             match Soup.trimmed_texts td3 with
             | td1 :: td2 :: rest ->
               let disposition =
                 match String.chop_prefix td1 ~prefix:"Disposed:" with
                 | Some s -> Some (Text.clean s)
                 | None -> failwith "Could not find disposition information"
               in
               let count_as_disposed =
                 match String.chop_prefix td2 ~prefix:"Count as Disposed:" with
                 | Some s -> Some (Text.clean s)
                 | None -> failwith "Could not find count as disposed information"
               in
               let violation_of =
                 match rest with
                 | [ "Violation of"; td4 ] -> Some (Text.clean td4)
                 | [] -> None
                 | _ ->
                   failwith
                     "Invalid page structure, end of fragments of second count column for the second line"
               in
               disposition, count_as_disposed, violation_of, party
             | [] -> None, None, None, party
             | _ ->
               failwith "Invalid page structure, fragments of second count column for the second line"
           )
           | _ ->
             failwith
               "Invalid page structure, could not locate distinct count columns for the second line"
         in
         let violation_of =
           match violation_of1, violation_of2 with
           | (Some xx as x), Some yy when Text.(xx = yy) -> x
           | Some _, (Some _ as y) -> y
           | (Some _ as x), None -> x
           | None, (Some _ as y) -> y
           | None, None -> None
         in
         { party; count_as_filed; date_of_offense; disposition; count_as_disposed; violation_of })

let process ~name_matcher uri raw =
  let open Soup.Infix in
  let html = Soup.parse raw in

  (* Process header *)
  let title, case_number, date_filed, date_closed, judge =
    let table = find_section ~html ~section_classes:[ "styletop" ] ~el_type:"table" () in
    match table $$ "tbody tr td" |> Soup.to_list with
    | [ left; right ] ->
      let title = Soup.texts left |> String.concat ~sep:" " |> Text.clean in
      let fragments = Soup.trimmed_texts right |> List.to_array in
      let case_number = find_prefixed ~prefix:"No. " fragments |> Text.clean in
      let date_filed =
        find_prefixed ~prefix:"Filed: " fragments |> Text.clean |> Text.to_string |> Date.of_string
      in
      let date_closed =
        Array.find_map fragments ~f:(String.chop_prefix ~prefix:"Closed: ")
        |> Option.map ~f:(fun x -> Text.clean x |> Text.to_string |> Date.of_string)
      in
      (* TODO: use find-prefixed on nested match *)
      let judge_or_appellate =
        begin
          match Array.find_map fragments ~f:(String.chop_prefix ~prefix:"Judge: ") with
          | Some s -> s
          | None -> (
            match Array.find_map fragments ~f:(String.chop_prefix ~prefix:"Appealed from: ") with
            | Some s -> s
            | None -> failwith "Unable to find Judge or Appellate Court"
          )
        end
        |> Text.clean
      in
      title, case_number, date_filed, date_closed, judge_or_appellate
    | _ -> failwith "Invalid page structure, could not find case header info"
  in
  let status = if Option.is_some date_closed then Completed else Open in

  (* Process parties *)
  let parties =
    let p = find_section ~html ~section_classes:[ "section"; "party" ] ~el_type:"p" () in
    let groups =
      Soup.descendants p
      |> Soup.to_list
      |> List.group ~break:(fun _left right ->
             Option.value_map (Soup.element right) ~default:false ~f:(fun el ->
                 String.(Soup.name el = "br")))
    in
    let queue = Queue.create () in
    List.iter groups ~f:(fun group ->
        let buf = Buffer.create 16 in
        let person_href =
          List.fold group ~init:None ~f:(fun acc node ->
              match Soup.element node with
              | None ->
                Buffer.add_string buf (Soup.to_string node);
                acc
              | Some el when String.(Soup.name el = "a") ->
                Option.filter (Soup.attribute "href" el) ~f:(fun s -> not (String.is_empty s))
              | Some _ -> None)
        in
        process_party
          (Buffer.contents buf |> Text.clean)
          (Option.map person_href ~f:Oscn.make_uri_from_href)
        |> Option.iter ~f:(Queue.enqueue queue));
    Queue.to_array queue
  in

  (* Parties lookups *)
  let is_defendant =
    Array.exists parties ~f:(function
      | { role = Defendant; name; _ } when name_matcher (Oscn.name_to_text name) -> true
      | _ -> false)
  in
  let arresting_agency =
    Array.find_map parties ~f:(function
      | { name; role = Arresting_agency; uri = _ } -> Some (Oscn.name_to_text name)
      | _ -> None)
  in

  (* Process events *)
  let events =
    let table =
      find_section ~html ~section_classes:[ "section"; "events" ] ~el_type:"table" ~empty_type:"p" ()
    in
    table
    $$ "tbody tr"
    |> Soup.to_list
    |> Array.of_list_map ~f:(fun row ->
           match row $$ "td" |> Soup.to_list with
           | [ td1; td2; td3; _td4 ] ->
             let party = td_party ~f:String.uppercase td2 in
             let datetime =
               let open Option.Monad_infix in
               td1 $? "font" >>= td_party >>| Oscn.parse_datetime ~section:"event"
             in
             let description =
               let tt = Soup.trimmed_texts td1 in
               (match datetime with
               | Some _dt -> List.slice tt 1 0
               | None -> tt)
               |> String.concat ~sep:" "
               |> String.uppercase
               |> Text.clean
             in
             {
               party;
               datetime;
               description;
               judge = Soup.texts td3 |> String.concat ~sep:" " |> Text.clean |> Text.require;
             }
           | _ -> failwith "Invalid events table structure")
    |> Array.filter ~f:(fun event ->
           let empty = Text.clean "" in
           match event with
           | { party = None; datetime = None; description; judge = None }
             when Text.( = ) description empty ->
             false
           | _ -> true)
  in

  (* Process counts *)
  let open_counts =
    try
      match html $? "h2.section.counts" with
      | None -> raise (Not_present "Did not find a counts section")
      | Some h2 -> process_counts_open [] h2 |> Array.of_list_rev
    with
    | Not_present _ -> [||]
  in

  let completed_counts =
    match html $$ "div.CountsContainer" |> Soup.to_list, status with
    | [], Open -> [||]
    | [], Completed when Option.is_some (html $? "h2.section.counts") -> [||]
    | [], Completed -> failwith "Invalid page structure, did not find a counts section on a Closed case."
    | containers, _ ->
      let queue = Queue.create () in
      List.iter containers ~f:(fun container -> Queue.enqueue_all queue (process_counts_closed container));
      Queue.to_array queue
  in

  (* Process dockets *)
  let dockets =
    let table = find_section ~html ~section_classes:[ "section"; "dockets" ] ~el_type:"table" () in
    table
    $$ "tbody tr"
    |> Soup.to_list
    |> Array.of_list_map ~f:(fun row ->
           let rows = row $$ "td" |> Soup.to_list in
           match rows with
           | [ td1; td2; td3; td4; td5; td6 ] ->
             {
               date =
                 Soup.texts td1
                 |> String.concat ~sep:" "
                 |> Text.clean
                 |> Text.require
                 |> Option.map ~f:(Oscn.parse_date ~section:"docket");
               code = Soup.texts td2 |> String.concat ~sep:" " |> Text.clean |> Text.require;
               description =
                 Option.value_map (Soup.child_element td3) ~default:"" ~f:(fun x ->
                     Soup.texts x |> String.concat ~sep:" ")
                 |> Text.clean;
               links =
                 td3
                 $$ "a[href]"
                 |> Soup.to_list
                 |> List.filter_map ~f:(fun link ->
                        Soup.attribute "href" link
                        |> Option.map ~f:(fun x -> Oscn.make_uri_from_href x |> Uri.to_string));
               count = Soup.texts td4 |> String.concat ~sep:" " |> Text.clean |> Text.require;
               party =
                 Soup.texts td5 |> String.concat ~sep:" " |> Text.clean |> Text.uppercase |> Text.require;
               amount =
                 Soup.texts td6
                 |> String.concat ~sep:" "
                 |> Text.clean
                 |> Text.require
                 |> Option.map ~f:(fun x -> Text.to_string x |> Money.amount);
             }
           | _ -> failwith "Invalid dockets table structure")
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
