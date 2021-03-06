open! Core_kernel
open S

let pool = Lwt_pool.create 2 (fun () -> Lwt.return_unit)

let debug_string_list ll =
  print_endline (`List (List.map ll ~f:(fun s -> `String s)) |> Yojson.Basic.pretty_to_string)

let make_uri_from_href href =
  let no_leading_slash = String.chop_prefix href ~prefix:"/" |> Option.value ~default:href in
  let fixed =
    if String.is_prefix no_leading_slash ~prefix:"dockets/"
    then sprintf "/%s" no_leading_slash
    else sprintf "/dockets/%s" no_leading_slash
  in
  (Uri.of_string fixed |> fun u -> Uri.with_scheme u (Some "https")) |> fun u ->
  Uri.with_host u (Some "www.oscn.net")

let fetch (meth : Cohttp.Code.meth) uri =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let module Body = Cohttp_lwt.Body in
  Lwt_pool.use pool (fun () ->
      let%lwt () = Lwt_unix.sleep 0.5 in
      let debug_uri = Uri.to_string uri in
      let t0 = Time_now.nanoseconds_since_unix_epoch () in
      let%lwt () = Lwt_io.printlf "%s %s" (Code.string_of_method meth) debug_uri in
      let%lwt res, body = Client.call meth uri in
      let t1 = Time_now.nanoseconds_since_unix_epoch () in
      let status = Response.status res in
      let%lwt () =
        Lwt_io.printlf "[%s][%s ms] %s" (Code.string_of_status status)
          Int63.((t1 - t0) / of_int 1_000_000 |> to_string_hum ~delimiter:',')
          debug_uri
      in
      match Code.code_of_status status with
      | 200 -> Body.to_string body
      | _ -> failwithf "The OSCN Dockets website failed with HTTP %s" (Code.string_of_status status) ())

let parse_role = function
| "Appellant" -> Appellant
| "Appellee" -> Appellee
| "Defendant" -> Defendant
| "Plaintiff" -> Plaintiff
| "Other" -> Other
| "ARRESTING OFFICER" -> Arresting_officer
| "ARRESTING AGENCY" -> Arresting_agency
| s -> failwithf "Invalid role: '%s'" s ()

let name_to_text = function
| Full_name { first_name; last_name } ->
  sprintf "%s, %s" (Text.to_string last_name) (Text.to_string first_name) |> Text.clean
| Other_name s -> s

let make_name_matcher { first_name; middle_name; last_name; dob_before = _; dob_after = _ } =
  begin
    match first_name, middle_name with
    | Some fn, Some mn -> Some (sprintf "%s, %s %s" last_name fn mn |> String.uppercase)
    | Some fn, None -> Some (sprintf "%s, %s" last_name fn |> String.uppercase)
    | None, None -> Some (sprintf "%s," last_name |> String.uppercase)
    | _ -> None
  end
  |> function
  | None -> (fun _name -> false)
  | Some needle ->
    let name_matcher left right =
      String.is_prefix ~prefix:left right || String.is_prefix ~prefix:right left
    in
    (fun name -> name_matcher needle (Text.to_string name))

let datetime_regex =
  Re2.create_exn
    "^[A-Z][a-z]+, ([A-Z][a-z]+) ([1-9][0-9]?), ([12][0-9]{3})(?: at ([0-9][0-2]?:[0-5][0-9] ?[AP]?M?))?$"

let date_regex = Re2.create_exn "^([01][0-9])-([0-3][0-9])-([12][0-9]{3})$"

let parse_month = function
| "January" -> Month.Jan
| "February" -> Month.Feb
| "March" -> Month.Mar
| "April" -> Month.Apr
| "May" -> Month.May
| "June" -> Month.Jun
| "July" -> Month.Jul
| "August" -> Month.Aug
| "September" -> Month.Sep
| "October" -> Month.Oct
| "November" -> Month.Nov
| "December" -> Month.Dec
| s -> failwithf "Invalid month: '%s'" s ()

(* Thursday, October 13, 2016 at 1:30PM *)
let parse_datetime ~section text =
  let raw = Text.to_string text in
  match Re2.find_submatches datetime_regex raw with
  | Ok [| _whole; Some m; Some d; Some y; t |] ->
    let date = Date.create_exn ~m:(parse_month m) ~d:(Int.of_string d) ~y:(Int.of_string y) in
    let time =
      Option.bind t ~f:(function
        | "0:00 AM"
         |"0:00AM" ->
          None
        | x -> Option.try_with (fun () -> Time.Ofday.of_string x))
      |> Option.value ~default:Time.Ofday.start_of_day
    in
    Time.of_date_ofday ~zone:Time.Zone.utc date time
  | Ok _
   |Error _ ->
    failwithf "Invalid %s datetime '%s'" section raw ()

(* 08-31-2015 *)
let parse_date ~section text =
  let raw = Text.to_string text in
  let drop_leading_zeroes s =
    String.chop_prefix ~prefix:"0" s |> Option.value ~default:s |> Int.of_string
  in
  match Re2.find_submatches date_regex raw with
  | Ok [| _whole; Some m; Some d; Some y |] ->
    Date.create_exn
      ~m:(m |> drop_leading_zeroes |> Month.of_int_exn)
      ~d:(drop_leading_zeroes d) ~y:(Int.of_string y)
  | Ok _
   |Error _ ->
    failwithf "Invalid %s date '%s'" section raw ()

let prepare_case ~name_matcher case =
  (* Done this way to ensure we get warnings if we miss a field *)
  let {
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
  } =
    case
  in
  match is_defendant with
  | false -> None
  | true ->
    case_to_yojson
      {
        status;
        uri;
        title;
        parties;
        case_number;
        date_filed;
        date_closed;
        judge;
        arresting_agency;
        events =
          Array.filter events ~f:(fun { party; _ } ->
              Option.value_map party ~default:true ~f:name_matcher);
        transactions =
          Array.filter transactions ~f:(fun { party; _ } ->
              Option.value_map party ~default:true ~f:name_matcher);
        open_counts;
        completed_counts =
          Array.filter completed_counts ~f:(fun { party; _ } ->
              Option.value_map party ~default:true ~f:name_matcher);
      }
    |> Option.return

let prepare_data ~name_matcher datas =
  let cases = List.filter_map datas ~f:(fun data -> prepare_case ~name_matcher data) in
  `Assoc [ "cases", `List cases ]
