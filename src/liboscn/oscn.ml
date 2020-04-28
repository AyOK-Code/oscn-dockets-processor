open! Core_kernel

open S

let pool = Lwt_pool.create 2 (fun () -> Lwt.return_unit)

let make_uri_from_href href =
  let no_leading_slash = String.chop_prefix href ~prefix:"/" |> Option.value ~default:href in
  let fixed =
    if String.is_prefix no_leading_slash ~prefix:"dockets/"
    then sprintf "/%s" no_leading_slash
    else sprintf "/dockets/%s" no_leading_slash
  in
  Uri.of_string fixed
  |> (fun u -> Uri.with_scheme u (Some "https"))
  |> (fun u -> Uri.with_host u (Some "www.oscn.net"))

let real_fetch uri =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let module Body = Cohttp_lwt.Body in
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

let parse_role = function
| "Defendant" -> Defendant
| "Plaintiff" -> Plaintiff
| "ARRESTING OFFICER" -> Arresting_officer
| "ARRESTING AGENCY" -> Arresting_agency
| s -> failwithf "Invalid role: '%s'" s ()

let name_to_text = function
| Full { first_name; last_name } -> sprintf "%s, %s" (Html.text_to_string last_name) (Html.text_to_string first_name) |> Html.clean
| Other_name s -> s

let datetime_regex = Re2.create_exn "^[A-Z][a-z]+, ([A-Z][a-z]+) ([1-9][0-9]?), ([12][0-9]{3})(?: at ([1-9][0-2]?:[0-5][0-9][AP]M))?$"

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

let parse_datetime text =
  let raw = Html.text_to_string text in
  begin match Re2.find_submatches datetime_regex raw with
  | Ok [|_whole; Some m; Some d; Some y; t|] ->
    let date = Date.create_exn ~m:(parse_month m) ~d:(Int.of_string d) ~y:(Int.of_string y) in
    let time = Option.value_map t ~default:Time.Ofday.start_of_day ~f:Time.Ofday.of_string  in
    Time.of_date_ofday ~zone:Time.Zone.utc date time
  | Ok _ -> failwithf "Invalid datetime '%s'" raw ()
  | Error err -> failwith (Error.to_string_hum err)
  end
