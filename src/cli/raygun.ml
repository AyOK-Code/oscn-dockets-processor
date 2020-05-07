open Core
open Cohttp
open Cohttp_lwt_unix
module Body = Cohttp_lwt.Body

let raygun_api_key = lazy (Sys.getenv "RAYGUN_API_KEY")
let raygun_additional_tags = lazy (
  Sys.getenv "RAYGUN_TAGS"
  |> Option.value_map ~default:[`String "OSCN Scraper"] ~f:(fun env ->
    String.split ~on:',' env
    |> List.filter_map ~f:(fun s ->
      let v = String.strip s in
      Option.some_if (String.is_empty v |> not) (`String v)
    )
    |> List.cons (`String "OSCN Scraper")
  )
)

let stacktrace_regex = Re2.create_exn {s|(file "(.*?)").*(line ([0-9]+)).*(characters ([0-9]+))|s}
let machine_name_promise = Lwt_unix.gethostname ()

let serialize ~tags ~custom ~machine_name ?req ?res_status exn =
  let string_or_null = Option.value_map ~f:(fun x -> `String x) ~default:`Null in
  let occured_on = Time.now () |> Time.to_string_iso8601_basic ~zone:Time.Zone.utc in

  let raw_stacktrace = Backtrace.get () |> Backtrace.to_string_list in
  let grouping_key = Libutils.Exception.full exn |> Md5.digest_string |> Md5.to_hex in
  let message = Libutils.Exception.human exn in
  let raw_message = Exn.to_string_mach exn in
  let class_name = begin match Sexp.of_string raw_message with
  | Sexp.List (Sexp.Atom name::_) -> name
  | _  | exception _ -> raw_message
  end
  in
  let stacktrace : Yojson.Basic.t list = List.map raw_stacktrace ~f:(fun line ->
      let default = `Assoc [
          "lineNumber", `Int 0;
          "columnNumber", `Int 0;
          "fileName", `String ".";
          "className", `String line;
          "methodName", `String ".";
        ]
      in
      begin match Re2.get_matches stacktrace_regex line with
      | Ok [] | Error _ -> default
      | Ok (first::_) ->
        begin match Re2.Match.get_all (Re2.without_trailing_none first) with
        | [|_whole; _file_anchor; file; _line_anchor; line; _col_anchor; col |] ->
          `Assoc [
            "lineNumber", `Int (Option.value_map line ~default:0 ~f:Int.of_string);
            "columnNumber", `Int (Option.value_map col ~default:0 ~f:Int.of_string);
            "fileName", `String (Option.value ~default:"." file);
            "className", `String ".";
            "methodName", `String ".";
          ]
        | _ -> default
        end
      end
    )
  in
  let request = begin match req with
  | None -> `Assoc []
  | Some req ->
    let uri = Request.uri req in
    let headers = Request.headers req in
    let from_multi_list = List.map ~f:(fun (k, v) -> k, `String (String.concat ~sep:", " v)) in
    let from_list = List.map ~f:(fun (k, v) -> k, `String v) in
    `Assoc [
      "hostName", (Uri.host uri |> string_or_null);
      "url", `String (Uri.path uri);
      "httpMethod", `String (Request.meth req |> Code.string_of_method);
      "iPAddress", (Header.get headers "x-forwarded-for" |> string_or_null);
      "queryString", `Assoc (Uri.query uri |> from_multi_list);
      "headers", `Assoc (Header.to_list headers |> from_list);
      "form", `Null;
      "rawData", `Null (* Sensitive *)
    ]
  end
  in
  let json = `Assoc [
      "occurredOn", `String occured_on;
      "details", `Assoc [
        "machineName", `String machine_name;
        "groupingKey", `String grouping_key;
        "version", `String "0.x";
        "client", `Assoc [
          "name", `String "OSCN Scraper";
          "version", `String "0.x";
          "clientUrl", `String "https://asemio.com"
        ];
        "error", `Assoc [
          "innerError", `Assoc [];
          "data", `Assoc [];
          "className", `String class_name;
          "message", `String message;
          "stackTrace", `List stacktrace;
        ];
        "environment", `Assoc [];
        "tags", `List (List.concat_no_order [(List.map tags ~f:(fun x -> `String x)); Lazy.force raygun_additional_tags]);
        "userCustomData", custom;
        "request", request;
        "response", Option.value_map res_status ~default:(`Assoc []) ~f:(fun status ->
          `Assoc ["statusCode", `Int status]
        );
        "user", (`Assoc []);
        "breadcrumbs", (`Assoc [])
      ]
    ]
  in
  json

let send ?(tags = []) ?(custom = `Assoc []) ?req ?res_status exn =
  begin match Lazy.force raygun_api_key with
  | None -> Lwt.return_unit
  | Some raygun_api_key ->
    let%lwt machine_name = machine_name_promise in
    let json = serialize ~tags ~custom ~machine_name ?req ?res_status exn in
    let headers = Header.of_list ["X-ApiKey", raygun_api_key] in
    let body = Body.of_string (Yojson.Basic.to_string json) in
    let uri = Uri.of_string "https://api.raygun.com/entries" in

    let%lwt res, body = Client.post ~body ~headers uri in
    let%lwt raw = Body.to_string body in

    begin match Response.status res |> Code.code_of_status with
    | 202 -> Lwt_io.write_line Lwt_io.stdout "💾 Exception successfully saved to Raygun"
    | status ->
      failwithf "❗ Failed to save exception to Raygun (HTTP %d):\n%s\n%s"
        status (Response.headers res |> Header.to_frames |> String.concat ~sep:"\n") raw ()
    end
  end
