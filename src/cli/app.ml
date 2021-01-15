open! Core

open Liboscn

let () = Lwt.async_exception_hook := (fun ex ->
    let open Lwt in
    Lwt_io.eprintlf "💀 UNCAUGHT EXCEPTION: %s" (Libutils.Exception.human ex)
    >>= (fun () -> exit 2)
    |> ignore
  )

let exec_search request =
  let name_matcher = Oscn.make_name_matcher request in
  let%lwt results = Search.scrape request () in
  let%lwt case_data =
    Lwt_list.filter_map_p
      (fun key ->
        let Search.{ uri; _ } = String.Table.find_exn results key in
        Lwt.catch
        (fun () -> let%lwt data = Lwt.map (Case.process ~name_matcher uri) (Oscn.fetch `GET uri) in
        Lwt.return_some data )
        (fun exn -> raise (Exn.reraisef exn "Exception for URI '%s'" (Uri.to_string uri) ()))
        )
      (String.Table.keys results)
  in
  let json : Yojson.Safe.t = Oscn.prepare_data ~name_matcher case_data in
  Lwt.return json

let exec_case S.{ person = request; case_uri = uri} =
  let name_matcher = Oscn.make_name_matcher request in
  let%lwt raw = Oscn.fetch `GET uri in
  let case_data = Case.process ~name_matcher uri raw in
  let json : Yojson.Safe.t = `Assoc [
      "case_data", (Oscn.prepare_case ~name_matcher case_data |> Option.value ~default:(`Null))
    ]
  in
  Lwt.return json

let help_message = {s|Usage methods:
1. search LAST_NAME [FIRST_NAME [MIDDLE_NAME]]
2. case URI LAST_NAME [FIRST_NAME [MIDDLE_NAME]]
3. serve|s}

let main = function
| [_; "--help"] -> Lwt_io.printl help_message

| _::"search"::ln::rest ->
  let last_name = ln in
  let first_name, middle_name = begin match rest with
  | []
  | ""::[]
  | ""::""::_ -> None, None
  | x::[] -> Some x, None
  | x::y::_ -> Some x, Some y
  end
  in
  let request = S.{ first_name; middle_name; last_name; dob_before = None; dob_after = None } in
  let%lwt out = Lwt.map Yojson.Safe.to_string (exec_search request) in
  let%lwt () = Lwt_io.with_file ~flags:Unix.[O_WRONLY; O_NONBLOCK; O_TRUNC; O_CREAT] ~mode:Output "cases.json" (fun oc ->
      Lwt_io.write oc out
    )
  in
  Lwt_io.printl "Successfully wrote cases.json"

| _::"case"::uri_str::ln::rest ->
  let last_name = ln in
  let first_name, middle_name = begin match rest with
  | []
  | ""::[]
  | ""::""::_ -> None, None
  | x::[] -> Some x, None
  | x::y::_ -> Some x, Some y
  end
  in
  let request = S.{ first_name; middle_name; last_name; dob_before = None; dob_after = None } in
  let uri = Uri.of_string uri_str in
  let%lwt out = Lwt.map Yojson.Safe.to_string (exec_case { person = request; case_uri = uri }) in
  let%lwt () = Lwt_io.with_file ~flags:Unix.[O_WRONLY; O_NONBLOCK; O_TRUNC; O_CREAT] ~mode:Output "case.json" (fun oc ->
      Lwt_io.write oc out
    )
  in
  Lwt_io.printl "Successfully wrote case.json"

| [_; "serve"] ->
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let module Body = Cohttp_lwt.Body in
  let exception Http_ok of string in
  let exception Http_bad_request of string in
  let exception Http_not_found of string in
  let exception Http_unauthorized of string in
  let port = begin match Sys.getenv "PORT" with
  | Some s -> Int.of_string s
  | None -> failwith "Missing environment variable: PORT"
  end
  in
  let api_key = begin match Sys.getenv "API_KEY" with
  | Some s -> s
  | None -> failwith "Missing environment variable: API_KEY"
  end
  in
  let timeout = 60 in
  let on_exn exn =
    Lwt.async (fun () -> Raygun.send exn);
    Lwt.async (fun () ->
      Lwt_io.eprintlf "❌ Unexpected exception during HTTP lifecycle: %s" (Libutils.Exception.human exn)
    )
  in
  let reply t0 code req ~body ?message () =
    let t1 = Time_now.nanoseconds_since_unix_epoch () in
    let status = Code.status_of_code code in
    let%lwt () = Lwt_io.write_line Lwt_io.stdout (
        sprintf "[%d] [%s]%s %s"
          code
          Int63.((t1 - t0) / (of_int 1_000_000) |> to_string_hum ~delimiter:',')
          (Header.get (Request.headers req) "x-forwarded-for" |> Option.value_map ~f:(sprintf " [%s]") ~default: "")
          (Option.value ~default:body message)
      )
    in
    begin match String.is_empty body with
    | true ->
      let body = Body.empty in
      Server.respond ~flush:true ~status ~body ()
    | false ->
      let headers = Header.of_list [
          "Content-Length", (String.length body |> succ |> Int.to_string);
          "Content-Type", "application/json";
        ]
      in
      let body = Body.of_string_list [body; "\n"] in
      Server.respond ~flush:true ~status ~headers ~body ()
    end
  in
  let callback _conn req req_body =
    let t0 = Time_now.nanoseconds_since_unix_epoch () in
    let path = Request.uri req |> Uri.path in
    let meth = Request.meth req in
    try%lwt
      (* HTTP validations *)
      let kind = begin match meth, path with
      | `GET, "/health" -> raise (Http_ok "OK")
      | `POST, "/search" -> `Search
      | `POST, "/case" -> `Case
      | _ -> raise (Http_not_found (sprintf "Invalid endpoint or method: %s '%s'" (Code.string_of_method meth) path))
      end
      in
      begin match meth, Header.get (Request.headers req) "api-key" with
      | `POST, Some passed when String.(=) api_key passed -> ()
      | `POST, Some _ -> raise (Http_unauthorized "Invalid 'api-key' header")
      | `POST, None -> raise (Http_unauthorized "HTTP header 'api-key' is required")
      | meth, _ -> raise (Http_bad_request (sprintf "HTTP method must be POST, received: %s" (Code.string_of_method meth)))
      end;

      (* Serve request *)
      let parse_request p raw =
        begin match Yojson.Safe.from_string raw |> p with
        | Ok x -> x
        | Error msg -> failwith msg
        end
      in
      let%lwt body =
        let%lwt raw = Body.to_string req_body in
        begin match kind with
        | `Search ->
          let request = parse_request Liboscn.S.search_request_of_yojson raw in
          Lwt.map Yojson.Safe.to_string (exec_search request)
        | `Case ->
          let request = parse_request Liboscn.S.case_request_of_yojson raw in
          Lwt.map Yojson.Safe.to_string (exec_case request)
        end
      in

      reply t0 200 req
        ~body
        ~message:(sprintf "OK %s" path)
        ()
    with
    | Http_ok body -> reply t0 200 req ~body ()
    | Http_bad_request body -> reply t0 400 req ~body ()
    | Http_unauthorized body -> reply t0 401 req ~body ()
    | Http_not_found body -> reply t0 404 req ~body ()
    | (Failure body as exn) ->
      let message = sprintf "❌ Exception during %s: %s" path body in
      Lwt.async (fun () -> Raygun.send ~req ~res_status:400 exn);
      reply t0 400 req ~body ~message ()
    | Lwt.Canceled ->
      let message = sprintf "Timed out after %d seconds." timeout in
      let%lwt () = Lwt_io.write_line Lwt_io.stdout message in
      failwith message
    | exn ->
      let message = sprintf "🔥 Unexpected error during %s: %s" path (Libutils.Exception.human exn) in
      Lwt.async (fun () -> Raygun.send ~req ~res_status:500 exn);
      reply t0 500 req ~body:"" ~message ()
  in
  let%lwt ctx = Conduit_lwt_unix.init ~src:"0.0.0.0" () in
  let server = Server.make ~callback () in
  let%lwt () = Lwt_io.printlf "Server started on port %d" port in
  Server.create
    ~timeout
    ~ctx:(Net.init ~ctx ())
    ~mode:(`TCP (`Port port))
    ~on_exn
    server

| argv ->
  failwithf "Invalid arguments: %s\n%s"
    (`List (List.map argv ~f:(fun x -> `String x)) |> Yojson.Basic.to_string) help_message ()

let () =
  Lwt_main.run (
    try%lwt
      main (Sys.get_argv () |> Array.to_list)
    with
    | (Failure _ as ex) | (Unix.Unix_error _ as ex) | (Exn.Reraised _ as ex) ->
      let message = Libutils.Exception.human ex in
      let%lwt () = Lwt_io.eprintlf "❌ An error occured:\n%s" message in
      exit 1
  )
