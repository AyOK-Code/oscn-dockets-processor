open! Core_kernel
open Cohttp
open Cohttp_lwt_unix
module Body = Cohttp_lwt.Body

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
