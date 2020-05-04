open! Core_kernel

let () = Lwt.async_exception_hook := (fun ex ->
    let open Lwt in
    Lwt_io.eprintlf "💀 UNCAUGHT EXCEPTION: %s" (Libutils.Exception.human ex)
    >>= (fun () -> exit 2)
    |> ignore
  )
let main () =
  let uri = Liboscn.Oscn.make_uri_from_href "GetCaseInformation.aspx?db=oklahoma&number=TR-1991-3731&cmid=180879" in

  let%lwt case = Liboscn.Case.scrape ~first_name:"daniel" ~last_name:"laughlin" uri in
  print_endline (Yojson.Safe.pretty_to_string (Liboscn.Oscn.prepare_data ~first_name:"daniel" ~last_name:"laughlin" [case]));
  Lwt.return_unit

let () =
  Lwt_main.run (
    try%lwt
      main ()
    with
    | (Failure _ as ex) | (Unix.Unix_error _ as ex) | (Exn.Reraised _ as ex) ->
      let message = Libutils.Exception.human ex in
      let%lwt () = Lwt_io.eprintlf "❌ An error occured:\n%s" message in
      exit 1
  )
