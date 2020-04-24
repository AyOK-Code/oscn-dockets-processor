open! Core_kernel

let () = Lwt.async_exception_hook := (fun ex ->
    let open Lwt in
    Lwt_io.eprintlf "💀 UNCAUGHT EXCEPTION: %s" (Libutils.Exception.human ex)
    >>= (fun () -> exit 2)
    |> ignore
  )
let main () =
  let uri = Liboscn.Oscn.make_uri_from_href "GetCaseInformation.aspx?db=oklahoma&number=TR-1991-3731&cmid=180879" in

  Liboscn.Case.scrape ~first_name:"robert" ~last_name:"johnson" uri

(* let%lwt results = Liboscn.Search.scrape ~last_name:"Johnson" ~first_name:"Bobby" () in *)
(* let j = `Assoc (String.Table.fold results ~init:[] ~f:(fun ~key ~data acc -> *)
(* (key, (Liboscn.Search.search_result_to_yojson data))::acc *)
(* )) *)
(* in *)
(* Lwt_io.printl (Yojson.Safe.pretty_to_string j |> (fun x -> String.slice x 0 200)) *)

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
