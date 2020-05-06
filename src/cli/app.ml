open! Core_kernel

open Liboscn

let () = Lwt.async_exception_hook := (fun ex ->
    let open Lwt in
    Lwt_io.eprintlf "💀 UNCAUGHT EXCEPTION: %s" (Libutils.Exception.human ex)
    >>= (fun () -> exit 2)
    |> ignore
  )
let main = function
| [|_; "run"; first_name; last_name|] ->
  let%lwt results = Search.scrape ~first_name ~last_name () in
  let%lwt case_data = String.Table.fold results ~init:[] ~f:(fun ~key:_ ~data acc ->
      let uri = data.uri in
      let p = Lwt.map (Case.process ~first_name ~last_name uri) (Oscn.fetch uri) in
      p::acc
    ) |> Lwt.all
  in
  let json : Yojson.Safe.t = Oscn.prepare_data ~first_name ~last_name case_data in
  print_endline (Yojson.Safe.pretty_to_string json);
  Lwt.return_unit
| [|_; "serve" |] -> failwith "Unimplemented: serve"
| argv ->
  failwithf "Invalid arguments: %s"
    (`List (Array.fold_right argv ~init:[] ~f:(fun x acc -> (`String x)::acc)) |> Yojson.Basic.to_string) ()

let () =
  Lwt_main.run (
    try%lwt
      main (Sys.argv)
    with
    | (Failure _ as ex) | (Unix.Unix_error _ as ex) | (Exn.Reraised _ as ex) ->
      let message = Libutils.Exception.human ex in
      let%lwt () = Lwt_io.eprintlf "❌ An error occured:\n%s" message in
      exit 1
  )
