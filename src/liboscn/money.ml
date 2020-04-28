open! Core_kernel

type t = string [@@deriving to_yojson]

let money_regex = Re2.create_exn "^\\$ (-?[0-9]+.[0-9]{2})$"

let amount s =
  begin match Re2.find_first ~sub:(`Index 1) money_regex s with
  | Ok x -> x
  | Error _ -> failwithf "Invalid amount: '%s'" s ()
  end
