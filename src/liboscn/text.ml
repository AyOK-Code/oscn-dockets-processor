open! Core_kernel

type t = string [@@deriving to_yojson]

let escape_table = function
| "nbsp" -> " "
| "amp" -> "&"
| "lt" -> "<"
| "gt" -> ">"
| "apos" -> "'"
| "quot" -> "\""
| s ->
  begin match String.chop_prefix ~prefix:"#" s with
  | None -> sprintf "&%s;" s
  | Some num ->
    try begin Int.of_string num |> Char.of_int_exn |> Char.to_string end
    with _ -> sprintf "&%s;" s
  end

let escape_regex = Re2.create_exn "&(.*?);"
let ws_regex = Re2.create_exn (sprintf "(?:\\s|\u{00A0})+")

let clean s =
  Re2.replace_exn escape_regex s ~f:(fun m ->
    Re2.Match.get_exn m ~sub:(`Index 1) |> escape_table
  )
  |> Re2.replace_exn ws_regex ~f:(fun _m -> " ")
  |> String.strip

let require text = Option.some_if (String.is_empty text |> not) text

let to_string = Fn.id
