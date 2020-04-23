open! Core_kernel

type text [@@deriving to_yojson]

val clean: string -> text

val text_to_string: text -> string
