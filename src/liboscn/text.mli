open! Core_kernel

type t [@@deriving to_yojson]

val clean: string -> t

val require: t -> t option

val to_string: t -> string
