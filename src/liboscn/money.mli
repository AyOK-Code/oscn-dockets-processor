open! Core_kernel

type t [@@deriving to_yojson]

val amount: string -> t
