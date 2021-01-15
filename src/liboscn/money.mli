open! Core_kernel

type t [@@deriving to_yojson]

val amount : string -> t

val is_negative : t -> bool
