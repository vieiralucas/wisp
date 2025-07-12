module Method : sig
  type t =
    | GET
    | POST
    | PUT
    | DELETE
    | PATCH
    | OPTIONS
    | HEAD
  [@@deriving eq, show]

  type error = InvalidMethod of string
end

module Path : sig
  type error =
    | PathEmpty
    | PathTooLong
    | PathInvalid of string
  [@@deriving eq, show]
end

type t

type error =
  | MethodError of Method.error
  | PathError of Path.error

val parse : 'a Eio.Flow.source -> (t, error) result
val meth : t -> Method.t
val get_header : t -> string -> string option
val path : t -> string
val str_body : t -> string
