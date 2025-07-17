module Method : sig
  type t =
    | GET
    | POST
    | PUT
    | DELETE
    | HEAD
    | OPTIONS
    | PATCH
    | TRACE
    | CONNECT
  [@@deriving show, eq]
end

module Request : sig
  type t

  val parser : t Parser.t
  val meth : t -> Method.t
  val get_header : t -> string -> string option
end
