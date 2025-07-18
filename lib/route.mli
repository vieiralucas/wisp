type t

val get : string -> Handler.t -> t
val post : string -> Handler.t -> t
val put : string -> Handler.t -> t
val delete : string -> Handler.t -> t
val patch : string -> Handler.t -> t
val options : string -> Handler.t -> t
