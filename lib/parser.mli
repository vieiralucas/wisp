type input

val input_of_bigstring : Bigstringaf.t -> input
val input_of_string : string -> input

type error = string * int

type 'a result =
  | Error of error
  | Ok of ('a * input)

type 'a partial

type 'a step_result =
  | Done of 'a result
  | Partial of 'a partial

type 'a t

val return : 'a -> 'a t
val char : char -> char t
val string : string -> string t
val take_while : (char -> bool) -> string t
val take_while1 : (char -> bool) -> string t
val optional : 'a t -> 'a option t
val many : 'a t -> 'a list t
val run : 'a t -> input -> (unit -> input option) -> 'a result
val remaining_string : input -> string
val ( <*> ) : 'a t -> 'b t -> ('a * 'b) t
val ( <* ) : 'a t -> 'b t -> 'a t
val ( *> ) : 'a t -> 'b t -> 'b t
val ( <|> ) : 'a t -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
