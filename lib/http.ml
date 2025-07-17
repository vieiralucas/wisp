module Method = struct
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

  let parser =
    let open Parser in
    string "GET"
    |> map (fun _ -> GET)
    <|> (string "POST" |> map (fun _ -> POST))
    <|> (string "PUT" |> map (fun _ -> PUT))
    <|> (string "DELETE" |> map (fun _ -> DELETE))
    <|> (string "HEAD" |> map (fun _ -> HEAD))
    <|> (string "OPTIONS" |> map (fun _ -> OPTIONS))
    <|> (string "PATCH" |> map (fun _ -> PATCH))
    <|> (string "TRACE" |> map (fun _ -> TRACE))
    <|> (string "CONNECT" |> map (fun _ -> CONNECT))
  ;;
end

let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false
;;

let is_alphanumeric c =
  is_digit c
  ||
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_valid_absolute_path_char c =
  is_alphanumeric c
  ||
  match c with
  | '-'
  | '_'
  | '.'
  | '~'
  | '!'
  | '$'
  | '&'
  | '\''
  | '('
  | ')'
  | '*'
  | '+'
  | ','
  | ';'
  | ':'
  | '@'
  | '/' -> true
  | _ -> false
;;

let is_valid_query_char c =
  is_valid_absolute_path_char c
  ||
  match c with
  | '=' | '%' | '#' -> true
  | _ -> false
;;

module OriginForm = struct
  type t = string

  let parser : t Parser.t =
    let open Parser in
    let path =
      char '/' *> take_while is_valid_absolute_path_char
      |> map (fun s -> "/" ^ s)
    in
    let query = optional (char '?' *> take_while is_valid_query_char) in
    let origin_form = path <*> query in
    origin_form
    |> map (fun (path, query_opt) ->
      match query_opt with
      | Some query -> path ^ "?" ^ query
      | None -> path)
  ;;
end

module AbsoluteForm = struct
  type t = string

  let parser : t Parser.t =
    let open Parser in
    string "http://" *> take_while1 is_valid_absolute_path_char
    |> map (fun path -> "http://" ^ path)
    <|> (string "https://" *> take_while1 is_valid_absolute_path_char
         |> map (fun path -> "https://" ^ path))
  ;;
end

module AuthorityForm = struct
  type t =
    { host : string
    ; port : int option
    }
  [@@deriving show, eq]

  let parser : t Parser.t =
    let open Parser in
    take_while1 (fun c -> is_alphanumeric c || c = '-' || c = '.')
    <*> optional (char ':' *> take_while1 is_digit |> map int_of_string)
    |> map (fun (host, port_opt) -> { host; port = port_opt })
  ;;
end

module RequestTarget = struct
  type t =
    | OriginForm of string
    | AbsoluteForm of string
    | AuthorityForm of AuthorityForm.t
    | AsteriskForm
  [@@deriving show, eq]

  let parser : t Parser.t =
    let open Parser in
    string "*"
    |> map (fun _ -> AsteriskForm)
    <|> (OriginForm.parser |> map (fun path -> OriginForm path))
    <|> (AbsoluteForm.parser |> map (fun path -> AbsoluteForm path))
    <|> (AuthorityForm.parser |> map (fun authority -> AuthorityForm authority))
  ;;
end

module Version = struct
  type t =
    | HTTP_1_0
    | HTTP_1_1
    | HTTP_2_0
  [@@deriving show, eq]

  let parser =
    let open Parser in
    string "HTTP/1.0"
    |> map (fun _ -> HTTP_1_0)
    <|> (string "HTTP/1.1" |> map (fun _ -> HTTP_1_1))
    <|> (string "HTTP/2.0" |> map (fun _ -> HTTP_2_0))
  ;;
end

module RequestLine = struct
  type t =
    { meth : Method.t
    ; _target : RequestTarget.t
    ; _version : Version.t
    }
  [@@deriving show, eq]

  let parser =
    let open Parser in
    Method.parser
    <* char ' '
    <*> (RequestTarget.parser <* char ' ')
    <*> Version.parser
    |> map (fun ((meth, target), version) ->
      { meth; _target = target; _version = version })
  ;;

  let meth { meth; _ } = meth
end

module Headers = struct
  type t = (string * string) list [@@deriving show, eq]

  let parser : t Parser.t =
    let open Parser in
    let key = take_while1 (fun c -> c <> ':') |> map String.trim in
    let value =
      take_while1 (fun c -> c <> '\r' && c <> '\n') |> map String.trim
    in
    let header = key <* char ':' <*> value <* string "\r\n" in
    many header
  ;;
end

module Request = struct
  type t =
    { meth : Method.t
    ; headers : Headers.t
    }
  [@@deriving show, eq]

  let parser : t Parser.t =
    let open Parser in
    RequestLine.parser
    <* string "\r\n"
    <*> Headers.parser
    |> map (fun (req, headers) -> { meth = RequestLine.meth req; headers })
  ;;

  let meth { meth; _ } = meth

  let get_header t key =
    List.find_opt
      (fun (k, _) ->
         String.equal (String.lowercase_ascii k) (String.lowercase_ascii key))
      t.headers
    |> Option.map snd
  ;;
end
