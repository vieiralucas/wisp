let ( let* ) = Result.bind
let chunk_size = 4006

module Method = struct
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

  let parse buf =
    let three = Eio.Buf_read.take 3 buf in
    match three with
    | "GET" -> Ok GET
    | "PUT" -> Ok PUT
    | str ->
      let four = str ^ Eio.Buf_read.take 1 buf in
      (match four with
       | "POST" -> Ok POST
       | "HEAD" -> Ok HEAD
       | str ->
         let five = str ^ Eio.Buf_read.take 1 buf in
         (match five with
          | "PATCH" -> Ok PATCH
          | str ->
            let six = str ^ Eio.Buf_read.take 1 buf in
            (match six with
             | "DELETE" -> Ok DELETE
             | str ->
               let seven = str ^ Eio.Buf_read.take 1 buf in
               (match seven with
                | "OPTIONS" -> Ok OPTIONS
                | str -> Error (InvalidMethod str)))))
  ;;
end

type t =
  { meth : Method.t
  ; path : string
  ; headers : (string * string) list
  ; body : Eio.Buf_read.t
  }

module Path = struct
  type state =
    | Start
    | Path

  type error =
    | PathEmpty
    | PathTooLong
    | PathInvalid of string
  [@@deriving eq, show]

  let validate_path str =
    (* check if path is in the origin form *)
    if String.starts_with str ~prefix:"/"
    then Ok str (* check if path is absolute form *)
    else if
      String.starts_with str ~prefix:"http://"
      || String.starts_with str ~prefix:"https://"
    then Ok str (* check if path is authority form *)
    else if String.contains str ':'
    then Ok str (* check if path is asterisk form *)
    else if str = "*"
    then Ok str
    else Error (PathInvalid str)
  ;;

  let parse buf =
    let buffer = Buffer.create 128 in
    let rec loop state n =
      if n > 8096
      then Error PathTooLong
      else (
        match Eio.Buf_read.peek_char buf with
        | None ->
          (* EOF *)
          begin
            match state with
            | Start -> Error PathEmpty
            | Path -> Ok (Buffer.contents buffer)
          end
        | Some c ->
          Eio.Buf_read.consume buf 1;
          let n = n + 1 in
          (match state with
           | Start ->
             if c = ' '
             then loop Start n
             else if c = '\r' || c = '\n'
             then Error PathEmpty
             else (
               Buffer.add_char buffer c;
               loop Path n)
           | Path ->
             if c = ' ' || c = '\r' || c = '\n'
             then Ok (Buffer.contents buffer)
             else (
               Buffer.add_char buffer c;
               loop Path n)))
    in
    let* path = loop Start 0 in
    validate_path path
  ;;
end

type error =
  | MethodError of Method.error
  | PathError of Path.error

let error_of_method_error me = MethodError me
let error_of_path_error pe = PathError pe

(* TODO: Implement header parsing *)
let parse_headers _buf = Ok []

let parse flow =
  let buf = Eio.Buf_read.of_flow flow ~max_size:chunk_size in
  let* meth = Method.parse buf |> Result.map_error error_of_method_error in
  let* path = Path.parse buf |> Result.map_error error_of_path_error in
  let* headers = parse_headers buf in
  let req = { meth; path; headers; body = buf } in
  Ok req
;;

let meth req = req.meth
let get_header req name = List.assoc_opt name req.headers
let path req = req.path

let str_body req =
  let buf = req.body in
  let rec read_all str =
    let chunk = Eio.Buf_read.take chunk_size buf in
    if chunk = "" then str else read_all (str ^ chunk)
  in
  read_all ""
;;
