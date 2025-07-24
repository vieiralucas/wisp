type t =
  { status : int
  ; headers : (string * string) list
  ; body : string option
  }

let text (body : string) : t =
  { status = 200; headers = [ "Content-Type", "text/plain" ]; body = Some body }
;;

let to_string (response : t) : string =
  let status_line = Printf.sprintf "HTTP/1.1 %d %s\r\n" response.status
    (match response.status with
     | 200 -> "OK"
     | 404 -> "Not Found"
     | _ -> "Unknown Status")
  in
  let headers =
    List.map (fun (k, v) -> Printf.sprintf "%s: %s\r\n" k v) response.headers
    |> String.concat ""
  in
  let body = match response.body with
    | Some b -> b ^ "\r\n"
    | None -> ""
  in
  status_line ^ headers ^ "\r\n" ^ body

let not_found : t =
  { status = 404
  ; headers = [ "Content-Type", "text/plain" ]
  ; body = Some "Not Found"
  }
;;
