let read_chunk socket =
  let buf = Cstruct.create 4096 in
  let n = Eio.Flow.single_read socket buf in
  if n = 0 then "" else Cstruct.to_string (Cstruct.sub buf 0 n)
;;

let handler socket _client_addr routes =
  match
    Parser.run Request.parser (Parser.input_of_string "") (fun () ->
      let chunk = read_chunk socket in
      if String.length chunk = 0
      then None
      else Some (Parser.input_of_string chunk))
  with
  | Parser.Ok (req, _) -> begin
    match Route.match_request req routes with
    | Some handler ->
      let response = handler req in
      let response_str = Response.to_string response in
      Eio.Flow.copy_string response_str socket;
      Eio.Flow.close socket
    | None ->
      let response_str = Response.to_string Response.not_found in
      Eio.Flow.copy_string response_str socket;
      Eio.Flow.close socket
  end
  | Parser.Error (msg, _) ->
    Printf.eprintf "Parsing failed: %s\n%!" msg;
    Eio.Flow.close socket
;;

(*Eio.Flow.close socket*)

let rec server sw listener routes =
  let socket, client_addr = Eio.Net.accept ~sw listener in
  Eio.Fiber.fork ~sw (fun () -> handler socket client_addr routes);
  server sw listener routes
;;

let listen ~net ~sw ~port routes =
  let listen_addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let listener =
    Eio.Net.listen ~reuse_addr:true ~backlog:10 ~sw net listen_addr
  in
  Printf.printf "Listening on 127.0.0.1:%d\n%!" port;
  server sw listener routes
;;
