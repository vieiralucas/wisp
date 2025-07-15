let handler socket _client_addr =
  let response =
    Printf.sprintf
      "HTTP/1.1 200 OK\r\nContent-Length: %d\r\n\r\n%s"
      (String.length "Hello World!")
      "Hello World!"
  in
  Eio.Flow.copy_string response socket;
  Eio.Flow.close socket
;;

let rec server sw listener =
  let socket, client_addr = Eio.Net.accept ~sw listener in
  Eio.Fiber.fork ~sw (fun () -> handler socket client_addr);
  server sw listener
;;

let main env sw =
  let listen_addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let net = Eio.Stdenv.net env in
  let listener =
    Eio.Net.listen ~reuse_addr:true ~backlog:10 ~sw net listen_addr
  in
  Printf.printf "Listening on 127.0.0.1:8080\n%!";
  server sw listener
;;

let () = Eio_main.run @@ fun env -> Eio.Switch.run @@ fun sw -> main env sw
