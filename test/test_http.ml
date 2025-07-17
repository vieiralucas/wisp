open Alcotest
open Wisp

let testable_method = Alcotest.testable Http.Method.pp Http.Method.equal

let () =
  run
    "Http"
    [ ( "Parser"
      , [ test_case "GET origin form request line" `Quick (fun () ->
            let input = Parser.input_of_string "GET /?arg1=val1 HTTP/1.1\r\n" in
            match Parser.run Http.Request.parser input (fun () -> None) with
            | Parser.Ok (result, _) ->
              check
                testable_method
                "Parsed method"
                (Http.Request.meth result)
                Http.Method.GET
            | Parser.Error (msg, off) ->
              failf "Parsing failed at offset %d: %s" off msg)
        ; test_case
            "GET origin form request line with headers"
            `Quick
            (fun () ->
               let input =
                 Parser.input_of_string
                   "GET /?arg1=val1 HTTP/1.1\r\nHost: http://hello.world\r\n"
               in
               match Parser.run Http.Request.parser input (fun () -> None) with
               | Parser.Ok (result, _) ->
                 check
                   testable_method
                   "Parsed method"
                   (Http.Request.meth result)
                   Http.Method.GET;
                 check
                   (option string)
                   "Parsed headers"
                   (Some "http://hello.world")
                   (Http.Request.get_header result "Host")
               | Parser.Error (msg, off) ->
                 failf "Parsing failed at offset %d: %s" off msg)
        ] )
    ]
;;
