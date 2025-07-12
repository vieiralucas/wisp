open Alcotest

let methTestable = Alcotest.testable Wisp.Http.Method.pp Wisp.Http.Method.equal

let () =
  let parse_request s =
    let flow = Eio.Flow.string_source s in
    Wisp.Http.parse flow
  in
  let test_parse_method str m () =
    let request_str =
      Printf.sprintf "%s /hello HTTP/1.1\r\nHost: example.com\r\n\r\n" str
    in
    match parse_request request_str with
    | Ok req -> check methTestable "method" m (Wisp.Http.meth req)
    | Error _ -> fail "Expected Ok but got Error"
  in
  let test_invalid_method () =
    match parse_request "INVALID /hello HTTP/1.1\r\n\r\n" with
    | Error (Wisp.Http.MethodError (Wisp.Http.Method.InvalidMethod "INVALID"))
      -> ()
    | Ok _ -> fail "Expected Error but got Ok"
    | Error _ -> fail "Expected MethodError but got a different error"
  in
  let test_parse_path str expected_path () =
    let request_str =
      Printf.sprintf "GET %s HTTP/1.1\r\nHost: example.com\r\n\r\n" str
    in
    match parse_request request_str with
    | Ok req -> check string "path" expected_path (Wisp.Http.path req)
    | Error _ -> fail "Expected Ok but got Error"
  in
  let test_parse_invalid_path str expected_error () =
    let request_str =
      Printf.sprintf "GET %s HTTP/1.1\r\nHost: example.com\r\n\r\n" str
    in
    match parse_request request_str with
    | Error (Wisp.Http.PathError e) ->
      check
        (Alcotest.of_pp Wisp.Http.Path.pp_error)
        "path error"
        expected_error
        e
    | Ok req ->
      let path = Wisp.Http.path req in
      fail (Printf.sprintf "Expected PathError but got Ok with path '%s'" path)
    | Error _ -> fail "Expected PathError but got a different error"
  in
  run
    "Http"
    [ ( "parsing methods"
      , [ test_case "GET" `Quick (test_parse_method "GET" Wisp.Http.Method.GET)
        ; test_case
            "POST"
            `Quick
            (test_parse_method "POST" Wisp.Http.Method.POST)
        ; test_case "PUT" `Quick (test_parse_method "PUT" Wisp.Http.Method.PUT)
        ; test_case
            "DELETE"
            `Quick
            (test_parse_method "DELETE" Wisp.Http.Method.DELETE)
        ; test_case
            "PATCH"
            `Quick
            (test_parse_method "PATCH" Wisp.Http.Method.PATCH)
        ; test_case
            "OPTIONS"
            `Quick
            (test_parse_method "OPTIONS" Wisp.Http.Method.OPTIONS)
        ; test_case
            "HEAD"
            `Quick
            (test_parse_method "HEAD" Wisp.Http.Method.HEAD)
        ; test_case "Invalid method" `Quick test_invalid_method
        ] )
    ; ( "parsing path"
      , [ test_case "Regular path" `Quick (test_parse_path " /hello" "/hello")
        ; test_case
            "Multiple spaces around path"
            `Quick
            (test_parse_path "   /hello   " "/hello")
        ; test_case
            "Empty path"
            `Quick
            (test_parse_invalid_path "" (Wisp.Http.Path.PathInvalid "HTTP/1.1"))
        ] )
    ]
;;
