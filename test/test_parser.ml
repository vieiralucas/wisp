open Alcotest
open Wisp

let () =
  run
    "Parser"
    [ ( "return"
      , [ test_case "success" `Quick (fun () ->
            let p = Parser.return 42 in
            let input = Parser.input_of_string "abc" in
            match Parser.run_once p input with
            | Parser.Ok (42, _) -> ()
            | _ -> fail "Expected Ok with 42 but got a different result")
        ] )
    ; ( "char"
      , [ test_case "success" `Quick (fun () ->
            let p = Parser.char 'a' in
            let input = Parser.input_of_string "abc" in
            match Parser.run_once p input with
            | Parser.Ok ('a', _) -> ()
            | _ -> fail "Expected Ok with 'a' but got a different result")
        ; test_case "failure" `Quick (fun () ->
            let p = Parser.char 'x' in
            let input = Parser.input_of_string "abc" in
            match Parser.run_once p input with
            | Parser.Error (msg, off) ->
              check string "error message" "Expected 'x'" msg;
              check int "error offset" 0 off
            | _ -> fail "Expected Error but got Ok or Partial")
        ] )
    ; ( "string"
      , [ (* Complete match tests *)
          test_case "complete match - exact" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "hello world" in
            match Parser.run_once p input with
            | Parser.Ok ("hello", remaining) ->
              check
                string
                "remaining input"
                " world"
                (Parser.remaining_string remaining)
            | _ -> fail "Expected Ok with 'hello' but got a different result")
        ; test_case "complete match - exact" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "hello" in
            match Parser.run_once p input with
            | Parser.Ok ("hello", remaining) ->
              check
                string
                "remaining input"
                ""
                (Parser.remaining_string remaining)
            | _ -> fail "Expected Ok with 'hello' but got a different result")
        ; test_case "complete match - with more" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "hello world" in
            match Parser.run_once p input with
            | Parser.Ok ("hello", remaining) ->
              check
                string
                "remaining input"
                " world"
                (Parser.remaining_string remaining)
            | _ -> fail "Expected Ok with 'hello' but got a different result")
        ; (* Empty string tests *)
          test_case "empty string - complete" `Quick (fun () ->
            let p = Parser.string "" in
            let input = Parser.input_of_string "hello" in
            match Parser.run_once p input with
            | Parser.Ok ("", remaining) ->
              check
                string
                "remaining input"
                "hello"
                (Parser.remaining_string remaining)
            | _ ->
              fail "Expected Ok with empty string but got a different result")
        ; (* Failure tests *)
          test_case "failure - wrong character" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "hi world" in
            match Parser.run_once p input with
            | Parser.Error (msg, off) ->
              check string "error message" "Expected 'hello'" msg;
              check int "error offset" 0 off
            | _ -> fail "Expected Error but got Ok or Partial")
        ; test_case "failure - prefix but wrong" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "hellx world" in
            match Parser.run_once p input with
            | Parser.Error (msg, off) ->
              check string "error message" "Expected 'hello'" msg;
              check int "error offset" 0 off
            | _ -> fail "Expected Error but got Ok or Partial")
        ; test_case "failure - shorter than expected" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "hell" in
            match Parser.run_once p input with
            | Parser.Error (msg, off) ->
              check string "error message" "Expected 'hello'" msg;
              check int "error offset" 0 off
            | _ -> fail "Expected Error but got Ok or Partial")
        ; test_case "failure - empty input" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "" in
            match Parser.run_once p input with
            | Parser.Error (msg, off) ->
              check string "error message" "Expected 'hello'" msg;
              check int "error offset" 0 off
            | _ -> fail "Expected Error but got Ok or Partial")
        ; (* Partial match tests *)
          test_case "partial - single character" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "h" in
            let result = Parser.step p input in
            match result with
            | Parser.Partial partial ->
              (* Continue with the partial input *)
              let input = Parser.input_of_string "ello world" in
              let result = Parser.run_once_partial partial input in
              (match result with
               | Parser.Ok (parsed, remaining) ->
                 check
                   string
                   "remaining input"
                   " world"
                   (Parser.remaining_string remaining);
                 check string "parsed string" "hello" parsed
               | Parser.Error (msg, off) ->
                 fail
                   (Printf.sprintf
                      "Expected Ok with 'hello' but got Error: %s at offset %d"
                      msg
                      off))
            | _ -> fail "Expected Partial but got Ok or Error")
        ; test_case "partial - multiple characters" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "he" in
            let result = Parser.step p input in
            match result with
            | Parser.Partial partial ->
              let input = Parser.input_of_string "llo world" in
              let result = Parser.run_once_partial partial input in
              (match result with
               | Parser.Ok ("hello", remaining) ->
                 check
                   string
                   "remaining input"
                   " world"
                   (Parser.remaining_string remaining)
               | _ -> fail "Expected Ok with 'hello' but got a different result")
            | _ -> fail "Expected Partial but got Ok or Error")
        ; test_case "partial - almost complete" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "hell" in
            let result = Parser.step p input in
            match result with
            | Parser.Partial partial ->
              let input = Parser.input_of_string "o world" in
              let result = Parser.run_once_partial partial input in
              (match result with
               | Parser.Ok ("hello", remaining) ->
                 check
                   string
                   "remaining input"
                   " world"
                   (Parser.remaining_string remaining)
               | _ -> fail "Expected Ok with 'hello' but got a different result")
            | _ -> fail "Expected Partial but got Ok or Error")
        ; (* Multi-step partial tests *)
          test_case "multi-step partial" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "h" in
            let result = Parser.step p input in
            match result with
            | Parser.Partial partial1 ->
              let input = Parser.input_of_string "e" in
              let result = Parser.step_partial partial1 input in
              (match result with
               | Parser.Partial partial2 ->
                 let input = Parser.input_of_string "llo world" in
                 let result = Parser.run_once_partial partial2 input in
                 (match result with
                  | Parser.Ok (parsed, remaining) ->
                    check
                      string
                      "remaining input"
                      " world"
                      (Parser.remaining_string remaining);
                    check string "parsed string" "hello" parsed
                  | Parser.Error (msg, off) ->
                    fail
                      (Printf.sprintf
                         "Expected Ok with 'hello' but got Error: %s at offset \
                          %d"
                         msg
                         off))
               | _ ->
                 fail
                   "Expected Partial from second step but got different result")
            | _ -> fail "Expected Partial but got Ok or Error")
        ; (* Partial failure tests *)
          test_case "partial failure - wrong continuation" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "he" in
            let result = Parser.step p input in
            match result with
            | Parser.Partial partial ->
              let input = Parser.input_of_string "xxx world" in
              let result = Parser.run_once_partial partial input in
              (match result with
               | Parser.Error (msg, off) ->
                 check string "error message" "Expected 'hello'" msg;
                 check int "error offset" 0 off
               | _ -> fail "Expected Error but got Ok or Partial")
            | _ -> fail "Expected Partial but got Ok or Error")
        ; test_case "partial failure - incomplete with Done" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "he" in
            let result = Parser.step p input in
            match result with
            | Parser.Partial partial ->
              let input = Parser.input_of_string "" in
              let result = Parser.run_once_partial partial input in
              (match result with
               | Parser.Error (msg, off) ->
                 check string "error message" "Expected 'hello'" msg;
                 check int "error offset" 0 off
               | _ -> fail "Expected Error but got Ok or Partial")
            | _ -> fail "Expected Partial but got Ok or Error")
        ; (* Edge case tests *)
          test_case "edge case - single character string" `Quick (fun () ->
            let p = Parser.string "a" in
            let input = Parser.input_of_string "abc" in
            match Parser.run_once p input with
            | Parser.Ok ("a", remaining) ->
              check
                string
                "remaining input"
                "bc"
                (Parser.remaining_string remaining)
            | _ -> fail "Expected Ok with 'a' but got a different result")
        ; test_case "edge case - very long string" `Quick (fun () ->
            let long_str = String.make 1000 'a' in
            let p = Parser.string long_str in
            let input = Parser.input_of_string (long_str ^ "b") in
            match Parser.run_once p input with
            | Parser.Ok (parsed, remaining) ->
              check string "parsed string" long_str parsed;
              check
                string
                "remaining input"
                "b"
                (Parser.remaining_string remaining)
            | _ ->
              fail "Expected Ok with long string but got a different result")
        ; test_case "edge case - unicode characters" `Quick (fun () ->
            let unicode_str = "café" in
            let p = Parser.string unicode_str in
            let input = Parser.input_of_string (unicode_str ^ " world") in
            match Parser.run_once p input with
            | Parser.Ok (parsed, remaining) ->
              check string "parsed string" unicode_str parsed;
              check
                string
                "remaining input"
                " world"
                (Parser.remaining_string remaining)
            | _ ->
              fail "Expected Ok with unicode string but got a different result")
        ; test_case "edge case - special characters" `Quick (fun () ->
            let special_str = "hello\n\t\r\000world" in
            let p = Parser.string special_str in
            let input = Parser.input_of_string (special_str ^ "end") in
            match Parser.run_once p input with
            | Parser.Ok (parsed, remaining) ->
              check string "parsed string" special_str parsed;
              check
                string
                "remaining input"
                "end"
                (Parser.remaining_string remaining)
            | _ ->
              fail
                "Expected Ok with special characters but got a different result")
        ; (* Offset tracking tests *)
          test_case "offset tracking - error position" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "abc" in
            match Parser.run_once p input with
            | Parser.Error (msg, off) ->
              check string "error message" "Expected 'hello'" msg;
              check int "error offset" 0 off
            | _ -> fail "Expected Error but got Ok or Partial")
        ; test_case "offset tracking - partial error position" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "he" in
            let result = Parser.step p input in
            match result with
            | Parser.Partial partial ->
              let input = Parser.input_of_string "xxx" in
              let result = Parser.run_once_partial partial input in
              (match result with
               | Parser.Error (msg, off) ->
                 check string "error message" "Expected 'hello'" msg;
                 check int "error offset" 0 off
               | _ -> fail "Expected Error but got Ok or Partial")
            | _ -> fail "Expected Partial but got Ok or Error")
        ] )
    ]
;;
