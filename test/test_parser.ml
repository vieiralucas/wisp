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
            | other ->
              fail
                (Printf.sprintf
                   "Expected Ok with 42 but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (%d, _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
        ] )
    ; ( "char"
      , [ test_case "success" `Quick (fun () ->
            let p = Parser.char 'a' in
            let input = Parser.input_of_string "abc" in
            match Parser.run_once p input with
            | Parser.Ok ('a', _) -> ()
            | other ->
              fail
                (Printf.sprintf
                   "Expected Ok with 'a' but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok ('%c', _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
        ; test_case "failure" `Quick (fun () ->
            let p = Parser.char 'x' in
            let input = Parser.input_of_string "abc" in
            match Parser.run_once p input with
            | Parser.Error (msg, off) ->
              check string "error message" "Expected 'x'" msg;
              check int "error offset" 0 off
            | other ->
              fail
                (Printf.sprintf
                   "Expected Error but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok ('%c', _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
        ; test_case "failure - empty buffer" `Quick (fun () ->
            let p = Parser.char 'a' in
            let input = Parser.input_of_string "" in
            match Parser.run_once p input with
            | Parser.Error (msg, off) ->
              check string "error message" "Expected 'a'" msg;
              check int "error offset" 0 off
            | other ->
              fail
                (Printf.sprintf
                   "Expected Error but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok ('%c', _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
        ; test_case "failure - step function" `Quick (fun () ->
            let p = Parser.char 'a' in
            let input = Parser.input_of_string "bcd" in
            (* Wrong character *)
            match Parser.step p input with
            | Parser.Done (Parser.Error (msg, off)) ->
              check string "error message" "Expected 'a'" msg;
              check int "error offset" 0 off
            | other ->
              fail
                (Printf.sprintf
                   "Expected Done(Error) but got: %s"
                   (match other with
                    | Parser.Done (Parser.Ok (v, _)) ->
                      Printf.sprintf "Done(Ok('%c', _))" v
                    | Parser.Done (Parser.Error (msg, off)) ->
                      Printf.sprintf "Done(Error(%s, %d))" msg off
                    | Parser.Partial _ -> "Partial")))
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
            | other ->
              fail
                (Printf.sprintf
                   "Expected Ok with 'hello' but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
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
            | other ->
              fail
                (Printf.sprintf
                   "Expected Ok with 'hello' but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
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
            | other ->
              fail
                (Printf.sprintf
                   "Expected Ok with 'hello' but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
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
            | other ->
              fail
                (Printf.sprintf
                   "Expected Ok with empty string but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
        ; (* Failure tests *)
          test_case "failure - wrong character" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "hi world" in
            match Parser.run_once p input with
            | Parser.Error (msg, off) ->
              check string "error message" "Expected 'hello'" msg;
              check int "error offset" 0 off
            | other ->
              fail
                (Printf.sprintf
                   "Expected Error but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
        ; test_case "failure - prefix but wrong" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "hellx world" in
            match Parser.run_once p input with
            | Parser.Error (msg, off) ->
              check string "error message" "Expected 'hello'" msg;
              check int "error offset" 0 off
            | other ->
              fail
                (Printf.sprintf
                   "Expected Error but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
        ; test_case "failure - shorter than expected" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "hell" in
            match Parser.run_once p input with
            | Parser.Error (msg, off) ->
              check string "error message" "Expected 'hello'" msg;
              check int "error offset" 0 off
            | other ->
              fail
                (Printf.sprintf
                   "Expected Error but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
        ; test_case "failure - empty input" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "" in
            match Parser.run_once p input with
            | Parser.Error (msg, off) ->
              check string "error message" "Expected 'hello'" msg;
              check int "error offset" 0 off
            | other ->
              fail
                (Printf.sprintf
                   "Expected Error but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
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
            | other ->
              fail
                (Printf.sprintf
                   "Expected Partial but got: %s"
                   (match other with
                    | Parser.Partial _ -> "Partial"
                    | Parser.Done _ -> "Done")))
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
               | other ->
                 fail
                   (Printf.sprintf
                      "Expected Ok with 'hello' but got: %s"
                      (match other with
                       | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                       | Parser.Error (msg, off) ->
                         Printf.sprintf "Error (%s, %d)" msg off)))
            | other ->
              fail
                (Printf.sprintf
                   "Expected Partial but got: %s"
                   (match other with
                    | Parser.Partial _ -> "Partial"
                    | Parser.Done _ -> "Done")))
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
               | other ->
                 fail
                   (Printf.sprintf
                      "Expected Ok with 'hello' but got: %s"
                      (match other with
                       | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                       | Parser.Error (msg, off) ->
                         Printf.sprintf "Error (%s, %d)" msg off)))
            | other ->
              fail
                (Printf.sprintf
                   "Expected Partial but got: %s"
                   (match other with
                    | Parser.Partial _ -> "Partial"
                    | Parser.Done _ -> "Done")))
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
                         "Expected Ok with 'hello' but got Error: %s at offset \n\
                         \                          %d"
                         msg
                         off))
               | other ->
                 fail
                   (Printf.sprintf
                      "Expected Partial from second step but got: %s"
                      (match other with
                       | Parser.Partial _ -> "Partial"
                       | Parser.Done _ -> "Done")))
            | other ->
              fail
                (Printf.sprintf
                   "Expected Partial but got: %s"
                   (match other with
                    | Parser.Partial _ -> "Partial"
                    | Parser.Done _ -> "Done")))
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
               | other ->
                 fail
                   (Printf.sprintf
                      "Expected Error but got: %s"
                      (match other with
                       | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                       | Parser.Error (msg, off) ->
                         Printf.sprintf "Error (%s, %d)" msg off)))
            | other ->
              fail
                (Printf.sprintf
                   "Expected Partial but got: %s"
                   (match other with
                    | Parser.Partial _ -> "Partial"
                    | Parser.Done _ -> "Done")))
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
               | other ->
                 fail
                   (Printf.sprintf
                      "Expected Error but got: %s"
                      (match other with
                       | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                       | Parser.Error (msg, off) ->
                         Printf.sprintf "Error (%s, %d)" msg off)))
            | other ->
              fail
                (Printf.sprintf
                   "Expected Partial but got: %s"
                   (match other with
                    | Parser.Partial _ -> "Partial"
                    | Parser.Done _ -> "Done")))
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
            | other ->
              fail
                (Printf.sprintf
                   "Expected Ok with 'a' but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
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
            | other ->
              fail
                (Printf.sprintf
                   "Expected Ok with long string but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
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
            | other ->
              fail
                (Printf.sprintf
                   "Expected Ok with unicode string but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
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
            | other ->
              fail
                (Printf.sprintf
                   "Expected Ok with special characters but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
        ; (* Offset tracking tests *)
          test_case "offset tracking - error position" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "abc" in
            match Parser.run_once p input with
            | Parser.Error (msg, off) ->
              check string "error message" "Expected 'hello'" msg;
              check int "error offset" 0 off
            | other ->
              fail
                (Printf.sprintf
                   "Expected Error but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
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
               | other ->
                 fail
                   (Printf.sprintf
                      "Expected Error but got: %s"
                      (match other with
                       | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                       | Parser.Error (msg, off) ->
                         Printf.sprintf "Error (%s, %d)" msg off)))
            | other ->
              fail
                (Printf.sprintf
                   "Expected Partial but got: %s"
                   (match other with
                    | Parser.Partial _ -> "Partial"
                    | Parser.Done _ -> "Done")))
        ] )
    ; ( "bigstring input"
      , [ test_case "parses char from bigstring input" `Quick (fun () ->
            let str = "abc" in
            let big =
              Bigstringaf.of_string ~off:0 ~len:(String.length str) str
            in
            let input = Parser.input_of_bigstring big in
            let p = Parser.char 'a' in
            match Parser.run_once p input with
            | Parser.Ok ('a', _) -> ()
            | other ->
              fail
                (Printf.sprintf
                   "Expected Ok with 'a' but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok ('%c', _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
        ] )
    ; ( "partial/incremental input"
      , [ test_case
            "returns error when no more input is available (run_partial)"
            `Quick
            (fun () ->
               let p = Parser.string "hello" in
               let input = Parser.input_of_string "he" in
               match Parser.step p input with
               | Parser.Partial partial ->
                 let get_more () = None in
                 (match Parser.run_partial partial input get_more with
                  | Parser.Error (msg, off) ->
                    check string "error message" "Expected 'hello'" msg;
                    check int "error offset" 0 off
                  | other ->
                    fail
                      (Printf.sprintf
                         "Expected Error but got: %s"
                         (match other with
                          | Parser.Ok (v, _) ->
                            Printf.sprintf "Ok (\"%s\", _)" v
                          | Parser.Error (msg, off) ->
                            Printf.sprintf "Error (%s, %d)" msg off)))
               | Parser.Done _ -> fail "Expected Partial but got Done")
        ; test_case
            "returns error when no more input is available (run)"
            `Quick
            (fun () ->
               let p = Parser.string "hello" in
               let input = Parser.input_of_string "he" in
               let get_more () = None in
               match Parser.run p input get_more with
               | Parser.Error (msg, off) ->
                 check string "error message" "Not enough input" msg;
                 check int "error offset" 0 off
               | other ->
                 fail
                   (Printf.sprintf
                      "Expected Error but got: %s"
                      (match other with
                       | Parser.Ok (v, _) -> Printf.sprintf "Ok (\"%s\", _)" v
                       | Parser.Error (msg, off) ->
                         Printf.sprintf "Error (%s, %d)" msg off)))
        ; test_case
            "continues partial parsing when more input is available \
             (run_partial)"
            `Quick
            (fun () ->
               let p = Parser.string "hello" in
               let input = Parser.input_of_string "he" in
               match Parser.step p input with
               | Parser.Partial partial ->
                 let get_more () = Some (Parser.input_of_string "lo world") in
                 (match
                    Parser.run_partial
                      partial
                      (Parser.input_of_string "l")
                      get_more
                  with
                  | Parser.Ok (parsed, remaining) ->
                    check string "parsed string" "hello" parsed;
                    check
                      string
                      "remaining input"
                      " world"
                      (Parser.remaining_string remaining)
                  | Parser.Error (msg, off) ->
                    fail
                      (Printf.sprintf
                         "Expected Ok with 'hello' but got Error: %s at offset \
                          %d"
                         msg
                         off))
               | Parser.Done _ -> fail "Expected Partial but got Done")
        ; test_case
            "continues partial parsing when more input is available (run)"
            `Quick
            (fun () ->
               let p = Parser.string "hello" in
               let input = Parser.input_of_string "he" in
               let get_more () = Some (Parser.input_of_string "llo world") in
               match Parser.run p input get_more with
               | Parser.Ok (parsed, remaining) ->
                 check string "parsed string" "hello" parsed;
                 check
                   string
                   "remaining input"
                   " world"
                   (Parser.remaining_string remaining)
               | Parser.Error (msg, off) ->
                 fail
                   (Printf.sprintf
                      "Expected Ok with 'hello' but got Error: %s at offset %d"
                      msg
                      off))
        ; test_case
            "returns error when get_more returns None in run_partial"
            `Quick
            (fun () ->
               let p = Parser.string "hello" in
               let input = Parser.input_of_string "he" in
               match Parser.step p input with
               | Parser.Partial partial ->
                 let get_more () = None in
                 (match
                    Parser.run_partial
                      partial
                      (Parser.input_of_string "")
                      get_more
                  with
                  | Parser.Error (msg, off) ->
                    check string "error message" "Not enough input" msg;
                    check int "error offset" 2 off
                  | other ->
                    fail
                      (Printf.sprintf
                         "Expected Error but got: %s"
                         (match other with
                          | Parser.Ok (v, _) ->
                            Printf.sprintf "Ok (\"%s\", _)" v
                          | Parser.Error (msg, off) ->
                            Printf.sprintf "Error (%s, %d)" msg off)))
               | other ->
                 fail
                   (Printf.sprintf
                      "Expected Partial but got: %s"
                      (match other with
                       | Parser.Partial _ -> "Partial"
                       | Parser.Done _ -> "Done")))
        ] )
    ; ( "string parser errors"
      , [ test_case
            "returns error when input does not match expected string (step)"
            `Quick
            (fun () ->
               let p = Parser.string "hello" in
               let input = Parser.input_of_string "hx" in
               match Parser.step p input with
               | Parser.Done (Parser.Error (msg, off)) ->
                 check string "error message" "Expected 'hello'" msg;
                 check int "error offset" 0 off
               | Parser.Done (Parser.Ok _) -> fail "Expected Error but got Ok"
               | Parser.Partial _ -> fail "Expected Done(Error) but got Partial")
        ; test_case
            "returns error when input has enough length but wrong content \
             (step)"
            `Quick
            (fun () ->
               let p = Parser.string "hello" in
               let input = Parser.input_of_string "hellx" in
               match Parser.step p input with
               | Parser.Done (Parser.Error (msg, off)) ->
                 check string "error message" "Expected 'hello'" msg;
                 check int "error offset" 0 off
               | Parser.Done (Parser.Ok _) -> fail "Expected Error but got Ok"
               | Parser.Partial _ -> fail "Expected Done(Error) but got Partial")
        ] )
    ; ( "step functions"
      , [ test_case "char parser step function" `Quick (fun () ->
            let p = Parser.char 'a' in
            let input = Parser.input_of_string "abc" in
            match Parser.step p input with
            | Parser.Done (Parser.Ok ('a', remaining)) ->
              check
                string
                "remaining input"
                "bc"
                (Parser.remaining_string remaining)
            | other ->
              fail
                (Printf.sprintf
                   "Expected Done(Ok('a', _)) but got: %s"
                   (match other with
                    | Parser.Done (Parser.Ok (v, _)) ->
                      Printf.sprintf "Done(Ok('%c', _))" v
                    | Parser.Done (Parser.Error (msg, off)) ->
                      Printf.sprintf "Done(Error(%s, %d))" msg off
                    | Parser.Partial _ -> "Partial")))
        ; test_case "return parser step function" `Quick (fun () ->
            let p = Parser.return 42 in
            let input = Parser.input_of_string "abc" in
            match Parser.step p input with
            | Parser.Done (Parser.Ok (42, remaining)) ->
              check
                string
                "remaining input"
                "abc"
                (Parser.remaining_string remaining)
            | other ->
              fail
                (Printf.sprintf
                   "Expected Done(Ok(42, _)) but got: %s"
                   (match other with
                    | Parser.Done (Parser.Ok (v, _)) ->
                      Printf.sprintf "Done(Ok(%d, _))" v
                    | Parser.Done (Parser.Error (msg, off)) ->
                      Printf.sprintf "Done(Error(%s, %d))" msg off
                    | Parser.Partial _ -> "Partial")))
        ] )
    ; ( "complete matches in step"
      , [ test_case "string parser step with complete match" `Quick (fun () ->
            let p = Parser.string "hello" in
            let input = Parser.input_of_string "hello world" in
            match Parser.step p input with
            | Parser.Done (Parser.Ok ("hello", remaining)) ->
              check
                string
                "remaining input"
                " world"
                (Parser.remaining_string remaining)
            | other ->
              fail
                (Printf.sprintf
                   "Expected Done(Ok(\"hello\", _)) but got: %s"
                   (match other with
                    | Parser.Done (Parser.Ok (v, _)) ->
                      Printf.sprintf "Done(Ok(\"%s\", _))" v
                    | Parser.Done (Parser.Error (msg, off)) ->
                      Printf.sprintf "Done(Error(%s, %d))" msg off
                    | Parser.Partial _ -> "Partial")))
        ] )
    ; ( "run function"
      , [ test_case "run with immediate completion" `Quick (fun () ->
            let p = Parser.char 'a' in
            let input = Parser.input_of_string "abc" in
            let get_more () = None in
            (* Should not be called *)
            match Parser.run p input get_more with
            | Parser.Ok ('a', remaining) ->
              check
                string
                "remaining input"
                "bc"
                (Parser.remaining_string remaining)
            | other ->
              fail
                (Printf.sprintf
                   "Expected Ok with 'a' but got: %s"
                   (match other with
                    | Parser.Ok (v, _) -> Printf.sprintf "Ok ('%c', _)" v
                    | Parser.Error (msg, off) ->
                      Printf.sprintf "Error (%s, %d)" msg off)))
        ] )
    ; ( "combine"
      , [ test_case "combine char" `Quick (fun () ->
            let pa = Parser.char 'a' in
            let pb = Parser.char 'b' in
            let p = Parser.combine pa pb in
            let input = Parser.input_of_string "ab" in
            let r = Parser.run p input (fun _ -> Option.none) in
            match r with
            | Parser.Ok ((a, b), remaining) ->
              check
                string
                "remaining input"
                ""
                (Parser.remaining_string remaining);
              check char "first char" 'a' a;
              check char "second char" 'b' b
            | Parser.Error (msg, off) ->
              fail
                (Printf.sprintf
                   "Expected Ok with ('a', 'b') but got Error: %s at offset %d"
                   msg
                   off))
        ] )
    ]
;;
