open Alcotest
open Wisp

(* Test helpers *)
let create_input str = Parser.input_of_string str

let create_bigstring_input str =
  Parser.input_of_bigstring
    (Bigstringaf.of_string ~off:0 ~len:(String.length str) str)
;;

let test_input_creation () =
  let test_string_input () =
    let input = create_input "hello" in
    check
      string
      "string input roundtrip"
      "hello"
      (Parser.remaining_string input)
  in
  let test_bigstring_input () =
    let input = create_bigstring_input "world" in
    check
      string
      "bigstring input roundtrip"
      "world"
      (Parser.remaining_string input)
  in
  test_string_input ();
  test_bigstring_input ()
;;

let test_return () =
  let test_return_success () =
    let parser = Parser.return 42 in
    let input = create_input "hello" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (value, remaining) ->
      check int "return value" 42 value;
      check
        string
        "return remaining string"
        "hello"
        (Parser.remaining_string remaining)
    | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
  in
  test_return_success ()
;;

let test_char () =
  let test_char_success () =
    let parser = Parser.char 'h' in
    let input = create_input "hello" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (value, remaining) ->
      check char "char value" 'h' value;
      check
        string
        "char remaining string"
        "ello"
        (Parser.remaining_string remaining)
    | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
  in
  let test_char_failure () =
    let parser = Parser.char 'x' in
    let input = create_input "hello" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (_, _) -> fail "Expected failure but got success"
    | Error (msg, _) -> check string "char error message" "Expected 'x'" msg
  in
  let test_char_empty_input () =
    let parser = Parser.char 'h' in
    let input = create_input "" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (_, _) -> fail "Expected failure but got success"
    | Error (msg, _) ->
      check string "char empty error message" "Expected 'h'" msg
  in
  test_char_success ();
  test_char_failure ();
  test_char_empty_input ()
;;

let test_string () =
  let test_string_success () =
    let parser = Parser.string "hello" in
    let input = create_input "hello world" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (value, remaining) ->
      check string "string value" "hello" value;
      check
        string
        "string remaining string"
        " world"
        (Parser.remaining_string remaining)
    | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
  in
  let test_string_failure () =
    let parser = Parser.string "world" in
    let input = create_input "hello" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (_, _) -> fail "Expected failure but got success"
    | Error (msg, _) ->
      check string "string error message" "Expected 'world'" msg
  in
  let test_string_partial_match () =
    let parser = Parser.string "hello" in
    let input = create_input "hel" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (_, _) -> fail "Expected failure but got success"
    | Error (msg, _) ->
      check string "string partial error message" "Not enough input" msg
  in
  let test_string_empty_input () =
    let parser = Parser.string "hello" in
    let input = create_input "" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (_, _) -> fail "Expected failure but got success"
    | Error (msg, _) ->
      check string "string empty error message" "Not enough input" msg
  in
  test_string_success ();
  test_string_failure ();
  test_string_partial_match ();
  test_string_empty_input ()
;;

let test_combine () =
  let test_combine_success () =
    let parser = Parser.combine (Parser.char 'h') (Parser.char 'e') in
    let input = create_input "hello" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (value, remaining) ->
      check (pair char char) "combine value" ('h', 'e') value;
      check
        string
        "combine remaining string"
        "llo"
        (Parser.remaining_string remaining)
    | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
  in
  let test_combine_first_failure () =
    let parser = Parser.combine (Parser.char 'x') (Parser.char 'e') in
    let input = create_input "hello" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (_, _) -> fail "Expected failure but got success"
    | Error (msg, _) ->
      check string "combine first error message" "Expected 'x'" msg
  in
  let test_combine_second_failure () =
    let parser = Parser.combine (Parser.char 'h') (Parser.char 'x') in
    let input = create_input "hello" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (_, _) -> fail "Expected failure but got success"
    | Error (msg, _) ->
      check string "combine second error message" "Expected 'x'" msg
  in
  let test_combine_operator () =
    let parser = Parser.( <*> ) (Parser.char 'h') (Parser.char 'e') in
    let input = create_input "hello" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (value, remaining) ->
      check (pair char char) "combine operator value" ('h', 'e') value;
      check
        string
        "combine operator remaining string"
        "llo"
        (Parser.remaining_string remaining)
    | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
  in
  test_combine_success ();
  test_combine_first_failure ();
  test_combine_second_failure ();
  test_combine_operator ()
;;

let test_streaming () =
  let test_string_streaming_success () =
    let parser = Parser.string "hello" in
    let input = create_input "hel" in
    let chunks = ref [ "lo" ] in
    let get_more () =
      match !chunks with
      | [] -> None
      | chunk :: rest ->
        chunks := rest;
        Some (create_input chunk)
    in
    let result = Parser.run parser input get_more in
    match result with
    | Ok (value, remaining) ->
      check string "streaming string value" "hello" value;
      check
        string
        "streaming string remaining string"
        ""
        (Parser.remaining_string remaining)
    | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
  in
  let test_string_streaming_failure () =
    let parser = Parser.string "hello" in
    let input = create_input "hel" in
    let chunks = ref [ "xy" ] in
    let get_more () =
      match !chunks with
      | [] -> None
      | chunk :: rest ->
        chunks := rest;
        Some (create_input chunk)
    in
    let result = Parser.run parser input get_more in
    match result with
    | Ok (_, _) -> fail "Expected failure but got success"
    | Error (msg, _) ->
      check string "streaming failure message" "Expected 'hello'" msg
  in
  let test_string_streaming_no_more_input () =
    let parser = Parser.string "hello" in
    let input = create_input "hel" in
    let get_more () = None in
    let result = Parser.run parser input get_more in
    match result with
    | Ok (_, _) -> fail "Expected failure but got success"
    | Error (msg, _) ->
      check string "streaming no more input message" "Not enough input" msg
  in
  test_string_streaming_success ();
  test_string_streaming_failure ();
  test_string_streaming_no_more_input ()
;;

let test_remaining_string () =
  let test_remaining_string_full () =
    let input = create_input "hello world" in
    check
      string
      "remaining string full"
      "hello world"
      (Parser.remaining_string input)
  in
  let test_remaining_string_partial () =
    let parser = Parser.string "hello " in
    let input = create_input "hello world" in
    match Parser.run parser input (fun () -> None) with
    | Ok (_, remaining) ->
      check
        string
        "remaining string partial"
        "world"
        (Parser.remaining_string remaining)
    | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
  in
  let test_remaining_string_empty () =
    let parser = Parser.string "hello world" in
    let input = create_input "hello world" in
    match Parser.run parser input (fun () -> None) with
    | Ok (_, remaining) ->
      check
        string
        "remaining string empty"
        ""
        (Parser.remaining_string remaining)
    | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
  in
  let test_remaining_string_beyond_end () =
    check
      string
      "remaining string beyond end"
      ""
      (Parser.remaining_string (create_input ""))
  in
  test_remaining_string_full ();
  test_remaining_string_partial ();
  test_remaining_string_empty ();
  test_remaining_string_beyond_end ()
;;

let test_complex_combinations () =
  let test_three_char_combination () =
    let parser =
      Parser.( <*> )
        (Parser.( <*> ) (Parser.char 'h') (Parser.char 'e'))
        (Parser.char 'l')
    in
    let input = create_input "hello" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (value, remaining) ->
      let (a, b), c = value in
      check (triple char char char) "three char value" ('h', 'e', 'l') (a, b, c);
      check
        string
        "three char remaining string"
        "lo"
        (Parser.remaining_string remaining)
    | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
  in
  let test_string_char_combination () =
    let parser = Parser.( <*> ) (Parser.string "hello") (Parser.char ' ') in
    let input = create_input "hello world" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (value, remaining) ->
      check (pair string char) "string char value" ("hello", ' ') value;
      check
        string
        "string char remaining string"
        "world"
        (Parser.remaining_string remaining)
    | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
  in
  test_three_char_combination ();
  test_string_char_combination ()
;;

let test_string_partial_streaming () =
  (* Partial match, then correct continuation *)
  let parser = Parser.string "foobar" in
  let input = create_input "foo" in
  let chunks = ref [ "bar" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok (v, remaining) ->
    check string "string partial streaming value" "foobar" v;
    check
      string
      "string partial streaming remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_string_partial_streaming_mismatch () =
  (* Partial match, then incorrect continuation *)
  let parser = Parser.string "foobar" in
  let input = create_input "foo" in
  let chunks = ref [ "baz" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check
      string
      "string partial streaming mismatch error"
      "Expected 'foobar'"
      msg
;;

let test_string_partial_streaming_no_more_input () =
  (* Partial match, but get_more returns None *)
  let parser = Parser.string "foobar" in
  let input = create_input "foo" in
  let get_more () = None in
  match Parser.run parser input get_more with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check
      string
      "string partial streaming no more input error"
      "Not enough input"
      msg
;;

let test_combine_partial_first () =
  (* First parser is partial, then completed *)
  let parser = Parser.combine (Parser.string "foo") (Parser.string "bar") in
  let input = create_input "fo" in
  let chunks = ref [ "o"; "bar" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok ((a, b), remaining) ->
    check string "combine partial first a" "foo" a;
    check string "combine partial first b" "bar" b;
    check
      string
      "combine partial first remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_combine_partial_second () =
  (* Second parser is partial, then completed *)
  let parser = Parser.combine (Parser.string "foo") (Parser.string "bar") in
  let input = create_input "foo" in
  let chunks = ref [ "ba"; "r" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok ((a, b), remaining) ->
    check string "combine partial second a" "foo" a;
    check string "combine partial second b" "bar" b;
    check
      string
      "combine partial second remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_combine_partial_second_mismatch () =
  (* Second parser is partial, then fails *)
  let parser = Parser.combine (Parser.string "foo") (Parser.string "bar") in
  let input = create_input "foo" in
  let chunks = ref [ "baz" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check string "combine partial second mismatch error" "Expected 'bar'" msg
;;

let test_combine_partial_first_no_more_input () =
  (* First parser is partial, get_more returns None *)
  let parser = Parser.combine (Parser.string "foo") (Parser.string "bar") in
  let input = create_input "fo" in
  let get_more () = None in
  match Parser.run parser input get_more with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check
      string
      "combine partial first no more input error"
      "Not enough input"
      msg
;;

let test_combine_partial_second_no_more_input () =
  (* Second parser is partial, get_more returns None *)
  let parser = Parser.combine (Parser.string "foo") (Parser.string "bar") in
  let input = create_input "foo" in
  let chunks = ref [ "ba" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  let get_more_none () = None in
  match
    Parser.run parser input (fun () ->
      match !chunks with
      | [] -> get_more_none ()
      | _ -> get_more ())
  with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check
      string
      "combine partial second no more input error"
      "Not enough input"
      msg
;;

let test_string_partial_mismatch () =
  (* Test the uncovered branch where available_str != expected_prefix *)
  let parser = Parser.string "foobar" in
  let input = create_input "fox" in
  let result = Parser.run parser input (fun () -> None) in
  match result with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check string "string partial mismatch error" "Expected 'foobar'" msg
;;

let test_combine_partial_a_error () =
  (* Test the uncovered branch where partial_a.step returns Done (Error ...) *)
  let parser = Parser.combine (Parser.string "foo") (Parser.string "bar") in
  let input = create_input "fo" in
  let chunks = ref [ "x" ] in
  (* This will cause the first parser to fail *)
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check string "combine partial a error" "Expected 'foo'" msg
;;

let test_combine_partial_a_pb_error () =
  (* Test the uncovered branch where pb.step returns Done (Error ...) in build_partial_a *)
  let parser = Parser.combine (Parser.string "foo") (Parser.string "bar") in
  let input = create_input "fo" in
  let chunks = ref [ "o"; "baz" ] in
  (* First parser succeeds, second fails *)
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check string "combine partial a pb error" "Expected 'bar'" msg
;;

let test_combine_partial_a_pb_success () =
  (* Test the uncovered branch where pb.step returns Done (Ok ...) in build_partial_a *)
  let parser = Parser.combine (Parser.string "foo") (Parser.string "bar") in
  let input = create_input "fo" in
  let chunks = ref [ "o"; "bar" ] in
  (* Both parsers succeed *)
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok ((a, b), remaining) ->
    check string "combine partial a pb success a" "foo" a;
    check string "combine partial a pb success b" "bar" b;
    check
      string
      "combine partial a pb success remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_combine_partial_a_recursive () =
  (* Test the uncovered branch where partial_a.step returns Partial in build_partial_a *)
  let parser = Parser.combine (Parser.string "foobar") (Parser.string "baz") in
  let input = create_input "fo" in
  let chunks = ref [ "ob"; "ar"; "baz" ] in
  (* First parser needs multiple chunks *)
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok ((a, b), remaining) ->
    check string "combine partial a recursive a" "foobar" a;
    check string "combine partial a recursive b" "baz" b;
    check
      string
      "combine partial a recursive remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_combine_partial_a_pb_done_error () =
  (* Test the uncovered branch where pb.step returns Done (Error ...) in build_partial_a *)
  (* Create a scenario where the first parser definitely triggers build_partial_a *)
  let parser =
    Parser.combine (Parser.string "foobarbazqux") (Parser.string "hello")
  in
  let input = create_input "foo" in
  (* Very partial input for a long string *)
  let chunks = ref [ "bar"; "baz"; "quxwrong" ] in
  (* Complete first, then wrong for second *)
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check string "combine partial a pb done error" "Expected 'hello'" msg
;;

let test_combine_partial_a_pb_done_success () =
  (* Test the uncovered branch where pb.step returns Done (Ok ...) in build_partial_a *)
  (* Create a scenario where the first parser definitely triggers build_partial_a *)
  let parser =
    Parser.combine (Parser.string "foobarbazqux") (Parser.string "hello")
  in
  let input = create_input "foo" in
  (* Very partial input for a long string *)
  let chunks = ref [ "bar"; "baz"; "quxhello" ] in
  (* Complete both parsers *)
  let get_more () =
    (* Get the next chunk *)
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      (* Return the chunk as input *)
      Some (create_input hd)
  in
  (* Run the parser *)
  match Parser.run parser input get_more with
  | Ok ((a, b), remaining) ->
    check string "combine partial a pb done success a" "foobarbazqux" a;
    check string "combine partial a pb done success b" "hello" b;
    check
      string
      "combine partial a pb done success remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

(* Test suite *)
let () =
  run
    "Parser"
    [ ( "input_creation"
      , [ test_case "input creation" `Quick test_input_creation ] )
    ; "return", [ test_case "return" `Quick test_return ]
    ; "char", [ test_case "char" `Quick test_char ]
    ; "string", [ test_case "string" `Quick test_string ]
    ; "combine", [ test_case "combine" `Quick test_combine ]
    ; "streaming", [ test_case "streaming" `Quick test_streaming ]
    ; ( "remaining_string"
      , [ test_case "remaining_string" `Quick test_remaining_string ] )
    ; ( "complex_combinations"
      , [ test_case "complex_combinations" `Quick test_complex_combinations ] )
    ; ( "string_partial_streaming"
      , [ test_case
            "string partial streaming"
            `Quick
            test_string_partial_streaming
        ; test_case
            "string partial streaming mismatch"
            `Quick
            test_string_partial_streaming_mismatch
        ; test_case
            "string partial streaming no more input"
            `Quick
            test_string_partial_streaming_no_more_input
        ] )
    ; ( "combine_partial"
      , [ test_case "combine partial first" `Quick test_combine_partial_first
        ; test_case "combine partial second" `Quick test_combine_partial_second
        ; test_case
            "combine partial second mismatch"
            `Quick
            test_combine_partial_second_mismatch
        ; test_case
            "combine partial first no more input"
            `Quick
            test_combine_partial_first_no_more_input
        ; test_case
            "combine partial second no more input"
            `Quick
            test_combine_partial_second_no_more_input
        ] )
    ; ( "edge_cases"
      , [ test_case
            "string partial mismatch"
            `Quick
            test_string_partial_mismatch
        ; test_case
            "combine partial a error"
            `Quick
            test_combine_partial_a_error
        ; test_case
            "combine partial a pb error"
            `Quick
            test_combine_partial_a_pb_error
        ; test_case
            "combine partial a pb success"
            `Quick
            test_combine_partial_a_pb_success
        ; test_case
            "combine partial a recursive"
            `Quick
            test_combine_partial_a_recursive
        ; test_case
            "combine partial a pb done error"
            `Quick
            test_combine_partial_a_pb_done_error
        ; test_case
            "combine partial a pb done success"
            `Quick
            test_combine_partial_a_pb_done_success
        ] )
    ]
;;
