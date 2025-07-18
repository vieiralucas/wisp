open Alcotest
open Wisp

(* Test helpers *)
let create_input str = Parser.input_of_string str
let ( <*> ) = Parser.( <*> )
let ( <* ) = Parser.( <* )
let ( *> ) = Parser.( *> )
let ( <|> ) = Parser.( <|> )

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
    let parser = Parser.char 'h' <*> Parser.char 'e' in
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
    let parser = Parser.char 'x' <*> Parser.char 'e' in
    let input = create_input "hello" in
    let result = Parser.run parser input (fun () -> None) in
    match result with
    | Ok (_, _) -> fail "Expected failure but got success"
    | Error (msg, _) ->
      check string "combine first error message" "Expected 'x'" msg
  in
  let test_combine_second_failure () =
    let parser = Parser.char 'h' <*> Parser.char 'x' in
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
  let parser = Parser.string "foo" <*> Parser.string "bar" in
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
  let parser = Parser.string "foo" <*> Parser.string "bar" in
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
  let parser = Parser.string "foo" <*> Parser.string "bar" in
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
  let parser = Parser.string "foo" <*> Parser.string "bar" in
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
  let parser = Parser.string "foo" <*> Parser.string "bar" in
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
  let parser = Parser.string "foo" <*> Parser.string "bar" in
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
  let parser = Parser.string "foo" <*> Parser.string "bar" in
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
  let parser = Parser.string "foo" <*> Parser.string "bar" in
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
  let parser = Parser.string "foobar" <*> Parser.string "baz" in
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
  let parser = Parser.string "foobarbazqux" <*> Parser.string "hello" in
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
  let parser = Parser.string "foobarbazqux" <*> Parser.string "hello" in
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

let test_combine_operator () =
  (* Test <*> operator - combines two parsers and returns both results *)
  let parser = Parser.char 'h' <*> Parser.char 'e' in
  let input = create_input "hello" in
  match Parser.run parser input (fun () -> None) with
  | Ok ((a, b), remaining) ->
    check char "combine operator first" 'h' a;
    check char "combine operator second" 'e' b;
    check
      string
      "combine operator remaining"
      "llo"
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_left_operator () =
  (* Test <* operator - runs two parsers but returns only the left result *)
  let parser = Parser.string "hello" <* Parser.char ' ' in
  let input = create_input "hello world" in
  match Parser.run parser input (fun () -> None) with
  | Ok (result, remaining) ->
    check string "left operator result" "hello" result;
    check
      string
      "left operator remaining"
      "world"
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_right_operator () =
  (* Test *> operator - runs two parsers but returns only the right result *)
  let parser = Parser.string "hello" *> Parser.string "world" in
  let input = create_input "helloworld" in
  match Parser.run parser input (fun () -> None) with
  | Ok (result, remaining) ->
    check string "right operator result" "world" result;
    check
      string
      "right operator remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_combine_operator_failure () =
  (* Test <*> operator with first parser failure *)
  let parser = Parser.char 'x' <*> Parser.char 'e' in
  let input = create_input "hello" in
  match Parser.run parser input (fun () -> None) with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) -> check string "combine operator failure" "Expected 'x'" msg
;;

let test_left_operator_failure () =
  (* Test <* operator with right parser failure *)
  let parser = Parser.string "hello" <* Parser.char 'x' in
  let input = create_input "hello world" in
  match Parser.run parser input (fun () -> None) with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) -> check string "left operator failure" "Expected 'x'" msg
;;

let test_right_operator_failure () =
  (* Test *> operator with left parser failure *)
  let parser = Parser.string "wrong" *> Parser.string "world" in
  let input = create_input "hello world" in
  match Parser.run parser input (fun () -> None) with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check string "right operator failure" "Expected 'wrong'" msg
;;

let test_combine_operator_partial () =
  (* Test <*> operator with partial input *)
  let parser = Parser.string "hello" <*> Parser.string "world" in
  let input = create_input "hello" in
  match Parser.run parser input (fun () -> None) with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check string "combine operator partial" "Not enough input" msg
;;

let test_left_operator_partial () =
  (* Test <* operator with partial input *)
  let parser = Parser.string "hello" <* Parser.string "world" in
  let input = create_input "hello" in
  match Parser.run parser input (fun () -> None) with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check string "left operator partial" "Not enough input" msg
;;

let test_right_operator_partial () =
  (* Test *> operator with partial input *)
  let parser = Parser.string "hello" *> Parser.string "world" in
  let input = create_input "hello" in
  match Parser.run parser input (fun () -> None) with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check string "right operator partial" "Not enough input" msg
;;

let test_combine_operator_streaming () =
  (* Test <*> operator with streaming input *)
  let parser = Parser.string "hello" <*> Parser.string "world" in
  let input = create_input "hel" in
  let chunks = ref [ "lo"; "world" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok ((a, b), remaining) ->
    check string "combine operator streaming first" "hello" a;
    check string "combine operator streaming second" "world" b;
    check
      string
      "combine operator streaming remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_left_operator_streaming () =
  (* Test <* operator with streaming input *)
  let parser = Parser.string "hello" <* Parser.string "world" in
  let input = create_input "hel" in
  let chunks = ref [ "lo"; "world" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok (result, remaining) ->
    check string "left operator streaming result" "hello" result;
    check
      string
      "left operator streaming remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_right_operator_streaming () =
  (* Test *> operator with streaming input *)
  let parser = Parser.string "hello" *> Parser.string "world" in
  let input = create_input "hel" in
  let chunks = ref [ "lo"; "world" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok (result, remaining) ->
    check string "right operator streaming result" "world" result;
    check
      string
      "right operator streaming remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_operator_chaining () =
  (* Test chaining multiple operators *)
  let parser =
    Parser.string "GET"
    <*> (Parser.char ' ' *> Parser.string "/api" <* Parser.char ' ')
    <*> Parser.string "HTTP/1.1"
  in
  let input = create_input "GET /api HTTP/1.1" in
  match Parser.run parser input (fun () -> None) with
  | Ok (((method_, path), version), remaining) ->
    check string "operator chaining method" "GET" method_;
    check string "operator chaining path" "/api" path;
    check string "operator chaining version" "HTTP/1.1" version;
    check
      string
      "operator chaining remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_operator_precedence () =
  (* Test operator precedence - <*> should bind tighter than <* and *> *)
  let parser = Parser.char 'a' <*> Parser.char 'b' <* Parser.char 'c' in
  let input = create_input "abc" in
  match Parser.run parser input (fun () -> None) with
  | Ok (result, remaining) ->
    check (pair char char) "operator precedence result" ('a', 'b') result;
    check
      string
      "operator precedence remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_left_operator_recursive_partial () =
  (* This test triggers the recursive partial case in <* *)
  let parser = Parser.string "foo" <* Parser.string "barbaz" in
  let input = create_input "foo" in
  let chunks = ref [ "bar"; "baz" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok (result, remaining) ->
    check string "left operator recursive partial result" "foo" result;
    check
      string
      "left operator recursive partial remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_left_operator_recursive_partial_error () =
  (* This test triggers the recursive partial case in <* with error *)
  let parser = Parser.string "foo" <* Parser.string "barbaz" in
  let input = create_input "foo" in
  let chunks = ref [ "bar"; "wrong" ] in
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
    check string "left operator recursive partial error" "Expected 'barbaz'" msg
;;

let test_right_operator_recursive_partial () =
  (* This test triggers the recursive partial case in *> *)
  let parser = Parser.string "foo" *> Parser.string "barbaz" in
  let input = create_input "foo" in
  let chunks = ref [ "bar"; "baz" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok (result, remaining) ->
    check string "right operator recursive partial result" "barbaz" result;
    check
      string
      "right operator recursive partial remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_right_operator_recursive_partial_error () =
  (* This test triggers the recursive partial case in *> with error *)
  let parser = Parser.string "foo" *> Parser.string "barbaz" in
  let input = create_input "foo" in
  let chunks = ref [ "bar"; "wrong" ] in
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
      "right operator recursive partial error"
      "Expected 'barbaz'"
      msg
;;

let test_choice_operator () =
  (* Test <|> operator - tries first parser, then second if first fails *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "hello" in
  match Parser.run parser input (fun () -> None) with
  | Ok (result, remaining) ->
    check string "choice operator first success" "hello" result;
    check
      string
      "choice operator first remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_choice_operator_second () =
  (* Test <|> operator when first parser fails, second succeeds *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "world" in
  match Parser.run parser input (fun () -> None) with
  | Ok (result, remaining) ->
    check string "choice operator second success" "world" result;
    check
      string
      "choice operator second remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_choice_operator_both_fail () =
  (* Test <|> operator when both parsers fail *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "xyz" in
  match Parser.run parser input (fun () -> None) with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    (* Should report the error from the first parser *)
    check
      string
      "choice operator both fail"
      "Expected 'hello' or Expected 'world'"
      msg
;;

let test_choice_operator_partial_first () =
  (* Test <|> operator with partial input for first parser *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "hel" in
  match Parser.run parser input (fun () -> None) with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check string "choice operator partial first" "Not enough input" msg
;;

let test_choice_operator_partial_second () =
  (* Test <|> operator with partial input for second parser *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "wor" in
  match Parser.run parser input (fun () -> None) with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check string "choice operator partial second" "Not enough input" msg
;;

let test_choice_operator_streaming_first () =
  (* Test <|> operator with streaming input for first parser *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "hel" in
  let chunks = ref [ "lo" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok (result, remaining) ->
    check string "choice operator streaming first" "hello" result;
    check
      string
      "choice operator streaming first remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_choice_operator_streaming_second () =
  (* Test <|> operator with streaming input for second parser *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "wor" in
  let chunks = ref [ "ld" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok (result, remaining) ->
    check string "choice operator streaming second" "world" result;
    check
      string
      "choice operator streaming second remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_choice_operator_streaming_both_fail () =
  (* Test <|> operator with streaming input where both parsers fail *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "hel" in
  let chunks = ref [ "xy" ] in
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
      "choice operator streaming both fail"
      "Expected 'hello' or Expected 'world'"
      msg
;;

let test_choice_operator_with_chars () =
  (* Test <|> operator with char parsers *)
  let parser = Parser.char 'a' <|> Parser.char 'b' in
  let input = create_input "a" in
  match Parser.run parser input (fun () -> None) with
  | Ok (result, remaining) ->
    check char "choice operator char first" 'a' result;
    check
      string
      "choice operator char first remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_choice_operator_with_chars_second () =
  (* Test <|> operator with char parsers, second succeeds *)
  let parser = Parser.char 'a' <|> Parser.char 'b' in
  let input = create_input "b" in
  match Parser.run parser input (fun () -> None) with
  | Ok (result, remaining) ->
    check char "choice operator char second" 'b' result;
    check
      string
      "choice operator char second remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_choice_operator_chaining () =
  (* Test chaining multiple <|> operators *)
  let parser =
    Parser.string "GET" <|> Parser.string "POST" <|> Parser.string "PUT"
  in
  let input = create_input "POST" in
  match Parser.run parser input (fun () -> None) with
  | Ok (result, remaining) ->
    check string "choice operator chaining" "POST" result;
    check
      string
      "choice operator chaining remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_choice_operator_with_combine () =
  (* Test <|> operator combined with <*> operator *)
  let parser =
    Parser.string "GET"
    <|> Parser.string "POST"
    <*> Parser.char ' ' *> Parser.string "/api"
  in
  let input = create_input "POST /api" in
  match Parser.run parser input (fun () -> None) with
  | Ok ((method_, path), remaining) ->
    check string "choice with combine method" "POST" method_;
    check string "choice with combine path" "/api" path;
    check
      string
      "choice with combine remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_choice_operator_empty_input () =
  (* Test <|> operator with empty input *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "" in
  match Parser.run parser input (fun () -> None) with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check string "choice operator empty input" "Not enough input" msg
;;

let test_choice_operator_streaming_no_more_input () =
  (* Test <|> operator with streaming where get_more returns None *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "hel" in
  let get_more () = None in
  match Parser.run parser input get_more with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check
      string
      "choice operator streaming no more input"
      "Not enough input"
      msg
;;

let test_choice_operator_partial_mismatch_first () =
  (* Test <|> operator where first parser partially matches then fails *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "helx" in
  match Parser.run parser input (fun () -> None) with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check
      string
      "choice operator partial mismatch first"
      "Expected 'hello' or Expected 'world'"
      msg
;;

let test_choice_operator_partial_mismatch_second () =
  (* Test <|> operator where second parser partially matches then fails *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "worx" in
  match Parser.run parser input (fun () -> None) with
  | Ok _ -> fail "Expected failure but got success"
  | Error (msg, _) ->
    check
      string
      "choice operator partial mismatch second"
      "Expected 'hello' or Expected 'world'"
      msg
;;

let test_choice_operator_build_partial_success () =
  (* Test the uncovered branch where build_partial gets a success from the second parser *)
  (* This tests the case where p1_err is Some and the second parser succeeds *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "wor" in
  let chunks = ref [ "ld" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok (result, remaining) ->
    check string "choice operator build partial success" "world" result;
    check
      string
      "choice operator build partial success remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_choice_operator_build_partial_error () =
  (* Test the uncovered branch where build_partial gets an error from the second parser *)
  (* This tests the case where p1_err is Some and the second parser fails *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "wor" in
  let chunks = ref [ "xy" ] in
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
      "choice operator build partial error"
      "Expected 'hello' or Expected 'world'"
      msg
;;

let test_choice_operator_build_partial_recursive () =
  (* Test the uncovered branch where build_partial gets a Partial from the second parser *)
  (* This tests the case where p1_err is Some and the second parser returns Partial *)
  let parser = Parser.string "hello" <|> Parser.string "worldwide" in
  let input = create_input "wor" in
  let chunks = ref [ "ld"; "wide" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok (result, remaining) ->
    check string "choice operator build partial recursive" "worldwide" result;
    check
      string
      "choice operator build partial recursive remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_choice_operator_build_partial_recursive_error () =
  (* Test the uncovered branch where build_partial gets a Partial then error from the second parser *)
  (* This tests the case where p1_err is Some, second parser returns Partial, then fails *)
  let parser = Parser.string "hello" <|> Parser.string "worldwide" in
  let input = create_input "wor" in
  let chunks = ref [ "ld"; "wrong" ] in
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
      "choice operator build partial recursive error"
      "Expected 'hello' or Expected 'worldwide'"
      msg
;;

let test_choice_operator_build_partial_recursive_success () =
  (* Test the uncovered branch where build_partial gets a Partial then success from the second parser *)
  (* This tests the case where p1_err is None, first parser returns Partial, then second succeeds *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "hel" in
  let chunks = ref [ "lo" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok (result, remaining) ->
    check
      string
      "choice operator build partial recursive success"
      "hello"
      result;
    check
      string
      "choice operator build partial recursive success remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_choice_operator_build_partial_none_success () =
  (* Test the uncovered branch where build_partial gets a success from the second parser when p1_err is None *)
  (* This tests the case where p1_err is None and the second parser succeeds *)
  let parser = Parser.string "hello" <|> Parser.string "world" in
  let input = create_input "wor" in
  let chunks = ref [ "ld" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok (result, remaining) ->
    check string "choice operator build partial none success" "world" result;
    check
      string
      "choice operator build partial none success remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_choice_operator_first_partial_then_second_succeeds () =
  let parser = Parser.string "hello" <|> Parser.string "hella" in
  let input = create_input "hell" in
  let chunks = ref [ "a" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok (result, remaining) ->
    check
      string
      "choice operator both partial then second succeeds"
      "hella"
      result;
    check
      string
      "choice operator both partial then second succeeds remaining"
      ""
      (Parser.remaining_string remaining)
  | Error (msg, _) -> fail ("Expected success but got error: " ^ msg)
;;

let test_choice_operator_first_partial_then_second_partial_and_succeeds () =
  let parser = Parser.string "hello" <|> Parser.string "hellal" in
  let input = create_input "hell" in
  let chunks = ref [ "a"; "l" ] in
  let get_more () =
    match !chunks with
    | [] -> None
    | hd :: tl ->
      chunks := tl;
      Some (create_input hd)
  in
  match Parser.run parser input get_more with
  | Ok (result, remaining) ->
    check
      string
      "choice operator both partial then second succeeds"
      "hellal"
      result;
    check
      string
      "choice operator both partial then second succeeds remaining"
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
    ; ( "http first line"
      , [ test_case "http first line" `Quick (fun () ->
            (* Example HTTP request line: "GET / HTTP/1.1\r\n" *)
            let parser =
              Parser.string "GET"
              <*> (Parser.char ' ' *> Parser.string "/" <* Parser.char ' ')
              <*> (Parser.string "HTTP/1.1" <* Parser.string "\r\n")
            in
            let input = create_input "GET / HTTP/1.1\r\n" in
            match Parser.run parser input (fun () -> None) with
            | Ok (((method_, path), version), remaining) ->
              (* Check the parsed values *)
              check string "method" "GET" method_;
              check string "path" "/" path;
              check string "version" "HTTP/1.1" version;
              check string "remaining" "" (Parser.remaining_string remaining)
            | Error (msg, _) -> fail ("Expected success but got error: " ^ msg))
        ] )
    ; ( "parser_operators"
      , [ test_case "combine operator" `Quick test_combine_operator
        ; test_case "left operator" `Quick test_left_operator
        ; test_case "right operator" `Quick test_right_operator
        ; test_case
            "combine operator failure"
            `Quick
            test_combine_operator_failure
        ; test_case "left operator failure" `Quick test_left_operator_failure
        ; test_case "right operator failure" `Quick test_right_operator_failure
        ; test_case
            "combine operator partial"
            `Quick
            test_combine_operator_partial
        ; test_case "left operator partial" `Quick test_left_operator_partial
        ; test_case "right operator partial" `Quick test_right_operator_partial
        ; test_case
            "combine operator streaming"
            `Quick
            test_combine_operator_streaming
        ; test_case
            "left operator streaming"
            `Quick
            test_left_operator_streaming
        ; test_case
            "right operator streaming"
            `Quick
            test_right_operator_streaming
        ; test_case
            "left operator recursive partial"
            `Quick
            test_left_operator_recursive_partial
        ; test_case
            "left operator recursive partial error"
            `Quick
            test_left_operator_recursive_partial_error
        ; test_case
            "right operator recursive partial"
            `Quick
            test_right_operator_recursive_partial
        ; test_case
            "right operator recursive partial error"
            `Quick
            test_right_operator_recursive_partial_error
        ; test_case "operator chaining" `Quick test_operator_chaining
        ; test_case "operator precedence" `Quick test_operator_precedence
        ; test_case "choice operator" `Quick test_choice_operator
        ; test_case "choice operator second" `Quick test_choice_operator_second
        ; test_case
            "choice operator both fail"
            `Quick
            test_choice_operator_both_fail
        ; test_case
            "choice operator partial first"
            `Quick
            test_choice_operator_partial_first
        ; test_case
            "choice operator partial second"
            `Quick
            test_choice_operator_partial_second
        ; test_case
            "choice operator streaming first"
            `Quick
            test_choice_operator_streaming_first
        ; test_case
            "choice operator streaming second"
            `Quick
            test_choice_operator_streaming_second
        ; test_case
            "choice operator streaming both fail"
            `Quick
            test_choice_operator_streaming_both_fail
        ; test_case
            "choice operator with chars"
            `Quick
            test_choice_operator_with_chars
        ; test_case
            "choice operator with chars second"
            `Quick
            test_choice_operator_with_chars_second
        ; test_case
            "choice operator chaining"
            `Quick
            test_choice_operator_chaining
        ; test_case
            "choice with combine"
            `Quick
            test_choice_operator_with_combine
        ; test_case
            "choice operator empty input"
            `Quick
            test_choice_operator_empty_input
        ; test_case
            "choice operator streaming no more input"
            `Quick
            test_choice_operator_streaming_no_more_input
        ; test_case
            "choice operator partial mismatch first"
            `Quick
            test_choice_operator_partial_mismatch_first
        ; test_case
            "choice operator partial mismatch second"
            `Quick
            test_choice_operator_partial_mismatch_second
        ; test_case
            "choice operator build partial success"
            `Quick
            test_choice_operator_build_partial_success
        ; test_case
            "choice operator build partial error"
            `Quick
            test_choice_operator_build_partial_error
        ; test_case
            "choice operator build partial recursive"
            `Quick
            test_choice_operator_build_partial_recursive
        ; test_case
            "choice operator build partial recursive error"
            `Quick
            test_choice_operator_build_partial_recursive_error
        ; test_case
            "choice operator build partial recursive success"
            `Quick
            test_choice_operator_build_partial_recursive_success
        ; test_case
            "choice operator build partial none success"
            `Quick
            test_choice_operator_build_partial_none_success
        ; test_case
            "choice operator first partial then second succeeds"
            `Quick
            test_choice_operator_first_partial_then_second_succeeds
        ; test_case
            "choice operator first partial then second partial and succeeds"
            `Quick
            test_choice_operator_first_partial_then_second_partial_and_succeeds
        ] )
    ]
;;
