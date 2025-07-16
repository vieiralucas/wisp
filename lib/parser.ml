type input =
  { buffer : Bigstringaf.t
  ; off : int
  }

let input_of_bigstring (buf : Bigstringaf.t) : input = { buffer = buf; off = 0 }

let input_of_string (str : string) : input =
  let len = String.length str in
  let buf = Bigstringaf.of_string ~off:0 ~len str in
  { buffer = buf; off = 0 }
;;

type error = string * int

type 'a result =
  | Error of error
  | Ok of ('a * input)

type 'a partial =
  { run : input -> 'a result
  ; step : input -> 'a step_result
  ; base_off : int
  }

and 'a step_result =
  | Done of 'a result
  | Partial of 'a partial

let step_result_from_result result =
  match result with
  | Ok (x, input) -> Done (Ok (x, input))
  | Error (msg, off) -> Done (Error (msg, off))
;;

type 'a t =
  { run : input -> 'a result
  ; step : input -> 'a step_result
  }

let return (x : 'a) : 'a t =
  let run input = Ok (x, input) in
  let step input = run input |> step_result_from_result in
  { run; step }
;;

let char (c : char) : char t =
  let run { buffer; off } =
    let len = Bigstringaf.length buffer - off in
    if len > 0 && Bigstringaf.get buffer off = c
    then (
      let new_input = { buffer; off = off + 1 } in
      Ok (c, new_input))
    else Error (Printf.sprintf "Expected '%c'" c, off)
  in
  let step input = run input |> step_result_from_result in
  { run; step }
;;

let string (str : string) : string t =
  let run seen_len { buffer; off } =
    let pending_str = String.sub str seen_len (String.length str - seen_len) in
    let str_len = String.length pending_str in
    let input_len = Bigstringaf.length buffer - off in
    if input_len >= str_len
    then
      if Bigstringaf.substring buffer ~off ~len:str_len = pending_str
      then (
        let new_input = { buffer; off = off + str_len } in
        Ok (str, new_input))
      else Error (Printf.sprintf "Expected '%s'" str, off)
    else Error (Printf.sprintf "Expected '%s'" str, off)
  in
  let step input =
    let rec aux seen_len base_off { buffer; off } =
      let pending_str =
        String.sub str seen_len (String.length str - seen_len)
      in
      let str_len = String.length pending_str in
      let input_len = Bigstringaf.length buffer - off in
      if input_len >= str_len
      then
        if Bigstringaf.substring buffer ~off ~len:str_len = pending_str
        then (
          let new_input = { buffer; off = off + str_len } in
          Done (Ok (str, new_input)))
        else Done (Error (Printf.sprintf "Expected '%s'" str, off))
      else (
        let available_str = Bigstringaf.substring buffer ~off ~len:input_len in
        let expected_prefix = String.sub pending_str 0 input_len in
        if available_str = expected_prefix
        then (
          let partial_run = run (seen_len + input_len) in
          let partial_step =
            aux (seen_len + input_len) (base_off + input_len)
          in
          Partial
            { run = partial_run
            ; step = partial_step
            ; base_off = base_off + input_len
            })
        else Done (Error (Printf.sprintf "Expected '%s'" str, off)))
    in
    aux 0 0 input
  in
  { run = run 0; step }
;;

let step (t : 'a t) (input : input) : 'a step_result = t.step input

let step_partial (partial : 'a partial) (input : input) : 'a step_result =
  partial.step input
;;

let rec run_partial
          (partial : 'a partial)
          (input : input)
          (get_more : unit -> input option)
  : 'a result
  =
  let step_result = partial.step input in
  match step_result with
  | Done result -> result
  | Partial partial ->
    (match get_more () with
     | None -> Error ("Not enough input", partial.base_off + input.off)
     | Some more_input -> run_partial partial more_input get_more)
;;

let run (t : 'a t) (input : input) (get_more : unit -> input option) : 'a result
  =
  match t.step input with
  | Done result -> result
  | Partial partial ->
    (match get_more () with
     | None -> Error ("Not enough input", input.off)
     | Some more_input -> run_partial partial more_input get_more)
;;

let run_once (t : 'a t) (input : input) : 'a result =
  match t.run input with
  | Ok (x, new_input) -> Ok (x, new_input)
  | Error (msg, off) -> Error (msg, off)
;;

let run_once_partial (partial : 'a partial) (input : input) : 'a result =
  match partial.run input with
  | Ok (x, new_input) -> Ok (x, new_input)
  | Error (msg, off) -> Error (msg, off)
;;

let remaining_string (input : input) : string =
  let len = Bigstringaf.length input.buffer - input.off in
  if len <= 0
  then ""
  else Bigstringaf.substring input.buffer ~off:input.off ~len
;;

let combine (pa : 'a t) (pb : 'b t) : ('a * 'b) t =
  let rec build_partial_b a (partial_b : 'b partial) =
    Partial
      { run =
          (fun input ->
            match partial_b.run input with
            | Ok (b, new_input) -> Ok ((a, b), new_input)
            | Error (msg, off) -> Error (msg, off))
      ; step =
          (fun input ->
            match partial_b.step input with
            | Done (Error (msg, off)) -> Done (Error (msg, off))
            | Done (Ok (b, new_input)) -> Done (Ok ((a, b), new_input))
            | Partial partial_b -> build_partial_b a partial_b)
      ; base_off = partial_b.base_off
      }
  in
  let rec build_partial_a (partial_a : 'a partial) =
    Partial
      { run =
          (fun input ->
            match partial_a.run input with
            | Ok (a, new_input) ->
              (match pb.run new_input with
               | Ok (b, final_input) -> Ok ((a, b), final_input)
               | Error (msg, off) -> Error (msg, off))
            | Error (msg, off) -> Error (msg, off))
      ; step =
          (fun input ->
            match partial_a.step input with
            | Done (Error (msg, off)) -> Done (Error (msg, off))
            | Done (Ok (a, new_input)) ->
              (match pb.step new_input with
               | Done (Error (msg, off)) -> Done (Error (msg, off))
               | Done (Ok (b, final_input)) -> Done (Ok ((a, b), final_input))
               | Partial partial_b -> build_partial_b a partial_b)
            | Partial partial_a -> build_partial_a partial_a)
      ; base_off = partial_a.base_off
      }
  in
  { run =
      (fun input ->
        match pa.run input with
        | Ok (a, new_input) ->
          (match pb.run new_input with
           | Ok (b, final_input) -> Ok ((a, b), final_input)
           | Error (msg, off) -> Error (msg, off))
        | Error (msg, off) -> Error (msg, off))
  ; step =
      (fun input ->
        match pa.step input with
        | Done (Error (msg, off)) -> Done (Error (msg, off))
        | Done (Ok (a, new_input)) ->
          (match pb.step new_input with
           | Done (Error (msg, off)) -> Done (Error (msg, off))
           | Done (Ok (b, final_input)) -> Done (Ok ((a, b), final_input))
           | Partial partial_b -> build_partial_b a partial_b)
        | Partial partial_a -> build_partial_a partial_a)
  }
;;

let ( <*> ) = combine
