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
    let rec aux seen_len { buffer; off } =
      let pending_str =
        String.sub str seen_len (String.length str - seen_len)
      in
      let str_len = String.length pending_str - seen_len in
      let input_len = Bigstringaf.length buffer - off in
      if input_len >= str_len
      then
        if Bigstringaf.substring buffer ~off ~len:str_len = pending_str
        then (
          let new_input = { buffer; off = off + str_len } in
          Done (Ok (str, new_input)))
        else Done (Error (Printf.sprintf "Expected '%s'" pending_str, off))
      else (
        let partial_run = run (seen_len + input_len) in
        let partial_step = aux (seen_len + input_len) in
        Partial { run = partial_run; step = partial_step })
    in
    aux 0 input
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
     | None -> Error ("Not enough input", input.off)
     | Some input -> run_partial partial input get_more)
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
