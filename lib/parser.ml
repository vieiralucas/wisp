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
  { step : input -> 'a step_result
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

type 'a t = { step : input -> 'a step_result }

let return (x : 'a) : 'a t =
  let run input = Ok (x, input) in
  let step input = run input |> step_result_from_result in
  { step }
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
  { step }
;;

let string (str : string) : string t =
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
          let partial_step =
            aux (seen_len + input_len) (base_off + input_len)
          in
          Partial { step = partial_step; base_off = base_off + input_len })
        else Done (Error (Printf.sprintf "Expected '%s'" str, off)))
    in
    aux 0 0 input
  in
  { step }
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

let remaining_string (input : input) : string =
  let len = Bigstringaf.length input.buffer - input.off in
  if len <= 0
  then ""
  else Bigstringaf.substring input.buffer ~off:input.off ~len
;;

let ( <*> ) (pa : 'a t) (pb : 'b t) : ('a * 'b) t =
  let rec build_partial_b a (partial_b : 'b partial) =
    Partial
      { step =
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
      { step =
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
  { step =
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

let ( <* ) (pa : 'a t) (pb : 'b t) : 'a t =
  let pab = pa <*> pb in
  let rec step (partial : ('a * 'b) partial) input =
    match partial.step input with
    | Done (Error (msg, off)) -> Done (Error (msg, off))
    | Done (Ok ((a, _), new_input)) -> Done (Ok (a, new_input))
    | Partial partial ->
      Partial { step = step partial; base_off = partial.base_off }
  in
  { step =
      (fun input ->
        match pab.step input with
        | Done (Error (msg, off)) -> Done (Error (msg, off))
        | Done (Ok ((a, _), new_input)) -> Done (Ok (a, new_input))
        | Partial partial ->
          Partial { step = step partial; base_off = partial.base_off })
  }
;;

let ( *> ) (pa : 'a t) (pb : 'b t) : 'b t =
  let pab = pa <*> pb in
  let rec step (partial : ('a * 'b) partial) input =
    match partial.step input with
    | Done (Error (msg, off)) -> Done (Error (msg, off))
    | Done (Ok ((_, b), new_input)) -> Done (Ok (b, new_input))
    | Partial partial ->
      Partial { step = step partial; base_off = partial.base_off }
  in
  { step =
      (fun input ->
        match pab.step input with
        | Done (Error (msg, off)) -> Done (Error (msg, off))
        | Done (Ok ((_, b), new_input)) -> Done (Ok (b, new_input))
        | Partial partial ->
          Partial { step = step partial; base_off = partial.base_off })
  }
;;

let concat_input (input1 : input) (input2 : input) : input =
  let result =
    Bigstringaf.create
      (Bigstringaf.length input1.buffer + Bigstringaf.length input2.buffer)
  in
  Bigstringaf.blit
    input1.buffer
    ~src_off:0
    result
    ~dst_off:0
    ~len:(Bigstringaf.length input1.buffer);
  Bigstringaf.blit
    input2.buffer
    ~src_off:0
    result
    ~dst_off:(Bigstringaf.length input1.buffer)
    ~len:(Bigstringaf.length input2.buffer);
  { buffer = result; off = input1.off }
;;

let ( <|> ) (p1 : 'a t) (p2 : 'a t) : 'a t =
  let rec build_partial
            (p1_input : input)
            (p1_err : string option)
            (partial : 'a partial)
    =
    Partial
      { step =
          (fun input ->
            let next_input = concat_input p1_input input in
            match partial.step input with
            | Done (Ok (x, new_input)) -> Done (Ok (x, new_input))
            | Done (Error (msg, off)) ->
              (match p1_err with
               | Some msg1 -> Done (Error (msg1 ^ " or " ^ msg, off))
               | None ->
                 let p1_err = Some msg in
                 (match p2.step next_input with
                  | Done (Ok (x, new_input)) -> Done (Ok (x, new_input))
                  | Done (Error (msg2, _)) ->
                    Done (Error (msg ^ " or " ^ msg2, off))
                  | Partial partial_2 ->
                    build_partial next_input p1_err partial_2))
            | Partial partial -> build_partial next_input p1_err partial)
      ; base_off = partial.base_off
      }
  in
  let step input =
    match p1.step input with
    | Done (Ok (x, new_input)) -> Done (Ok (x, new_input))
    | Done (Error (msg, off)) ->
      let p1_err = Some msg in
      (match p2.step input with
       | Done (Ok (x, new_input)) -> Done (Ok (x, new_input))
       | Done (Error (msg2, _)) -> Done (Error (msg ^ " or " ^ msg2, off))
       | Partial partial_2 -> build_partial input p1_err partial_2)
    | Partial partial_1 -> build_partial input None partial_1
  in
  { step }
;;

let map (f : 'a -> 'b) (p : 'a t) : 'b t =
  let rec step (partial : 'a partial) input =
    match partial.step input with
    | Done (Error (msg, off)) -> Done (Error (msg, off))
    | Done (Ok (x, new_input)) -> Done (Ok (f x, new_input))
    | Partial partial ->
      Partial { step = step partial; base_off = partial.base_off }
  in
  { step =
      (fun input ->
        match p.step input with
        | Done (Error (msg, off)) -> Done (Error (msg, off))
        | Done (Ok (x, new_input)) -> Done (Ok (f x, new_input))
        | Partial partial ->
          Partial { step = step partial; base_off = partial.base_off })
  }
;;

let take_while (f : char -> bool) : string t =
  let step input =
    let rec aux acc { buffer; off } =
      if off < Bigstringaf.length buffer && f (Bigstringaf.get buffer off)
      then
        aux
          (acc ^ String.make 1 (Bigstringaf.get buffer off))
          { buffer; off = off + 1 }
      else Done (Ok (acc, { buffer; off }))
    in
    aux "" input
  in
  { step }
;;

let fail (msg : string * int) : 'a t =
  let step _input = Done (Error msg) in
  { step }
;;

let bind (f : 'a -> 'b t) (a : 'a t) : 'b t =
  let btt = map f a in
  let rec run_b_partial (partial : 'b partial) input =
    match partial.step input with
    | Done (Error (msg, off)) -> Done (Error (msg, off))
    | Done (Ok (b, input)) -> Done (Ok (b, input))
    | Partial partial ->
      Partial { step = run_b_partial partial; base_off = partial.base_off }
  in
  let rec run_bt_partial (partial : 'b t partial) input =
    match partial.step input with
    | Done (Error (msg, off)) -> Done (Error (msg, off))
    | Done (Ok (bt, input)) ->
      (match bt.step input with
       | Done (Error (msg, off)) -> Done (Error (msg, off))
       | Done (Ok (result, new_input)) -> Done (Ok (result, new_input))
       | Partial partial ->
         Partial { step = run_b_partial partial; base_off = partial.base_off })
    | Partial partial ->
      Partial { step = run_bt_partial partial; base_off = partial.base_off }
  in
  let step input =
    match btt.step input with
    | Done (Error (msg, off)) -> Done (Error (msg, off))
    | Done (Ok (bt, input)) ->
      (match bt.step input with
       | Done (Error (msg, off)) -> Done (Error (msg, off))
       | Done (Ok (result, new_input)) -> Done (Ok (result, new_input))
       | Partial partial ->
         Partial { step = run_b_partial partial; base_off = partial.base_off })
    | Partial partial ->
      Partial { step = run_bt_partial partial; base_off = partial.base_off }
  in
  { step }
;;

let take_while1 (f : char -> bool) : string t =
  take_while f
  |> bind (fun acc ->
    if String.length acc > 0
    then return acc
    else fail ("Expected at least one character", 0))
;;

let optional (p : 'a t) : 'a option t =
  let rec run_partial (partial : 'a partial) input =
    match partial.step input with
    | Done (Ok (x, new_input)) -> Done (Ok (Some x, new_input))
    | Done (Error _) -> Done (Ok (None, input))
    | Partial partial ->
      Partial { step = run_partial partial; base_off = partial.base_off }
  in
  let step input =
    match p.step input with
    | Done (Ok (x, new_input)) -> Done (Ok (Some x, new_input))
    | Done (Error _) -> Done (Ok (None, input))
    | Partial partial ->
      Partial { step = run_partial partial; base_off = partial.base_off }
  in
  { step }
;;

let many (p : 'a t) : 'a list t =
  let rec run_partial (acc : 'a list) (partial : 'a partial) (input : input)
    : 'a list step_result
    =
    match partial.step input with
    | Done (Ok (x, new_input)) -> step (x :: acc) new_input
    | Done (Error _) -> Done (Ok (List.rev acc, input))
    | Partial partial ->
      Partial { step = run_partial acc partial; base_off = partial.base_off }
  and step acc input =
    match p.step input with
    | Done (Ok (x, new_input)) -> step (x :: acc) new_input
    | Done (Error _) -> Done (Ok (List.rev acc, input))
    | Partial partial ->
      Partial { step = run_partial [] partial; base_off = partial.base_off }
  in
  { step = step [] }
;;
