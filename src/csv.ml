open Core
open Async
open! Int.Replace_polymorphic_compare
include Delimited_kernel.Csv
open Deferred.Let_syntax

(* the maximum read/write I managed to get off of a socket or disk was 65k *)
let buffer_size = 10 * 65 * 1024

let fold_reader'
      ?strip
      ?(skip_lines=0)
      ?sep
      ?quote
      ?header
      ?on_invalid_row
      builder
      ~init
      ~f
      r
  =
  let%bind () = Shared.drop_lines r skip_lines in
  match%bind
    match Header_parse.create ?strip ?sep ?quote ?header builder with
    | Second header_map -> return (Some (header_map, None))
    | First header_parse ->
      let buffer = Bytes.create buffer_size in
      Deferred.repeat_until_finished header_parse (fun header_parse ->
        match%bind Reader.read r buffer ~len:buffer_size with
        | `Eof ->
          let%map () = Reader.close r in
          if Header_parse.is_at_beginning_of_row header_parse
          then failwith "Header line was not found"
          else failwith "Header is incomplete"
        | `Ok len ->
          return
            (match Header_parse.input header_parse ~len buffer with
             | First header_parse -> `Repeat header_parse
             | Second (headers, input) -> `Finished (Some (headers, Some input))))
  with
  | None -> return init
  | Some (header_map, trailing_input) ->
    let state =
      create_parse_state
        ?strip
        ?sep
        ?quote
        ?on_invalid_row
        ~header_map
        builder
        ~init:(Queue.create ())
        ~f:(fun queue elt ->
          Queue.enqueue queue elt;
          queue)
    in
    let state =
      Option.fold trailing_input ~init:state ~f:(fun state input ->
        Parse_state.input_string state input)
    in
    let buffer = Bytes.create buffer_size in
    Deferred.repeat_until_finished (state, init) (fun (state, init) ->
      match%bind Reader.read r buffer ~len:buffer_size with
      | `Eof ->
        let state = Parse_state.finish state in
        let%bind init = f init (Parse_state.acc state) in
        let%map () = Reader.close r in
        `Finished init
      | `Ok i ->
        let state = Parse_state.input state buffer ~len:i in
        let%map init = f init (Parse_state.acc state) in
        Queue.clear (Parse_state.acc state);
        `Repeat (state, init))
;;

let bind_without_unnecessary_yielding x ~f =
  match Deferred.peek x with
  | Some x -> f x
  | None -> Deferred.bind x ~f
;;

let fold_reader ?strip ?skip_lines ?sep ?quote ?header ?on_invalid_row builder ~init ~f r
  =
  fold_reader'
    ?strip
    ?skip_lines
    ?sep
    ?quote
    ?header
    ?on_invalid_row
    builder
    ~init
    r
    ~f:(fun acc queue ->
      Queue.fold queue ~init:(return acc) ~f:(fun deferred_acc row ->
        bind_without_unnecessary_yielding deferred_acc ~f:(fun acc -> f acc row)))
;;

let fold_reader_without_pushback
      ?strip
      ?skip_lines
      ?sep
      ?quote
      ?header
      ?on_invalid_row
      builder
      ~init
      ~f
      r
  =
  fold_reader'
    ?strip
    ?skip_lines
    ?sep
    ?quote
    ?header
    ?on_invalid_row
    builder
    ~init
    r
    ~f:(fun acc queue -> return (Queue.fold queue ~init:acc ~f))
;;

let fold_reader_to_pipe
      ?strip
      ?skip_lines
      ?sep
      ?quote
      ?header
      ?on_invalid_row
      builder
      reader
  =
  let r, w = Pipe.create () in
  let write_to_pipe : unit Deferred.t =
    let%bind () =
      fold_reader'
        ?strip
        ?skip_lines
        ?sep
        ?quote
        ?header
        ?on_invalid_row
        builder
        ~init:()
        reader
        ~f:(fun () queue ->
          if Pipe.is_closed w
          then (
            let%bind () = Reader.close reader in
            Deferred.never ())
          else Pipe.transfer_in w ~from:queue)
    in
    return (Pipe.close w)
  in
  don't_wait_for write_to_pipe;
  r
;;

(** Non-applicative interface. All readers defined below will raise if they encounter unparsable content. *)
let of_reader ?strip ?skip_lines ?sep ~header reader =
  fold_reader_to_pipe ?strip ?skip_lines ?sep ~header Row.builder reader
;;

let create_reader ?strip ?skip_lines ?sep ~header filename =
  let open Deferred.Let_syntax in
  Reader.open_file filename >>| of_reader ?strip ?skip_lines ?sep ~header
;;

let parse_string ?strip ?sep ~header csv_string =
  fold_string
    ?strip
    ?sep
    ~header
    Row.builder
    csv_string
    ~init:(Fast_queue.create ())
    ~f:(fun queue row ->
      Fast_queue.enqueue queue row;
      queue)
  |> Fast_queue.to_list
;;

let write_field ~sep w field =
  Writer.write w (Delimited_kernel.Csv_writer.maybe_escape_field ~sep field)
;;

let rec write_line ~sep ~line_break w line =
  match line with
  | [] -> Writer.write w line_break
  | [ field ] ->
    write_field ~sep w field;
    write_line ~sep ~line_break w []
  | field :: rest ->
    write_field ~sep w field;
    Writer.write_char w sep;
    write_line ~sep ~line_break w rest
;;

let of_writer ?(sep=',') ?(line_breaks=`Windows) writer =
  let line_break =
    match line_breaks with
    | `Unix -> "\n"
    | `Windows -> "\r\n"
  in
  let pipe_r, pipe_w = Pipe.create () in
  don't_wait_for (Writer.transfer writer pipe_r (write_line ~sep ~line_break writer));
  upon (Pipe.closed pipe_w) (fun () -> don't_wait_for (Writer.close writer));
  pipe_w
;;

let create_writer ?sep ?line_breaks filename =
  let open Deferred.Let_syntax in
  let%map w = Writer.open_file filename in
  of_writer ?sep ?line_breaks w
;;
