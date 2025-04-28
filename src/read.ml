open Core
open Async
include Delimited_kernel.Read
open Deferred.Let_syntax

(* the maximum read/write I managed to get off of a socket or disk was 65k *)
let buffer_size = 10 * 65 * 1024

module Streaming = struct
  include Delimited_kernel.Read.Streaming

  let input_reader t r =
    let buffer = Bytes.create buffer_size in
    Deferred.repeat_until_finished t (fun t ->
      match%map Reader.read r buffer ~len:buffer_size with
      | `Eof -> `Finished t
      | `Ok len ->
        let t = Streaming.input t buffer ~len in
        `Repeat t)
  ;;

  let read_file
    ?strip
    ?sep
    ?quote
    ?start_line_number
    ?on_invalid_row
    ?header
    builder
    ~init
    ~f
    ~filename
    =
    let t =
      create
        ?strip
        ?sep
        ?quote
        ?start_line_number
        ?on_invalid_row
        ?header
        builder
        ~init
        ~f
    in
    let%map t = Reader.with_file filename ~f:(input_reader t) in
    let t = finish t in
    t
  ;;
end

let fold_reader_internal
  ?strip
  ?(skip_lines = 0)
  ?sep
  ?quote
  ?header
  ?on_invalid_row
  builder
  ~init
  ~f
  ~what_to_enqueue
  r
  =
  let%bind () = Shared.drop_lines r skip_lines in
  let buffer = Bytes.create buffer_size in
  let queue = Queue.create () in
  let state =
    Streaming.create_indexed
      ?strip
      ?sep
      ?quote
      ~start_line_number:(skip_lines + 1)
      ?header
      ?on_invalid_row
      builder
      ~init:()
      ~f:(fun line_number () elt ->
        Queue.enqueue queue (what_to_enqueue elt ~line_number))
  in
  Deferred.repeat_until_finished (state, init) (fun (state, acc) ->
    match%bind Reader.read r buffer ~len:buffer_size with
    | `Eof ->
      let (_ : unit Streaming.t) = Streaming.finish state in
      let%bind acc = if Queue.is_empty queue then return acc else f acc queue in
      Queue.clear queue;
      let%map () = Reader.close r in
      `Finished acc
    | `Ok len ->
      let state = Streaming.input state buffer ~len in
      let%map acc = if Queue.is_empty queue then return acc else f acc queue in
      Queue.clear queue;
      `Repeat (state, acc))
;;

let fold_readeri'
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
  fold_reader_internal
    ?strip
    ?skip_lines
    ?sep
    ?quote
    ?header
    ?on_invalid_row
    builder
    ~init
    ~f
    ~what_to_enqueue:(fun line ~line_number -> line_number, line)
    r
;;

let fold_reader' ?strip ?skip_lines ?sep ?quote ?header ?on_invalid_row builder ~init ~f r
  =
  fold_reader_internal
    ?strip
    ?skip_lines
    ?sep
    ?quote
    ?header
    ?on_invalid_row
    builder
    ~init
    ~f
    ~what_to_enqueue:(fun line ~line_number:_ -> line)
    r
;;

let fold_queue acc queue ~f =
  Queue.fold queue ~init:(return acc) ~f:(fun deferred_acc elt ->
    let bind_without_unnecessary_yielding x ~f =
      match Deferred.peek x with
      | Some x -> f x
      | None -> Deferred.bind x ~f
    in
    bind_without_unnecessary_yielding deferred_acc ~f:(fun acc -> f acc elt))
;;

let fold_readeri ?strip ?skip_lines ?sep ?quote ?header ?on_invalid_row builder ~init ~f r
  =
  fold_readeri'
    ?strip
    ?skip_lines
    ?sep
    ?quote
    ?header
    ?on_invalid_row
    builder
    ~init
    r
    ~f:(fold_queue ~f:(fun acc (line_number, elt) -> f line_number acc elt))
;;

let fold_reader ?strip ?skip_lines ?sep ?quote ?header ?on_invalid_row builder ~init ~f r =
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
    ~f:(fold_queue ~f)
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

let fold_reader_without_pushbacki
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
  fold_readeri'
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
      return
        (Queue.fold queue ~init:acc ~f:(fun acc (line_number, elt) ->
           f line_number acc elt)))
;;

let pipe_of_reader ?strip ?skip_lines ?sep ?quote ?header ?on_invalid_row builder reader =
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

let create_reader ?strip ?skip_lines ?sep ?quote ?header ?on_invalid_row builder filename =
  let%bind reader = Reader.open_file filename in
  let on_invalid_row =
    match on_invalid_row with
    | None -> On_invalid_row.raise_with_filename ~filename
    | Some on_invalid_row -> on_invalid_row
  in
  Monitor.handle_errors
    (fun () ->
      return
        (pipe_of_reader
           ?strip
           ?skip_lines
           ?sep
           ?quote
           ?header
           ~on_invalid_row
           builder
           reader))
    (fun exn ->
      don't_wait_for (Reader.close reader);
      raise (Monitor.extract_exn exn))
;;

let load_lines ?strip ?skip_lines ?sep ?quote ?header ?on_invalid_row builder filename =
  create_reader ?strip ?skip_lines ?sep ?quote ?header ?on_invalid_row builder filename
  >>= Pipe.to_list
;;

let rec do_line_skip
  ~newlines_between_reads
  ~skip_lines
  ~skipped_so_far
  ~stream
  ~input_pipe
  =
  if skipped_so_far >= skip_lines
  then return stream
  else (
    match%bind Pipe.read input_pipe with
    | `Eof ->
      raise_s
        [%message
          "Delimited.Read.pipe_of_chunks: Reached EOF while skipping lines"
            (skip_lines : int)
            (skipped_so_far : int)]
    | `Ok input ->
      let rec skip_lines_in_chunk ~pos ~skipped_so_far =
        match String.index_from input pos '\n' with
        | None ->
          let skipped_so_far = skipped_so_far + if newlines_between_reads then 1 else 0 in
          do_line_skip
            ~newlines_between_reads
            ~skip_lines
            ~skipped_so_far
            ~stream
            ~input_pipe
        | Some end_pos ->
          let skipped_so_far = skipped_so_far + 1 in
          if skipped_so_far >= skip_lines
          then (
            let stream = Streaming.input_string stream ~pos:(end_pos + 1) input in
            return stream)
          else skip_lines_in_chunk ~pos:(end_pos + 1) ~skipped_so_far
      in
      skip_lines_in_chunk ~pos:0 ~skipped_so_far)
;;

let parse_pipe
  ~newlines_between_reads
  ?strip
  ?(start_line_number = 1)
  ?(skip_lines = 0)
  ?sep
  ?quote
  ?header
  ?on_invalid_row
  builder
  input_pipe
  =
  Pipe.create_reader ~close_on_exception:false (fun writer ->
    let init =
      Streaming.create
        ?header
        ?strip
        ?sep
        ?quote
        ~start_line_number:(start_line_number + skip_lines)
        ?on_invalid_row
        builder
        (* The accumulator is a deferred that waits for all the outstanding row outputs to
           be consumed. Then you can get good pushback behaviour by waiting for the
           accumulator before feeding any more input in (which indeed is what the
           [Pipe.fold] below does). *)
        ~init:(return ())
        ~f:(fun _ row ->
          (* Waiting for the accumulator is not necessary, since [Pipe.write] should not
             become determined until all previous writes have become determined anyway. *)
          Pipe.write writer row)
    in
    let%bind init =
      do_line_skip
        ~newlines_between_reads
        ~skip_lines
        ~skipped_so_far:0
        ~stream:init
        ~input_pipe
    in
    let%bind stream =
      Pipe.fold input_pipe ~init ~f:(fun stream input ->
        let stream = Streaming.input_string stream input in
        let stream =
          if newlines_between_reads then Streaming.input_string stream "\n" else stream
        in
        let%map () = Streaming.acc stream in
        stream)
    in
    let stream = Streaming.finish stream in
    (* I think this last wait for the accumulator before closing the pipe is probably
       unnecessary. The [Streaming.finish] call should already have written everything to
       the pipe. But it's easy to write [f] in the streaming fold such that that's not
       true, or at least not obviously true, e.g. by waiting for the accumulator before
       doing the pipe write. So this seems safer against possible future refactors. *)
    Streaming.acc stream)
;;

let pipe_of_chunks
  ?strip
  ?start_line_number
  ?skip_lines
  ?sep
  ?quote
  ?header
  ?on_invalid_row
  builder
  input_pipe
  =
  parse_pipe
    ~newlines_between_reads:false
    ?strip
    ?start_line_number
    ?skip_lines
    ?sep
    ?quote
    ?header
    ?on_invalid_row
    builder
    input_pipe
;;

let pipe_of_lines
  ?strip
  ?start_line_number
  ?skip_lines
  ?sep
  ?quote
  ?header
  ?on_invalid_row
  builder
  input_pipe
  =
  parse_pipe
    ~newlines_between_reads:true
    ?strip
    ?start_line_number
    ?skip_lines
    ?sep
    ?quote
    ?header
    ?on_invalid_row
    builder
    input_pipe
;;
