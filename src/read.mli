open Core
open Async

include
  module type of Delimited_kernel.Read
  with module Streaming := Delimited_kernel.Read.Streaming
(** @open *)

module Streaming : sig
  include module type of Delimited_kernel.Read.Streaming (** @open *)

  (** [input_reader t reader] feeds all bytes from [reader] to the delimited parser. It
      does not call [finish] after reaching [EOF], one need to call it explicitly if
      needed. *)
  val input_reader : 'a t -> Reader.t -> 'a t Deferred.t

  (** Reads all bytes from [filename] and call [finish] after reaching [EOF]. *)
  val read_file
    :  ?strip:bool
    -> ?sep:char
    -> ?quote:[ `No_quoting | `Using of char ]
    -> ?start_line_number:int
    -> ?on_invalid_row:'a On_invalid_row.t
    -> ?header:Header.t
    -> 'a builder_t
    -> init:'b
    -> f:('b -> 'a -> 'b)
    -> filename:string
    -> 'b t Deferred.t
end

(** Async helpers for delimited parsing *)

(** [fold_reader ?strip ?skip_lines ?sep ?quote ~init ~f r] produces a value by folding
    over a csv document read from [r]. The reader will be closed on EOF.

    If [strip] is true, leading and trailing whitespace is stripped from each field.
    Default value is false.

    If [skip_lines] > 0, that many lines are skipped at the start of the input. Note that
    this skips lines without doing any CSV parsing of the lines being skipped, so newlines
    within a quoted field are treated identically to newlines outside a quoted field. An
    exception will be raised if the input has fewer than [skip_lines] lines. Default value
    is 0.

    [sep] is the character that separates fields within a row. Default value is ','

    [quote] defines a character to use for quoting. [ `Using '"' ] implements the MS Excel
    convention: either a field is unquoted, or it has leading and trailing quotes and
    internal escaped characters are represented as quote-char char, e.g., {i "\n} to
    escape a newline. [`No_quoting] means all characters are literal. The default is
    [`Using '"'] *)
val fold_reader
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char ]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> init:'b
  -> f:('b -> 'a -> 'b Deferred.t)
  -> Reader.t
  -> 'b Deferred.t

(** Same as [fold_reader], except that it also passes the line number of the current row
    to [f]. *)
val fold_readeri
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char ]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> init:'b
  -> f:(int -> 'b -> 'a -> 'b Deferred.t)
  -> Reader.t
  -> 'b Deferred.t

(** [fold_reader' ?strip ?skip_lines ?sep ?quote ~init ~f r] works similarly to
    [fold_reader], except for the [f] argument. [fold_reader'] runs [f] on batches of
    [Row.t]s rather than running [f] on each individual row. *)
val fold_reader'
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char ]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> init:'b
  -> f:('b -> 'a Queue.t -> 'b Deferred.t)
  -> Reader.t
  -> 'b Deferred.t

(** Same as [fold_reader'], except that each element in a batch of [Row.t]'s is a tuple of
    line number and row. *)
val fold_readeri'
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char ]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> init:'b
  -> f:('b -> (int * 'a) Queue.t -> 'b Deferred.t)
  -> Reader.t
  -> 'b Deferred.t

(** Same as [fold_reader] but the fold function does not exert pushback on the fold. *)
val fold_reader_without_pushback
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char ]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> Reader.t
  -> 'b Deferred.t

(** Same as [fold_reader_without_pushback], except that it also passes the line number of
    the current row to [f]. *)
val fold_reader_without_pushbacki
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char ]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> init:'b
  -> f:(int -> 'b -> 'a -> 'b)
  -> Reader.t
  -> 'b Deferred.t

(** [pipe_of_reader t reader] produces a pipe reader of parsed values. *)
val pipe_of_reader
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char ]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> Reader.t
  -> 'a Pipe.Reader.t

(** [pipe_of_chunks] is like [pipe_of_reader] except taking a pipe of input strings
    instead of a reader.

    Note that the fragmentation of the strings in the input pipe is irrelevant: they
    aren't assumed to be separate lines, so each string can contain multiple lines or be a
    fragment of a line, and must contain explicit newline charcters to separate each line.
    If you're using the output of [Reader.lines] here, note that it removes the newlines,
    so you'd need to add them back in, or see [pipe_of_lines].

    [skip_lines] has the same caveat as for [fold_reader]: the lines are skipped before
    CSV parsing starts, so it does not treat newlines within quoted strings specially, and
    it raises an exception if the input doesn't contain at least that many lines. *)
val pipe_of_chunks
  :  ?strip:bool
  -> ?start_line_number:int
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char ]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> string Pipe.Reader.t
  -> 'a Pipe.Reader.t

(** [pipe_of_lines] is like [pipe_of_chunks], but it assumes that each element of the
    input pipe is a separate line of a csv, inserting newlines in between them. The
    newline insertion is not "smart" in any way, so in particular:

    - input elements with unquoted newlines in them are treated as multiple lines,
    - any existing trailing newlines in the input elements would still have a newline
      inserted after them, resulting in blank lines,
    - input elements that start but don't close a quoted string will lead to weird
      results. *)
val pipe_of_lines
  :  ?strip:bool
  -> ?start_line_number:int
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char ]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> string Pipe.Reader.t
  -> 'a Pipe.Reader.t

(** [create_reader t filename] opens a reader for the given filename & returns a pipe of
    its parsed values. *)
val create_reader
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char ]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> string
  -> 'a Pipe.Reader.t Deferred.t

(** [load_lines t filename] opens a reader for the given filename & returns a list of its
    parsed values. *)
val load_lines
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char ]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> string
  -> 'a list Deferred.t
