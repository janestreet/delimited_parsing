open! Core
open! Async
open Delimited_kernel

include module type of Delimited_kernel.Csv

(** [fold_reader ?strip ?skip_lines ?sep ?quote ~init ~f r] produces a value by folding
    over a csv document read from [r].

    If [strip] is true, leading and trailing whitespace is stripped from each field.
    Default value is false.

    If [skip_lines] > 0, that many lines are skipped at the start of the input.
    Note that this skips lines without doing any CSV parsing of the lines being skipped,
    so newlines within a quoted field are treated identically to newlines outside a
    quoted field.
    Default value is 0.

    [sep] is the character that separates fields within a row.
    Default value is ','

    [quote] defines a character to use for quoting. [ `Using '"' ]
    implements the MS Excel convention: either a field is unquoted, or it has leading and
    trailing quotes and internal escaped characters are represented as quote-char char,
    e.g., {|"a|} for [a].  [ `No_quoting ] means all characters are literal.
    The default is [ `Using '"' ].
*)
val fold_reader
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[`No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a on_invalid_row
  -> 'a t
  -> init:'b
  -> f:('b -> 'a -> 'b Deferred.t)
  -> Reader.t
  -> 'b Deferred.t

(** [of_reader' ?strip ?skip_lines ?sep ?quote ~init ~f r] works similarly to
    [of_reader], except for the [f] argument. [of_reader'] runs [f] on batches
    of [Row.t]s rather than running [f] on each individual row.
*)
val fold_reader'
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[`No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a on_invalid_row
  -> 'a t
  -> init:'b
  -> f:('b -> 'a Queue.t -> 'b Deferred.t)
  -> Reader.t
  -> 'b Deferred.t

val fold_reader_without_pushback
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[`No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a on_invalid_row
  -> 'a t
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> Reader.t
  -> 'b Deferred.t

val fold_reader_to_pipe
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[`No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a on_invalid_row
  -> 'a t
  -> Reader.t
  -> 'a Pipe.Reader.t

(** Non-applicative non-folding interface. All readers defined below will raise if they encounter unparsable content. *)

(** [of_reader ?strip ~header r] returns a row pipe based on data read from
    the provided reader.
*)
val of_reader
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> header:Header.t
  -> Reader.t
  -> Row.t Pipe.Reader.t

(** [create_reader ?strip ~header filename] same as of_reader, but creates the reader
    for you *)
val create_reader
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> header:Header.t
  -> string
  -> Row.t Pipe.Reader.t Deferred.t

val of_writer
  :  ?sep:char
  -> ?line_breaks:[`Unix | `Windows] (** default is [`Windows] *)
  -> Writer.t
  -> string list Pipe.Writer.t

val create_writer
  :  ?sep:char
  -> ?line_breaks:[`Unix | `Windows] (** default is [`Windows] *)
  -> string
  -> string list Pipe.Writer.t Deferred.t
