(** Read CSVs & CSV-like delimited formats (following the CSV quoting behaviour).

    These formats are loosely documented by RFC 4180: https://www.ietf.org/rfc/rfc4180.txt *)
module Read = Read

(** Write CSVs & CSV-like delimited formats. *)
module Write = struct
  include Write

  (** Helper for converting from the old interface. Use this snippet in files to disable
      the expert interface:

      {[
        module Delimited = struct
          include Delimited
          module Write = Delimited.Write.Without_expert
        end
      ]} *)
  module Without_expert : module type of Write with module Expert := Write.Expert = Write
end

(** {1}
    Modules shared between multiple parsers. *)
module Shared = Shared
