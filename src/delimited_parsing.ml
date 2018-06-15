(** [Delimited_parsing] contains parsers for three common file formats: *)

(** [Character_separated_without_quoting] parses fields separated by a character, where
    fields may contain escaped characters (e,g, [\n]) but fields may not be quoted (e.g.,
    ["foo bar"]). *)
module Character_separated_without_quoting =
  Character_separated_without_quoting

(** [Csv] parses character-separated values where fields may be quoted and quotation
    marks within quoted fields are escaped with another quotation mark, MSExcel-style. *)
module Csv =
  Csv

(** [Positional] parses fixed-width fields. *)
module Positional = Positional

(** {1} Modules shared between multiple parsers. *)

module Header = Header
module Row = Row

(** {1} For writing out values into delimited data. *)

module Builder = Builder
module Shared = Shared
