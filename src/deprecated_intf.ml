(* New code looking for a CSV parser should use [Delimited_parsing.Csv]. It is faster,
   tested, and supported.

   For legacy code already using the deprecated interface, we have provided a drop-in
   replacement which implements the old [Csv] interface using the new CSV parser. To
   upgrade, make this one-line change: {[

     open Delimited_parsing.Replace_deprecated_csv

   ]}

   Note that only the CSV parser is deprecated. The [Character_separated_without_quoting]
   and [Positional] parsers are identical between the current and deprecated interfaces.
*)

module type Deprecated = sig

  module Header = Header

  module Row : Row_intf.Row

  include Character_separated_without_quoting_intf.Character_separated_without_quoting
    with module Row := Row

  module Csv : Deprecated_csv_intf.Deprecated_csv
    with module Row := Row

  module Positional: Positional_intf.Positional
    with module Row := Row

end
