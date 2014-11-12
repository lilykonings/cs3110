(* The standard type for a city *)
type city = float*float*string

val city_to_string : city -> string

val parse : string -> city list
