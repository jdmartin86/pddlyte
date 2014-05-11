(* util.mli *)

val sprintf : ('a, unit, string) format -> 'a
val printf : ('a, out_channel, unit) format -> 'a
val spaces : int -> string 
val string_of_syms : string list -> string
val permutation : 'a list -> 'a list
val filter_duplicates : 'a list -> 'a list
val heads : 'a list list -> 'a list
