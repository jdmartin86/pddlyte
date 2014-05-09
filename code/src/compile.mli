(* compile.mli *)

type instruction =
  | Push_action of string
  | Push_pred of string
  | Pop_pred
  | Goal
(* TODO: clean up! *)
val string_of_instruction : instruction -> string
val print_bytecode : instruction list -> string 
val known_action : Ast.sym -> Ast.action list -> bool 
val translate_params : Ast.atom list -> string
val translate_action : Ast.predicate -> instruction 
val translate_pred : Ast.predicate -> instruction 
val pop_for : int -> instruction list
val translate :
  Ast.predicate list list -> Strips.strips_problem -> instruction list
