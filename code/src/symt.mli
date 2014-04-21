(* symbol table interface *)

type sym = string

(* environment *)
type env = { parent: env option ; bindings: (sym, sym) Hashtbl.t }
 
(* symbol table for planner *)
type plan_table = (* TODO: make these names consistent with others *)
{ 
  init : Ast.predicate list;
  goal : Ast.predicate list;
  ops  : Ast.action list ;
}

(* convert an ast expression into an symt expression *)
val symt_of_ast : Ast.expr -> plan_table

(* convert an symt expression into a string *)
val string_of_symt : plan_table -> string

(* test symt *)
val symt_test : in_channel -> unit
