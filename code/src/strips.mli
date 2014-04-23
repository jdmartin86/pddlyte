(* symbol table interface *)

type sym = string

(* environment *)
type env = { parent: env option ; bindings: (sym, sym) Hashtbl.t }
 
(* symbol table for planner *)
type strips_problem = 
{ 
  init : Ast.predicate list;
  goal : Ast.predicate list;
  ops  : Ast.action list ;
}

val make : env option -> env

val make_strips : strips_problem
(* convert an ast expression into a strips problem *) 
val strips_of_ast : env -> strips_problem -> Ast.expr -> strips_problem
