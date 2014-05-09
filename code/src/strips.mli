(* symbol table interface *)

type sym = string

(* problem table for planner *)
type strips_problem = (* TODO: make these names consistent with others *)
    { 
      mutable init : Ast.predicate list;
      mutable goal : Ast.predicate list;
      mutable ops  : Ast.action list   ;
    }

(* environment *)
type env = 
    { 
      parent: env option; 
      bindings: (sym, sym) Hashtbl.t;
      mutable problem: strips_problem 
    }

val make : env option -> env

(* convert an ast expression into a strips problem *) 
val strips_of_ast : env -> Ast.expr -> unit
