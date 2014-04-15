(* the abstract syntax tree *)

(* procedures ... not really though*)
type proc = And | Not 

(* symbols *)
type sym = string

(* think of better name later *)
type atom =
  | Atom_var of sym (* ?symbol *)
  | Atom_gnd of sym (* symbol *)

type conjunction = 
  | Conj_var of sym * atom list (* (conjname vatom v/gatom?) *)
  | Conj_gnd of sym * atom list (* (conjname gatom gatom?) *)
  
type expr =
  | Expr_unit (* () *)                          
  | Expr_sym        of sym (* identifiers *)
  | Expr_domain     of sym * expr list (* (define ( domain pman ) ... )*)
  | Expr_problem    of sym * expr list (* (define ( problem prob ) ...)*)
  | Expr_predicates of conjunction list  (* :predicates body *)
  | Expr_action     of action list (* :action ... *)
  | Expr_objects    of sym list (* :objects body *)
  | Expr_init       of conjunction list (* :init body *)
  | Expr_goal       of conjunction list (* :goal body *)
  | Expr_proc       of proc * conjunction list (* and, not *)

and action =
{
  name          : sym; (* action name *)
  parameters    : atom list;
  preconditions : expr list; (* mix of procedures and conjunctions *)
  effects       : expr list (* mix of procedures and conjunctions *)
}

type program = expr list (* domain and problem *)

(* convert s-expression into ast expression *)
val ast_of_sexpr : Sexpr.expr -> expr

(* convert an ast expression into a string. *)
val string_of_ast : expr -> string

(* test ast *)
val ast_test : in_channel -> unit

