(* the abstract syntax tree *)

(* symbols *)
type sym = string

type atom =
  | Atom_var of sym (* ?symbol *)
  | Atom_gnd of sym (* symbol *)
  | Atom_nil

type predicate = 
  | Pred_var of sym * atom list (* (predname vatom v/gatom?) *)
  | Pred_gnd of sym * atom list (* (predname gatom gatom?) *)
  | Pred_nil

type conjunction =
  | Conj_and of conjunction list 
  | Conj_neg of predicate 
  | Conj_pos of predicate
  | Conj_nil

type action =
{
  name         : sym;
  parameters   : atom list;
  precondition : conjunction; 
  effect       : conjunction 
}

type expr =
  | Expr_domain     of sym * expr list (* (define ( domain pman ) ... )*)
  | Expr_problem    of sym * expr list (* (define ( problem prob ) ...)*)
  | Expr_predicates of predicate list  (* :predicates body *)
  | Expr_init       of predicate list  (* :init body *)
  | Expr_goal       of predicate list  (* :goal body *)
  | Expr_action     of action          (* :action ... *)
  | Expr_objects    of atom list       (* :objects body *)
  | Expr_sym        of sym             (* identifiers *)
  | Expr_unit                          (* () *)                          

type program = expr list (* domain and problem *)

(* convert s-expression into ast expression *)
val ast_of_sexpr : Sexpr.expr -> expr

(* convert an ast expression into a string. *)
val string_of_ast : expr -> string

(* test ast *)
val ast_test : in_channel -> unit

