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
  | Expr_unit                          (* () *)                          

val sprintf : ('a, unit, string) format -> 'a
val spaces : int -> string 
val string_of_syms : string list -> string 
val sym_of_atom : Sexpr.atom -> string 
val astatom_of_atomsym : Sexpr.atom -> atom
val astatom_of_sexpr : Sexpr.expr -> atom 
val pred_of_sexpr : Sexpr.expr -> predicate
val params_of_sexpr : Sexpr.expr list -> atom list 
val conj_of_sexpr : Sexpr.expr -> conjunction 
val action_of_sexpr : Sexpr.expr list -> action 
val ast_of_sexpr : Sexpr.expr -> expr 
val string_of_atom : atom -> sym 
val string_of_pred : predicate -> string
val string_of_conj : conjunction -> string
val string_of_params : atom list -> string
val string_of_precond : conjunction -> string
val string_of_effect : conjunction -> string 
val string_of_action : action -> string
val string_of_ast : expr -> string 
