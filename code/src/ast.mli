(* the abstract syntax tree *)

type proc = And | Or | Not

(* atoms *)
type sym = string

(* abstract syntax tree nodes *)
type expr =
  | Expr_unit                            
  | Expr_int    of int                  
  | Expr_sym    of sym                  
  | Expr_lst    of expr list
  | Expr_define of sym  * expr
  | Expr_domain of sym  * expr list          
  | Expr_pred   of sym  * expr list 
  | Expr_action of sym  * expr list
  | Expr_objs   of sym  * expr list
  | Expr_init   of sym  * expr list
  | Expr_goal   of sym  * expr list
  | Expr_proc   of proc * expr list

(* convert s-expression into ast expression 
val ast_of_sexpr : Sexpr.expr -> expr
*)
val ast_of_sexpr : Sexpr.expr -> expr

(* Convert an ast expression into a string. *)
val string_of_ast : expr -> string

(* Test ast *)
val ast_test : in_channel -> unit

