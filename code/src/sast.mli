(* semantically-analyzed, abstract syntax tree *)

(* symbols *)
type sym = string

(* semantically-analyzed ast nodes *)
type s_expr = 
  | SExpr_unit                            
  | SExpr_int    of int                  
  | SExpr_sym    of sym                  
  | SExpr_lst    of s_expr list
  | SExpr_define of sym * s_expr
  | SExpr_domain of sym * s_expr list
  | SExpr_type   of sym * s_expr list
  | SExpr_pred   of sym * s_expr list 
  | SExpr_action of sym * s_expr list
  | SExpr_objs   of sym * s_expr list
  | SExpr_init   of sym * s_expr list
  | SExpr_goal   of sym * s_expr list
  | SExpr_proc   of Ast.proc * s_expr list

(* convert an ast expression into an sast expression *)
val sast_of_ast : Ast.expr -> s_expr

(* convert an sast expression into a string *)
val string_of_sast : s_expr -> string

(* test sast *)
val sast_test : in_channel -> unit
