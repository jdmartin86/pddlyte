(* the abstract syntax tree *)

(* atoms *)
type sym = string

(* abstract syntax tree nodes *)
type expr =
  | Expr_unit                          (* () *) 
  | Expr_int    of int                 (* 0-9 *)
  | Expr_sym    of sym                 (* a-z *)
  | Expr_lst  of expr list           (* ( ( 1 ) ( 2 ( 3 ) ) 4 ) *)
  | Expr_apply of expr * expr list
  | Expr_define of sym * expr          (* ( define (1) ( 2 3 4 ) 5 ) *)
  | Expr_lambda of sym list * expr list (*  *)

(* convert s-expression into ast expression 
val ast_of_sexpr : Sexpr.expr -> expr
*)
val ast_of_sexpr : Sexpr.expr -> expr

(* Convert an AST expression into a string. *)
val string_of_ast : expr -> string

(* Test the conversion from S-expressions to AST expressions 
   by reading in a file, converting all S-expressions
   in the file into the internal representation of S-expressions,
   converting each of those S-expressions into AST expressions,
   and pretty-printing them. 
*)
val ast_test : in_channel -> unit

