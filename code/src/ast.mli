(* the abstract syntax tree *)

(* s-expressions are converted into the ast here *)

(* atoms *)
type sym = string


(* expressions *)
type expr =
  | Expr_unit
  | Expr_int  of int
  | Expr_sym  of sym
  | Expr_define  of sym * expr

(* convert s-expression into ast expression *)
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

