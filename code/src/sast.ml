(* semantically-analyzed, abstract syntax tree *)

open Ast

(* exceptions *)
exception T

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

(* some notion of a program
type s_program = {
  SExpr_lst
  SExpr_define
  SExpr_types
  SExpr_pred
  SExpr_action

  SExpr_define
  SExpr_domain
  SExpr_objs
  SExpr_init
  SExpr_goal
}
*)
(* convert an ast expression into an sast expression *)
let rec sast_of_ast ast = 
  ( match ast with
    | Expr_unit -> 
      SExpr_unit
    | Expr_int i -> 
      SExpr_int(i)
    | Expr_sym s -> 
      SExpr_sym(s)
    | Expr_lst l -> 
      SExpr_lst( List.map sast_of_ast l )
    | Expr_define(n, l) -> 
      SExpr_define( n , sast_of_ast l )
    | Expr_domain(n, l) -> 
      SExpr_domain( n , List.map sast_of_ast l )
    | Expr_pred(n, l) -> 
      SExpr_pred( n , List.map sast_of_ast l )
    | Expr_action(n, l) ->
      SExpr_action( n , List.map sast_of_ast l )
    | Expr_objs(n, l) -> 
      SExpr_objs( n , List.map sast_of_ast l )
    | Expr_init(n, l) -> 
      SExpr_init( n , List.map sast_of_ast l )
    | Expr_goal(n, l) -> 
      SExpr_goal( n , List.map sast_of_ast l )
    | Expr_proc(n, l) -> 
      SExpr_proc( n , List.map sast_of_ast l )
  )

(* convert an sast expression into a string *)
let string_of_sast sast =
  let sprintf  = Printf.sprintf in
  let spaces n = String.make n ' ' in
  let rec string_of_syms sym_lst = 
    match sym_lst with
      | []   -> ""
      | [s] -> s
      | h::t -> h ^ " " ^ (string_of_syms t)
  in
  let rec iter sast indent =
    let string_of_exprs e_list =
      (List.fold_left (^) ""
	 (List.map
            (fun e -> "\n" ^ iter e (indent + 2))
            e_list))
    in
    match sast with
      | SExpr_unit    -> sprintf "%sUNIT"       (spaces indent) 
      | SExpr_int  i  -> sprintf "%sINT[ %d ]"  (spaces indent) i
      | SExpr_sym  s  -> sprintf "%sSYM[ %s ]"  (spaces indent) s
      | SExpr_lst  l  ->
	sprintf "%sLIST[%s ]"
	  (spaces indent ) 
	  (string_of_exprs l )
      | SExpr_define (id, e) -> 
	sprintf "%sDEFINE[%s\n%s ]" 
          (spaces indent) id (iter e (indent + 2))
      | SExpr_pred (key, preds) ->
	sprintf "%sPREDICATE[%s%s ]"
          (spaces indent) key (string_of_exprs preds)
      | SExpr_action (key, preds) ->
	sprintf "%sACTION[%s%s ]"
	  (spaces indent) key (string_of_exprs preds)
      | SExpr_domain (key, preds) ->
	sprintf "%sDOMAIN[%s%s ]"
	  (spaces indent) key (string_of_exprs preds)
      | SExpr_type (key, preds) ->
	sprintf "%sTYPES[%s%s ]"
	  (spaces indent) key (string_of_exprs preds)
      | SExpr_objs (key, preds) ->
	sprintf "%sOBJECTS[%s%s ]"
	  (spaces indent) key (string_of_exprs preds)
      | SExpr_init (key, preds) ->
	sprintf "%sINIT[%s%s ]"
	  (spaces indent) key (string_of_exprs preds)
      | SExpr_goal (key, preds) ->
	sprintf "%sGOAL[%s%s ]"
	  (spaces indent) key (string_of_exprs preds)
      | SExpr_proc (key, preds) ->
	( match key with
	  | And ->
	    sprintf "%sPROC[%s%s ]"
	      (spaces indent) "and" (string_of_exprs preds)
	  | Or ->
	    sprintf "%sPROC[%s%s ]"
	      (spaces indent) "or" (string_of_exprs preds)
	  | Not ->
	    sprintf "%sPROC[%s%s ]"
	      (spaces indent) "not" (string_of_exprs preds)
	)
  in
  "\n" ^ iter sast 0 ^ "\n"


(* test sast *)
let sast_test infile =
  let lexbuf = Lexing.from_channel infile in
  let rec loop () =
    let sexpr = Parser.parse Lexer.token lexbuf in
    match sexpr with
      | None -> ()
      | Some s ->
        let ast = ast_of_sexpr s in
	let sast = sast_of_ast ast in 
        Printf.printf "%s\n" ( string_of_sast sast ); 
        flush stdout;
        loop ()
  in
  loop ()
