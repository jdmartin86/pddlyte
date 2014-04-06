(*
 * ast.ml
 *
 *	Abstract syntax tree.
 *)

open Sexpr

type proc = And | Or | Not

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
 
let ast_of_atom atm = 
  ( match atm with
      | Atom_unit  -> Expr_unit
      | Atom_int n -> Expr_int n
      | Atom_sym s -> Expr_sym s
  )

let rec ast_of_sexpr sx = match sx with
  | Expr_atom a -> ast_of_atom a
  | Expr_list l ->
    ( match l with (* distinguish atom lists and expr lists *)
      | []   -> failwith "empty list not implemented"
      | [x]  -> 
	( match x with
	  | Expr_list ll ->
	    Expr_lst( List.map ast_of_sexpr ll )
	  | _ -> failwith "no idea"
	)
      | h::t -> 
	( match h with 
	  | Expr_atom( Atom_sym "define" ) -> 
	    Expr_define( "define" , ast_of_sexpr (Expr_list t) )(* complete l *)
	  | Expr_atom( Atom_sym ":predicates" ) -> 
	    Expr_pred( ":predicates" , List.map ast_of_sexpr t )
	  | Expr_atom( Atom_sym ":action" ) ->
	    Expr_action( ":action", List.map ast_of_sexpr t )
	  | Expr_atom( Atom_sym ":domain" ) ->
	    Expr_domain( ":domain" , List.map ast_of_sexpr t )
	  | Expr_atom( Atom_sym ":objects" ) ->
	    Expr_objs( ":objects" , List.map ast_of_sexpr t )
	  | Expr_atom( Atom_sym ":init" ) ->
	    Expr_init( ":init" , List.map ast_of_sexpr t )
	  | Expr_atom( Atom_sym ":goal" ) ->
	    Expr_goal( ":goal" , List.map ast_of_sexpr t )
	  | Expr_atom( Atom_sym "and" ) ->
	    Expr_proc( And , List.map ast_of_sexpr t )
	  | Expr_atom( Atom_sym "not" ) ->
	    Expr_proc( Not , List.map ast_of_sexpr t )
	  | Expr_atom( Atom_sym "or" ) ->
	    Expr_proc( Or , List.map ast_of_sexpr t )
	  | _ -> Expr_lst( List.map ast_of_sexpr l )
	)
      | _ -> failwith "list of list"
    )

let string_of_ast ast =
   let sprintf  = Printf.sprintf in  (* to make the code cleaner *)
   let spaces n = String.make n ' ' in
   let rec string_of_syms sym_lst = 
      match sym_lst with
         | []   -> ""
         | [s] -> s
         | h::t -> h ^ " " ^ (string_of_syms t)
   in
   let rec iter ast indent =
     let string_of_exprs e_list =
       (List.fold_left (^) ""
	  (List.map
             (fun e -> "\n" ^ iter e (indent + 2))
             e_list))
     in
     match ast with
       | Expr_unit    -> sprintf "%sUNIT"       (spaces indent) 
       | Expr_int  i  -> sprintf "%sINT[ %d ]"  (spaces indent) i
       | Expr_sym  s  -> sprintf "%sSYM[ %s ]"  (spaces indent) s
       | Expr_lst  l  ->
	 sprintf "%sLIST[%s ]"
	   (spaces indent ) 
	   (string_of_exprs l )
       | Expr_define (id, e) -> 
	 sprintf "%sDEFINE[%s\n%s ]" 
           (spaces indent) id (iter e (indent + 2))
       | Expr_pred (key, preds) ->
	 sprintf "%sPREDICATE[%s%s ]"
           (spaces indent) key (string_of_exprs preds)
       | Expr_action (key, preds) ->
	 sprintf "%sACTION[%s%s ]"
	   (spaces indent) key (string_of_exprs preds)
       | Expr_domain (key, preds) ->
	 sprintf "%sDOMAIN[%s%s ]"
	   (spaces indent) key (string_of_exprs preds)
       | Expr_objs (key, preds) ->
	 sprintf "%sOBJECTS[%s%s ]"
	   (spaces indent) key (string_of_exprs preds)
       | Expr_init (key, preds) ->
	 sprintf "%sINIT[%s%s ]"
	   (spaces indent) key (string_of_exprs preds)
       | Expr_goal (key, preds) ->
	 sprintf "%sGOAL[%s%s ]"
	   (spaces indent) key (string_of_exprs preds)
       | Expr_proc (key, preds) ->
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
   "\n" ^ iter ast 0 ^ "\n"

let ast_test infile =
  let lexbuf = Lexing.from_channel infile in
  let rec loop () =
    let sexpr  = Parser.parse Lexer.token lexbuf in
    match sexpr with
      | None -> ()
      | Some s ->
        let expr = ast_of_sexpr s in
        Printf.printf "%s\n" (string_of_ast expr); 
        flush stdout;
        loop ()
  in
  loop ()


