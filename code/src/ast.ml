(*
 * ast.ml
 *
 *	Abstract syntax tree.
 *)

open Sexpr

type sym = string

(* abstract syntax tree nodes *)
type expr =
  | Expr_unit                           (*  () *) 
  | Expr_int    of int                  (* 0-9 *)
  | Expr_sym    of sym                  (* a-z *)
  | Expr_lst    of expr list
  | Expr_define of sym * expr           (* ( define (1) ( 2 3 4 ) 5 ) *)
  | Expr_pred   of sym * expr list
(*
  | Expr_action of sym * expr list
  | Expr_lambda of sym list * expr list (*  *)
*)
let ast_of_atom atm = 
  ( match atm with
      | Atom_unit  -> Expr_unit
      | Atom_int n -> Expr_int n
      | Atom_sym s -> Expr_sym s
  )

let rec ast_of_sexpr sx = match sx with
  | Expr_atom a ->
    ( match a with
      | Atom_unit  -> Expr_unit
      | Atom_int n -> Expr_int n
      | Atom_sym s -> Expr_sym s
    )
  | Expr_list l ->
    ( match l with (* distinguish atom lists and expr lists *)
      | h::t -> 
	( match h with 
	  | Expr_atom a ->
	    ( match a with
	      | Atom_sym s -> 
		( match s with
		  | "define" -> 
		    Expr_define( s , ast_of_sexpr (Expr_list t) )
		  | ":predicates" -> 
		    Expr_pred( s , List.map ast_of_sexpr t )
		  | _ -> Expr_lst( List.map ast_of_sexpr l )
		)
	      | _ -> failwith "not a symbol -- unreachable"
	    )
	  | Expr_list ll -> ast_of_sexpr (Expr_list ll)
	  | _ -> failwith "list head is not an atom -- unreachable"
	)
      | _ -> failwith "non-symbol head -- won't be a list"  
    )
  | _ -> failwith "singleton or empty list"

     (* | Expr_lst( List.map ast_of_sexpr l ) *)
(*   
     ( match l with
     |	[] -> ast_of_sexpr (Expr_atom (Atom_unit))
     | h :: t ->  as lst -> Expr_lst( List.map ast_of_sexpr lst )
     )

*)
(*
  | Expr_list l -> Expr_lst(
    let rec map f lst a =
      match lst with
	| [] -> a
	| h::t -> map f t (f h :: a)
    in map (ast_of_sexpr) l [])
*)

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


