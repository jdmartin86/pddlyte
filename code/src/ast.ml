(*
 * ast.ml
 *
 *	Abstract syntax tree.
 *)

open Sexpr

type sym = string

(* abstract syntax tree nodes *)
type expr =
  | Expr_unit                          (* () *) 
  | Expr_int    of int                 (* 0-9 *)
  | Expr_sym    of sym                 (* a-z *)
  | Expr_lst    of expr list
  | Expr_apply  of expr * expr list      
  | Expr_define of sym * expr          (* ( define (1) ( 2 3 4 ) 5 ) *)
  | Expr_lambda of sym list * expr list (*  *)


let rec ast_of_sexpr sx = match sx with
  | Expr_atom a ->
    (match a with
      | Atom_unit  -> Expr_unit
      | Atom_int n -> Expr_int n
      | Atom_sym s -> Expr_sym s
    )
  | Expr_list l ->  Expr_lst( List.map ast_of_sexpr l )
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
(*
 Expr_lst( List.map ( fun s -> ast_of_sexpr sx s ) l )
*)
let string_of_ast ast =
   let sprintf  = Printf.sprintf in  (* to make the code cleaner *)
   let spaces n = String.make n ' ' in
   let rec string_of_ids id_lst = 
      match id_lst with
         | []   -> ""
         | [id] -> id
         | h::t -> h ^ " " ^ (string_of_ids t)
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
         | Expr_lambda (ids, body) ->
              sprintf "%sLAMBDA[(%s)%s ]"
                 (spaces indent)
                 (string_of_ids ids)
                 (string_of_exprs body)
         | Expr_apply (operator, operands) ->
              sprintf "%sAPPLY[\n%s%s ]"
                 (spaces indent)
                 (iter operator (indent + 2))
                 (string_of_exprs operands)
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


