(*
 * ast.ml
 *
 * Abstract syntax tree.
 *)

open Sexpr

(* procedures ... not really though*)
type proc = And | Not 

(* symbols *)
type sym = string

(* think of better name later *)
type atom =
  | Atom_var of sym (* ?symbol *)
  | Atom_gnd of sym (* symbol *)

type conjunction = 
  | Conj_var of sym * atom list (* (conjname vatom v/gatom?) *)
  | Conj_gnd of sym * atom list (* (conjname gatom gatom?) *)
  
type expr =
  | Expr_unit (* () *)                          
  | Expr_sym        of sym (* identifiers *)
  | Expr_domain     of sym * expr list (* (define ( domain pman ) ... )*)
  | Expr_problem    of sym * expr list (* (define ( problem prob ) ...)*)
  | Expr_predicates of conjunction list  (* :predicates body *)
  | Expr_action     of action list (* :action ... *)
  | Expr_objects    of sym list (* :objects body *)
  | Expr_init       of conjunction list (* :init body *)
  | Expr_goal       of conjunction list (* :goal body *)
  | Expr_proc       of proc * conjunction list (* and, not *)

and action =
{
  name          : sym; (* action name *)
  parameters    : atom list;
  preconditions : expr list; (* mix of procedures and conjunctions *)
  effects       : expr list (* mix of procedures and conjunctions *)
}

type program = expr list (* domain and problem *)

let sym_of_atom a = 
  ( match a with 
    | Atom_unit -> "" 
    | Atom_sym s -> s
  ) 

(* sexpr.atom -> ast.expr *)
let ast_of_atom atm = 
  match atm with
    | Atom_unit  -> Expr_unit
    | Atom_sym s -> Expr_sym s

(* sexpr.atom -> ast.atom *)
let astatom_of_atomsym atom_sym =
  ( match atom_sym with
    | Atom_sym s -> 
      let prefix = String.get s 0 in
      ( match prefix with
	| '?' -> Atom_var s
	| _ -> Atom_gnd s
      )
    | _ -> failwith "only accepts type atom sym"
  )


(* sexpr.atom list -> conjunction *)
let conj_of_expr expr = 
  ( match expr with
    | []    -> failwith "conjugates must be non-empty"
    | [n]   -> failwith "conjugates must have non-empty params"
    | _ -> 
      let len = List.length expr in
      ( match len with 
	| 3 -> 
	  let p1 = List.nth expr 1 in 
	  let a1 = astatom_of_atomsym p1 in
	  ( match a1 with 
	    | Atom_var va ->
	      let p2 = List.nth expr 2 in
	      let a2 = astatom_of_atomsym p2 in
	      Conj_var( sym_of_atom (List.nth expr 0) , [a1;a2] )
	    | Atom_gnd ga -> 
	      let p2 = List.nth expr 2 in 
	      let a2 = astatom_of_atomsym p2 in
	      ( match a2 with
		| Atom_var va -> 
		  Conj_var( sym_of_atom (List.nth expr 0) , [a1;a2] )
		| _ -> 
		  Conj_gnd( sym_of_atom (List.nth expr 0) , [a1;a2] )
	      )
	  )
	| 2 ->
	  let p1 = List.nth expr 1 in 
	  let a1 = astatom_of_atomsym p1 in
	  ( match a1 with 
	    | Atom_var va -> 
	      Conj_var( sym_of_atom (List.nth expr 0) , [a1] )
	    | Atom_gnd ga -> 
	      Conj_gnd( sym_of_atom (List.nth expr 0) , [a1] )
	  )
	| _ -> failwith "conjunctions may not have more than two params"
      )   
  )


(* this will need to be recursive for nested calls *)
let conj_of_sexpr sx = 
( match sx with 
  | Expr_list l ->
    ( match l with
      | [ Expr_atom ( Atom_sym name ) ; Expr_atom p1 ; Expr_atom p2 ] ->
	let a1 = astatom_of_atomsym p1 and 
	    a2 = astatom_of_atomsym p2 in
	( match a1 with 
	  | Atom_var _ -> 
	    Conj_var( name , [a1 ; a2] )
	  | Atom_gnd _ ->
	    ( match a2 with
	      | Atom_var _ -> 
		Conj_var( name , [a1 ; a2] )
	      | Atom_gnd _ -> 
		Conj_gnd( name , [a1 ; a2] )
	    )
	)
      | [ Expr_atom ( Atom_sym name ) ; Expr_atom p1 ] ->
	let a1 = astatom_of_atomsym p1 in
	( match a1 with 
	  | Atom_var _ -> 
	    Conj_var( name , [a1] )
	  | _ ->
	    Conj_gnd( name , [a1] )
	) 
    )
  | _ -> failwith "unrecognized conjunction structure"
)
(* tasks 
- parse a domain and arbitrary list tail 
- pattern match like i used to, then populate expressions on a 
finer scale? 

find domain prefix
   - process body recursively
find predicates prefix
   - Expr_predicates( List.map conj_of_sexpr t )
      input: t = [ Expr_list(Atom Atom Atom) Expr_list(Atom Atom Atom) ]
      map: f([ a , b , c , ... ]) --> [f(a) , f(b) , f(c) , ...]
      conj_of_sexpr(Expr_list)
find actions prefix
   Expr_action( List.map action_of_sexpr t )
   --> action_of_sexpr = expression mixutre -> action
   - find parameters string
     input: Expr_list(Atom Atom Atom ... )
     apply: let action.parameters = List.map astatom_of_sexpr
     output: [astatom astatom astatom ...]
   - find precondition string
     input: procedure list [Expr_proc( conj conj Expr_proc(conj) ...) ]
     apply: List.map ast_of_sexpr t : recursive ast_of_sexpr call on each
           - reference the type tutorial. it's recursive. make the same
     output: list of recursive type 
   - find effect string
     input: procedure list, possibly empty: [and conj conj 
     apply: same procedure as preconditions
     output: same type
find problem prefix
   - process body recursively
find objects prefix
   - process the same way as parameters
find init prefix
   - process conjunctions the same as preconditions
find goal prefix 
   - process conjunctions the same as preconditions

*)

let rec ast_of_sexpr sx =  
  (match sx with
    | Expr_atom a -> ast_of_atom a
    | Expr_list l -> 
      ( match l with
        (* predicates *)
	| Expr_atom ( Atom_sym ":predicates" ) :: body ->
	  Expr_predicates( List.map conj_of_sexpr body )
	| Expr_atom ( Atom_sym ":action" ) :: body ->
	  failwith "progress"
	| Expr_atom _ :: (* define *)
	    Expr_list l :: 
	    body -> (* body *)
	  ( match l with 
	    (* domain *)
	    | [Expr_atom ( Atom_sym "domain" ) ; 
	       Expr_atom ( Atom_sym name )] ->
	      Expr_domain( name , List.map ast_of_sexpr body )
	    (* problem *)
	    | [Expr_atom ( Atom_sym "problem" ) ; 
	       Expr_atom ( Atom_sym name )] -> 
	      Expr_problem( name , List.map ast_of_sexpr body )
	  )
	| Expr_atom a :: _  -> failwith "atom"
	| Expr_list l :: _  -> failwith "first element must be an atom"
      )
  )

let string_of_conjs conj = 
  ( match conj with
    | _ -> failwith "conj print"
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
       | Expr_sym  s  -> sprintf "%sSYM[ %s ]"  (spaces indent) s
       | Expr_domain ( name , body ) -> 
	 sprintf "%sDOMAIN[%s\n%s ]" 
           (spaces indent) name (string_of_exprs body)
       | Expr_predicates( conjlist ) -> 
	 sprintf "%sPREDICATES[ ]"
	   ( spaces indent ) 
       | _ -> failwith "print not implemented yet"
(*
       | Expr_apply  l  ->
	 sprintf "%sLIST[%s ]"
	   (spaces indent ) 
	   (string_of_exprs l )  
       | Expr_predicates (key, preds) ->
	 sprintf "%sPREDICATE[%s%s ]"
           (spaces indent) key (string_of_exprs preds)
       | Expr_action (key, preds) ->
	 sprintf "%sACTION[%s%s ]"
	   (spaces indent) key (string_of_exprs preds)
       | Expr_objects (preds) ->
	 sprintf "%sOBJECTS[%s%s ]"
	   (spaces indent) (string_of_exprs preds)
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
	   | Not ->
	     sprintf "%sPROC[%s%s ]"
	       (spaces indent) "not" (string_of_exprs preds)
	 )
	 *)
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


