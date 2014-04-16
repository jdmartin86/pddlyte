(*
 * ast.ml
 *
 * Abstract syntax tree.
 *)

open Sexpr

(* symbols *)
type sym = string

type atom =
  | Atom_var of sym (* ?symbol *)
  | Atom_gnd of sym (* symbol *)

type predicate = 
  | Pred_var of sym * atom list (* (predname vatom v/gatom?) *)
  | Pred_gnd of sym * atom list (* (predname gatom gatom?) *)

type conjunction =
  | Conj_and of conjunction list 
  | Conj_neg of predicate 
  | Conj_pos of predicate

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
  | Expr_action     of action list     (* :action ... *)
  | Expr_init       of conjunction     (* :init body *)
  | Expr_goal       of conjunction     (* :goal body *)
  | Expr_objects    of atom list       (* :objects body *)
  | Expr_sym        of sym             (* identifiers *)
  | Expr_unit                          (* () *)                          

type program = expr list (* domain and problem *)

let sym_of_atom a = 
  ( match a with 
    | Atom_unit -> "" (* shouldn't be used *)
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

(* sexpr.expr -> ast.atom *)
let astatom_of_sexpr sx =
  ( match sx with
    | Expr_atom ( Atom_sym s ) -> 
      let prefix = String.get s 0 in
      ( match prefix with
	| '?' -> Atom_var s
	| _ -> Atom_gnd s
      )
    | _ -> failwith "only accepts type atom sym"
  )


(* this will need to be recursive for nested calls *)
(* sexpr.expr -> ast.predicate *)  
let pred_of_sexpr sx = 
( match sx with 
  | Expr_list l ->
    ( match l with
      | [ Expr_atom ( Atom_sym name ) ; Expr_atom p1 ; Expr_atom p2 ] ->
	let a1 = astatom_of_atomsym p1 and 
	    a2 = astatom_of_atomsym p2 in
	( match a1 with 
	  | Atom_var _ -> 
	    Pred_var( name , [a1 ; a2] )
	  | Atom_gnd _ ->
	    ( match a2 with
	      | Atom_var _ -> 
		Pred_var( name , [a1 ; a2] )
	      | Atom_gnd _ -> 
		Pred_gnd( name , [a1 ; a2] )
	    )
	)
      | [ Expr_atom ( Atom_sym name ) ; Expr_atom p1 ] ->
	let a1 = astatom_of_atomsym p1 in
	( match a1 with 
	  | Atom_var _ -> 
	    Pred_var( name , [a1] )
	  | _ ->
	    Pred_gnd( name , [a1] )
	) 
    )
  | _ -> failwith "unrecognized conjunction structure"
)

(* sexpr.expr list -> atom list *)
let params_of_sexpr sx = 
  ( match sx with 
    | Expr_atom _ :: _ ->
      List.map astatom_of_sexpr sx
    | _ -> failwith "syntax error: unrecognized parameter"
  )

let rec conj_of_sexpr sx = 
  ( match sx with 
    | Expr_list l ->
      ( match l with
	| Expr_atom ( Atom_sym "and" ) :: body -> (* list of conjs *)
	  Conj_and ( List.map conj_of_sexpr body )
	| [ Expr_atom ( Atom_sym "not" ) ; p1 ] -> 
	  Conj_neg ( pred_of_sexpr p1 ) (*TODO: think about nesting ands *)
	| [ Expr_atom ( Atom_sym name ) ; Expr_atom p1 ; Expr_atom p2 ] ->
	  let a1 = astatom_of_atomsym p1 and 
	      a2 = astatom_of_atomsym p2 in
	  ( match a1 with 
	    | Atom_var _ -> 
	      Conj_pos(Pred_var( name , [a1 ; a2] ))
	    | Atom_gnd _ ->
	      ( match a2 with
		| Atom_var _ -> 
		  Conj_pos(Pred_var( name , [a1 ; a2] ))
		| Atom_gnd _ -> 
		  Conj_pos(Pred_gnd( name , [a1 ; a2] ))
	      )
	  )
	| [ Expr_atom ( Atom_sym name ) ; Expr_atom p1 ] ->
	  let a1 = astatom_of_atomsym p1 in
	  ( match a1 with 
	    | Atom_var _ -> 
	      Conj_pos(Pred_var( name , [a1] ))
	    | _ ->
	      Conj_pos(Pred_gnd( name , [a1] ))
	  ) 
      )
    | _ -> failwith "syntax error: unrecognized conjunction" 
  )

(* recursive type ... some day
type conjunction =
  | Conj_and of conjunction list 
  | Conj_neg of conjunction 
  | Conj_pos of conjunction
  | Pred     of predicate
 evaluates nested conjunction lists to preds 
let rec resolve_conj pred =
( match pred with 
  | Conj_and ( c ) -> Conj_and ( List.map resolve_conj c )
  | Conj_neg ( c ) -> Conj_neg ( resolve_conj c )
  | Conj_pos ( c ) -> Conj_pos ( resolve_conj c )
  | Pred p -> Pred p  
) 
*)

(* sexpr.expr -> ast.action *)
(* probably the most hacked code i've ever written *)
let action_of_sexpr sx =
  ( match sx with
    | Expr_atom ( Atom_sym ":action" ) :: body ->
      failwith "progress"
	( match body with
	  | Expr_atom ( Atom_sym name ) :: body -> (* action name *)
	    ( match body with  
	      | Expr_atom ( Atom_sym ":parameters" ) :: 
		  Expr_list params :: body ->
		( match body with 
		  | Expr_atom ( Atom_sym ":precondition" ) ::
		      Expr_list precond :: body ->
		    ( match body with 
		      | Expr_atom ( Atom_sym ":effect" ) :: 
			  effect -> 
			{ 
			  name = name ;
			  parameters = params_of_sexpr params;
			  precondition = conj_of_sexpr (Expr_list(precond));
			  effect = conj_of_sexpr (Expr_list(effect))
			}
		      | _ -> failwith "syntax error: unrecognized action structure"
		    )
		)
	    )
	)
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
	  Expr_predicates( List.map pred_of_sexpr body )
	| Expr_atom ( Atom_sym ":action" ) :: body -> 
	  Expr_action( List.map action_of_sexpr 
			 ([Expr_atom ( Atom_sym ":action" ) :: body]) )
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
       | Expr_predicates( predlist ) -> 
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


