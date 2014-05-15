(* ast.ml *)
open Sexpr
open Util

type sym = string

type atom =
  | Atom_var of sym (* ?symbol *)
  | Atom_gnd of sym (* symbol *)
  | Atom_nil

type predicate = 
  | Pred_var of sym * atom list (* (predname vatom v/gatom?) *)
  | Pred_gnd of sym * atom list (* (predname gatom gatom?) *)
  | Pred_nil

type conjunction =
  | Conj_and of conjunction list 
  | Conj_neg of predicate 
  | Conj_pos of predicate
  | Conj_nil

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
  | Expr_init       of predicate list  (* :init body *)
  | Expr_goal       of predicate list  (* :goal body *)
  | Expr_action     of action          (* :action ... *)
  | Expr_objects    of atom list       (* :objects body *)
  | Expr_unit                          (* () *)                          

let sprintf  = Printf.sprintf

let spaces n = String.make n ' '

(* string from string list *)
let rec string_of_syms sym_lst = 
  (match sym_lst with
    | []   -> ""
    | [s] -> s
    | h::t -> h ^ " " ^ (string_of_syms t)
  )

(* sexpr.atom -> string *)
let sym_of_atom a = 
  ( match a with 
    | Atom_unit -> "" (* shouldn't be used *)
    | Atom_sym s -> s
    | Atom_int _ -> 
      let error_msg = "improper symbol conversion" in
      failwith error_msg
  ) 

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

(* TODO: account for nil cases *)
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
	      | Atom_nil ->
		let error_msg = "unrecognized predicate" in
		failwith error_msg
	    )
	  | Atom_nil ->
	    let error_msg = "unrecognized predicate" in
	    failwith error_msg
	)
      | [ Expr_atom ( Atom_sym name ) ; Expr_atom p1 ] ->
	let a1 = astatom_of_atomsym p1 in
	( match a1 with 
	  | Atom_var _ -> 
	    Pred_var( name , [a1] )
	  | _ ->
	    Pred_gnd( name , [a1] )
	) 
      | _ -> 
	let error_msg = "unrecognized predicate" in
	failwith error_msg
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

(* sexpr.expr -> conjunction *)
let rec conj_of_sexpr sx = 
  ( match sx with 
    | Expr_list l ->
      ( match l with
	| [ Expr_atom( Atom_sym name ) ; Expr_atom p1 ; Expr_atom p2 ] ->
	  let a1 = astatom_of_atomsym p1 
	  and a2 = astatom_of_atomsym p2 in
	  ( match a1 with 
	    | Atom_var _ -> 
	      Conj_pos(Pred_var( name , [a1 ; a2] ))
	    | Atom_gnd _ ->
	      ( match a2 with
		| Atom_var _ -> 
		  Conj_pos(Pred_var( name , [a1 ; a2] ))
		| Atom_gnd _ -> 
		  Conj_pos(Pred_gnd( name , [a1 ; a2] ))
		| Atom_nil ->
		  let error_msg = "unrecognized predicate" in
		  failwith error_msg
	      )
	    | Atom_nil ->
	      let error_msg = "unrecognized predicate" in
	      failwith error_msg
	  )
	| [ Expr_atom ( Atom_sym name ) ; Expr_atom p1 ] ->
	  let a1 = astatom_of_atomsym p1 in
	  ( match a1 with 
	    | Atom_var _ -> 
	      Conj_pos(Pred_var( name , [a1] ))
	    | _ ->
	      Conj_pos(Pred_gnd( name , [a1] ))
	  )
	| Expr_atom ( Atom_sym "and" ) :: body -> (* body is list *)
	  Conj_and ( List.map conj_of_sexpr body )
	| [ Expr_atom ( Atom_sym "not" ) ; p1 ] -> 
	  Conj_neg ( pred_of_sexpr p1 ) (*TODO: nesting ands *) 
	| _ -> 
	  let error_msg = "invalid conjunction" in
	  failwith error_msg
      )
    | _ -> 
      let error_msg = "invalid conjunction" in
      failwith error_msg
  )

(* sexpr.expr list -> ast.action *)
let action_of_sexpr sx =
  ( match sx with
    | Expr_atom _ :: (* :action *)
	Expr_atom ( Atom_sym name ) ::
        Expr_atom ( Atom_sym ":parameters" ) :: Expr_list params ::
        Expr_atom ( Atom_sym ":precondition" ) :: Expr_list precond :: 
	Expr_atom ( Atom_sym ":effect" ) :: Expr_list effect :: [] ->
      { 
	name = name;
	parameters = params_of_sexpr params;
	precondition = conj_of_sexpr (Expr_list(precond));
	effect = conj_of_sexpr (Expr_list(effect))
      } 
    | _ -> 
      let error_msg = "unrecognized action structure" in
      failwith error_msg
  )

let rec ast_of_sexpr sx =  
  (match sx with
    | Expr_atom a -> 
      let error_msg = "unrecognized s-expression" in
      failwith error_msg
    | Expr_list l -> 
      ( match l with
	| Expr_atom ( Atom_sym ":predicates" ) :: body ->
	  Expr_predicates( List.map pred_of_sexpr body )
	| Expr_atom ( Atom_sym ":action" ) :: body -> 
	  Expr_action( action_of_sexpr l )
	| Expr_atom ( Atom_sym ":objects" ) :: body -> 
	  Expr_objects( List.map astatom_of_sexpr body )
	| Expr_atom ( Atom_sym ":init" ) :: body -> 
	  Expr_init( List.map pred_of_sexpr body )
	| Expr_atom ( Atom_sym ":goal" ) :: body -> 
	  Expr_goal( List.map pred_of_sexpr body )
	| Expr_atom ( Atom_sym "define" ) ::
	    Expr_list l :: body -> (* body *)
	  ( match l with 
	    | [Expr_atom ( Atom_sym "domain" ) ; 
	       Expr_atom ( Atom_sym name )] ->
	      Expr_domain( name , List.map ast_of_sexpr body )
	    | [Expr_atom ( Atom_sym "problem" ) ; 
	       Expr_atom ( Atom_sym name )] -> 
	      Expr_problem( name , List.map ast_of_sexpr body )
	    | _ -> 
	      let error_msg = "unrecognized s-expression" in
	      failwith error_msg
	  )
	| _ -> 
	  let error_msg = "unrecognized s-expression" in
	  failwith error_msg
      )
  )


let string_of_atom atom =
  ( match atom with 
    | Atom_var a -> a
    | Atom_gnd a -> a
    | Atom_nil -> "NIL"
  )
 
let string_of_pred pred = 
  ( match pred with 
    | Pred_var( name , params ) ->
      sprintf "PRED_VAR( %s , %s )\n" 
	name 
	(string_of_syms(List.map string_of_atom params))
    | Pred_gnd( name , params ) ->
      sprintf "PRED_GND( %s , %s )\n" 
	name 
	(string_of_syms(List.map string_of_atom params))
    | Pred_nil -> 
      "PRED_NIL()\n"
  )

let rec string_of_conj conj = 
  let recurse = string_of_conj in
  ( match conj with 
    | Conj_and c -> 
      sprintf "CONJ_AND(%s)"
      (string_of_syms (List.map recurse c))
    | Conj_pos c -> 
      sprintf "CONJ_POS(%s)"
	(string_of_pred c)
    | Conj_neg c -> 
      sprintf "CONJ_NEG(%s)"
	(string_of_pred c)
    | Conj_nil -> ""
  )

let string_of_params params = 
  sprintf "PARAMETERS( %s )"
  (string_of_syms (List.map string_of_atom params))

let string_of_precond precond =
  sprintf "PRECONDITION( %s )"
  (string_of_conj precond)

let string_of_effect effect =
  sprintf "EFFECT( %s )"
  (string_of_conj effect)

let string_of_action act = 
  act.name ^ "\n" ^
  (string_of_params act.parameters) ^ "\n" ^ 
  (string_of_precond act.precondition) ^ "\n" ^
  (string_of_effect act.effect)


let string_of_ast ast =
  let rec iter ast indent =
    let string_of_exprs e_list =
      (List.fold_left (^) ""
	 (List.map (fun e -> "\n" ^ iter e (indent + 2)) e_list))
    in
    ( match ast with
      | Expr_unit -> sprintf "%sUNIT" (spaces indent) 
      | Expr_domain ( name , body ) -> 
	sprintf "%sDOMAIN[ %s\n%s ]" 
          (spaces indent) name (string_of_exprs body)
      | Expr_problem ( name , body ) -> 
	sprintf "%sPROBLEM[ %s\n%s ]" 
          (spaces indent) name (string_of_exprs body)
      | Expr_predicates( preds ) -> 
	sprintf "%sPREDICATES[ %s ]\n"
	  ( spaces indent )
	  (string_of_syms (List.map string_of_pred preds))
      | Expr_action ( act ) ->
	sprintf "%sACTION[ %s ]\n"
	  (spaces indent) 
	  (string_of_action act)
      | Expr_objects ( objs ) ->
	sprintf "%sOBJECTS[ %s ]\n"
	  (spaces indent) 
	  (string_of_syms (List.map string_of_atom objs))
      | Expr_init ( preds ) ->
	sprintf "%sINIT[ %s ]\n"
	  (spaces indent) 
	  (string_of_syms (List.map string_of_pred preds))
      | Expr_goal ( preds ) ->
	sprintf "%sGOAL[ %s ]\n"
	  (spaces indent) 
	  (string_of_syms (List.map string_of_pred preds))
    )
  in "\n" ^ iter ast 0 ^ "\n"
