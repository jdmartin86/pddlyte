(* symbol table generation *)
open Ast

(* TODO: consolidate these *)
exception Multiple_domains of string
exception Unknown_sym of string
exception Compile_error of string

type sym = string

(* environment *)
type env = { parent: env option ; bindings: (sym, sym) Hashtbl.t }

(* symbol table for planner *)
type strips_problem = (* TODO: make these names consistent with others *)
{ 
  init : Ast.predicate list;
  goal : Ast.predicate list;
  ops  : Ast.action list   ;
}

let make_strips = { init = [] ; goal = [] ; ops = [] }

(* Environments. *)
let make parent = { parent = parent; bindings = Hashtbl.create 5 }

(* lookup symbol-table entry *)
let rec lookup env name = 
   let { parent = p ; bindings = b } = env in
   try  Hashtbl.find b name
   with Not_found ->
       ( match p with
	 | Some( parent ) -> lookup parent name
	 | None -> raise Not_found
       )

let lookup_atom env atom = 
  ( match atom with 
    | Atom_var a -> lookup env a
    | Atom_gnd a -> lookup env a 
    | _ -> raise Not_found
  )

(* insert entry into symbol table *)
let add env name value = 
   let { parent = _ ; bindings = b } = env in
   Hashtbl.add b name value

(* insert a list of entries into symbol table *)
let add_all env names values = 
   let pairs = List.combine names values in
      List.iter (fun (x, y) -> add env x y) pairs

let atom_name a =
  let stop = (String.index a '-') in
  String.sub a 0 stop

let atom_type a =
  let start = (String.index a '-') + 1 in
  let stop = (String.length a) - start in
  String.sub a start stop

let translate_variable_param env param =
  ( match param with 
    | Atom_var a -> (* split type from name *)
      add env (atom_type a) (atom_name a)
    | Atom_gnd a -> 
      let error_msg = "parameters must be homogeneously variable" in
      raise (Compile_error error_msg)
    | _ -> failwith "parameter improperly parsed"
  )

let translate_var_params env params = 
  let translate_var_param = translate_variable_param env in
  List.iter translate_var_param params

let translate_grounded_param env param =
  ( match param with  (* seek type *)
    | Atom_gnd a -> 
      add env ":static" (atom_name a)
    | _ -> failwith "parameter improperly parsed"
  )

let translate_gnd_params env params = 
  let translate_gnd_param = translate_grounded_param env in
  List.iter translate_gnd_param params

(*TODO: describe restrictions on names *)
let translate_predicate env pred =
    ( match pred with 
      | Pred_var( name , params ) -> 
	add env ":predicate" name ;
	let parent = Some(env) in 
	let new_env = make parent in
	add new_env ":predicate" name ;
	translate_var_params new_env params
      | Pred_gnd( name , params ) -> 
	add env ":predicate" name ;
	let parent = Some(env) in 
	let new_env = make parent in 
	add new_env ":predicate" name ;
	translate_gnd_params new_env params
      | _ -> failwith "predicate improperly parsed"
    )

(* env -> pred list -> unit *)
let translate_predicates env preds =
  let translate_pred = translate_predicate env in 
  List.iter translate_pred preds

(* ensure the actions are type safe before passing to the planner
   - planner gets an action list 
     - applicable_actions uses preconditions
     - successor function uses effects
   - populate symbol with actions to type check 
   ->> add action name to global symbol table
     ->> make child symbol table for action parameters
       ->> make child symbol table for predconditions
       ->> make child symbol table for effects
*)

(* do i need another symbol table? or just to verify types and
 add to plan table?

- ensure the predicate name exits
- ensure the parameter names are in the param list
- if these pass, add them to the plan table
*)

let check_params env params = 
  List.iter (fun p -> ignore( lookup_atom env p )) params  

let check_var_pred env pred = 
  ( match pred with 
    | Pred_var( name , params ) ->
      let value = lookup env name in
      ( match value with
	| ":predicate" -> 
	  check_params env params
	| _ -> 
	  let error_msg = "undefined predicate" in
	  raise (Compile_error error_msg)
      )
    | _ -> 
      let error_msg = "predicates must have variable parameters" in
	raise (Compile_error error_msg) 
  )

let rec translate_precondition env precond =
  let recurse = translate_precondition env in 
  ( match precond with 
    | Conj_and conj -> 
      List.iter recurse conj
    | Conj_neg pred -> 
      check_var_pred env pred
    | Conj_pos pred -> 
      check_var_pred env pred
    | _ -> 
      let error_msg = "empty precondition" in
	raise (Compile_error error_msg)
  )

let translate_effect env effect = 
  translate_precondition env effect 

(* TODO: this is quite ugly ... *)
let translate_action env act = 
  let { 
        name = n ; 
        parameters = params ; (* enforce variable homogeneity *)
        precondition = precond ;
        effect = eff 
      } = act in
  add env n ":action" ; (* add action name to global list *)
  let parent = Some(env) in 
  let param_env = make parent in (* make parameter table *)
  translate_var_params param_env params ;
  translate_precondition param_env precond;
  translate_effect param_env eff  

let translate_object env param =
  ( match param with  (* seek type *)
    | Atom_gnd a -> 
      add env (atom_name a) (atom_type a)
    | Atom_var a ->
      let error_msg = "objects may only contain grounded atoms" in
      raise (Compile_error error_msg)
    | _ -> failwith "parameter improperly parsed"
  )

let translate_objects env params = 
  let translate_obj = translate_object env in
  List.iter translate_obj params

let check_params env params = 
  List.iter (fun p -> ignore( lookup_atom env p )) params  

let check_gnd_pred env pred = 
  ( match pred with 
    | Pred_gnd( name , params ) ->
      let value = lookup env name in
      ( match value with
	| ":predicate" -> 
	  check_params env params
	| _ -> 
	  let error_msg = "undefined predicate" in
	  raise (Compile_error error_msg)
      )
    | _ -> 
      let error_msg = "states must have grounded parameters" in
	raise (Compile_error error_msg) 
  )

let translate_state env state =
  let check_state = check_gnd_pred env in
  List.iter check_state state

(* ast -> plan_table *)
(* TODO: add type checking and semantic checks! *)
let rec translate_ast env ast = 
  let recurse = translate_ast env in
  ( match ast with 
    | Expr_predicates( preds ) ->
      translate_predicates env preds
    | Expr_action( act ) -> 
      translate_action env act
    | Expr_objects( objs ) -> 
      translate_objects env objs
    | Expr_init( init ) -> 
      translate_state env init
    | Expr_goal( goal ) -> 
      translate_state env goal
    | Expr_domain( name , body ) -> 
      add env name ":domain" ; List.iter recurse body
    | Expr_problem( name , body ) -> 
      let parent = Some(env) in
      let new_env = make parent in
      add env name ":problem" ; List.iter (translate_ast new_env) body
  )

let prepare_strips_problem env strips ast = 
  ( match ast with
    | Expr_action( act ) -> 
      let { init = s0 ; goal = g ; ops = acts } = strips in
          { init = s0 ; goal = g ; ops = act::acts }
    | Expr_init( init ) ->
      let { init = s0   ; goal = g ; ops = acts } = strips in
          { init = init ; goal = g ; ops = acts }
    | Expr_goal( goal ) -> 
      let { init = s0 ; goal = g    ; ops = acts } = strips in
          { init = s0 ; goal = goal ; ops = acts }
    | _ -> failwith "progress"
  )

let strips_of_ast env prob ast =
  let _ = translate_ast env ast in
  prepare_strips_problem env prob ast
