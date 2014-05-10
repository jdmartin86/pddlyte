(* symbol table generation *)
open Ast

type sym = string

(* problem table for planner *)
type strips_problem = 
    { 
      mutable init : Ast.predicate list;
      mutable goal : Ast.predicate list;
      mutable ops  : Ast.action list   ;
    }

(* environment *)
type env = 
    { 
      parent: env option; 
      bindings: (sym, sym) Hashtbl.t;
      mutable problem: strips_problem (* TODO: test this *)
    }

(** level-one dependency **)

(* create a new strips problem and symbol table *)
let make parent = 
  { 
    parent = parent; 
    bindings = Hashtbl.create 5;
    problem = { init = [] ; goal = [] ; ops = [] }
  }

(* lookup symbol table entry *)
let rec lookup env name = 
   let { parent = p ; bindings = b } = env in
   try  Hashtbl.find b name
   with Not_found ->
       ( match p with
	 | Some( parent ) -> lookup parent name
	 | None ->
	   let error_msg = "unknown symbol: " ^ name in
	   failwith error_msg 
       )

(* lookup symbol table entry with an atom *)
let lookup_atom env atom = 
  ( match atom with 
    | Atom_var a -> lookup env a
    | Atom_gnd a -> lookup env a 
    | _ ->  
      let error_msg = "unknown atom" in
      failwith error_msg
  )

(* insert entry into symbol table *)
let add env name value = 
   let { parent = _ ; bindings = b } = env in
   Hashtbl.add b name value

(* insert initial state into strips problem *)
let add_init env state = 
  env.problem.init <- state

(* insert goal state into strips problem *)
let add_goal env state = 
  env.problem.goal <- state

(* insert operator into strips problem *)
let add_action env act = 
  let acts = env.problem.ops in
  env.problem.ops <- act::acts

(* split the atom for its name, plus semantic checks *) 
let atom_name a =
  if String.contains a '-' then 
    let stop = (String.index a '-') in
    String.sub a 0 stop
  else
    let error_msg = "expected a type specification for " ^ a in
    failwith error_msg

(* split the atom for its type, plus semantic checks *)
let atom_type a =
  if String.contains a '-' then 
    let start = (String.index a '-') + 1 in
    let stop = (String.length a) - start in
    String.sub a start stop
  else
    let error_msg = "expected a type specification for " ^ a in
    failwith error_msg

(* insert grounded atom into symbol table using object semantics *)
let translate_object env param =
  ( match param with  (* seek type *)
    | Atom_gnd a -> 
      add env (atom_name a) (atom_type a)
    | Atom_var a ->
      let error_msg = "objects may only contain grounded atoms" in
      failwith error_msg
    | _ -> failwith "parameter improperly parsed"
  )

(* prints the strips problem *)
let string_of_strips prob =
  let { init = s0 ; goal = g ; ops = acts } = prob in
  let title = 
    "STRIPS PROBLEM\n" in
  let initial_state =
   "INITIAL STATE:\n" 
    ^ (string_of_syms (List.map string_of_pred s0)) in   
  let goal_state =
    "GOAL STATE:\n" 
    ^ (string_of_syms (List.map string_of_pred g)) in
  let actions = 
    "ACTIONS:\n" 
    ^ (string_of_syms (List.map string_of_action acts)) in
  sprintf "%s\n%s\n%s\n%s\n"
    (title)
    (initial_state)
    (goal_state)
    (actions)

(** level-two dependency **)

(* insert a grounded atom into symbol table  *)
let translate_grounded_param env param =
  ( match param with  (* TODO: type? *)
    | Atom_gnd a -> 
      add env ":static" (atom_name a)
    | _ -> failwith "parameter improperly parsed"
  )

(* insert variable atom into symbol table using parameter semantics *)
let translate_variable_param env param =
  ( match param with 
    | Atom_var a ->
      add env (atom_name a) (atom_type a)
    | Atom_gnd a -> 
      let error_msg = 
	"expected "
	^ a
	^ " to be a variable parameter" in
      failwith error_msg
    | _ -> failwith "parameter improperly parsed"
  )

(* insert objects into symbol table *)
let translate_objects env params = 
  let translate_obj = translate_object env in
  List.iter translate_obj params

(* lookup parameters *)
let check_params env params = 
  List.iter (fun p -> ignore( lookup_atom env p )) params  

(** level-three dependency **)

(* insert a list of variable atoms using parameter sematics *)
let translate_var_params env params = 
  let translate_var_param = translate_variable_param env in
  List.iter translate_var_param params

(* insert grounded atoms into symbol table using parameter semantics *)
let translate_gnd_params env params = 
  let translate_gnd_param = translate_grounded_param env in
  List.iter translate_gnd_param params

(* lookup predicate declaration *)
let check_var_pred env pred = 
  ( match pred with 
    | Pred_var( name , params ) -> 
      let value = lookup env name in
      ( match value with
	| ":predicate" -> 
	  check_params env params
	| _ -> 
	  let error_msg = "undefined predicate" in
	  failwith error_msg
      )
    | _ -> 
      let error_msg = "predicates must have variable parameters" in
      failwith error_msg
  )

(* lookup a grounded predicate using state semantics *)
let check_gnd_pred env pred = 
  ( match pred with 
    | Pred_gnd( name , params ) ->
      let value = lookup env name in
      ( match value with
	| ":predicate" -> 
	  check_params env params
	| _ -> 
	  let error_msg = "undefined predicate" in
	  failwith error_msg
      )
    | _ -> 
      let error_msg = "states must have grounded parameters" in
	failwith error_msg 
  )

(** level-four dependency **)

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
      failwith error_msg
  )

let translate_effect env effect = 
  translate_precondition env effect 

let translate_state env state =
  let check_state = check_gnd_pred env in
  List.iter check_state state

(*TODO: describe restrictions on names -- handle nil case*)
let translate_predicate env pred =
    ( match pred with 
      | Pred_var( name , params ) ->
	let _ = add env name ":predicate" in
	translate_var_params env params
      | Pred_gnd( name , params ) -> 
	add env name ":predicate" ;
	translate_gnd_params env params
      | _ -> 
	let error_msg = "predicate improperly parsed" in
	failwith error_msg
    )

(** level-five dependency **)

(* inset predicates into symbol table *)
let translate_predicates env preds =
  let translate_pred = translate_predicate env in 
  List.iter translate_pred preds

(* insert action into symbol table and stips problem *)
let translate_action env act = 
  let { 
        name = n ; 
        parameters = params ; (* enforce variable homogeneity *)
        precondition = precond ;
        effect = eff 
      } = act in
  let _ = add env n ":action" in (* add action name to global list *)
  let parent = Some(env) in 
  let param_env = make parent in (* make parameter table *)
  let _ = translate_var_params param_env params in (* add params *)
  let _ = translate_precondition param_env precond in
  translate_effect param_env eff  

(** level-six dependency **)

(* TODO: add type checking and semantic checks! *)
(* mapping from ast to strips problem -- semantic checks burried *) 
let rec strips_of_ast env ast = 
  let recurse = strips_of_ast env in
  ( match ast with 
    | Expr_predicates( preds ) -> 
      translate_predicates env preds
    | Expr_action( act ) ->
      let _ = translate_action env act in
      add_action env act
    | Expr_objects( objs ) -> 
      translate_objects env objs
    | Expr_init( init ) -> 
      let _ = translate_state env init in
      let p = env.parent in (* TODO: cleanup *)
      ( match p with
	| Some( parent ) ->
	  add_init parent init
	| None ->
	  let error_msg = "symbol table has no parent" in
	  failwith error_msg
      )
    | Expr_goal( goal ) -> 
      let _ = translate_state env goal in
      let p = env.parent in
      ( match p with
	 | Some( parent ) ->
	   add_goal parent goal
	 | None ->
	   let error_msg = "symbol table has no parent" in
	   failwith error_msg
       )
    | Expr_domain( name , body ) -> 
      add env name ":domain" ; List.iter recurse body
    | Expr_problem( name , body ) ->
      let parent = Some(env) in
      let new_env = make parent in
      add env name ":problem" ; List.iter (strips_of_ast new_env) body
    | _ ->
      let error_msg = 
	"program may only contain a domain and problem declaration" in
      failwith error_msg	  
  )
