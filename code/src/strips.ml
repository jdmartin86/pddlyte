(* symbol table generation *)
open Ast
open Util

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
    let error_msg = "expected a type specification for '" ^ a ^"'"  in
    failwith error_msg

(* split the atom for its type, plus semantic checks *)
let atom_type a =
  if String.contains a '-' then 
    let start = (String.index a '-') + 1 in
    let stop = (String.length a) - start in
    String.sub a start stop
  else
    let error_msg = "expected a type specification for '" ^ a ^ "'" in
    failwith error_msg

(* insert grounded atom into symbol table using object semantics *)
let translate_object env obj =
 let { parent = genv ; bindings = local } = env in
  ( match genv with
    | None -> 
      let error_msg = "no global symbol table for parameters" in
      failwith error_msg
    | Some( global_env ) -> 
      let { parent = _ ; bindings = global } = global_env in
      ( match obj with  (* seek type *)
	| Atom_gnd a -> 
	  ( try 
	      let _ = Hashtbl.find local (atom_name a) in
	      let error_msg = 
		"an object named '"
		^ (atom_name a)
		^ "' has already been declared in this scope" in
	      failwith error_msg
	    with Not_found -> (* object name is unique *)
	      ( try 
		  let _ = Hashtbl.find global (":type-"^(atom_type a)) in
		  add env (atom_name a) (atom_type a)
		with Not_found -> 
		  let error_msg = 
		    "expected type declaration for '"
		    ^ (atom_type a)
		    ^ "'"
		  in failwith error_msg
	      )
	  )	    
	| Atom_var a ->
	  let error_msg = 
	    "expected '"
            ^ a 
            ^ "' to be a grounded atom" in
	  failwith error_msg
	| Atom_nil -> 
	  let error_msg = "expected object declaration to be non-empty" in
	  failwith error_msg
      )
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

(* *)
let translate_predicate_param env param = (* local parameter scope *)
  let { parent = genv ; bindings = local } = env in
  ( match genv with
    | None -> 
      let error_msg = "no global symbol table for parameters" in
      failwith error_msg
    | Some( global_env ) -> 
      let { parent = _ ; bindings = global } = global_env in
      ( match param with 
	| Atom_var a ->
	  ( try (* parameter name must be unique in local scope *)
	      let _ = Hashtbl.find local (atom_name a) in
	      let error_msg = 
		"a predicate parameter named '"
		^ (atom_name a)
		^ "' has already been declared in its scope" in
	      failwith error_msg
	    with Not_found -> (* parameter name is unique *)
	      let _ = add env (atom_name a) (atom_type a) in
	      Hashtbl.replace global (":type-"^(atom_type a)) ":type"
	  )
	| Atom_gnd a -> 
	  let error_msg = 
	    "expected '"
	    ^ a
	    ^ "' to be a variable parameter" in
	  failwith error_msg
	| _ -> 
	  let error_msg = 
	    "expected  parameter declaration to be non-empty" in
	  failwith error_msg
      )
  )
(* insert variable atom into symbol table using parameter semantics *)
let translate_parameter_declaration env param =
  ( match param with 
    | Atom_var a ->
(* check types of each atom exist and are used properly in declaration *)
      let { parent = p ; bindings = b } = env in
      ( try 
	  let _ = Hashtbl.find b (atom_name a) in
	  let error_msg = 
	    "a parameter named '"
	    ^ (atom_name a)
	    ^ "' has already been declared in this scope" in
	  failwith error_msg
	with Not_found -> (* parameter name is unique *)
	  add env (atom_name a) (atom_type a)
      )
    | Atom_gnd a -> 
      let error_msg = 
	"expected '"
	^ a
	^ "' to be a variable parameter" in
      failwith error_msg
    | _ -> 
      let error_msg = 
	"expected parameter declaration to be non-empty" in
      failwith error_msg
  )

(* insert objects into symbol table *)
let translate_objects env params = 
  let translate_obj = translate_object env in
  List.iter translate_obj params

(* lookup parameters *)
let check_params env params = (* params = atom list *)
  let { parent = genv ; bindings = local } = env in
  ( match genv with
    | None -> 
      let error_msg = "no global symbol table for predicate" in
      failwith error_msg
    | Some( global_env ) -> 
      let rec check_parameter parameters =
	( match parameters with 
	  | [] -> ()
	  | Atom_var(a)::t -> 
	    ( try (* check that parameter name exists in the local scope *)
		let _ = Hashtbl.find local a in (* type not specified *)
		check_parameter t
	      with Not_found ->
		let error_msg = 
		  "expected parameter declaration for '"
		  ^ a
		  ^ "'"
		in failwith error_msg
	    )
	  | Atom_gnd(a)::t ->
	    ( try (* check that parameter name exists in the local scope *)
		let _ = Hashtbl.find local a in (* type not specified *)
		check_parameter t
	      with Not_found ->
		let error_msg = 
		  "expected parameter declaration for '"
		  ^ a
		  ^ "'"
		in failwith error_msg
	    )
	  | _ -> 
	    let error_msg = "expected grounded or variable atom" in
	    failwith error_msg
	)
      in check_parameter params
  )

(** level-three dependency **)

(* insert a list of variable atoms using parameter sematics *)
let translate_parameter_declarations env params = 
  let translate_param = translate_parameter_declaration env in
  List.iter translate_param params

(* check and insert a list of predicate parameters into the symbol table *)
let translate_predicate_params env params = 
  let translate_param = translate_predicate_param env in (* local parameter scope *)
  List.iter translate_param params

(* insert grounded atoms into symbol table using parameter semantics *)
let translate_gnd_params env params = 
  let translate_gnd_param = translate_grounded_param env in
  List.iter translate_gnd_param params

(* lookup predicate declaration *)
let check_var_pred env pred = (* local action parameter scope *)
  let { parent = genv ; bindings = local } = env in
  ( match genv with
    | None -> 
      let error_msg = "no global symbol table for predicate" in
      failwith error_msg
    | Some( global_env ) -> 
      let { parent = _ ; bindings = global } = global_env in     
      ( match pred with 
	| Pred_var( name , params ) -> (* check pred is declared in global *)
	  ( try
	      let _ = Hashtbl.find global (":predicate-"^name) in
	      check_params env params 
	    with Not_found -> 
	      let error_msg = 
		"expected declaration for predicate '"
		^ name in
	      failwith error_msg
	  )
	| Pred_gnd( name , params ) -> 
	  let error_msg = 
	    "expected '"
            ^ name
            ^ "' to be a variable predicate" 
	  in failwith error_msg
	| Pred_nil ->
	  let error_msg = 
	    "expected predicate to be non-empty" in
	  failwith error_msg
      )
  )

(* lookup a grounded predicate using state semantics *)
let check_gnd_pred env pred = 
let { parent = genv ; bindings = local } = env in
  ( match genv with
    | None -> 
      let error_msg = "no global symbol table for predicate" in
      failwith error_msg
    | Some( global_env ) -> 
      let { parent = _ ; bindings = global } = global_env in     
      ( match pred with 
	| Pred_gnd( name , params ) -> (* check pred is declared in global *)
	  ( try
	      let _ = Hashtbl.find global (":predicate-"^name) in
	      check_params env params 
	    with Not_found -> 
	      let error_msg = 
		"expected declaration for predicate '"
		^ name in
	      failwith error_msg
	  )
	| Pred_var( name , params ) -> 
	  let error_msg = 
	    "expected '"
            ^ name
            ^ "' to be a grounded predicate" 
	  in failwith error_msg
	| Pred_nil ->
	  let error_msg = 
	    "expected predicate to be non-empty" in
	  failwith error_msg
      )
  )

(** level-four dependency **)

let rec translate_precondition env precond = (* local action scope *)
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

(* check and add a predicate declaration into the symbol table *)
let translate_predicate_declaration env pred =
    ( match pred with 
      | Pred_var( name , params ) ->
	let { parent = p ; bindings = b } = env in
	( try 
	  let _ = Hashtbl.find b (":predicate-"^name) in
	  let error_msg = 
	    "a predicate named '"
	    ^ name
	    ^ "' has already been declared in this scope" in
	  failwith error_msg
	with Not_found -> (* predicate name is unique *)
	  let _ = add env (":predicate-"^name) ":predicate" in
	  let parent = Some(env) in 
	  let param_env = make parent in 
	  translate_predicate_params param_env params
	)
      | Pred_gnd( name , params ) ->
	let error_msg = 
	  "expected '"
	  ^ name
	  ^ "' to be a variable predicate" in
	failwith error_msg
      | Pred_nil ->
	let error_msg = 
	  "predicate declaration should be non-empty" in
	failwith error_msg
    )

(* add parameter types to global scope *)
let translate_predicate_param_to_global env pred =
  ( match pred with
    | Pred_var( name , params ) ->
      let rec loop count p =	
	( match p with 
	  | [] -> ()
	  | Atom_var(a)::t -> 
	    let c = sprintf "%d" count in 
	    let _ = 
	      add env 
	      ("predicate-"^name^"-p"^c^"-"^(atom_type a)) 
	      ":typespec" in
	    loop (count+1) t
	  | _ ->
	    let error_msg = 
	      "expected variable atom before this point ..." in
	    failwith error_msg
	)
      in loop 0 params
    | _ -> 
      let error_msg = "expected variable predicate before this point..." in
      failwith error_msg
  )

let check_var_pred_types local_env pred =
( match pred with 
  | Pred_var( name , params ) ->
    let rec loop count p =
      ( match p with
	| [] -> ()
	| Atom_var(a)::t -> 
	  let action_type = lookup local_env a in
	  let c = sprintf "%d" count in 
	  let action_type_spec = "predicate-"^name^"-p"^c^"-"^action_type in
	  let { parent = genv ; bindings = local } = local_env in
	  ( match genv with
	    | None -> 
	      let error_msg = "no global symbol table for predicate" in
	      failwith error_msg
	    | Some( global_env ) -> 
	      let { parent = _ ; bindings = global } = global_env in   
	      ( try 
		  let _ = Hashtbl.find global action_type_spec in
		  loop (count+1) t
		with Not_found ->
		  let error_msg = 
		    "expected a different type for parameter '"
		    ^ c
		    ^ "' of action predicate '"
		    ^ name
		    ^ "'"
		  in failwith error_msg	
	      )	 
	  ) 
	| _ ->
	  let error_msg = 
	    "expected variable atom before this point ..." in
	  failwith error_msg
      )
    in loop 0 params
  | _ -> 
    let error_msg = 
      "expected variable predicate before this point ..." in
    failwith error_msg
)
  
let check_gnd_pred_types local_env pred =
( match pred with 
  | Pred_gnd( name , params ) ->
    let rec loop count p =
      ( match p with
	| [] -> ()
	| Atom_gnd(a)::t -> 
	  let state_type = lookup local_env a in
	  let c = sprintf "%d" count in 
	  let state_type_spec = "predicate-"^name^"-p"^c^"-"^state_type in
	  let { parent = genv ; bindings = local } = local_env in
	  ( match genv with
	    | None -> 
	      let error_msg = "no global symbol table for predicate" in
	      failwith error_msg
	    | Some( global_env ) -> 
	      let { parent = _ ; bindings = global } = global_env in   
	      ( try 
		  let _ = Hashtbl.find global state_type_spec in
		  loop (count+1) t
		with Not_found ->
		  let error_msg = 
		    "expected a different type for parameter '"
		    ^ c
		    ^ "' of state predicate '"
		    ^ name
		    ^ "'"
		  in failwith error_msg	
	      )	 
	  ) 
	| _ ->
	  let error_msg = 
	    "expected grounded atom before this point ..." in
	  failwith error_msg
      )
    in loop 0 params
  | _ -> 
    let error_msg = 
      "expected grounded predicate before this point ..." in
    failwith error_msg
)

let rec check_types local_env conj =
  let recurse = check_types local_env in 
  ( match conj with 
    | Conj_and c -> 
      List.iter recurse c
    | Conj_neg pred -> 
      check_var_pred_types local_env pred
    | Conj_pos pred -> 
      check_var_pred_types local_env pred
    | _ -> 
      let error_msg = "empty conjugate" in
      failwith error_msg
  )


(** level-five dependency **)

(* inset predicates into symbol table *)
let translate_predicate_declaration env preds =
  let translate_pred = translate_predicate_declaration env in 
  let _ = List.iter translate_pred preds in
  let translate_to_global = translate_predicate_param_to_global env in
  List.iter translate_to_global preds

(* insert action into symbol table and stips problem *)
let translate_action env act = 
  let { 
        name = n ; 
        parameters = params ; (* enforce variable homogeneity *)
        precondition = precond ;
        effect = eff 
      } = act in
  let { parent = p ; bindings = b } = env in
      ( try 
	  let _ = Hashtbl.find b (":action-"^n) in
	  let error_msg = 
	    "an action named '"
	    ^ n
	    ^ "' has already been declared in this scope" in
	  failwith error_msg
	with Not_found -> (* parameter name is unique *)
	  let _ = add env (":action-"^n) ":action" in 
	  let parent = Some(env) in 
	  let param_env = make parent in (* make parameter table *)
	  let _ = translate_parameter_declarations param_env params in 
	  let _ = translate_precondition param_env precond in
	  let _ = check_types param_env precond in
	  let _ = translate_effect param_env eff in
	  check_types param_env eff
      )

(** level-six dependency **)

(* mapping from ast to strips problem -- semantic checks burried *) 
let rec strips_of_ast env ast = 
  let recurse = strips_of_ast env in
  ( match ast with 
    | Expr_predicates( preds ) -> 
      translate_predicate_declaration env preds
    | Expr_action( act ) ->
      let _ = translate_action env act in
      add_action env act
    | Expr_objects( objs ) -> 
      translate_objects env objs
    | Expr_init( init ) -> 
      let _ = translate_state env init in
      let check_problem_types = check_gnd_pred_types env in
      let _ = List.iter check_problem_types init in 
      let p = env.parent in (* TODO: cleanup *)
      ( match p with
	| Some( parent ) ->
	  add_init parent init
	| None ->
	  let error_msg = ":init symbol table has no parent" in
	  failwith error_msg
      )
    | Expr_goal( goal ) -> 
      let _ = translate_state env goal in
      let check_problem_types = check_gnd_pred_types env in
      let _ = List.iter check_problem_types goal in 
      let p = env.parent in
      ( match p with
	 | Some( parent ) ->
	   add_goal parent goal
	 | None ->
	   let error_msg = ":goal symbol table has no parent" in
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
