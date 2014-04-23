(* symbol table generation *)
open Ast

(* TODO: consolidate these *)
exception Multiple_domains of string
exception Unknown_sym of string
exception Compile_error of string

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

(* Environments. *)
let make parent = 
  { parent = parent; 
    bindings = Hashtbl.create 5;
    problem = { init = [] ; goal = [] ; ops = [] }
  }

(* lookup symbol-table entry *)
let rec lookup env name = 
   let { parent = p ; bindings = b } = env in
   try  Hashtbl.find b name
   with Not_found ->
       ( match p with
	 | Some( parent ) -> lookup parent name
	 | None ->
	   let error_msg = "Unknown symbol: " ^ name in
	   raise (Compile_error error_msg) 
       )

let lookup_atom env atom = 
  ( match atom with 
    | Atom_var a -> lookup env a
    | Atom_gnd a -> lookup env a 
    | _ ->  
      let error_msg = "Unknown atom: ???" in
      raise (Compile_error error_msg)
  )

(* insert entry into symbol table *)
let add env name value = 
   let { parent = _ ; bindings = b } = env in
   Hashtbl.add b name value

let add_init env state = 
  env.problem.init <- state

let add_goal env state = 
  env.problem.goal <- state

let add_action env act = 
  let acts = env.problem.ops in
  env.problem.ops <- act::acts

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
      add env (atom_name a) (atom_type a);
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

(*TODO: describe restrictions on names -- handle nil case*)
let translate_predicate env pred =
    ( match pred with 
      | Pred_var( name , params ) -> 
	add env name ":predicate" ;
	translate_var_params env params
      | Pred_gnd( name , params ) -> 
	add env name ":predicate" ;
	translate_gnd_params env params
      | _ -> 
	let error_msg = "predicate improperly parsed" in
	raise (Compile_error error_msg)
    )

let translate_predicates env preds =
  let translate_pred = translate_predicate env in 
  List.iter translate_pred preds

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
  translate_var_params param_env params ; (* add params *)
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
let rec strips_of_ast env ast = 
  let recurse = strips_of_ast env in
  ( match ast with 
    | Expr_predicates( preds ) -> 
      translate_predicates env preds
    | Expr_action( act ) ->
      translate_action env act;
      add_action env act(*; failwith (string_of_strips env.problem);*)
    | Expr_objects( objs ) -> 
      translate_objects env objs
    | Expr_init( init ) -> 
      translate_state env init;
      let p = env.parent in (* TODO: cleanup *)
      ( match p with
	| Some( parent ) ->
	  add_init parent init
	| None ->
	  let error_msg = "symbol table has no parent" in
	  raise (Compile_error error_msg) 
      )
    | Expr_goal( goal ) -> 
      translate_state env goal;
      let p = env.parent in
      ( match p with
	 | Some( parent ) ->
	   add_goal parent goal
	 | None ->
	   let error_msg = "symbol table has no parent" in
	   raise (Compile_error error_msg) 
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
      raise (Compile_error error_msg)	  
  )

(** test functions **)
let sprintf  = Printf.sprintf   (* to make the code cleaner *)
let spaces n = String.make n ' '

let rec string_of_syms sym_lst = 
  (match sym_lst with
    | []   -> ""
    | [s] -> s
    | h::t -> h ^ " " ^ (string_of_syms t)
  )

let string_of_atom atom =
  ( match atom with 
    | Atom_var a -> a
    | Atom_gnd a -> a
    | Atom_nil -> ""
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

let string_of_params params = 
  sprintf "PARAMETERS[ %s ]"
  (string_of_syms (List.map string_of_atom params))

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

let string_of_precond precond =
  sprintf "PRECONDITION[ %s ]"
  (string_of_conj precond)

let string_of_effect effect =
  sprintf "EFFECT[ %s ]"
  (string_of_conj effect)

let string_of_action act = 
  act.name ^ "\n" ^
  (string_of_params act.parameters) ^ "\n" ^ 
  (string_of_precond act.precondition) ^ "\n" ^
  (string_of_effect act.effect)

let string_of_strips prob =
  let { init = s0 ; goal = g ; ops = acts } = prob in
  let title = 
    "STRIPS PROBLEM\n" in
  let initial_state =
   "INITIAL STATE:\n" ^ 
     (string_of_syms (List.map string_of_pred s0)) in   
  let goal_state =
    "GOAL STATE:\n" ^
    (string_of_syms (List.map string_of_pred g)) in
  let actions = 
    "ACTIONS:\n" ^
    (string_of_syms (List.map string_of_action acts)) in
  sprintf "%s\n%s\n%s\n%s\n"
    (title)
    (initial_state)
    (goal_state)
    (actions)

let strips_test infile =
  let lexbuf = Lexing.from_channel infile in
  let new_env = make None in
  let rec loop env =
    let sexpr  = Parser.parse Lexer.token lexbuf in
    match sexpr with
      | None -> () 
      | Some s ->
        let ast = ast_of_sexpr s in
	let _ = strips_of_ast env ast in
        Printf.printf "%s\n" (string_of_strips env.problem); 
        flush stdout;
        loop env
  in loop new_env
