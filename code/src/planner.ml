(* planner *)
open Ast
open Strips

exception Planner_error of string 

(* ( atom , atom ) Hashtbl *)
module Atomhash = Hashtbl.Make
  (struct
    type t = atom
    let equal x y = x = y
    let hash = Hashtbl.hash
   end)


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
  sprintf "PARAMETERS( %s )"
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
  sprintf "PRECONDITION( %s )"
  (string_of_conj precond)

let string_of_effect effect =
  sprintf "EFFECT( %s )"
  (string_of_conj effect)
(** end test functions **)

(* ast.conjunction -> ast.predicate *)
let pred_of_conj c =
( match c with
    | Conj_neg nc -> nc
    | Conj_pos pc -> pc
    | _ -> Pred_nil
)

(* ast.conjuction -> ast.predicate *)
let ppred_of_conj c = 
( match c with 
  | Conj_pos pc -> pc
  | _ -> Pred_nil
)

(* ast.action -> ast.predicate list *)
let effect_of_act a =
  let effect = a.effect in
( match effect with 
  | Conj_and c -> List.map pred_of_conj c
  | Conj_pos c | Conj_neg c -> [c]
  | Conj_nil -> []
)

(* ast.action -> ast.predicate list *)
let  ppreds_of_act act = 
  let precond = act.precondition in
  ( match precond with
    | Conj_and c -> List.map ppred_of_conj c (* conjunction list *)
    | Conj_pos c -> [c] (* singleton conjunction *)
    | _ -> []
  )

(* ast.predicate -> ast.sym *)
let predname_of_pred p =
  ( match p with
    | Pred_var ( name , _ ) -> name 
    | Pred_gnd ( name , _ ) -> name
    | _ -> ""
  )

(* ast.sym -> ast.predicate list -> ast.predicate *)
let pred_of_predname p l =
 ( match l with 
   | Pred_var( name , preds ) :: _ when name = p-> 
     Pred_var( name , preds )  
   | Pred_gnd( name , preds ) :: _ when name = p ->
     Pred_gnd( name , preds ) 
   | _ -> failwith "no predicate matches candidate name"
 )

(* ast.predicate -> ast.atom list *)
let predval_of_pred p = 
  ( match p with 
    | Pred_var ( _ , value ) -> value 
    | Pred_gnd ( _ , value ) -> value
    | _ -> [Atom_nil]
  )

(* predicate list -> sym list *)
let prednames_of_preds preds = List.map predname_of_pred preds

(* 'a list -> 'a list -> 'a list *)
let intersect l1 l2 = List.filter (fun x -> List.mem x l2) l1 

(* ast.predicate list -> ast.predicate list -> ast.sym list *)
let intersect_prednames l1 l2 = 
  intersect ( prednames_of_preds l1 ) ( prednames_of_preds l2 )

(* TODO: consolidate all these intersection methods
 * intersect_preds can be written more generally, but i only need 
 * it for a specific purpose. tail recursion would be cool too.
 *)
(* ast.predicate list -> ast.predicate list -> ast.predicate list *)
(* p = reference pred name 
 * l = list to match on 
 *)

(* ast.sym -> ast.predicate list -> ast.predicate list *)
let intersect_preds p l =
  let match_name = 
    ( fun x -> if p = (predname_of_pred x) then true else false) in
  List.filter match_name l

(* incarnation of iter2 with hash tables *)
let rec bind env names values = 
( match ( names , values ) with
  | ( [] , [] ) -> env
  | n::t1 , v::t2 -> Atomhash.replace env n v; bind env t1 t2 
  | ( _ , _ ) -> invalid_arg "bind"
) 

(* 
 * copy global env and update pp names with sp vals
 *)
let extend env sp pp = 
  let test_env = Atomhash.copy env in
  let names = predval_of_pred pp in
  let values = predval_of_pred sp in
  bind test_env names values

let retract env pp =
  let names = predval_of_pred pp in
  List.iter (fun k -> Atomhash.remove env k ) names


(* verifies if a set of temporary bindings is valid 
 * 
 * - get all the names (i.e. keys) of test bindings
 * - loop over test names 
 *   - find the corresponding test value
 *   - get the reference values
 *   - compare the test value with reference value;
 *     add bindings when they don't exist;
 *     
 *)
(* env1: test bindings
 * env2: reference bindings
 * this just checks if each is the same actually 
 *)
let dump_keys env =  Atomhash.fold (fun k _ acc -> k::acc) env []
let dump_vals env =  Atomhash.fold (fun _ v acc -> v::acc) env []

let dump_hash env = 
  let keys = dump_keys env in
  let vals = dump_vals env in
  Printf.printf "\nKEYS:%s \nVALS:%s"
  (string_of_syms(List.map string_of_atom keys)) 
  (string_of_syms(List.map string_of_atom vals)) 

let bindings_valid test_env ref_env = 
  let test_names = dump_keys test_env in (*lookup every name in ref*)
  let test_vals = dump_vals test_env in
  let rec loop acc names values  = 
    ( match ( names , values ) with 
      | ( [] , [] ) -> acc
      | (test_name::t1 , test_val::t2 )-> 
	(try 
	  let ref_val = Atomhash.find ref_env test_name in
	  if test_val = ref_val then loop (true::acc) t1 t2
	  else [false] (* value mismatch *)
	with Not_found -> (* name not found *)
(*
	  let _ = Printf.printf "\nTEST NAME: %s = %s not found"
	    (string_of_atom test_name) 
	    (string_of_atom test_val) in
	  let _ = Printf.printf "\nPRE REPLACEMENT" in
	  let _ = Printf.printf "\n--TEST ENV--" in
	  let _ = dump_hash test_env in
	  let _ = Printf.printf "\n--REF ENV--" in
	  let _ = dump_hash ref_env in
*)
	  let _ =  Atomhash.replace ref_env test_name test_val in
(*	 
	  let _ = Printf.printf "\nPOST REPLACEMENT" in
	  let _ = Printf.printf "\n--TEST ENV--" in
	  let _ = dump_hash test_env in
	  let _ = Printf.printf "\n--REF ENV--" in
	  let _ = dump_hash ref_env in
*)	 
	  loop acc t1 t2 )
      | ( _ , _ ) -> 
	let error_msg = "improper input" in
	raise (Planner_error error_msg)
    )
      in List.fold_left (fun x y -> x && y ) true (loop [] test_names test_vals)
    

(* nice debug print
let error_msg = 
	"\nPOS PREDS:"^
	(string_of_syms (List.map string_of_pred pp)) ^
	"\nP:"^
	(string_of_syms (List.map string_of_pred [p])) ^
	"\nSTATE: " ^ 
	(string_of_syms (List.map string_of_pred s)) ^
	"\nINTERSECTION:" ^
	(string_of_syms (List.map string_of_pred sps))
      in
      failwith error_msg;
*)

(* DON'T DELETE UNTIL DOCUMENTED
   action parameters will appear in the order they're declared
   in the parameters section
*)
let name_of_param atom =
  ( match atom with
    | Atom_gnd a ->
      let stop = (String.index a '-') in
      let name =  String.sub a 0 stop in
      Atom_gnd name
    | Atom_var a ->
      let stop = (String.index a '-') in
      let name =  String.sub a 0 stop in
      Atom_var name
    | _ -> 
      let error_msg = "usage" in
      failwith error_msg
  )

let ground_action env act =
  let names = List.map name_of_param act.parameters in
  let vals = List.map (fun n -> Atomhash.find env n) names in
  Pred_gnd( act.name , vals ) 

(* 'a list -> 'a list -> bool *)
let goal_test s1 s2 = 
  let intersection = intersect s1 s2 in
  ( match intersection with
    | [] -> true
    | _ -> false
  ) 

(* preconds -> pred *)
let partition_conjunction conj = 
  ( match conj with 
    | Conj_and c -> List.partition 
      ( fun x -> match x with | Conj_pos _ -> true | _ -> false ) c
    | Conj_pos c -> ( [Conj_pos c] , []  )
    | Conj_neg c -> ( []  , [Conj_neg c] )
    | Conj_nil   -> ( []  , []  )
  )

let partition_to_predicates conj = 
  let ( pc , nc ) = partition_conjunction conj in
  let pred_of = List.map pred_of_conj in
  ( pred_of pc , pred_of nc )

(* 'a -> 'a list -> 'a list *)
let remove_pred l p = 
  let rec loop acc l =
    ( match l with
      | [] -> acc
      | h::t -> 
	if h = p then loop acc t
	else loop (h::acc) t
    )
  in loop [] l

let find_action acts name =
  let filter_name = 
    (fun a -> if a.name = name then true else false ) in 
  List.find filter_name acts

(* returns an enviornment mapping parameter names to values *)
let get_action_bindings act act_def =
  let size = List.length act_def.parameters in
  let env = Atomhash.create size in
  let names = List.map name_of_param act_def.parameters in
  let vals = predval_of_pred act in
  let _ = List.iter2 ( fun n v -> Atomhash.add env n v ) names vals in
  env

let ground_pred env pred =
  let name = predname_of_pred pred in
  let param_names = predval_of_pred pred in (* var vals = param names *)
  let vals = 
    List.map ( fun n -> Atomhash.find env n ) param_names in
  ( match pred with 
    | Pred_var( name , _ ) -> Pred_gnd( name , vals )
    | _ -> 
      let error_msg = "Failed to ground predicate" in
      raise ( Planner_error error_msg )
  )

(* partitions an action effect and outputs the bound predicates *)
let partition_to_grounded_effect acts act =
  let name = predname_of_pred act in
  let act_def = find_action acts name in
  let env = get_action_bindings act act_def in (* create bindings *)
  let ( pp , np ) = partition_to_predicates act_def.effect in
  let pe = List.map (ground_pred env) pp in (* list of preds*)
  let ne = List.map (ground_pred env) np in
  ( pe , ne )

let complimentary_effects act act_defs = 
  let ( pe , ne ) = partition_to_grounded_effect act_defs act in
  let intersection = intersect pe ne in
  ( match intersection with 
    | [] -> false
    | _ -> true
  )

let  remove_complimentary_effects acts act_defs =
  let rec loop acc a =
  ( match a with
    | [] -> List.rev acc
    | h::t -> 
      if (complimentary_effects h act_defs) then loop acc t 
      else loop (h::acc) t
  )
  in loop [] acts


let rec applicable_actions acc act act_defs ( pos_preds , neg_preds ) env s  =
  ( match pos_preds with
    | [] ->       (* TODO: add support for negative preds *)
      let a = ground_action env act in
      let cmp_eff = true&&(complimentary_effects a act_defs) in 
      if cmp_eff then 
      else a::acc
    | pp :: pt -> (* choose a predicate *)
      let name = predname_of_pred pp in
      let sps = intersect_preds name s in (* common ppreds with state *)
     (* let error_msg = 
	    "\nNAME: " ^
	    (name) ^
	    "\nSTATE: " ^
	    (string_of_syms (List.map string_of_pred s)) ^
	    "\nINTERSECTION: " ^
	    (string_of_syms (List.map string_of_pred sps))
      in
      failwith error_msg;*)
      let rec loop sps = 
	( match sps with 
	  | [] -> 
	    let error_msg = "state has no matching preds with " ^ 
	    "\nNAME: " ^
	    (name) ^
	    "\nSTATE: " ^
	    (string_of_syms (List.map string_of_pred s)) ^
	    "\nSP: " ^
	    (string_of_syms (List.map string_of_pred sps))
	    in
	    raise (Planner_error error_msg)
	  | sp::t -> 
(*	    let _ =

	      Printf.printf"\n\nSPS:%s" 
	      (string_of_syms (List.map string_of_pred sps)) in
*)
	    let tenv = extend env sp pp in
(*
	    let _ = Printf.printf "\nTEST ENV" in
	    let _ = dump_hash tenv in
	    let _ = Printf.printf "\nREF ENV" in
	    let _ = dump_hash env in
*)	    
	    let valid_bindings = bindings_valid tenv env in
	   
	    if ( valid_bindings ) then (
	      
	      if ( pt = [] ) then
	      
	      
	      applicable_actions acc act act_defs ( pt , neg_preds ) tenv s
	    ) 
	    else (
	      let _ = retract env pp in
	      loop t )
	) in loop sps
  )

(* loop over all operators and accumulate applicable ops *)
(* ast.pred list -> ast.action list -> ast.pred list *)  
let app_ops s acts = 
  let rec loop acc ops =
    ( match ops with 
      | [] -> List.flatten acc 
      | op::t -> 
	let ( pp , np ) = partition_to_predicates op.precondition in
	let env = Atomhash.create 10 in
	let ao = applicable_actions [] op acts ( pp , np ) env s in

	let _ = Printf.printf "\nAPP ACTIONS: %s"
	  (string_of_syms (List.map string_of_pred ao))
	in

	let ao = remove_complimentary_effects ao acts in
	
	let _ = Printf.printf "\nAPP ACTIONS: %s"
	  (string_of_syms (List.map string_of_pred ao))
	in
	
	loop ( ao::acc ) t
    ) in loop [] acts
  
(* TODO: successor function *)
(* state, op -> state *)
(* ast.predicate list -> ast.predicate -> ast.predicate list *)
(*
 * - remove negative effects
 * - apply positive effects
 *)
let succ state act acts = (* act is a grounded pred list *)
  let ( pe , ne ) = partition_to_grounded_effect acts act in
  
  let _ = Printf.printf "\nPOS EFFECTS: %s"
    (string_of_syms (List.map string_of_pred pe)) in
  let _ = Printf.printf "\nNEG EFFECTS: %s"
    (string_of_syms (List.map string_of_pred ne)) in

  let rec loop s ne = (* loop through negative effects first *)
    ( match ne with
      | [] -> 
	let rec iloop s pe =  (* loop through positive *)
	  ( match pe with
	    | [] -> List.rev s
	    | h::t -> iloop (h::s) t
	  )
	in iloop s pe
      | h::t -> loop (remove_pred s h) t 
    )
  in loop state ne

(* TODO: planner!!!! 
 * - how to handle cycling 
 * - how to handle deadends -- backtrack to op choice
 * - how to handle multiple goal states
 * - how to handle complimentary effects 
 * 
 * - a cool spot to parallelize is the applicable action choice
 *  - use consistent notation with this and report
 *)
let fsearch problem = 
  let { init = s0 ; goal = g ; ops = acts } = problem in
  let rec loop plan s aset =
    if ( goal_test s g ) then plan
    else
      let app = app_ops s aset in (* set of all app ops in s *)
      
      let _ = Printf.printf "\nSTATE = %s "
	(string_of_syms (List.map string_of_pred s)) in
(*
      let _ = Printf.printf "\nAPP OPS = %s "
	(string_of_syms (List.map string_of_pred app)) in
*)
      ( match app with (* app = list of preds *)
	| [] -> 
	  let error_msg = "no applicable operators" in
	  raise (Planner_error error_msg)
	| a::t -> (* TODO: implement backtracking!*)
          let s = succ s a acts in
	  
	  let _ = Printf.printf "\nSUCC: %s"
	    (string_of_syms (List.map string_of_pred app)) in
	  loop (a::plan) s acts  (* plan from there *)
      )
  in loop [] s0 acts

let solve problem =
  let plan = fsearch problem in
  failwith "PROGRESS!!!"

let planner_test infile =
  let lexbuf = Lexing.from_channel infile in
  let new_env = make None in
  let rec loop env =
    let sexpr  = Parser.parse Lexer.token lexbuf in
    match sexpr with
      | None ->
	let problem = env.problem in 
	let plan = solve problem in () 
      | Some s ->
        let ast = ast_of_sexpr s in
	let _ = strips_of_ast env ast in
        flush stdout;
        loop env
  in loop new_env
