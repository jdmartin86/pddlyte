open Ast
open Strips
open Util

(** level-one dependency **)

(* ( atom , atom ) Hashtbl *)
module Atomhash = Hashtbl.Make
  (struct
    type t = atom
    let equal x y = x = y
    let hash = Hashtbl.hash
   end)

(* returns string from state *)
let string_of_state state = 
    (string_of_syms (List.map string_of_pred state))

(* returns string from plan *)
let string_of_plan plan =  
  sprintf "\n%s" 
    (string_of_syms (List.map string_of_state plan))

(* returns the intersection of two lists *)
let intersect l1 l2 = List.filter (fun x -> List.mem x l2) l1

(* returns the disjunction of two lists *)
let disjoin l1 l2 = List.filter (fun x -> not(List.mem x l2)) l1

(* removes the last state-action combination from a partial plan *)
let backtrack pplan = 
  ( match pplan with 
    | state::act::prior_pplan -> prior_pplan
    | _ -> 
      let error_msg = "backtrack: improper input" in
      failwith error_msg
  )

(* sorts a list of partial plans with respect to their depth *)
let prioritize pplans =
  let randomized_pplans = permutation pplans in
  List.sort ( fun x y -> 
    let l1 = List.length x and l2 = List.length y in
  if l1 < l2 then 1 else if l1 = l2 then 0 else -1 ) randomized_pplans

(* 'a -> 'a list -> 'a list *)
let remove pred state = 
  List.filter (fun x -> if x = pred then false else true ) state

(* returns operator with matching name *)
let lookup_op opset name =
  let filter_name = 
    (fun op -> if op.name = name then true else false ) in 
  List.find filter_name opset

(* parses atom for parameter name *)
let param_name atom =
  try 
  ( match atom with
    | Atom_gnd a ->
      let stop = (String.index a '-') in
      let name =  String.sub a 0 stop in
      Atom_gnd name
    | Atom_var a ->
      let stop = (String.index a '-') in
      let name =  String.sub a 0 stop in
      Atom_var name
    | Atom_nil -> Atom_nil
  )
  with Not_found -> 
    let error_msg = "No parameter type specified in: " in
    failwith error_msg
  

(* partitions a conjunction into positive and negative lists *)
let partition_conjunction conj = 
  ( match conj with 
    | Conj_and c -> List.partition 
      ( fun x -> match x with | Conj_pos _ -> true | _ -> false ) c
    | Conj_pos c -> ( [Conj_pos c] , []  )
    | Conj_neg c -> ( []  , [Conj_neg c] )
    | Conj_nil   -> ( []  , []  )
  )

(* ast.conjunction -> ast.predicate *)
let conj_pred conj =
( match conj with
    | Conj_neg nc -> nc
    | Conj_pos pc -> pc
    | _ -> Pred_nil
)

let dump_keys env =  Atomhash.fold (fun k _ acc -> k::acc) env []

let dump_vals env =  Atomhash.fold (fun _ v acc -> v::acc) env []

let pred_name pred =
  ( match pred with
    | Pred_var ( name , _ ) -> name 
    | Pred_gnd ( name , _ ) -> name
    | _ -> ""
  )

let pred_val pred = 
  ( match pred with 
    | Pred_var ( _ , value ) -> value 
    | Pred_gnd ( _ , value ) -> value
    | _ -> [Atom_nil]
  )

(* incarnation of iter2 with hash table replacement *)
let rec bind env names values = 
( match ( names , values ) with
  | ( [] , [] ) -> env
  | n::t1 , v::t2 -> Atomhash.replace env n v; bind env t1 t2 
  | ( _ , _ ) -> invalid_arg "bind"
)

(* filters states that are elements of the visited list *)
let filter_visited succs visited = 
  let visited_states = heads visited in (* visited = list^3 *)
  List.filter( 
    fun act_succ -> 
      if ((intersect [(List.tl act_succ)] visited_states) = []) then true
      else false) succs

(* return the visited partial plan, if previously visted, else [] *)
let visited state pplan = 
  let rec try_remembering acc pp =
    ( match pp with
      | [] -> acc
      | s::remaining_pp -> (* s = state ; t = act::state list *)
	if s = state then (* pplan head is a state *)
	  try_remembering pp []
	else
	  ( match remaining_pp with
	    | [] ->
	      try_remembering [] [] (* discovered new state *)
	    | prior_act_state::t -> 
	      let prior_state = List.tl prior_act_state in
	      let remaining_pplan = prior_state::t in
	      try_remembering [] remaining_pplan
	  )
    )
  in try_remembering [] pplan

(* swap an old element for a new element in some list *)
let swap old_pred new_pred state =
  let rec loop new_state old_state =
  ( match old_state with
    | [] -> List.rev new_state
    | state_pred::remaining_preds -> 
      if state_pred = old_pred then 
	loop (new_pred::new_state) remaining_preds
      else
	loop (state_pred::new_state) remaining_preds
  )
  in loop [] state

(** level-two dependency **)
(* grounds a variable predicate with a set of bindings *)
let ground_pred env pred =
  let keys = pred_val pred in
  let vals = 
    List.map ( fun k -> Atomhash.find env k ) keys in
  ( match pred with 
    | Pred_var( name , _ ) -> Pred_gnd( (pred_name pred) , vals )
    | _ -> 
      let error_msg = "Failed to ground predicate" in
      failwith error_msg
  )

(* grounds an operator with a set of bindings *)
let ground_op env op =
  let names = List.map param_name op.parameters in
  let vals = List.map (fun n -> Atomhash.find env n) names in
  Pred_gnd( op.name , vals ) 

(* returns the bindings for an action *)
let action_bindings action op =
  let env = Atomhash.create (List.length op.parameters) in
  let names = List.map param_name op.parameters in
  let vals = pred_val action in
  let _ = List.iter2 ( fun n v -> Atomhash.add env n v ) names vals in
  env

(* extend bindings such that names = vals *)
let extend env name_pred val_pred = 
  let test_env = Atomhash.copy env in
  bind test_env (pred_val name_pred) (pred_val val_pred)

(* checks if test bindings are consistent with reference bindings *)
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
	  loop acc t1 t2 )
      | ( _ , _ ) -> 
	let error_msg = "bindings valid: improper input" in
	failwith error_msg
    )
      in List.fold_left (fun x y -> x && y ) true (loop [] test_names test_vals)

(* returns a list of matching state predicates *) 
let matching_preds pred state =
  let name = pred_name pred in 
  let match_name = 
    ( fun pred -> if name = (pred_name pred) then true else false) in
  List.filter match_name state

(* partitions a conjunction into positive and negative predicates *)
let partition_to_predicates conj = 
  let ( pos_conj , neg_conj ) = partition_conjunction conj in
  let pred_of = List.map conj_pred in
  ( pred_of pos_conj , pred_of neg_conj )

(* checks if the goal is a subset of partial plan *)
let goal_test pplan goal =
  let state = List.hd pplan in
  let rec loop check_sum goal_preds = 
    ( match goal_preds with 
      | [] -> check_sum 
      | gp::remaining_goal_preds -> 
	let intersection = intersect [gp] state in
	( match intersection with
	  | [] -> [false]
	  | _ -> loop (true::check_sum) remaining_goal_preds
	)
    ) 
in List.fold_left (fun x y -> x && y ) true (loop [] goal)

(** level-three dependency **)

(* checks if the partial plan is the intial state *)
let search_exhausted pplan init_state =
  goal_test pplan init_state (* TODO: is this valid for all cases?*)

(* outputs partitioned, grounded effects *)
let partition_to_grounded_effect op action =
  let env = action_bindings action op in (* create bindings *)
  let ( pos_preds , neg_preds ) = partition_to_predicates op.effect in
  let pos_effs = List.map (ground_pred env) pos_preds in 
  let neg_effs = List.map (ground_pred env) neg_preds in
  ( pos_effs , neg_effs )

(** level-four dependency **)

(* check if positive effects are the same as the negative effects *) 
let complimentary_effects action op = 
  let ( pos_effs , neg_effs ) = partition_to_grounded_effect op action in
  let intersection = intersect pos_effs neg_effs in
  ( match intersection with 
    | [] -> false
    | _ -> true
  )

(* apply an action and return the successor state *)
let successor state opset action = (* act is a grounded pred list *)
  let op = lookup_op opset (pred_name action) in
  let ( pos_effs , neg_effs ) = partition_to_grounded_effect op action in
  let rec remove_neg_effects s peffs neffs =
    ( match neffs with
      | [] -> action::peffs@s
      | neg_eff::t -> (* check for repeating effects in strips *)
	let matching_effects = matching_preds neg_eff pos_effs in
	( match matching_effects with
	  | [] ->
	    remove_neg_effects (remove neg_eff s) t peffs
	  | [pe] ->
	    remove_neg_effects (swap neg_eff pe s) t (remove pe peffs)
	  |  _ -> 
	    let error_msg = 
	      "Predicates may only appear once in effect declaration" in
	    failwith error_msg
	)
    )
  in remove_neg_effects state pos_effs neg_effs

(** level-five dependency **)

(* attempts to unify state predicates with an operator's preconditions *)
let rec unify op ( pos_preds , neg_preds ) env state  =
  ( match pos_preds with
    | [] -> 
      ( match neg_preds with 
	| [] -> [ground_op env op]
	| np::remaining_np ->
	  let state_preds = matching_preds np state in 
	  let rec unify_npreds all_sp = 
	    ( match all_sp with 
	      | [] -> 
		let error_msg = "no matching predicates with state" in
		failwith error_msg
	      | sp::remaining_sp -> 
		let tenv = extend env np sp in
		if ( bindings_valid tenv env ) then 
		  unify op ( [] , remaining_np ) tenv state 
		else  
		  []
	    ) in unify_npreds state_preds
      )
    | pp::remaning_pp ->
      (* look at common state predictes *)
      let state_preds = matching_preds pp state in 
      let rec unify_ppreds all_sp = 
	( match all_sp with 
	  | [] -> []
	  | sp::remaining_sp -> 
	    let tenv = extend env pp sp in (* temp sub.*)
	    if ( bindings_valid tenv env ) then
	      ( match remaning_pp with 
		| [] -> (* check for complimentary effects *) 
		  let action = ground_op tenv op in
		  if ( complimentary_effects action op ) then [] 
		  else unify op ( remaning_pp , neg_preds ) tenv state
		| _ -> unify op ( remaning_pp , neg_preds ) tenv state
	      )
	    else (* try unifying pp with another state pred *)
	      unify_ppreds remaining_sp 
	) in unify_ppreds state_preds
  )

(** level-six dependency **)
(* return the first valid action in a state *)
let applicable_instance op state = 
  let ( pos_preds , neg_preds ) = 
    partition_to_predicates op.precondition in
  let env = Atomhash.create 10 in
  unify op ( pos_preds , neg_preds ) env state

(* accumulate action instances from every possible state permutation *)
let applicable_instances op state =
  let max_permutations = List.length state in
  let rec find_instances app_actions permutations s =
    if permutations = max_permutations then 
      app_actions
    else 
      let app_action = applicable_instance op s in
      let s1 = (List.tl s)@[(List.hd s)] in
      find_instances (app_action::app_actions) (permutations+1) s1 
  in List.flatten ( find_instances [] 0 state )

(* all applicable actions, includes backward moves, but no self loops *)
let applicable_actions state opset = 
  let rec next_op actions ops =
    ( match ops with 
      | [] -> List.flatten actions
      | op::remaining_ops -> 
	(* list of all op's instances in the state  *)
	let action_instances = applicable_instances op state in
	next_op (action_instances::actions) remaining_ops
    ) 
  in next_op [] opset

(* returns all available nodes not previously visited *)
let successors pplan opset explored =
  let state = List.hd pplan in
  let actions = applicable_actions state opset in 
  ( match actions with
    | [] -> 
      let error_msg =
	"no applicable actions found for state" in
      failwith error_msg
    | _ ->
      let all_succs = 
	filter_visited(
	  filter_duplicates( 
	    List.map (successor state opset) actions)) explored in
      let rec filter applicable_pplans succs = 
	( match succs with 
	  | [] -> applicable_pplans
	  | act_succ::remaining_succs ->
	    let act = List.hd act_succ and succ = List.tl act_succ in
	     if succ = state then(* already filtered *)
	      filter applicable_pplans remaining_succs
	    else (* back move *)
	       let prior_pplan = visited succ pplan in 
	       ( match prior_pplan with 
		 | [] -> 
		   let new_pplan = succ::[act]::pplan in
		   filter (new_pplan::applicable_pplans) remaining_succs
		 | p::pp ->
		   filter (prior_pplan::applicable_pplans) remaining_succs
	       )	
	)
      in filter [] all_succs
  )

(* states are partial plans *)
let fsearch problem = 
  let { init = s0 ; goal = g ; ops = opset } = problem in
  let rec dfs state explored =
    if ( goal_test state g ) then List.rev state 
    else
      let fringe = successors state opset explored in
      ( match fringe with 
	| [] -> 
	  if search_exhausted state s0 then 
	    let error_msg = "Problem has no solution" in
	    failwith error_msg
	  else
	    dfs (backtrack state) (state::explored)
	| _ -> 
	  let priority_queue = prioritize fringe in
	  let succ = List.hd priority_queue in
	  dfs succ (state::explored)
      )
  in dfs [s0] []
  
let solve problem =
  fsearch problem
  
