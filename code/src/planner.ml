(* planner *)
open Ast
open Strips

(* ( atom , atom ) Hashtbl *)
module Atomhash = Hashtbl.Make
  (struct
    type t = atom
    let equal x y = x = y
    let hash = Hashtbl.hash
   end)

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

(* preconds -> pred *)
let partition_conj_to_pred p = 
  ( match p with 
    | Conj_and c -> List.partition 
      ( fun x -> match x with | Conj_pos _ -> true | _ -> false ) c
    | Conj_pos c -> ( [Conj_pos c] , []  )
    | Conj_neg c -> ( []  , [Conj_neg c] )
    | Conj_nil   -> ( []  , []  )
  )

(* preconds -> preds 
let partition_pred p = List.partition 
      ( fun x -> match x with | Pred_pos _ -> true | _ -> false ) p
*)
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

(* 
 * intersect_preds can be written more generally, but i only need 
 * it for a specific purpose. tail recursion would be cool too.
 *)
(* ast.predicate list -> ast.predicate list -> ast.predicate list *)
let rec intersect_preds l1 l2 =
  ( match l1, l2 with 
    | Pred_gnd( n1 , p1 )::t1 , Pred_gnd( n2 , p2 )::t2 when n1 = n2 ->
      Pred_gnd( n1 , p1 )::intersect_preds t1 t2
    | Pred_gnd( n1 , p1 )::t1 , Pred_var( n2 , p2 )::t2 when n1 = n2 ->
      Pred_gnd( n1 , p1 )::intersect_preds t1 t2
    | Pred_var( n1 , p1 )::t1 , Pred_gnd( n2 , p2 )::t2 when n1 = n2 ->
      Pred_gnd( n2 , p2 )::intersect_preds t1 t2
    | _  , []     -> []
    | [] , _      -> []
    | _::_ , _::_ -> []
  )  

(* 'a * 'b -> 'a *)
let tup1 t = 
( match t with 
  | x , _ -> x
)

(* 'a * 'b -> 'b *)
let tup2 t = 
( match t with 
  | _ , x -> x 
)  

(* incarnation of map2 with hash tables *)
let rec map2_hashadd env names values = 
( match ( names , values ) with
  | ( [] , [] ) -> []
  | n::t1 , v::t2 -> 
    let e = Hashtbl.add env n v in e::map2_hashadd env t1 t2 
  | ( _ , _ ) -> invalid_arg "add"
) 

(* incarnation of iter2 with hash tables *)
let rec bind env names values = 
( match ( names , values ) with
  | ( [] , [] ) -> env
  | n::t1 , v::t2 -> Atomhash.add env n v; bind env t1 t2 
  | ( _ , _ ) -> invalid_arg "bind"
) 

(* create temporary bindings for comparison *)
let extend sp pp = 
  let env = Atomhash.create 2 and
      names = predval_of_pred pp and
      values = predval_of_pred sp in
  bind env names values

(* verifies if a set of temporary bindings is valid *)
let bindings_valid env1 env2 = 
  let names = Atomhash.fold (fun k _ acc -> k::acc) env1 [] in 
  let rec loop acc names = 
    ( match names with 
      | [] -> []
      | n :: t ->
	let v = Atomhash.find env1 n 
	and ref = Atomhash.find_all env2 n in
	( match ref with
	  | [] -> Atomhash.add env1 n v ; loop (true::acc) t
	  | [r] -> if v = r then loop (true::acc) t else [false] 
	  | r::_ -> failwith "duplicate bindings"
	)
    ) in List.fold_left (fun x y -> x || y ) true (loop [] names)

(* state, action -> action list *)
(* pred list -> action list -> action list *)
(* ---------------------------------------*)
(* applicable_actions 
 *
 * This function finds the applicable instances of an operator
 * for a given state. This is the meat of the planner.
 * 
 * Input 
 *         act: action to process
 *         env: a parameter substitution -- baby symbol table
 *           s: state to apply action
 *)
let rec applicable_actions acc act ( pp , np ) env s  =
  ( match pp with
    | [] -> 
      (* TODO: add support for negative preds *)
      let vals = Atomhash.fold (fun _ v acc -> v::acc) env [] in 
      (Pred_gnd( act.name , vals )) :: acc (* op, grounded-param list *) 
    | p :: pt -> (* choose a predicate *)
      let sps = intersect_preds [p] s in
      let rec loop sps = 
	( match sps with 
	  | [] -> failwith "empty intersection"
	  | sp::t -> 
	    let tenv = extend sp p in 
	    if ( bindings_valid tenv env ) then(* tenv semantics wrong *)
	      applicable_actions acc act ( pt , np ) tenv s
	    else 
	      loop t
	) in loop sps
  )

(* fetch action list from symbol table *)
let actions_of_env env = failwith "not implemented"

(* 'a list -> 'a list -> bool *)
let goal_test s1 s2 = 
  let s12 = intersect s1 s2 in
  ( match s12 with
    | [] -> true
    | _ -> false
  ) 

(* loop over all operators and accumulate applicable ops 
let app_ops s ops = 
  let rec loop acc ops =
    ( match ops with 
      | [] -> acc 
      | op::t -> 
	let ( pp , np ) = partition_preds op.precondition
	and env = Atomhash.create 10
	and ao = applicable_actions [] op ( pp , np ) env s
	in loop ( ao::acc ) t
    ) in loop [] ops
*)
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

(* TODO: successor function *)
(* state, op -> state *)
(* ast.predicate list -> ast.predicate -> ast.predicate list 
let succ s o env = (* o is a grounded pred list *)
  let act = Atomhash.find env (predname_of_pred o) (* TODO: globalhash *)
  and ( pos , neg ) = partition_preds act 
  and ne = List.map pred_of_conj neg    (* pred list *)
  and pe = List.map pred_of_conj pos in (* pred list *)
  let rec loop s ne =
    ( match ne with
      | [] -> 
	let rec iloop s pe = 
	  ( match pe with
	    | [] -> s
	    | h::t -> iloop (h::s) t
	  )
	in iloop s pe
      | h::t -> loop (remove_pred s h) t 
    )
  in loop s ne
*)
(* TODO: planner!!!! 
 * - how to handle cycling 
 * - how to handle deadends -- backtrack to op choice
 * - how to handle multiple goal states
 * - how to handle complimentary effects 
 *
let fsearch problem = 
  let ops = problem.actions 
  and s0 = problem.init 
  and g = problem.goal in 
  let rec loop plan s ops =
    if ( goal_test s g ) then plan
    else 
      let app = app_ops s ops in (* set of all app ops in s *)
      ( match app with
	| [] -> failwith "no applicable operators"
	| o::t ->             (* choose applicable operator *)
	  let s = succ s o in (* TODO: transistion to next state *)
	  loop (o::plan) s t  (* plan from there *)
      )
  in loop [] s0
 *)
let plan problem =
  failwith "progress"

let planner_test infile =
  let lexbuf = Lexing.from_channel infile in
  let new_env = make None in
  let rec loop env =
    let sexpr  = Parser.parse Lexer.token lexbuf in
    match sexpr with
      | None -> 
	(* plan here *)
	() 
      | Some s ->
        let ast = ast_of_sexpr s in
	let _ = strips_of_ast env ast in
        flush stdout;
        loop env
  in loop new_env
