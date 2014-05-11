(* planner interface *)

module Atomhash :
  sig
    type key = Ast.atom
    type 'a t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
  end

val intersect : 'a list -> 'a list -> 'a list
val backtrack : 'a list -> 'a list 
val prioritize : 'a list list -> 'a list list
val remove : 'a -> 'a list -> 'a list 
val lookup_op : Ast.action list -> Ast.sym -> Ast.action
val param_name : Ast.atom -> Ast.atom
val partition_conjunction :
  Ast.conjunction -> Ast.conjunction list * Ast.conjunction list
val conj_pred : Ast.conjunction -> Ast.predicate
val dump_keys : 'a Atomhash.t -> Atomhash.key list
val dump_vals : 'a Atomhash.t -> 'a list
val pred_name : Ast.predicate -> Ast.sym
val pred_val : Ast.predicate -> Ast.atom list
val bind : 'a Atomhash.t -> Atomhash.key list -> 'a list -> 'a Atomhash.t
val visited : Ast.predicate list -> Ast.predicate list list ->
  Ast.predicate list list
val swap : 'a -> 'a -> 'a list -> 'a list
val ground_pred : Ast.atom Atomhash.t -> Ast.predicate -> Ast.predicate
val ground_op : Ast.atom Atomhash.t -> Ast.action -> Ast.predicate
val action_bindings : Ast.predicate -> Ast.action -> Ast.atom Atomhash.t 
val extend :
  Ast.atom Atomhash.t ->
  Ast.predicate -> Ast.predicate -> Ast.atom Atomhash.t
val bindings_valid : 'a Atomhash.t -> 'a Atomhash.t -> bool
val matching_preds :
  Ast.predicate -> Ast.predicate list -> Ast.predicate list
val partition_to_predicates :
  Ast.conjunction -> Ast.predicate list * Ast.predicate list
val goal_test : 'a list list -> 'a list -> bool 
val search_exhausted : 'a list list -> 'a list -> bool
val partition_to_grounded_effect :
  Ast.action -> Ast.predicate -> Ast.predicate list * Ast.predicate list
val complimentary_effects : Ast.predicate -> Ast.action -> bool
val successor :
  Ast.predicate list ->
  Ast.action list -> Ast.predicate -> Ast.predicate list
val unify :
  Ast.action ->
  Ast.predicate list * Ast.predicate list ->
  Ast.atom Atomhash.t -> Ast.predicate list -> Ast.predicate list
val applicable_instance :
  Ast.action -> Ast.predicate list -> Ast.predicate list
val applicable_instances :
  Ast.action -> Ast.predicate list -> Ast.predicate list
val applicable_actions :
  Ast.predicate list -> Ast.action list -> Ast.predicate list
val successors :
  Ast.predicate list list ->  Ast.action list -> 
  Ast.predicate list list list -> Ast.predicate list list list 
val fsearch : Strips.strips_problem -> Ast.predicate list list 
val solve : Strips.strips_problem -> Ast.predicate list list 
val string_of_plan : Ast.predicate list list -> string 

