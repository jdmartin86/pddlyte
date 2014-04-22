(* symbol table generation *)
open Ast

exception Multiple_domains of string
exception Unknown_sym of string
exception Compile_error of string

type sym = string

(* environment *)
type env = { parent: env option ; bindings: (sym, sym) Hashtbl.t }

(* symbol table for planner *)
type plan_table = (* TODO: make these names consistent with others *)
{ 
  init : Ast.predicate list;
  goal : Ast.predicate list;
  ops  : Ast.action list ;
}

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
  let len = (String.length a) - stop in
  String.sub a 0 stop

let atom_type a =
  let start = (String.index a '-') + 1 in
  let stop = (String.length a) - start in
  String.sub a start stop

let translate_variable_param env param =
  ( match param with 
    | Atom_var a -> (* split type from name *)
      add env (atom_type a) (atom_name a)
    | _ -> failwith "parameter improperly parsed"
  )

let translate_var_params env params = 
  let translate_var_param = translate_variable_param env in
  List.iter translate_var_param params

let translate_predicate env pred =
    ( match pred with 
      | Pred_var( name , params ) -> 
	add env "predicate" name ;
	let parent = Some(env) in 
	let new_env = make parent in
	add new_env ":predicate" name ;
	translate_var_params new_env params
      | Pred_gnd( name , params ) -> 
	failwith "progress"
    )

(* env -> pred list -> unit *)
let translate_predicates env preds =
  let translate_pred = translate_predicate env in 
  List.iter translate_pred preds

(* - add predicate name to global table 
   - create symbol table for each predicate
*)

(* ast -> plan_table *)
(* TODO: add type checking and semantic checks! *)
let rec translate_ast env ast = 
  let recurse = translate_ast env in
  ( match ast with 
    | Expr_predicates( preds ) ->
      translate_predicates env preds
    | Expr_action( act ) -> failwith "progress"
    | Expr_objects( objs ) -> failwith "progress"
    | Expr_init( init ) -> failwith "progress"
    | Expr_goal( goal ) -> failwith "progress"
    | Expr_domain( name , body ) -> 
	add env "domain" name ; List.iter recurse body
    | Expr_problem( name , body ) -> 
      	add env "problem" name ; List.iter recurse body
  )


let plantable_of_ast ast = 
  let env = make None in  
  translate_ast env ast

let symt_of_ast ast =
  let env = make None in  
  failwith "not implemented"
  (* translate_ast env ast *)

(* debug stuff *)
let string_of_symt symt =
  let sprintf  = Printf.sprintf in
  let spaces n = String.make n ' ' in
  let rec string_of_syms sym_lst = 
    match sym_lst with
      | []   -> ""
      | [s] -> s
      | h::t -> h ^ " " ^ (string_of_syms t)
  in
  let rec iter symt indent =
    let string_of_exprs e_list =
      (List.fold_left (^) ""
	 (List.map
            (fun e -> "\n" ^ iter e (indent + 2))
            e_list))
    in
    match symt with
      | _ -> failwith "progress"
  in
  "\n" ^ iter symt 0 ^ "\n"

(* test symt *)
let symt_test infile =
  let lexbuf = Lexing.from_channel infile in
  let rec loop () =
    let sexpr = Parser.parse Lexer.token lexbuf in
    match sexpr with
      | None -> ()
      | Some s ->
        let ast = ast_of_sexpr s in
	let symt = symt_of_ast ast in 
        Printf.printf "%s\n" ( string_of_symt symt ); 
        flush stdout;
        loop ()
  in
  loop ()
