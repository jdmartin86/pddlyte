(* symbol table generation *)
open Ast

exception Multiple_domains of string
exception Unknown_sym of string

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
   let { parent = p; bindings = b } = env in
   try 
     Hashtbl.find b name
   with Not_found ->
       ( match p with
	 | Some( parent ) -> lookup parent name
	 | _ -> raise (Unknown_sym name)
       )

(* insert entry into symbol table *)
let add env name value = 
   let { parent = _ ; bindings = b } = env in
   Hashtbl.add b name value

(* insert a list of entries into symbol table *)
let add_all env names values = 
   let pairs = List.combine names values in
      List.iter (fun (x, y) -> add env x y) pairs

let rec symt_of_ast ast = 
( match ast with 
  | Expr_domain( n , body ) -> failwith "progress" 
  | _ -> failwith "progress"
)

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
