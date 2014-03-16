(*
 * ast.ml
 *
 *     Abstract syntax tree.
 *)

type sym = string

type expr =
   | Expr_unit
   | Expr_int    of int
   | Expr_sym     of sym
   | Expr_define of sym * expr


let rec ast_of_sexpr sx =
   (* TODO *)
   failwith "not implemented yet"
                     

let string_of_ast ast =
   let sprintf  = Printf.sprintf in  (* to make the code cleaner *)
   let spaces n = String.make n ' ' in
   let rec string_of_ids id_lst = 
      match id_lst with
         | [] -> ""
         | [id] -> id
         | h :: t -> h ^ " " ^ (string_of_ids t)
   in

   let rec iter ast indent =
      let string_of_exprs e_list =
         (List.fold_left (^) ""
             (List.map
                 (fun e -> "\n" ^ iter e (indent + 2))
                 e_list))
      in
      match ast with
         | Expr_unit    -> sprintf "%sUNIT"       (spaces indent) 
         | Expr_int  i  -> sprintf "%sINT[ %d ]"  (spaces indent) i
         | Expr_sym   id -> sprintf "%sSYM[ %s ]"   (spaces indent) id
         | Expr_define (id, e) -> 
              sprintf "%sDEFINE[%s\n%s ]" 
                 (spaces indent) id (iter e (indent + 2))
   in
      "\n" ^ iter ast 0 ^ "\n"


let ast_test infile =
   let lexbuf = Lexing.from_channel infile in
   let rec loop () =
      let sexpr  = Parser.parse Lexer.token lexbuf in
         match sexpr with
            | None -> ()
            | Some s ->
                 let expr = ast_of_sexpr s in
                    Printf.printf "%s\n" (string_of_ast expr); 
                    flush stdout;
                    loop ()
   in
      loop ()


