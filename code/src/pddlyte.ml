open Strips
(* 
   compiler flags
   -a: print ast
   -p: print plan (action sequence)
   -c: compile and print instructions
   -e: execute instructions 
*)

(* compiler flags *)
type flag = Ast | Plan | Compile | Execute

(* decode the compiler flag *)
let decode_flag argv =
  if Array.length argv > 1 then
    List.assoc argv.(1) 
      [ ("-a", Ast);
	("-p", Plan);
	("-c", Compile);
	("-e", Execute)]
  else Execute

(* open infile *)
let decode_infile argv =
  if Array.length argv > 1 then
    open_in argv.(2)
  else
    open_in argv.(1)

(* compile to ast *)
let print_ast infile =
  let lexbuf = Lexing.from_channel infile in
  let rec loop () =
    let sexpr  = Parser.parse Lexer.token lexbuf in
    ( match sexpr with
      | None -> () 
      | Some s ->
        let ast = Ast.ast_of_sexpr s in
        Printf.printf "%s\n" (Ast.string_of_ast ast); 
        flush stdout;
        loop ()
    )
  in loop ()

(* compile to plan *)
let print_plan infile =
  let lexbuf = Lexing.from_channel infile in
  let env = Strips.make None in
  let rec loop () =
    let sexpr  = Parser.parse Lexer.token lexbuf in
    (match sexpr with
      | None -> 
	let problem = env.problem in 
	let plan = Planner.solve problem in
	Printf.printf "%s" (Planner.string_of_plan plan); 
	flush stdout;()
      | Some s ->
        let ast = Ast.ast_of_sexpr s in
	let _ = Strips.strips_of_ast env ast in
        flush stdout;
        loop ()
    )
  in loop () 

(* compile to bytecode *)
let compile_program infile =
  let lexbuf = Lexing.from_channel infile in
  let env = Strips.make None in
  let rec loop () =
    let sexpr  = Parser.parse Lexer.token lexbuf in
    ( match sexpr with
      | None -> 
	let problem = env.problem in 
	let plan = Planner.solve problem in 
	let instructions = Compile.translate plan problem in
	Printf.printf "%s" (Compile.print_bytecode instructions);
	flush stdout;()
      | Some s ->
        let ast = Ast.ast_of_sexpr s in
	let _ = Strips.strips_of_ast env ast in
        flush stdout;
        loop ()
    )
  in loop () 

(* compile and execute bytecode *)
let execute_program infile =
  let lexbuf = Lexing.from_channel infile in
  let env = Strips.make None in
  let rec loop () =
    let sexpr  = Parser.parse Lexer.token lexbuf in
   ( match sexpr with
      | None -> 
	let problem = env.problem in 
	let plan = Planner.solve problem in 
	let instructions = Compile.translate plan problem in
	Execute.execute_bytecode instructions; ()
      | Some s ->
        let ast = Ast.ast_of_sexpr s in
	let _ = Strips.strips_of_ast env ast in
        flush stdout;
        loop ()
   )
  in loop () 

(* process input *)
let run_program flag infile =
   ( match flag with
       | Ast -> 
	 ( try
	     print_ast infile
	   with (Failure f) ->
	     Printf.fprintf stderr "\nERROR: %s\n" f;
	     close_in infile
	 )
       | Plan -> 
	 ( try
	     print_plan infile
	   with (Failure f) ->
	     Printf.fprintf stderr "\nERROR: %s\n" f;
	     close_in infile
	 )
       | Compile -> 
	 ( try
	     compile_program infile
	   with (Failure f) ->
	     Printf.fprintf stderr "\nERROR: %s\n" f;
	     close_in infile
	 )
       | Execute ->  
	 ( try
	     execute_program infile
	   with (Failure f) ->
	     Printf.fprintf stderr "\nERROR: %s\n" f;
	     close_in infile
	 )
     )
 
(* pddlyte top-level *)   
let _ = 
   if (Array.length Sys.argv > 3 || Array.length Sys.argv < 1) then
      Printf.fprintf stderr "usage: %s [flag] [input_filename]\n" Sys.argv.(0)
   else
     let flag = decode_flag Sys.argv in
     let infile = decode_infile Sys.argv in
     run_program flag infile 
    
