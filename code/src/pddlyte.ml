open Strips

let run_program infile =
  let lexbuf = Lexing.from_channel infile in
  let env = Strips.make None in
  let empty_strips_prob = Strips.make_strips in
  let rec loop prob =
    let sexpr  = Parser.parse Lexer.token lexbuf in
    match sexpr with
      | None -> 
	(* plan prob *) 
	() 
      | Some s ->
        let ast = Ast.ast_of_sexpr s in
        (*Printf.printf "%s\n" (string_of_ast ast);*)
	let partial_strips_prob = 
	  Strips.strips_of_ast env prob ast in
        flush stdout;
        loop partial_strips_prob
  in loop empty_strips_prob 

let _ = 
   if Array.length Sys.argv <> 2 then
      Printf.fprintf stderr "usage: %s input_filename\n" Sys.argv.(0)
   else
      let infile = open_in Sys.argv.(1) in
         try
            run_program infile
         with (Failure f) ->
            Printf.fprintf stderr "\nERROR: %s\n" f;
         close_in infile
