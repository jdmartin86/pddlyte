(* lexer.mll
 *
 * This file converts program text into tokens
 *)

{ open Parser }

let wspc = [' ' '\t' '\n']
let chrs = ['A'-'Z' 'a'-'z' '+' '-' '*' '/' '=' '<' '>' '!' ':' '_' '?']
let digt = ['0'-'9']
let symb = chrs(chrs | digt)+

rule token = parse
  | wspc       		{ token lexbuf }
  | ';'	       		{ comment lexbuf }
  | '('	       		{ TOK_LPAR }
  | ')'			{ TOK_RPAR }
  | "NIL"         	{ TOK_UNIT }
  | (digt)+ as nm	{ TOK_INT(int_of_string nm)} (* unused *)
  | symb    as sm  	{ TOK_SYM(sm) }
  | eof	       		{ TOK_EOF }
  | _ as char { failwith ("illegal character " ^ Char.escaped char) }
  and comment = parse
  | '\n'   	{ token lexbuf }	(* commentary ends with lines *)
  | _ 	   	{ comment lexbuf }	(* ignore other characters *)
