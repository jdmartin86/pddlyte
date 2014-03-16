/* parser 
 * 
 * this translates a sequence of tokens into s-expressions.
 *
 * each call to the parser returns an s-expression. at the end of the file
 *the parser will return None; hence the program returns option.
 */

%{ (* header *) %}

/* token declarations */
%token TOK_LPAR TOK_RPAR
%token 		TOK_UNIT
%token <int> 	TOK_INT
%token <string> TOK_SYM
%token		TOK_EOF

/* associativity */ 

/* parse the five non-terminals of the syntax */
%start parse 
%type <Sexpr.expr option> parse
%type <Sexpr.expr>	  sexpr
%type <Sexpr.atom>	  atom
%type <Sexpr.expr list>	  slist
%type <Sexpr.expr list>	  sexpr_list

%%

/* rules */

parse:
/* an s-expression, or none if eof is encountered*/
 | TOK_EOF	 { None }
 | sexpr 	 { Some $1 }

sexpr:
/* an s-expresion, an atom or list of s-expressions */
 | atom		{ Expr_atom($1) }
 | slist	{ Expr_list($1) }

atom:
/* an atom, which can be a unit, int or string. */
 | TOK_UNIT	{ Atom_unit	}
 | TOK_INT	{ Atom_int ($1) }
 | TOK_SYM	{ Atom_sym ($1) }

slist:
/* a list of s-expressions, with parentheses */
 | TOK_LPAR sexpr_list TOK_RPAR	{ List.rev $2 } 

sexpr_list:
/* the list contents of s-expressions, sans parentheses. */
 | /* nothing */    { [ ] } 	 /* empty list */
 | sexpr_list sexpr { $2 :: $1 } /* sexpr list */


%%
(* trailer *)
