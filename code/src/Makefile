#
# Makefile for pddlyte
#

# default target
default: lexer_test parser_test ast_test

INTF = sexpr.cmi parser.cmi parser_utils.cmi lexer_utils.cmi ast.cmi
OBJS = sexpr.cmo parser.cmo lexer.cmo parser_utils.cmo lexer_utils.cmo \
       ast.cmo

# patterns
%.ml : %.mll
	ocamllex $<

%.ml %.mli : %.mly
	ocamlyacc $<

%.cmo : %.ml %.cmi
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<


# compilation
lexer_test : $(INTF) $(OBJS) lexer_test.cmo
	ocamlc -o $@ $(OBJS) lexer_test.cmo

parser_test : $(INTF) $(OBJS) parser_test.cmo
	ocamlc -o $@ $(OBJS) parser_test.cmo

ast_test : $(INTF) $(OBJS) ast_test.cmo
	ocamlc -o $@ $(OBJS) ast_test.cmo

lexer.cmo : lexer.ml
	ocamlc -c $<

lexer_test.cmo : lexer_test.ml
	ocamlc -c $<

parser_test.cmo : parser_test.ml
	ocamlc -c $<

ast_test.cmo : ast_test.ml
	ocamlc -c $<


# tests
test_lexer : lexer_test
	lexer_test test/lex/lex_test_prog

test_parser : parser_test
	parser_test test/parse/parse_test_prog

test_ast : ast_test
	ast_test factorial.bs

# cleanup
.PHONY : clean
clean :
	rm -f pddlyte lexer.ml parser.ml parser.mli lexer_test parser_test \
	ast_test \
	*.cmo *.cmi *.out *.diff *.mll~ *.mli~ *.ml~ *.mly~

