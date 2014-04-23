#
# Makefile for PDDLyte
#

# Default compilation target.
default: lexer_test parser_test ast_test \
	 pddlyte  #planner_test 

#
# Variables.
#

OCAMLLEX  = ocamllex
OCAMLYACC = ocamlyacc
OCAMLC    = ocamlc
OEXT	  = cmo

INTERFACES =		\
    sexpr.cmi           \
    parser.cmi          \
    parser_utils.cmi    \
    lexer_utils.cmi     \
    ast.cmi		\
    strips.cmi		\
   # planner.cmi		\

OBJS =                  \
    sexpr.$(OEXT)       \
    parser.$(OEXT)      \
    lexer.$(OEXT)       \
    parser_utils.$(OEXT)\
    lexer_utils.$(OEXT) \
    ast.$(OEXT)		\
    strips.$(OEXT)	\
    #planner.cmo	\

#
# Patterns.
#

%.ml: %.mll
	$(OCAMLLEX) $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $<

%.$(OEXT): %.ml %.cmi
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<
%.cma: %ml
	$(OCAMLC) -c $<
#
# Compilation targets.
#

lexer_test: $(INTERFACES) $(OBJS) lexer_test.cmo
	$(OCAMLC) -o $@ $(OBJS) lexer_test.cmo

parser_test: $(INTERFACES) $(OBJS) parser_test.cmo
	$(OCAMLC) -o $@ $(OBJS) parser_test.cmo

ast_test: $(INTERFACES) $(OBJS) ast_test.cmo
	$(OCAMLC) -o $@ $(OBJS) ast_test.cmo

pddlyte: $(INTERFACES) $(OBJS) pddlyte.cmo
	$(OCAMLC) -o $@ $(OBJS) pddlyte.cmo

lexer.cmo: lexer.ml
	$(OCAMLC) -c $<

lexer_test.cmo: lexer_test.ml
	$(OCAMLC) -c $<

parser_test.cmo: parser_test.ml
	$(OCAMLC) -c $<

ast_test.cmo: ast_test.ml
	$(OCAMLC) -c $<

pddlyte.cmo: pddlyte.ml
	$(OCAMLC) -c $<

#
# Tests.
#

test_lexer: lexer_test
	./lexer_test tokens.bs

test_parser: parser_test
	./parser_test craptest

test_ast: ast_test
	./ast_test craptest

test_pddlyte: pddlyte
	./pddlyte craptest

#
# Cleanup.
#

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml	\
	*.mll~ *.mly~ *.ml~ *.mli~ *.c~ *.out	\
	lexer_test parser_test ast_test		\
	pddlyte master craptest~		\
