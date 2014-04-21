#
# Makefile for PDDLyte
#

# Default compilation target.
default: lexer_test parser_test ast_test \
	 symt_test #planner_test #sast_test

#
# Variables.
#

OCAMLLEX  = ocamllex
OCAMLYACC = ocamlyacc
OCAMLC    = ocamlc

INTERFACES =		\
    sexpr.cmi           \
    parser.cmi          \
    parser_utils.cmi    \
    lexer_utils.cmi     \
    ast.cmi		\
    symt.cmi		\
   # planner.cmi		\
   # sast.cmi		

OBJS =                  \
    sexpr.cmo           \
    parser.cmo          \
    lexer.cmo           \
    parser_utils.cmo    \
    lexer_utils.cmo     \
    ast.cmo		\
    symt.cmo		\
    #planner.cmo		\

#    sast.cmo		

#
# Patterns.
#

%.ml: %.mll
	$(OCAMLLEX) $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $<

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<
%.cma: %ml
	$(OCAMLC) -c $<
#
# Compilation targets.
#

lexer_test: $(INTERFACES) $(OBJS) lexer_test.cmo
	ocamlc -o $@ $(OBJS) lexer_test.cmo

parser_test: $(INTERFACES) $(OBJS) parser_test.cmo
	ocamlc -o $@ $(OBJS) parser_test.cmo

ast_test: $(INTERFACES) $(OBJS) ast_test.cmo
	ocamlc -o $@ $(OBJS) ast_test.cmo

symt_test: $(INTERFACES) $(OBJS) symt_test.cmo
	ocamlc -o $@ $(OBJS) symt_test.cmo

#sast_test: $(INTERFACES) $(OBJS) sast_test.cmo
#	ocamlc -o $@ $(OBJS) sast_test.cmo

#planner_test: $(INTERFACES) $(OBJS) planner_test.cmo
#	ocamlc -o $@ $(OBJS) planner_test.cmo

lexer.cmo: lexer.ml
	$(OCAMLC) -c $<

lexer_test.cmo: lexer_test.ml
	$(OCAMLC) -c $<

parser_test.cmo: parser_test.ml
	$(OCAMLC) -c $<

ast_test.cmo: ast_test.ml
	$(OCAMLC) -c $<

symt_test.cmo: symt_test.ml
	$(OCAMLC) -c $<

#sast_test.cmo: sast_test.ml
#	$(OCAMLC) -c $<

#planner_test.cmo: planner_test.ml
#	$(OCAMLC) -c $<

#
# Tests.
#

test_lexer: lexer_test
	./lexer_test tokens.bs

test_parser: parser_test
	./parser_test craptest

test_ast: ast_test
	./ast_test craptest

test_symt: symt_test
	./symt_test craptest

#test_sast: sast_test
#	./sast_test craptest

#test_planner: planner_test
#	./planner_test craptest

#
# Cleanup.
#

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml	\
	*.mll~ *.mly~ *.ml~ *.mli~ *.c~ *.out	\
	lexer_test parser_test ast_test		\
	sast_test master craptest~		\
