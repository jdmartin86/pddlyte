#
# Makefile for PDDLyte
#

# Default compilation target.
default: pddlyte  

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
    ast.cmi		\
    util.cmi		\
    strips.cmi		\
    planner.cmi		\
    compile.cmi		\
    execute.cmi 	\

OBJS =                  \
    sexpr.$(OEXT)       \
    parser.$(OEXT)      \
    lexer.$(OEXT)       \
    ast.$(OEXT)		\
    util.$(OEXT)	\
    strips.$(OEXT)	\
    planner.$(OEXT)	\
    compile.$(OEXT)	\
    execute.$(OEXT)	\

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

pddlyte: $(INTERFACES) $(OBJS) pddlyte.cmo
	$(OCAMLC) -o $@ $(OBJS) pddlyte.cmo

lexer.cmo: lexer.ml
	$(OCAMLC) -c $<

pddlyte.cmo: pddlyte.ml
	$(OCAMLC) -c $<

#
# Cleanup.
#

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml	\
	*.mll~ *.mly~ *.ml~ *.mli~ *.c~ *.sh~	\
	lexer_test parser_test ast_test		\
	strips_test planner_test pddlyte  	\
	master craptest~ *.out *.log *.diff	\
