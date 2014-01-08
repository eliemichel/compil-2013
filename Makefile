CMX=env.cmx ast.cmx mips.cmx parser.cmx lexer.cmx typer.cmx prod.cmx main.cmx
GEN=lexer.ml parser.ml parser.mli
BIN=minic++
LIBS=


NO_COLOR=\033[0m
OK_COLOR=\033[32;01m
ERROR_COLOR=\033[31;01m
WARN_COLOR=\033[33;01m
HIGHLIGHT_COLOR=\033[1m

all: $(BIN)
	@echo "$(HIGHLIGHT_COLOR)  ./$(BIN) --help$(NO_COLOR)"
	@./$(BIN) --help


$(BIN): $(CMX)
	@echo "Linking for $(BIN)..."
	@ocamlopt -o $(BIN) $(LIBS) $(CMX)
	@echo "$(OK_COLOR)Done.$(NO_COLOR)"

.SUFFIXES: .mli .ml .cmi .cmx .mll .mly

.mli.cmi:
	ocamlopt -c $<

.ml.cmx: 
	ocamlopt -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir -v $<

.mly.mli:
	menhir -v $<

.PHONY: all clean dep

clean:
	@echo "Cleaning directory..."
	@rm -v -f .depend *.cmi *.o *.automaton *.conflicts *~ $(BIN) $(CMX) $(GEN)
	@echo "$(OK_COLOR)Done.$(NO_COLOR)"

dep: all

.depend: $(GEN)
	rm -f .depend
	ocamldep *.mli *.ml > .depend

ifeq ($(MAKECMDGOALS),dep)
include .depend
endif

