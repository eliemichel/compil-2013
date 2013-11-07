CMX=parser.cmx lexer.cmx main.cmx
GEN=lexer.ml
BIN=minic++


all: $(BIN)
	./$(BIN) --parse-only test.cpp

$(BIN): $(CMX)
	ocamlopt -o $(BIN) $(CMX)

.SUFFIXES: .ml .cmx .mll

.ml.cmx:
	ocamlopt -c $<

.mll.ml:
	ocamllex $<

clean:
	rm -f *.cmi *.o *~ $(BIN) $(CMX) $(GEN)


