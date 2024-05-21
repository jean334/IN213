CAMLC=$(BINDIR)ocamlc

COMPFLAGS=-I `ocamlfind ocamlc -package graphics`
CAML_B_LFLAGS = `ocamlfind query -predicates byte -a-format graphics`

SOURCES = foo.ml
OBJS = $(SOURCES:.ml=.cmo)
EXEC = my-wonderful-program.x

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CAMLC) $(CAML_B_LFLAGS) $(COMPFLAGS) -linkpkg $(OBJS) -o $(EXEC)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.SUFFIXES: .mll .mly

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -linkpkg -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

clean:
	rm -f *.cm[io] *.cmx *~ .*~ *.o *.x

#ocamlfind ocamlc -package graphics -linkpkg foo.ml -o TEST
