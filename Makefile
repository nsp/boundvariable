LIBS = # -libs unix,oUnit
LFLAGS = # -lflags -I,+oUnit,-I,+site-lib/oUnit
CFLAGS = # -cflags -I,+oUnit,-I,+site-lib/oUnit,-I,+oUnitDiff,-I,+site-lib/oUnitDiff
MAIN = main

default: byte

byte:
	ocamlbuild $(LIBS) $(LFLAGS) $(CFLAGS) $(MAIN).byte

native:
	ocamlbuild $(LIBS) $(LFLAGS) $(CFLAGS) $(MAIN).native

clean:
	ocamlbuild -clean