LIBS = # -libs unix,oUnit
LFLAGS = # -lflags -I,+oUnit,-I,+site-lib/oUnit
CFLAGS = # -cflags -I,+oUnit,-I,+site-lib/oUnit,-I,+oUnitDiff,-I,+site-lib/oUnitDiff
MAIN = main.byte

default: build

build:
	ocamlbuild $(LIBS) $(LFLAGS) $(CFLAGS) $(MAIN)

clean:
	ocamlbuild -clean