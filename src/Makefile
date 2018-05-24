.PHONY: all clean debug

OCAMLBUILD=ocamlbuild -use-ocamlfind

all: stats

stats:
	$(OCAMLBUILD) shstats.native

clean:
	$(OCAMLBUILD) -clean
