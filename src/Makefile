.PHONY: all clean debug headers

OCAMLBUILD=ocamlbuild -use-ocamlfind

all: stats

stats:
	$(OCAMLBUILD) shstats.native

clean:
	$(OCAMLBUILD) -clean

headers:
	headache -h .header *.ml *.mli analyzers/*.ml
