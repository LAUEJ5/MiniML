all: evaluation expr miniml miniml_parse miniml_lex tests evaluation_tests

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

miniml_lex: miniml_lex.mll
	ocamlbuild -use-ocamlfind miniml_lex.byte
	
miniml_parse: miniml_parse.mly
	ocamlbuild -use-ocamlfind miniml_parse.byte

tests: tests.ml
	ocamlbuild -use-ocamlfind tests.byte

evaluation_tests: evaluation_tests.ml
	ocamlbuild -use-ocamlfind evaluation_tests.byte

clean:
	rm -rf _build *.byte