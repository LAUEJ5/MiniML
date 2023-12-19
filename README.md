Metacircular OCaml language Interpreter with no type inference, ensuring Turing-completeness. Three iterations are implemented: one based on the substitution model, another on a dynamically scoped environment model, and a third featuring extensions such as lexical scoping.

To execute the program, first select which method of evaluation you would like it to perfom in the very bottom of the evaluation.ml file. Next run the following commands in your terminal:

1. \# make all
2. \# ocamlbuild -use-ocamlfind miniml.byte
3. \# ./miniml.byte

Et voila! Happy evaluating!
