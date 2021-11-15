build: 
	ocamlbuild -use-ocamlfind interpreter.d.byte
clean: 
	ocamlbuild -clean
.PHONY: 
	build clean
