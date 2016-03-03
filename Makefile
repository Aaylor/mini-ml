OCAMLBUILD = ocamlbuild
FOLDERS = src/interpreter,src/mini-caml,src/typechecker,src/log,src/cli

.PHONY = all
all:
	$(OCAMLBUILD) -ocamlopt "ocamlopt -g" -Is $(FOLDERS) src/main.native

clean:
	$(OCAMLBUILD) -clean
