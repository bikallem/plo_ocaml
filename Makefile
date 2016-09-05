PROGRAM=plo

# OCaml build tool.
OCB=ocamlbuild
OPTIONS=-use-ocamlfind -classic-display

all: clean indent byte native top

docs:	
	$(OCB) $(PROGRAM).docdir/index.html

clean: 
	$(OCB) -clean 

indent:
	ocp-indent -i src/*.ml
	#ocp-indent -i src/*.mli	
	ocp-indent -i test/*.ml
	#ocp-indent -i test/*.mli

byte:
	$(OCB) $(OPTIONS) $(PROGRAM).byte 

native:
	$(OCB) $(OPTIONS) $(PROGRAM).native

test_lexer:
	$(OCB) $(OPTIONS) test_lexer.byte 

top: clean byte
	$(OCB) $(OPTIONS) $(PROGRAM).top

run-top: top
	./$(PROGRAM).top -safe-string
