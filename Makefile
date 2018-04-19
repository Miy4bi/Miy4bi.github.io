spl:
	ocamlc -c lex.ml
	ocamlc -c engine.ml
	ocamlc -c parser.ml
	ocamlc -o spl lex.cmo engine.cmo parser.cmo

clean:
	rm -f spl lex.cmo lex.cmi engine.cmo engine.cmi parser.cmo parser.cmi
