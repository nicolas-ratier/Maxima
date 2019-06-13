
main:
	ocamlc -c maxima.ml
	ocamlc -c example.ml
	ocamlc -o example unix.cma str.cma maxima.cmo example.cmo
	./example
