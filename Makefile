all : scanner.ml parser.ml parser.mli

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

clean :
	rm -rf scanner.ml parser.ml parser.mli