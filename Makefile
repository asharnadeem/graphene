scanner.ml : scanner.mll
	ocamllex $^

clean :
	rm -rf scanner.ml