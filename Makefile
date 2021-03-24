all : graphene.native printbig.o

test : all testall.sh
	./testall.sh
	rm -rf testall.log test-* fail-*

test-clean: all testall.sh
	rm -rf testall.log test-* fail-*

graphene.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind graphene.native

printbig : printbig.c
	cc -o printbig -DBUILD_TEST printbig.c

clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff printbig.o test-* fail-*