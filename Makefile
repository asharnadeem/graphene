all : graphene.native printbig.o graphene.bc

test : all testall.sh
	./testall.sh
	rm -rf testall.log test-* fail-*

test-clean: all testall.sh
	rm -rf testall.log test-* fail-*

graphene.bc: graphene.c
	clang-10 -emit-llvm graphene.c -c -o graphene.bc

graphene.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis,llvm.bitreader graphene.native

printbig : printbig.c
	cc -o printbig -DBUILD_TEST printbig.c

clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff printbig.o test-* fail-*