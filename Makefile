all : graphene.native graphene.o

test : all testall.sh
	./testall.sh
	rm -rf testall.log test-* fail-*

test-clean: all testall.sh
	rm -rf testall.log test-* fail-*

graphene: graphene.c
	cc -o graphene -DBUILD_TEST graphene.c

graphene.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis,llvm.bitreader graphene.native


clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff printbig.o graphene.o test-* fail-*