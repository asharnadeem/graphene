all : clean graphene.native graphene.o graphene.bc test

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

graphene.bc : graphene.c
	clang-10 -emit-llvm -o graphene.bc -c graphene.c

clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff graphene.o test-* fail-* graphene.bc