OCAMLBUILD=ocamlbuild -classic-display \
		-tags bin_annot,debug,thread \
		-libs str,unix \
		-I src 
TARGET=native
BYTECODE=byte



example:
	$(OCAMLBUILD) main.$(TARGET) \

test :
	$(OCAMLBUILD) main.$(BYTECODE) \

clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
