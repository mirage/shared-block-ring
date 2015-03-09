.PHONY: all clean install build
all: build doc

NAME=shared-block-ring
J=4

export OCAMLRUNPARAM=b

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.ml: _oasis
	oasis setup

setup.data: setup.bin
	@./setup.bin -configure --enable-tests

build: setup.data setup.bin
	@./setup.bin -build -j $(J)

doc: setup.data setup.bin
	@./setup.bin -doc -j $(J)

install: setup.bin
	@./setup.bin -install

uninstall:
	@ocamlfind remove $(NAME) || true

test: setup.bin build
	@./setup.bin -test

reinstall: setup.bin
	@ocamlfind remove $(NAME) || true
	@./setup.bin -reinstall

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin

release:
	# Remove our dependencies on oasis and bisect
	sed -i -r s'/, bisect//g' _oasis
	sed -i -r s'/\"bisect\"//g' opam
	sed -i -r s'/\"oasis\"//g' opam
	# Remove our aversion to OASIS autogen
	sed -i -r s'/setup.ml//g' .gitignore
	sed -i -r s'/myocamlbuild.ml//g' .gitignore
	sed -i -r s'/_tags//g' .gitignore
	sed -i -r s'/\*.mllib//g' .gitignore
	sed -i -r s'/\*.mldylib//g' .gitignore
	sed -i -r s'/\*.mlpack//g' .gitignore
	sed -i -r s'/META//g' .gitignore
	oasis setup

coverage:
	rm -f _build/*.out
	BISECT_FILE=_build/coverage ./setup.bin -test
	(cd _build; bisect-report co*.out -summary-only -html /vagrant/report/)
