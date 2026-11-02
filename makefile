.PHONY: clean all

default: all

all: 
	dune build pif.exe
	@cp -f _build/default/pif.exe pif


clean:
	dune clean
	@rm -f pif
