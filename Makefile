.PHONY: all

%.hs: %.hsc
	hsc2hs $<

%.o: %.hs
	ghc --make $<

all: ForeignFFI.o
	ghci FFI -lffi
