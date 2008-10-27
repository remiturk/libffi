.PHONY: all

%.hs: %.hsc
	hsc2hs -I /scratch/rturk/libffi/include $<

%.o: %.hs
	ghc --make $<

all: Foreign/LibFFI/Internal.o
	ghci Foreign/LibFFI -lffi
