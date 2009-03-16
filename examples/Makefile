.PHONY: all clean

all: MemSpeed CCall CTime mytime.so

%.o: %.c
	gcc $(CFLAGS) -fPIC -c $<

%.so: %.o
	gcc $(LDFLAGS) -nostartfiles -shared -Wl,-soname,$@ $< -o $@

mytime.so: mytime.o

%.hs: %.hsc
	hsc2hs $<

%: %.hs
	ghc --make $<

CTime: CTime.hs

clean:
	rm -f CTime CTime.hs *.o *.hi *.so core core.*
