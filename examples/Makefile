.PHONY: all clean

all: Time mytime.so

%.o: %.c
	gcc $(CFLAGS) -fPIC -c $<

%.so: %.o
	gcc $(LDFLAGS) -nostartfiles -shared -Wl,-soname,$@ $< -o $@

mytime.so: mytime.o

%.hs: %.hsc
	hsc2hs $<

%: %.hs
	ghc --make $<

Time: Time.hs

clean:
	rm -f Time Time.hs *.o *.hi *.so core core.*
