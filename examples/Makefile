.PHONY: all
all: MemSpeed CCall CTime mytime.so

%.o: %.c
	gcc $(CFLAGS) -fPIC -c $<

%.so: %.o
	gcc $(LDFLAGS) -nostartfiles -shared -Wl,-soname,$@ $< -o $@

.PHONY: MemSpeed
MemSpeed:
	cabal build exe:$@

.PHONY: CCall
CCall:
	cabal build exe:$@

.PHONY: CTime
CTime: mytime.so
	cabal build exe:$@

.PHONY: clean
clean:
	rm -f *.o *.so
	cabal clean
