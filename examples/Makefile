MACHINE := $(shell $(CC) -dumpmachine)
ifneq (, $(findstring darwin, $(MACHINE)))
	OS := darwin
else ifneq (, $(findstring cygwin, $(MACHINE)))
	OS := windows
else ifneq (, $(findstring mingw, $(MACHINE)))
	OS := windows
else ifneq (, $(findstring windows, $(MACHINE)))
	OS := windows
else
	OS := unix
endif

ifeq ($(OS),darwin)
	DLLEXT := dylib
else ifeq ($(OS),windows)
	DLLEXT := dll
else
	DLLEXT := so
endif

.PHONY: all
all: MemSpeed CCall CTime mytime.$(DLLEXT)

%.o: %.c
	$(CC) $(CFLAGS) -fPIC -c $<

%.$(DLLEXT): %.o
	$(CC) $(LDFLAGS) -shared $< -o $@

.PHONY: MemSpeed
MemSpeed:
	cabal build exe:$@

.PHONY: CCall
CCall:
	cabal build exe:$@

.PHONY: CTime
CTime: mytime.$(DLLEXT)
	cabal build exe:$@

.PHONY: clean
clean:
	rm -f *.o *.$(DLLEXT)
	cabal clean
