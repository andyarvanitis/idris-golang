.PHONY: build configure install lib_clean test

include config.mk
-include custom.mk

install:
	$(CABAL) install $(CABALFLAGS)

build: dist/setup-config
	$(CABAL) build $(CABALFLAGS)

test: test_cpp test_go

test_cpp:
	cd ../Idris-dev/test && ./runtest.pl without io003 reg031 --codegen cpp

test_go:
	cd ../Idris-dev/test && ./runtest.pl without io003 reg031 --codegen go


lib_clean:
clean:
	$(MAKE) -C cpprts clean
	$(MAKE) -C gorts clean

dist/setup-config:
	$(CABAL) configure $(CABALFLAGS)
