.PHONY: build configure install lib_clean test

include config.mk
-include custom.mk

install:
	git submodule update --init
	$(CABAL) install $(CABALFLAGS)

build: dist/setup-config
	git submodule update --init
	$(CABAL) build $(CABALFLAGS)

test: test_go

test_go:
	cd ../Idris-dev/test && ./runtest.pl without io003 reg031 effects002 --codegen go


lib_clean:
clean:
	$(MAKE) -C gorts clean

dist/setup-config:
	$(CABAL) configure $(CABALFLAGS)
