VERBOSITY ?= 1

BIN = cabal-dev/bin

LEVELDBDIR = `pwd`/leveldb
LIBLEVELDB = $(LEVELDBDIR)/libleveldb.a

LIBHSLEVELDB = dist/build/libHSleveldb-haskell-*.a
EXECUTABLES  = dist/test/test

HADDOCK = dist/doc/html/leveldb-haskell/*.html
HOOGLE  = dist/doc/html/leveldb-haskell/leveldb-haskell.txt

.PHONY: all test doc clean

all : $(LIBLEVELDB) $(LIBHSLEVELDB)

test : $(EXECUTABLES)
		$(BIN)/test

doc : $(HADDOCK) $(HOOGLE)

clean :
		rm -r dist/

$(LIBLEVELDB) :
		(cd leveldb && make && cd -)

$(HADDOCK) :
		runhaskell Setup.hs haddock --hyperlink-source

$(HOOGLE) :
		runhaskell Setup.hs haddock --hoogle

$(LIBHSLEVELDB) $(EXECUTABLES) :
		cabal-dev install \
--verbose=$(VERBOSITY) \
--extra-lib-dirs=$(LEVELDBDIR) \
--solver=modular \
--force-reinstalls
