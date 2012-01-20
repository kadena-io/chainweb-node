VERBOSITY ?= 1

LEVELDBDIR = `pwd`/leveldb
LIBLEVELDB = $(LEVELDBDIR)/libleveldb.a

LIBHSLEVELDB = dist/build/*.a
EXECUTABLES  = dist/build/test/test

HADDOCK = dist/doc/html/leveldb-haskell/*.html
HOOGLE  = dist/doc/html/leveldb-haskell/leveldb-haskell.txt

.PHONY: all test doc clean

all : $(LIBHSLEVELDB)

test : $(EXECUTABLES)
		dist/build/test/test

doc : $(HADDOCK) $(HOOGLE)

clean :
		rm -rf dist/
		(cd leveldb && make clean)

$(LIBLEVELDB) :
		(cd leveldb && make)

$(HADDOCK) :
		runhaskell Setup.hs haddock --hyperlink-source

$(HOOGLE) :
		runhaskell Setup.hs haddock --hoogle

$(LIBHSLEVELDB) $(EXECUTABLES) : $(LIBLEVELDB)
		cabal-dev install \
--verbose=$(VERBOSITY) \
--extra-lib-dirs=$(LEVELDBDIR) \
--solver=modular \
--force-reinstalls
