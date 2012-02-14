VERBOSITY ?= 1

LIBHSLEVELDB = dist/build/*.a

HADDOCK = dist/doc/html/leveldb-haskell/*.html
HOOGLE  = dist/doc/html/leveldb-haskell/leveldb-haskell.txt

.PHONY: all test doc clean prune

all : $(LIBHSLEVELDB)

doc : $(HADDOCK) $(HOOGLE)

clean :
		rm -rf dist/
		rm -f *.buildinfo
		rm -rf autom4te.cache/
		rm -f config.log
		rm -f config.status

prune : clean
		rm -rf cabal-dev/

$(HADDOCK) :
		runhaskell Setup.hs haddock --hyperlink-source

$(HOOGLE) :
		runhaskell Setup.hs haddock --hoogle

configure :
		autoconf

$(LIBHSLEVELDB) : configure
		cabal-dev install --verbose=$(VERBOSITY)
