VERBOSITY ?= 1

LIBHSLEVELDB = dist/build/*.a
LIBLEVELDB   = /usr/local/lib/libleveldb*

HADDOCK = dist/doc/html/leveldb-haskell/*.html
HOOGLE  = dist/doc/html/leveldb-haskell/leveldb-haskell.txt

.PHONY: all test doc clean prune travis

all : $(LIBHSLEVELDB)

doc : $(HADDOCK) $(HOOGLE)

clean :
		rm -rf dist/

prune : clean
		rm -rf cabal-dev/

travis : $(LIBLEVELDB)
		cabal install

$(HADDOCK) :
		runhaskell Setup.hs haddock --hyperlink-source

$(HOOGLE) :
		runhaskell Setup.hs haddock --hoogle

$(LIBHSLEVELDB) :
		cabal-dev install --verbose=$(VERBOSITY)

$(LIBLEVELDB) :
		(cd /tmp; \
			git clone ttps://code.google.com/p/leveldb/; \
			cd leveldb; \
			make; \
			sudo mv ./libleveldb* /usr/local/lib;)
