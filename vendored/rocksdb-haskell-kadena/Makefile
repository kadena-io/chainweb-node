VERBOSITY ?= 1

LIBHSROCKSDB = dist/build/*.a
LIBROCKSDB   = /usr/local/lib/librocksdb*

HADDOCK = dist/doc/html/rocksdb-haskell/*.html
HOOGLE  = dist/doc/html/rocksdb-haskell/rocksdb-haskell.txt

.PHONY: all test doc clean prune travis

all : $(LIBHSROCKSDB)

doc : $(HADDOCK) $(HOOGLE)

clean :
		rm -rf dist/

prune : clean
		rm -rf cabal-dev/

travis : $(LIBROCKSDB)
		echo "All good!"

$(HADDOCK) :
		runhaskell Setup.hs haddock --hyperlink-source

$(HOOGLE) :
		runhaskell Setup.hs haddock --hoogle

$(LIBHSROCKSDB) :
		cabal-dev install --verbose=$(VERBOSITY)

$(LIBROCKSDB) :
		(cd /tmp; \
			git clone https://github.com/facebook/rocksdb.git; \
			cd rocksdb; \
			make clean; make; \
			sudo mv ./librocksdb.a /usr/local/lib; \
			sudo cp -R include/ /usr/local/include/)
