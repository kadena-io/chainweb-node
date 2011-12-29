BIN = cabal-dev/bin
LEVELDB = /Users/kim/src/vendor/c/leveldb

VERBOSITY ?= 1

all :
		cabal-dev install \
--verbose=$(VERBOSITY) \
--extra-include-dirs=$(LEVELDB)/include \
--extra-lib-dirs=$(LEVELDB) \
--solver=modular \
--force-reinstalls

test :
		$(BIN)/test

doc :
		runhaskell Setup.hs haddock
		runhaskell Setup.hs haddock --hoogle --hyperlink-source
