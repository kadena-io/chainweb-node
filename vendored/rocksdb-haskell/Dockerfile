FROM agrafix/ghc7.6
# dependencies
RUN apt-get update -qq
RUN apt-get -y install gcc-4.7 libgflags-dev libsnappy-dev zlib1g-dev libbz2-dev
RUN git clone https://github.com/facebook/rocksdb.git
RUN cd rocksdb && make
RUN cd rocksdb && mv ./librocksdb.a /usr/local/lib
RUN cd rocksdb && cp -R include/ /usr/local/include/

# now the library itself
RUN mkdir /rocksdb-haskell
ADD . /rocksdb-haskell
WORKDIR rocksdb-haskell
RUN cabal update
RUN cabal install --only-dependencies --enable-tests
RUN cabal configure --enable-tests
RUN cabal build
RUN cabal test
