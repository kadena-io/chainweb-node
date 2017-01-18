This library provides Haskell bindings to
[RocksDB](http://rocksdb.org)

[![Build Status](https://travis-ci.org/serokell/rocksdb-haskell.svg?branch=master)](https://travis-ci.org/serokell/rocksdb-haskell)
[![Windows build status](https://ci.appveyor.com/api/projects/status/x4dmt91wuk8dglw0/branch/master?svg=true)](https://ci.appveyor.com/project/jagajaga/rocksdb-haskell)

## History

Version 0.1.x:

* windows support and fixes
* initial fork of leveldb-haskell

## Installation

Prerequisites:

* [GHC 7.*](http://www.haskell.org/ghc)
* [Cabal](http://www.haskell.org/cabal), version 1.3 or higher
* [RocksDB](http://rocksdb.org)
* Optional: [Snappy](http://code.google.com/p/snappy),
  if compression support is desired

To install the latest version from hackage:

```shell
$ cabal install rocksdb-haskell
```

To install from checked-out source:

```shell
$ cabal install
```

## Notes

This library is in very early stage and has seen very limited testing. Comments
and contributions are welcome.

## Bugs and Contributing

Please report issues via http://github.com/agrafix/rocksdb-haskell/issues.<br />
Patches are best submitted as pull requests, or via email
(mail@agrafix.net).

## License

BSD 3, see LICENSE file.
