This library provides Haskell bindings to
[LevelDB](http://leveldb.googlecode.com)

## Installation

Prerequisites:

* [GHC 7.*](http://www.haskell.org/ghc)
* [Cabal](http://www.haskell.org/cabal)
* [LevelDB](http://code.google.com/p/leveldb)
* Optional: [Snappy](http://code.google.com/p/snappy),
  if compression support is desired

**Note:** as of version 1.3, LevelDB can be built as a shared library. Thus, as
of version 0.1.0 of this library, LevelDB is no longer bundled and must be
installed on the target system.

To install the latest version from hackage:

```shell
$ cabal install leveldb-haskell
```

To install from checked-out source:

```shell
$ cabal install
```

## Notes

This library is in very early stage and has seen very limited testing. Comments
and contributions are welcome.

## Bugs and Contributing

Please report issues via http://github.com/kim/leveldb-haskell/issues.<br />
Patches are best submitted as pull requests, or via email
(kim.altintop@gmail.com).

## License

BSD 3, see LICENSE file.
