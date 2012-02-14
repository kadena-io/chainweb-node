This library provides Haskell bindings to
[LevelDB](http://leveldb.googlecode.com)

## Installation

Prerequisites:

* [GHC 7.*](http://www.haskell.org/ghc)
* [Cabal](http://www.haskell.org/cabal)
* Optional: [Snappy](http://code.google.com/p/snappy),
  if compression support is desired
* Optional: `autoconf`

To install the latest version from hackage:

```shell
$ cabal install leveldb-haskell
```

To install from checked-out source:

```shell
$ autoconf
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
