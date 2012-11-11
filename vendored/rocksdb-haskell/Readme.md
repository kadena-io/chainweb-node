This library provides Haskell bindings to
[LevelDB](http://leveldb.googlecode.com)

## History

Version 0.2.0:

* requires LevelDB v1.7
* support for filter policy (LevelDB v1.5), either custom or using the built-in
  bloom filter implementation
* write batch values no longer require a `memcpy` to be early-finalizer-safe
  (introduced in 0.1.1)

Version 0.1.0:

* memory (foreign pointers) is managed through
  [ResourceT](http://hackage.haskell.org/package/resourcet). Note that this
  requires to lift monadic actions inside the `MonadResource` monad, see the
  examples.
* links against shared library (LevelDB v1.3 or higher)
* LevelDB 1.3 API fully supported (including custom comparators, excluding
  custom environments)

Version 0.0.x:

* experimental releases

## Installation

Prerequisites:

* [GHC 7.*](http://www.haskell.org/ghc)
* [Cabal](http://www.haskell.org/cabal), version 1.3 or higher
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
