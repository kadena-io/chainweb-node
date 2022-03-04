This package provides some storage tools that are used in
[chainweb](https://github.com/kadena-io/chainweb).

## Installation

This package depends on RocksDb and a C++ compiler which must be installed prior to building.

On Ubuntu:

```bash
sudo apt-get install librocksdb-dev g++
```

On MacOSX using [homebrew](https://brew.sh):

```bash
brew install rocksdb
```

On Windows using [msys2](https://www.msys2.org):

```bash
pacman -S mingw-w64-x86_64-rocksdb g++
```

On windows, for GHC to find installed libraries you'll have to add the following
settings to your `cabal.project.local` file:

```cabal
package rocksdb-haskell
  extra-lib-dirs: C:\\msys64\\mingw64\\lib
  extra-include-dirs: C:\\msys64\\mingw64\\include
```
