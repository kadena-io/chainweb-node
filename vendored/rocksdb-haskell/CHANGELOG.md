## 1.0.1
 * Add support for UTF-8 characters in a database's path

## 1.0.0

 * First version by Serokell

## 0.3x.0

 * ResourceT is no longer compulsory


## 0.2.0

 * requires LevelDB v1.7
 * support for filter policy (LevelDB v1.5), either custom or using the built-in
   bloom filter implementation
 * write batch values no longer require a `memcpy` to be early-finalizer-safe
   (introduced in 0.1.1)


## 0.1.0

 * memory (foreign pointers) is managed through
   [ResourceT](http://hackage.haskell.org/package/resourcet). Note that this
   requires to lift monadic actions inside the `MonadResource` monad, see the
   examples.
 * links against shared library (LevelDB v1.3 or higher)
 * LevelDB 1.3 API fully supported (including custom comparators, excluding
   custom environments)


## 0.0.x

 * experimental releases
