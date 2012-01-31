Comprehensive walkthough of the functionality provided by this library.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import Control.Monad
> import Data.ByteString.Char8 hiding (take)
> import Prelude hiding (putStrLn)
> import System.FilePath
>
> import Database.LevelDB
>
> import Debug.Trace
>
>
> main :: IO ()
> main =

Almost all operations on a leveldb database occur within the @withLevelDB@
bracket, to ensure proper resource de-/allocation.

>     withLevelDB dbdir [ CreateIfMissing, CacheSize 2048 ] $ \db -> do

The basic operations are @put@, @get@ and @delete@, straightforwardly:

>         put db [] "foo" "bar"
>         get db [ FillCache ] "foo" >>= print
>         delete db [] "foo"
>         get db [ FillCache ] "foo" >>= print

Additionally, we can write several key/value pairs in a batch, which is
guaranteed to be atomic. Note the use of the @Sync@ option here: it tells
leveldb to flush the data to disk before returning. While syncing may have a
considerable performance impact, it is most useful with batch operations.

Also note that we're taking a snapshot of the database before issueing the
write.

>         withSnapshot db $ \snap -> do
>             write db [ Sync ] [ Put "a" "one"
>                               , Put "b" "two"
>                               , Put "c" "three" ]

Now, we can perform a snapshot read. As expected, this will output nothing,
since we took the snapshot before writing.

>             dumpEntries db [ UseSnapshot snap, FillCache ]

Conversely, we should see the values just written in a "dirty" read:

>             dumpEntries db [ FillCache ]


Let's inspect the state of our database, just for the fun of it.

>         approximateSize db ("a", "z") >>= print
>         getProperty db SSTables >>= printProperty "sstables"
>         getProperty db Stats    >>= printProperty "stats"
>         getProperty db (NumFilesAtLevel 1) >>= printProperty "num files at level"

Similar to the batch write above, we can also use @write@ to delete key/value
pairs:

>         write db [ Sync ] [ Del "a", Del "b", Del "c" ]
>         dumpEntries db [ FillCache ]
>
>     where
>         dbdir = "/" </> "tmp" </> "leveltest"
>
>         dumpEntries db opts =
>             withIterator db opts $ \iter -> do
>                 iterFirst iter
>                 iterEntries iter print
>
>         iterEntries iter f = do
>             valid <- iterValid iter
>             when valid $ do
>                 key <- iterKey iter
>                 val <- iterValue iter
>                 _   <- f (key, val)
>                 _   <- iterNext iter
>                 iterEntries iter f
>
>         printProperty l p = do
>             putStrLn l
>             maybe (putStrLn "n/a") putStrLn $ p
