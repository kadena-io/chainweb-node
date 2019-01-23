{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chainweb.BlockPayloadDB.FS
  ( FsDB(..)
  , withDB
  ) where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import Control.Monad (liftM, void, when, (>=>))
import Control.Parallel.Strategies
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Random.MWC as MWCB
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32)
import qualified System.Directory as Dir
import System.Path (Absolute, Path, (</>))
import qualified System.Path as Path
import qualified System.Random.MWC as MWC

import Chainweb.BlockHash (BlockHashBytes(..))
import Chainweb.BlockHeader (BlockPayloadHash(..))
import Chainweb.BlockPayloadDB
import Chainweb.Utils (Codec(..))

data FsDB t = FsDB {
    _fsDbRoot :: Path Absolute
  , _fsDbConfig :: {-# UNPACK #-} !(PayloadConfig t)
  , _fsDbGen :: MVar (MWC.GenIO)
}


withDB :: NFData t
       => FilePath              -- ^ db root
       -> PayloadConfig t       -- ^ description of payloads
       -> (DB t -> IO b)        -- ^ user handler
       -> IO b
withDB root0 config userFunc = do
    root <- Path.makeAbsolute $ Path.fromFilePath root0
    Dir.createDirectoryIfMissing True $ Path.toFilePath root
    gen <- newMWC
    let fsdb = FsDB root config gen
    let db = DB { payloadLookup = fsLookup fsdb
                , payloadInsert = fsInsert fsdb
                , payloadDelete = fsDelete fsdb }
    userFunc db


fsInsert :: NFData t => FsDB t -> Vector t -> IO (Vector (Either String ()))
fsInsert fsdb requests = do
    gen <- splitMWC fsdb
    suffixes <- V.replicateM (V.length requests) $ randomSuffix gen
    let inputs = V.zip3 hashes requests suffixes `using` parVector 32
    Async.mapConcurrently insertOne inputs

  where
    randomSuffix gen = (B.unpack . B16.encode) <$> MWCB.randomGen gen 6
    hashes = V.map hash requests
    root = _fsDbRoot fsdb
    hash = payloadHash $ _fsDbConfig fsdb
    encode = codecEncode $ payloadCodec $ _fsDbConfig fsdb
    toEither m = (Right <$> m) `catches`
        -- rethrow on ThreadKilled
        [ Handler $ \(e :: AsyncException) -> throwIO e
        , Handler $ \(e :: SomeException) -> return (Left (show e)) ]

    insertOne (h, t, sfx) = toEither $ do
        let (parent, fn) = getBlockPath root h
        Dir.createDirectoryIfMissing True $ Path.toFilePath parent
        let destPath = Path.toFilePath (parent </> Path.fromUnrootedFilePath fn)
        ex <- Dir.doesFileExist destPath
        when (not ex) $ do
            let tmpPath = destPath ++ "." ++ sfx
            B.writeFile tmpPath (encode t) `onException` rmDashF tmpPath
            Dir.renameFile tmpPath destPath

fsLookup :: FsDB t -> Vector BlockPayloadHash -> IO (Vector (Maybe t))
fsLookup fsdb = Async.mapConcurrently lookupOne
  where
    root = _fsDbRoot fsdb
    decode = codecDecode $ payloadCodec $ _fsDbConfig fsdb
    lookupOne h = do
        let fp = getBlockFilePath root h
        ex <- Dir.doesFileExist fp
        if ex
          then decode <$> B.readFile fp
          else return Nothing


fsDelete :: FsDB t -> Vector BlockPayloadHash -> IO ()
fsDelete fsdb = Async.mapConcurrently_ deleteOne
  where
    root = _fsDbRoot fsdb
    deleteOne = rmDashF . getBlockFilePath root


parVector :: NFData a => Int -> Strategy (Vector a)
parVector n = liftM V.fromList . parListChunk n rdeepseq . V.toList


getBlockPath :: Path Absolute                  -- ^ payload store root
             -> BlockPayloadHash               -- ^ block payload hash
             -> (Path Absolute, FilePath)  -- ^ (dirname, filename)
getBlockPath root (BlockPayloadHash (BlockHashBytes hash)) = (dir, B.unpack fn)
  where
    b16hash = B16.encode hash
    (pfx1, r1) = B.splitAt 2 b16hash
    (pfx2, fn) = B.splitAt 2 r1
    unp = Path.fromUnrootedFilePath . B.unpack
    dir = root </> unp pfx1 </> unp pfx2


getBlockFilePath :: Path Absolute -> BlockPayloadHash -> FilePath
getBlockFilePath root h = fp
  where
    (parent, fn) = getBlockPath root h
    fp = Path.toFilePath (parent </> Path.fromUnrootedFilePath fn)


splitMWC :: FsDB t -> IO MWC.GenIO
splitMWC (FsDB _ _ mv) = withMVar mv (mkIV >=> MWC.initialize)
  where
    mkIV :: MWC.GenIO -> IO (VU.Vector Word32)
    mkIV = flip MWC.uniformVector 32


newMWC :: IO (MVar MWC.GenIO)
newMWC = MWC.createSystemRandom >>= newMVar


eatIOExceptions :: IO () -> IO ()
eatIOExceptions = handle $ \(e :: IOException) -> void $ evaluate e


rmDashF :: FilePath -> IO ()
rmDashF f = eatIOExceptions $ Dir.removeFile f
