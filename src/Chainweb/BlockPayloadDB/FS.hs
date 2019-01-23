{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chainweb.BlockPayloadDB.FS
  ( FsDB(..)
  , FsOps(..)
  , withDB
  , withDB'
  , systemFsOps
  ) where

------------------------------------------------------------------------------
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (void, when)
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Random.MWC as MWCB
import Data.Vector (Vector)
import qualified System.Directory as Dir
import System.Path (Absolute, Path, (</>))
import qualified System.Path as Path
import qualified System.Random.MWC as MWC
------------------------------------------------------------------------------
import Chainweb.BlockHash (BlockHashBytes(..))
import Chainweb.BlockHeader (BlockPayloadHash(..))
import Chainweb.BlockPayloadDB
import Chainweb.Utils (Codec(..))
------------------------------------------------------------------------------

-- TODO: use base64 instead? slower, but wastes fewer bytes for filenames

-- | A content-addressed store for block payloads using a direct filesystem
-- representation. Block payload hashes are converted to hex strings and mapped
-- to files rooted at the given directory. We use a 3-deep nesting, e.g. block
-- payload hash
--
-- >  f22136124cd3e1d65a48487cecf310771b2fd1e83dc032e3d19724160ac0ff71
--
-- would be stored at
--
-- >  $ROOT/f22/136/124cd3e1d65a48487cecf310771b2fd1e83dc032e3d19724160ac0ff71
--
-- Filesystem operations are abstracted by passing an 'FsOps' record; this will
-- allow us to plug in a backend using S3 later.
data FsDB t = FsDB {
    _fsDbRoot :: Path Absolute
  , _fsDbConfig :: {-# UNPACK #-} !(PayloadConfig t)
  , _fsOps :: {-# UNPACK #-} !FsOps
}

-- | Filesystem operations for the content-addressed store backend. Use
-- 'systemFsOps' to use the local filesystem.
data FsOps = FsOps {
    opDoesFileExist :: FilePath -> IO Bool
        -- ^ does a file at this path exist?
  , opMakeAbsolutePath :: FilePath -> IO (Path Absolute)
        -- ^ makes the given path absolute.
  , opCreateDirectory :: FilePath -> IO ()
        -- ^ makes a directory and all of its parents.
  , opWriteAtomic :: FilePath -> ByteString -> IO ()
        -- ^ atomic-writes the given contents to the filesystem. On local
        -- filesystems this means a write to a temporary file and an
        -- atomic-rename.
  , opDeleteFile :: FilePath -> IO ()
        -- ^ deletes the given file, silently.
  , opReadFile :: FilePath -> IO ByteString
        -- ^ strictly reads the given path into a bytestring.
}


-- | Given a root directory and a payload config, generates a block payload 'DB'
-- and runs the given user handler with it.
withDB :: FilePath              -- ^ db root
       -> PayloadConfig t       -- ^ description of payloads
       -> (DB t -> IO b)        -- ^ user handler
       -> IO b
withDB fp cfg userHandler =
    systemFsOps >>= \ops -> withDB' ops fp cfg userHandler


-- | Given a filesystem implementation, a root directory, and a payload config,
-- generates a block payload 'DB' and runs the given user handler with it.
withDB' :: FsOps                 -- ^ filesystem abstraction
        -> FilePath              -- ^ db root
        -> PayloadConfig t       -- ^ description of payloads
        -> (DB t -> IO b)        -- ^ user handler
        -> IO b
withDB' ops root0 config userFunc = do
    root <- opMakeAbsolutePath ops root0
    opCreateDirectory ops $ Path.toFilePath root
    let fsdb = FsDB root config ops
    let db = DB { payloadDbConfig = config
                , payloadLookup = fsLookup fsdb
                , payloadInsert = fsInsert fsdb
                , payloadDelete = fsDelete fsdb }
    userFunc db


fsInsert :: FsDB t -> Vector t -> IO (Vector (Either String ()))
fsInsert fsdb = Async.mapConcurrently insertOne
  where
    ops = _fsOps fsdb
    root = _fsDbRoot fsdb
    hash = payloadHash $ _fsDbConfig fsdb
    encode = codecEncode $ payloadCodec $ _fsDbConfig fsdb
    toEither m = (Right <$> m) `catches`
        -- rethrow on ThreadKilled
        [ Handler $ \(e :: AsyncException) -> throwIO e
        , Handler $ \(e :: SomeException) -> return (Left (show e)) ]

    insertOne t = toEither $ do
        let h = hash t
        let (parent, fn) = getBlockPath root h
        opCreateDirectory ops $ Path.toFilePath parent
        let destPath = Path.toFilePath (parent </> Path.fromUnrootedFilePath fn)
        ex <- opDoesFileExist ops destPath
        when (not ex) $ opWriteAtomic ops destPath (encode t)


fsLookup :: FsDB t -> Vector BlockPayloadHash -> IO (Vector (Maybe t))
fsLookup fsdb = Async.mapConcurrently lookupOne
  where
    ops = _fsOps fsdb
    root = _fsDbRoot fsdb
    decode = codecDecode $ payloadCodec $ _fsDbConfig fsdb
    lookupOne h = do
        let fp = getBlockFilePath root h
        ex <- opDoesFileExist ops fp
        if ex
          then decode <$> opReadFile ops fp
          else return Nothing


fsDelete :: FsDB t -> Vector BlockPayloadHash -> IO ()
fsDelete fsdb = Async.mapConcurrently_ deleteOne
  where
    ops = _fsOps fsdb
    root = _fsDbRoot fsdb
    deleteOne = opDeleteFile ops . getBlockFilePath root


getBlockPath :: Path Absolute                  -- ^ payload store root
             -> BlockPayloadHash               -- ^ block payload hash
             -> (Path Absolute, FilePath)  -- ^ (dirname, filename)
getBlockPath root (BlockPayloadHash (BlockHashBytes hash)) = (dir, B.unpack fn)
  where
    b16hash = B16.encode hash
    (pfx1, r1) = B.splitAt 3 b16hash
    (pfx2, fn) = B.splitAt 3 r1
    unp = Path.fromUnrootedFilePath . B.unpack
    dir = root </> unp pfx1 </> unp pfx2


getBlockFilePath :: Path Absolute -> BlockPayloadHash -> FilePath
getBlockFilePath root h = fp
  where
    (parent, fn) = getBlockPath root h
    fp = Path.toFilePath (parent </> Path.fromUnrootedFilePath fn)


newMWC :: IO (MVar MWC.GenIO)
newMWC = MWC.createSystemRandom >>= newMVar


eatIOExceptions :: IO () -> IO ()
eatIOExceptions = handle $ \(e :: IOException) -> void $ evaluate e


systemFsOps :: IO FsOps
systemFsOps = do
    mv <- newMWC
    return $ FsOps Dir.doesFileExist mkAbsolute mkdir (writeAtomic mv) rmFile B.readFile
  where
    mkAbsolute = Path.makeAbsolute . Path.fromFilePath
    mkdir = Dir.createDirectoryIfMissing True
    randomBytes mv = withMVar mv $ flip MWCB.randomGen 6
    rmFile = eatIOExceptions . Dir.removeFile
    writeAtomic mv fp s = do
        suffix <- (B.unpack . B16.encode) <$> randomBytes mv
        let tmp = fp ++ ('.' : suffix)
        let write = B.writeFile tmp s >> Dir.renameFile tmp fp
        write `onException` rmFile tmp
