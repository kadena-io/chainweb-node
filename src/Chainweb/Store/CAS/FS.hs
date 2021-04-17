{-# LANGUAGE ScopedTypeVariables #-}

module Chainweb.Store.CAS.FS
  ( FsDB(..)
  , FsOps(..)
  , withDB
  , withDB'
  , systemFsOps
  ) where

------------------------------------------------------------------------------
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.FixedThreadPool (ThreadPool)
import qualified Control.Concurrent.FixedThreadPool as TP
import Control.Exception
import Control.Monad (unless)
import Control.Monad.Trans.Except
import Data.Bytes.Put (runPutS)
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified System.Directory as Dir
import System.Path (Absolute, Path, (</>))
import qualified System.Path as Path
------------------------------------------------------------------------------
import Chainweb.BlockHeader (BlockPayloadHash, encodeBlockPayloadHash)
import Chainweb.Store.CAS
import Chainweb.Utils (Codec(..), eatIOExceptions, randomByteString)
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
  , _fsTP :: {-# UNPACK #-} !ThreadPool
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
    numC <- getNumCapabilities
    TP.withThreadPool numC $ \tp -> do
        root <- opMakeAbsolutePath ops root0
        opCreateDirectory ops $ Path.toFilePath root
        let fsdb = FsDB root config ops tp
        let db = DB { casDbConfig = config
                    , casLookup = fsLookup fsdb
                    , casInsert = fsInsert fsdb
                    , casDelete = fsDelete fsdb }
        userFunc db


fsInsert :: FsDB t -> Vector t -> IO ()
fsInsert fsdb v = TP.mapAction_ tp insertOne (V.toList v)
  where
    tp = _fsTP fsdb
    ops = _fsOps fsdb
    root = _fsDbRoot fsdb
    hash = payloadHash $ _fsDbConfig fsdb
    encode = codecEncode $ payloadCodec $ _fsDbConfig fsdb
    insertOne t = do
        let h = hash t
        let (parent, fn) = getBlockPath root h
        opCreateDirectory ops $ Path.toFilePath parent
        let destPath = Path.toFilePath (parent </> Path.fromUnrootedFilePath fn)
        ex <- opDoesFileExist ops destPath
        unless ex $ opWriteAtomic ops destPath (encode t)


fsLookup :: FsDB t -> Vector BlockPayloadHash -> IO (Vector (Maybe t))
fsLookup fsdb v = TP.mapAction tp lookupOne (V.toList v) >>= toV
  where
    toV es = V.fromList <$> (runExceptT (traverse (ExceptT . return) es)
                             >>= either throwIO return)

    tp = _fsTP fsdb
    ops = _fsOps fsdb
    root = _fsDbRoot fsdb
    decode = codecDecode $ payloadCodec $ _fsDbConfig fsdb
    lookupOne h = do
        let fp = getBlockFilePath root h
        ex <- opDoesFileExist ops fp
        if ex
          then either (const Nothing) Just . decode <$> opReadFile ops fp
          else return Nothing


fsDelete :: FsDB t -> Vector BlockPayloadHash -> IO ()
fsDelete fsdb v = TP.mapAction_ tp deleteOne (V.toList v)
  where
    tp = _fsTP fsdb
    ops = _fsOps fsdb
    root = _fsDbRoot fsdb
    deleteOne = opDeleteFile ops . getBlockFilePath root


getBlockPath :: Path Absolute                  -- ^ payload store root
             -> BlockPayloadHash               -- ^ block payload hash
             -> (Path Absolute, FilePath)  -- ^ (dirname, filename)
getBlockPath root hash = (dir, B.unpack fn)
  where
    b16hash = B16.encode $ runPutS $ encodeBlockPayloadHash hash
    (pfx1, r1) = B.splitAt 3 b16hash
    (pfx2, fn) = B.splitAt 3 r1
    unp = Path.fromUnrootedFilePath . B.unpack
    dir = root </> unp pfx1 </> unp pfx2


getBlockFilePath :: Path Absolute -> BlockPayloadHash -> FilePath
getBlockFilePath root h = fp
  where
    (parent, fn) = getBlockPath root h
    fp = Path.toFilePath (parent </> Path.fromUnrootedFilePath fn)


systemFsOps :: IO FsOps
systemFsOps = do
    return $ FsOps Dir.doesFileExist mkAbsolute mkdir writeAtomic rmFile B.readFile
  where
    mkAbsolute = Path.makeAbsolute . Path.fromFilePath
    mkdir = Dir.createDirectoryIfMissing True
    rmFile = eatIOExceptions . Dir.removeFile
    writeAtomic fp s = do
        suffix <- B.unpack . B16.encode <$> randomByteString 6
        let tmp = fp ++ ('.' : suffix)
        let write = B.writeFile tmp s >> Dir.renameFile tmp fp
        write `onException` rmFile tmp
