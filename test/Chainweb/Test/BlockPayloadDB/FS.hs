module Chainweb.Test.BlockPayloadDB.FS (tests) where

------------------------------------------------------------------------------
import Control.Exception
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Random.MWC as MWCB
import qualified System.Directory as Dir
import System.Path ((</>))
import qualified System.Path as Path
import Test.Tasty
------------------------------------------------------------------------------
import Chainweb.BlockPayloadDB
import qualified Chainweb.BlockPayloadDB.FS as FS
import Chainweb.Test.BlockPayloadDB


withDB :: (DB MockPayload -> IO a) -> IO a
withDB userFunc = bracket mkdir rmRF go
  where
    go root = FS.withDB root mockPayloadConfig userFunc

    mkdir = mask $ \restore -> do
        tmpdir <- (Path.fromFilePath <$> Dir.getTemporaryDirectory) >>= restore . Path.makeAbsolute
        suffix <- (Path.fromUnrootedFilePath . B.unpack . B16.encode) <$> MWCB.random 8
        let p = Path.toFilePath (tmpdir </> suffix)
        restore (Dir.createDirectoryIfMissing True p)
        return p
    rmRF = Dir.removeDirectoryRecursive

tests :: TestTree
tests = testGroup "BlockPayloadDB.FS" $ blockPayloadDBTests (PayloadDBWithFunc withDB)
