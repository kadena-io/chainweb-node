module Chainweb.Test.Store.CAS.FS (tests) where

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
import Chainweb.Store.CAS
import qualified Chainweb.Store.CAS.FS as FS
import Chainweb.Test.Store.CAS


-- TODO: plug in mock filesystem here so that we don't hammer the local filesystem?

withDB :: (DB MockPayload -> IO a) -> IO a
withDB userFunc = bracket mkdir rmRF go
  where
    go root = FS.withDB root mockPayloadConfig userFunc

    mkdir = mask $ \restore -> do
        tmpdir <- (Path.fromFilePath <$> Dir.getTemporaryDirectory) >>= restore . Path.makeAbsolute
        suffix <- (Path.fromUnrootedFilePath . B.unpack . B16.encodeBase16') <$> MWCB.random 8
        let p = Path.toFilePath (tmpdir </> suffix)
        restore (Dir.createDirectoryIfMissing True p)
        return p
    rmRF = Dir.removeDirectoryRecursive

tests :: TestTree
tests = testGroup "Chainweb.Store.CAS.FS" $ casDbTests (CasDbWithFunc withDB)
