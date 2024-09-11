module Chainweb.Test.Mempool.InMem
  ( tests
  ) where

------------------------------------------------------------------------------
import Control.Concurrent.MVar
import qualified Data.Vector as V
import Test.Tasty
------------------------------------------------------------------------------
import qualified Chainweb.Mempool.InMem as InMem
import Chainweb.Mempool.InMemTypes (InMemConfig(..))
import Chainweb.Mempool.Mempool
import Chainweb.Test.Mempool (InsertCheck, MempoolWithFunc(..))
import qualified Chainweb.Test.Mempool
import Chainweb.Utils (Codec(..))
------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Chainweb.Test.Mempool"
            $ Chainweb.Test.Mempool.tests
            $ MempoolWithFunc wf
  where
    wf :: (InsertCheck -> MempoolBackend MockTx -> IO a) -> IO a
    wf f = do
        mv <- newMVar (pure . V.map Right)
        let cfg = InMemConfig txcfg mockBlockGasLimit 0 2048 Right (checkMv mv) (1024 * 10)
        mp <- InMem.startInMemoryMempoolTest cfg
        f mv mp

    checkMv :: MVar (t -> IO b) -> t -> IO b
    checkMv mv xs = do
        f <- readMVar mv
        f xs

    txcfg = TransactionConfig mockCodec hasher hashmeta mockGasPrice mockGasLimit
                              mockMeta
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec
