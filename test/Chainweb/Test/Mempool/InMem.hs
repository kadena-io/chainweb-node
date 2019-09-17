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
import Chainweb.Test.Mempool (MempoolWithFunc(..))
import qualified Chainweb.Test.Mempool
import Chainweb.Utils (Codec(..))
------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Chainweb.Mempool.InMem"
            $ Chainweb.Test.Mempool.tests
            $ MempoolWithFunc wf
  where
    wf f = do
        mv <- newMVar (\v -> V.mapM (const $ return True) v)
        let cfg = InMemConfig txcfg mockBlockGasLimit 2048 (checkMv mv)
        InMem.withInMemoryMempool cfg $ f mv

    checkMv mv xs = do
        f <- readMVar mv
        f xs

    txcfg = TransactionConfig mockCodec hasher hashmeta mockGasPrice mockGasLimit
                              mockMeta
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec

