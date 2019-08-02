module Chainweb.Test.Mempool.InMem
  ( tests
  ) where

------------------------------------------------------------------------------
import Test.Tasty
------------------------------------------------------------------------------
import qualified Chainweb.Mempool.InMem as InMem
import Chainweb.Mempool.InMemTypes (InMemConfig(..))
import Chainweb.Mempool.Mempool
import Chainweb.Test.Mempool (MempoolWithFunc(..))
import qualified Chainweb.Test.Mempool
import Chainweb.Utils (Codec(..))
import Data.CAS.RocksDB
------------------------------------------------------------------------------

tests :: RocksDb -> TestTree
tests rdb = testGroup "Chainweb.Mempool.InMem"
            $ Chainweb.Test.Mempool.tests
            $ MempoolWithFunc
            $ InMem.withInMemoryMempool cfg rdb
  where
    txcfg = TransactionConfig mockCodec hasher hashmeta mockGasPrice mockGasLimit
                              mockMeta (const $ return True)
    -- run the reaper @100Hz for testing
    cfg = InMemConfig txcfg mockBlockGasLimit (hz 100) 2048 True
    hz x = 1000000 `div` x
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec
----------------------------------------------------------------------------------------------------
