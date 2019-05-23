module Chainweb.Test.Mempool.InMem
  ( tests
  ) where

------------------------------------------------------------------------------
import Test.Tasty
------------------------------------------------------------------------------
import Chainweb.BlockHeaderDB
import Chainweb.Mempool.InMemTypes (InMemConfig(..))
import qualified Chainweb.Mempool.InMem as InMem
import Chainweb.Mempool.Mempool
import Chainweb.Test.Mempool (MempoolWithFunc(..))
import Chainweb.Test.Utils (toyChainId, toyVersion)
import qualified Chainweb.Test.Mempool
import Chainweb.Utils (Codec(..))
import Data.CAS.RocksDB
------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Chainweb.Mempool.InMem"
            $ Chainweb.Test.Mempool.tests
            $ MempoolWithFunc
            $ InMem.withTestInMemoryMempool
                  cfg
                  (withTempRocksDb "mempool-socket-tests")
                  (withBlockHeaderDb' toyVersion toyChainId)
  where
    withBlockHeaderDb' v cid rdb f = withBlockHeaderDb rdb v cid f
    txcfg = TransactionConfig mockCodec hasher hashmeta mockGasPrice mockGasLimit
                              mockMeta (const $ return True)
    -- run the reaper @100Hz for testing
    cfg = InMemConfig txcfg mockBlockGasLimit (hz 100)
    hz x = 1000000 `div` x
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec
----------------------------------------------------------------------------------------------------
