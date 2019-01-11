module Chainweb.Test.Mempool.InMem
  ( tests
  ) where

------------------------------------------------------------------------------
import Test.Tasty
------------------------------------------------------------------------------
import Chainweb.Mempool.InMem (InMemConfig(..))
import qualified Chainweb.Mempool.InMem as InMem
import Chainweb.Mempool.Mempool
    (Codec(..), TransactionConfig(..), chainwebTestHashMeta,
    chainwebTestHasher)
import Chainweb.Test.Mempool
    (MempoolWithFunc(..), MockTx(..), mockBlocksizeLimit, mockCodec)
import qualified Chainweb.Test.Mempool
------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "mempool/inMem" $ Chainweb.Test.Mempool.tests
                                  $ MempoolWithFunc
                                  $ InMem.withInMemoryMempool cfg
  where
    txcfg = TransactionConfig mockCodec hasher hashmeta mockFees mockSize mockMeta
    -- run the reaper @100Hz for testing
    cfg = InMemConfig txcfg mockBlocksizeLimit (hz 100)
    hz x = 1000000 `div` x
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec
