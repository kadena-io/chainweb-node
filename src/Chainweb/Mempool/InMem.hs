-- | A mock in-memory mempool backend that does not persist to disk.

module Chainweb.Mempool.InMem
  ( InMemoryMempool(..)
  , InMemConfig(..)
  , withInMemoryMempool
  ) where

import Control.Concurrent.MVar
import Control.Exception (bracket, mask)
import Data.ByteString.Char8 (ByteString)
import Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as PSQ
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int (Int64)

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Mempool.Mempool

-- | PSQ is a min-heap, this is a quick and dirty way of negating the priority
-- calculation
newtype NegatedReward = NegatedReward TransactionReward
toNegated t = NegatedReward (0 - t)
fromNegated (NegatedReward t) = 0 - t

type PSQ t = HashPSQ TransactionHash NegatedReward t


data InMemConfig t = InMemConfig {
    _inmem_codec :: Codec t
  , _inmem_hasher :: ByteString -> TransactionHash
  , _inmem_hash_meta :: HashMeta
  , _inmem_tx_reward :: t -> TransactionReward
  , _inmem_tx_size :: t -> Int64
}


newtype InMemoryMempool t = InMemoryMempool {
    _inmem_lock :: MVar (InMemoryMempoolData t)
}


data InMemoryMempoolData t = InMemoryMempoolData {
    _inmem_cfg :: InMemConfig t
  , _inmem_pending :: PSQ t
    -- | We've seen this in a valid block, but if it gets forked and loses we'll have to
  , _inmem_validated :: PSQ (ValidatedTransaction t)
  , _inmem_confirmed :: HashSet TransactionHash
}


makeInMemPool :: IO (InMemoryMempool t)
makeInMemPool = undefined

destroyInMemPool :: InMemoryMempool t -> IO ()
destroyInMemPool = undefined

toMempoolBackend :: InMemoryMempool t -> MempoolBackend t
toMempoolBackend = undefined

withInMemoryMempool :: (MempoolBackend t -> IO a) -> IO a
withInMemoryMempool f = bracket makeInMemPool destroyInMemPool (f . toMempoolBackend)
