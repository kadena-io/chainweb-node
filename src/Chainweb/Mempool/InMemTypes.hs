{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | A mock in-memory mempool backend that does not persist to disk.
module Chainweb.Mempool.InMemTypes
  ( _defaultTxQueueLen
  , InMemConfig(..)
  , InMemoryMempool(..)
  , InMemoryMempoolData(..)
  , Priority
  , PSQ
  , RecentItem
  , RecentLog(..)
  ) where

------------------------------------------------------------------------------
import Control.Concurrent.MVar (MVar)

import Data.HashMap.Strict (HashMap)
import Data.HashPSQ (HashPSQ)
import Data.IORef (IORef)
import Data.Ord (Down(..))
import Data.Tuple.Strict
import qualified Data.Vector as V

import Pact.Types.Gas (GasPrice(..))

-- internal imports

import Chainweb.Mempool.Mempool

------------------------------------------------------------------------------
-- | Priority for the search queue
type Priority = (Down GasPrice, GasLimit)

------------------------------------------------------------------------------
-- | Priority search queue -- search by transaction hash in /O(log n)/ like a
-- tree, find-min in /O(1)/ like a heap
type PSQ t = HashPSQ TransactionHash Priority t

------------------------------------------------------------------------------
_defaultTxQueueLen :: Int
_defaultTxQueueLen = 64

------------------------------------------------------------------------------
-- | Configuration for in-memory mempool.
data InMemConfig t = InMemConfig {
    _inmemTxCfg :: {-# UNPACK #-} !(TransactionConfig t)
  , _inmemTxBlockSizeLimit :: !GasLimit
  , _inmemMaxRecentItems :: {-# UNPACK #-} !Int
}

------------------------------------------------------------------------------
data InMemoryMempool t = InMemoryMempool {
    _inmemCfg :: !(InMemConfig t)
  , _inmemDataLock :: !(MVar (InMemoryMempoolData t))
  , _inmemNonce :: !ServerNonce
}

------------------------------------------------------------------------------
data InMemoryMempoolData t = InMemoryMempoolData {
    _inmemPending :: !(IORef (PSQ t))
  , _inmemRecentLog :: !(IORef RecentLog)
  , _inmemQuarantine :: !(IORef (HashMap TransactionHash t))
}

------------------------------------------------------------------------------
type RecentItem = T2 MempoolTxId TransactionHash
data RecentLog = RecentLog {
    _rlNext :: {-# UNPACK #-} !MempoolTxId
  , _rlRecent :: !(V.Vector RecentItem)
  }
