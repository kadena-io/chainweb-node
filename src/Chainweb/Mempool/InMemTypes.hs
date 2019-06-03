{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
  ) where

------------------------------------------------------------------------------
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)

import Data.HashMap.Strict (HashMap)
import Data.HashPSQ (HashPSQ)
import Data.HashSet (HashSet)
import Data.Int (Int64)
import Data.IORef (IORef)
import Data.Ord (Down(..))
import Data.Word (Word64)

import Foreign.ForeignPtr

import Pact.Types.Gas (GasPrice(..))

-- internal imports

import Chainweb.BlockHeader
import Chainweb.Mempool.Mempool

------------------------------------------------------------------------------
-- | Priority for the search queue
type Priority = (Down GasPrice, Int64)

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
  , _inmemTxBlockSizeLimit :: {-# UNPACK #-} !Int64
  , _inmemReaperIntervalMicros :: {-# UNPACK #-} !Int
}

------------------------------------------------------------------------------
data InMemoryMempool t = InMemoryMempool {
    _inmemCfg :: InMemConfig t
  , _inmemDataLock :: MVar (InMemoryMempoolData t)
  , _inmemReaper :: ThreadId
  -- TODO: reap expired transactions
}

------------------------------------------------------------------------------
data InMemoryMempoolData t = InMemoryMempoolData {
    _inmemPending :: !(IORef (PSQ t))
    -- | We've seen this in a valid block, but if it gets forked and loses
    -- we'll have to replay it.
    --
    -- N.B. atomic access to these IORefs is not necessary -- we hold the lock here.
  , _inmemValidated :: !(IORef (HashMap TransactionHash (ValidatedTransaction t)))
  , _inmemConfirmed :: !(IORef (HashSet TransactionHash))
  , _inmemLastNewBlockParent :: !(IORef (Maybe BlockHeader))
  , _inmemNextTxId :: !(ForeignPtr Word64)   -- faster than IORef for counts
}
