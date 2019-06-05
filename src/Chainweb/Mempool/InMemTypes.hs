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
  , RecentItem
  , RecentLog(..)
  , emptyRecentLog
  , recordRecentTransactions
  , getRecentTxs
  ) where

------------------------------------------------------------------------------
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.DeepSeq

import Data.HashMap.Strict (HashMap)
import Data.HashPSQ (HashPSQ)
import Data.HashSet (HashSet)
import Data.Int (Int64)
import Data.IORef (IORef)
import Data.Ord (Down(..))
import Data.Vector (Vector)
import qualified Data.Vector as V

import Pact.Types.Gas (GasPrice(..))

import Prelude hiding (pred)

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
  , _inmemMaxRecentItems :: {-# UNPACK #-} !Int
}

------------------------------------------------------------------------------
data InMemoryMempool t = InMemoryMempool {
    _inmemCfg :: InMemConfig t
  , _inmemDataLock :: MVar (InMemoryMempoolData t)
  , _inmemReaper :: ThreadId
  , _inmemNonce :: ServerNonce
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
  , _inmemRecentLog :: !(IORef RecentLog)
}

------------------------------------------------------------------------------
type RecentItem = (MempoolTxId, TransactionHash)
data RecentLog = RecentLog {
    _rlNext :: {-# UNPACK #-} !MempoolTxId
  , _rlRecent :: ![RecentItem]
  }

emptyRecentLog :: RecentLog
emptyRecentLog = RecentLog 0 []

recordRecentTransactions :: Int -> Vector TransactionHash -> RecentLog -> RecentLog
recordRecentTransactions maxNumRecent newTxs rlog = rlog'
  where
    !rlog' = RecentLog { _rlNext = newNext
                       , _rlRecent = newL
                       }

    numNewItems = V.length newTxs
    oldNext = _rlNext rlog
    newNext = oldNext + fromIntegral numNewItems
    newL' = _rlRecent rlog ++ ([oldNext..] `zip` V.toList newTxs)
    newL = force $ take maxNumRecent newL'


-- | Get the recent transactions from the transaction log. Returns Nothing if
-- the old high water mark is too out of date.
getRecentTxs :: Int -> MempoolTxId -> RecentLog -> Maybe (Vector TransactionHash)
getRecentTxs maxNumRecent oldHw rlog =
    if oldHw <= oldestHw || oldHw > oldNext
      then Nothing
      else if oldHw == oldNext
              then Just V.empty
              else Just $! txs

  where
    oldNext = _rlNext rlog
    oldestHw = oldNext - fromIntegral maxNumRecent
    txs = V.fromList $ map snd $ filter pred $ _rlRecent rlog
    pred x = fst x >= oldHw
