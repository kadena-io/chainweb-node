{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | An in-memory mempool backend that does not persist to disk.
module Chainweb.Mempool.InMemTypes
  ( InMemConfig(..)
  , InMemoryMempool(..)
  , InMemoryMempoolData(..)
  , PendingEntry(..)
  , PendingMap
  , RecentItem
  , RecentLog(..)
  , MempoolStats(..)
  , BadMap
  ) where

------------------------------------------------------------------------------
import Control.Concurrent.MVar (MVar)
import Control.DeepSeq

import Data.Aeson
import qualified Data.ByteString.Short as SB
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef)
import Data.Ord
import qualified Data.Vector as V

import GHC.Generics

import Numeric.Natural

-- internal imports

import Chainweb.Mempool.CurrentTxs
import Chainweb.Mempool.Mempool
import Chainweb.Time (Micros(..), Time(..))
import Chainweb.Utils (T2)

------------------------------------------------------------------------------
data PendingEntry = PendingEntry
    { _inmemPeGasPrice :: !GasPrice
    , _inmemPeGasLimit :: !GasLimit
    , _inmemPeBytes :: !SB.ShortByteString
    , _inmemPeExpires :: !(Time Micros)
    } deriving (Eq, Generic, Show, NFData)

instance Ord PendingEntry where
    compare = compare `on` (Down . _inmemPeGasPrice)

type PendingMap = HashMap TransactionHash PendingEntry

------------------------------------------------------------------------------
-- | Configuration for in-memory mempool.
data InMemConfig t = InMemConfig {
    _inmemTxCfg :: {-# UNPACK #-} !(TransactionConfig t)
  , _inmemTxBlockSizeLimit :: !GasLimit
  , _inmemTxMinGasPrice :: !GasPrice
  , _inmemMaxRecentItems :: {-# UNPACK #-} !Int
  , _inmemPreInsertPureChecks :: t -> Either InsertError t
  , _inmemPreInsertBatchChecks
        :: V.Vector (T2 TransactionHash t)
        -> IO (V.Vector (Either (T2 TransactionHash InsertError) (T2 TransactionHash t)))
  , _inmemCurrentTxsSize :: !Natural
    -- ^ The number of active transactions in validated blocks that can be
    -- distinguished. If there are more txs than this number, checks can
    -- return false negatives (and a very small amount of false positives).
    --
    -- The set uses 16 bytes per entry.
}

------------------------------------------------------------------------------
data InMemoryMempool t = InMemoryMempool {
    _inmemCfg :: !(InMemConfig t)
  , _inmemDataLock :: !(MVar (InMemoryMempoolData t))
  , _inmemNonce :: !ServerNonce
}


------------------------------------------------------------------------------
type BadMap = HashMap TransactionHash (Time Micros)

------------------------------------------------------------------------------
data InMemoryMempoolData t = InMemoryMempoolData {
    _inmemPending :: !(IORef PendingMap)
    -- ^ The set of pending transactions

  , _inmemRecentLog :: !(IORef RecentLog)
    -- ^ The log of all recently added transactions. This is used to compute the
    -- highwater mark for synchronization with remote mempools.

  , _inmemBadMap :: !(IORef BadMap)
    -- ^ Non-expired transactions that failed during pact validation and are
    -- known to be bad. Those must not be attempted again because the user would
    -- possibly have to pay gas for it several times.

  , _inmemCurrentTxs :: !(IORef CurrentTxs)
    -- ^ The set of non-expired transactions that have been added to a block.
    -- Transactions are removed from the set of pending transactions when they
    -- are added to a block. This set is used to prevent transactions from being
    -- re-inserts when synchronizing with nodes that haven't yet validated the
    -- block.
}

------------------------------------------------------------------------------
type RecentItem = T2 MempoolTxId TransactionHash
data RecentLog = RecentLog {
    _rlNext :: {-# UNPACK #-} !MempoolTxId
  , _rlRecent :: !(V.Vector RecentItem)
  }

------------------------------------------------------------------------------
data MempoolStats = MempoolStats
    { _mStatsPendingCount :: {-# UNPACK #-} !Int
    , _mStatsRecentCount :: {-# UNPACK #-} !Int
    , _mStatsBadlistCount :: {-# UNPACK #-} !Int
    , _mStatsCurrentTxsCount :: {-# UNPACK #-} !Int
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, NFData)
