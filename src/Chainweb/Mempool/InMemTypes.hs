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
import Data.Tuple.Strict
import qualified Data.Vector as V

import GHC.Generics

import Pact.Types.Gas (GasPrice(..))

-- internal imports

import Chainweb.Mempool.CurrentTxIndex
import Chainweb.Mempool.Mempool
import Chainweb.Time (Micros(..), Time(..))

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
  , _inmemMaxRecentItems :: {-# UNPACK #-} !Int
  , _inmemPreInsertPureChecks :: t -> Either InsertError t
  , _inmemPreInsertBatchChecks
        :: V.Vector (T2 TransactionHash t)
        -> IO (V.Vector (Either (T2 TransactionHash InsertError) (T2 TransactionHash t)))
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
    _inmemCountPending :: !(IORef Int)
  , _inmemPending :: !(IORef PendingMap)
  , _inmemRecentLog :: !(IORef RecentLog)
  , _inmemBadMap :: !(IORef BadMap)
  , _inmemCurrentTxIdx :: !(IORef CurrentTxIdx)
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
    , _mStatsCurrentTxIdxCount :: {-# UNPACK #-} !Int
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, NFData)
