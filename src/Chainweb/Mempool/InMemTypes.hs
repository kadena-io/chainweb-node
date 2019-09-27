{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | A mock in-memory mempool backend that does not persist to disk.
module Chainweb.Mempool.InMemTypes
  ( InMemConfig(..)
  , InMemoryMempool(..)
  , InMemoryMempoolData(..)
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
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef)
import Data.Tuple.Strict
import qualified Data.Vector as V

import GHC.Generics

-- internal imports

import Chainweb.Mempool.Mempool
import Chainweb.Time (Micros(..), Time(..))

------------------------------------------------------------------------------
type PendingMap = HashMap TransactionHash SB.ShortByteString

------------------------------------------------------------------------------
-- | Configuration for in-memory mempool.
data InMemConfig t = InMemConfig {
    _inmemTxCfg :: {-# UNPACK #-} !(TransactionConfig t)
  , _inmemTxBlockSizeLimit :: !GasLimit
  , _inmemMaxRecentItems :: {-# UNPACK #-} !Int
    -- Here True means 'OK to insert'
  , _inmemPreInsertCheck :: !(V.Vector t -> IO (V.Vector Bool))
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
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, NFData)
