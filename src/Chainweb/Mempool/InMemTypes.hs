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
  , SubscriptionId
  , TxEdit (..)
  , TxSubscriberMap
  , TxBroadcaster(..)
  ) where

------------------------------------------------------------------------------
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM.TBMChan (TBMChan)

import Data.HashMap.Strict (HashMap)
import Data.HashPSQ (HashPSQ)
import Data.HashSet (HashSet)
import Data.Int (Int64)
import Data.IORef (IORef)
import Data.Ord (Down(..))
import Data.Vector (Vector)
import Data.Word (Word64)

import Pact.Types.Gas (GasPrice(..))

import System.Mem.Weak (Weak)

-- internal imports

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Mempool.Mempool
import Chainweb.Payload.PayloadStore

------------------------------------------------------------------------------
-- | Priority for the search queue
type Priority = (Down GasPrice, Int64)

------------------------------------------------------------------------------
-- | Priority search queue -- search by transaction hash in /O(log n)/ like a
-- tree, find-min in /O(1)/ like a heap
type PSQ t = HashPSQ TransactionHash Priority t

type SubscriptionId = Word64

------------------------------------------------------------------------------
-- | Transaction edits -- these commands will be sent to the broadcast thread
-- over an STM channel.
data TxEdit t = Subscribe !SubscriptionId (Subscription t) (MVar (IORef (Subscription t)))
              | Unsubscribe !SubscriptionId
              | Transactions (Vector t)
              | Close
type TxSubscriberMap t = HashMap SubscriptionId (Weak (IORef (Subscription t)))

-- | The 'TxBroadcaster' is responsible for broadcasting new transactions out
-- to any readers. Commands are posted to the channel and are executed by a
-- helper thread.
data TxBroadcaster t = TxBroadcaster {
    _txbSubIdgen :: {-# UNPACK #-} !(IORef SubscriptionId)
  , _txbThread :: {-# UNPACK #-} !(MVar ThreadId)
  , _txbQueue :: {-# UNPACK #-} !(TBMChan (TxEdit t))
  , _txbThreadDone :: {-# UNPACK #-} !(MVar ())
}

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
data InMemoryMempool t cas = InMemoryMempool {
    _inmemCfg :: InMemConfig t
  , _inmemDataLock :: MVar (InMemoryMempoolData t)
  , _inmemBroadcaster :: TxBroadcaster t
  , _inmemReaper :: ThreadId
  , _inmemBlockHeaderDb :: BlockHeaderDb
  , _inmemPayloadStore :: Maybe (PayloadDb cas)
  -- TODO: reap expired transactions
}

------------------------------------------------------------------------------
data InMemoryMempoolData t = InMemoryMempoolData {
    _inmemPending :: IORef (PSQ t)
    -- | We've seen this in a valid block, but if it gets forked and loses
    -- we'll have to replay it.
    --
    -- N.B. atomic access to these IORefs is not necessary -- we hold the lock here.
  , _inmemValidated :: IORef (HashMap TransactionHash (ValidatedTransaction t))
  , _inmemConfirmed :: IORef (HashSet TransactionHash)
  , _inmemLastNewBlockParent :: IORef (Maybe BlockHeader)
}
