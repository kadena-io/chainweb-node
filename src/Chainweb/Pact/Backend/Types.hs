{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Chainweb.Pact.Backend.Types
    ( SQLiteEnv
    , BlockHandle(..)
    , blockHandleTxId
    , blockHandlePending
    , SQLitePendingData(..)
    , emptyBlockHandle
    , emptySQLitePendingData
    , pendingWrites
    , pendingSuccessfulTxs
    , pendingTableCreation
    , SQLiteRowDelta(..)
    , Historical(..)
    , throwIfNoHistory
    , _Historical
    , _NoHistory
    , ChainwebPactDb(..)
    , HeaderOracle(..)
    ) where

import Control.Exception.Safe (MonadThrow)
import Control.Lens
import Chainweb.Version
import Database.SQLite3.Direct (Database)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.HashSet (HashSet)
import Control.DeepSeq (NFData)
import GHC.Generics
import GHC.Stack

import Chainweb.BlockHash
import Pact.Core.Command.Types
import qualified Pact.Core.Persistence as Pact
import qualified Pact.Core.Builtin as Pact
import qualified Pact.Core.Evaluate as Pact
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import qualified Pact.Core.SPV as Pact

import Chainweb.BlockHeight
import qualified Chainweb.Pact.Backend.InMemDb as InMemDb
import Chainweb.Parent
import Chainweb.Utils
import Chainweb.BlockPayloadHash

data HeaderOracle = HeaderOracle
    -- this hash must always have a child
    { consult :: !(Parent BlockHash -> IO Bool)
    , chain :: !ChainId
    }

instance HasChainId HeaderOracle where
    _chainId oracle = oracle.chain

-- | The Pact database as it's provided by the checkpointer.
data ChainwebPactDb = ChainwebPactDb
    { doChainwebPactDbTransaction
        :: forall a
        . BlockHandle
        -> Maybe RequestKey
        -> (Pact.PactDb Pact.CoreBuiltin Pact.Info -> Pact.SPVSupport -> IO a)
        -> IO (a, BlockHandle)
        -- ^ Give this function a BlockHandle representing the state of a pending
        -- block and it will pass you a PactDb which contains the Pact state as of
        -- that point in the block. After you're done, it passes you back a
        -- BlockHandle representing the state of the block extended with any writes
        -- you made to the PactDb.
        -- Note also that this function handles registering
        -- transactions as completed, if you pass it a RequestKey.
    , lookupPactTransactions :: Vector RequestKey -> IO (HashMap RequestKey (T3 BlockHeight BlockPayloadHash BlockHash))
        -- ^ Used to implement transaction polling.
    }

type SQLiteEnv = Database

-- | While a block is being run, mutations to the pact database are held
-- in RAM to be written to the DB in batches at @save@ time. For any given db
-- write, we need to record the table name, the current tx id, the row key, and
-- the row value.
--
data SQLiteRowDelta = SQLiteRowDelta
    { _deltaTableName :: !Text
    , _deltaTxId :: {-# UNPACK #-} !Pact.TxId
    , _deltaRowKey :: !ByteString
    , _deltaData :: !ByteString
    } deriving (Show, Generic, Eq)

instance Ord SQLiteRowDelta where
    compare a b = compare aa bb
        where
        aa = (_deltaTableName a, _deltaRowKey a, _deltaTxId a)
        bb = (_deltaTableName b, _deltaRowKey b, _deltaTxId b)
    {-# INLINE compare #-}

-- | Between a @restore..save@ bracket, we also need to record which tables
-- were created during this block (so the necessary @CREATE TABLE@ statements
-- can be performed upon block save).
type SQLitePendingTableCreations = HashSet Text

-- | Pact transaction hashes resolved during this block.
type SQLitePendingSuccessfulTxs = HashSet ByteString

-- | A collection of pending mutations to the pact db. We maintain two of
-- these; one for the block as a whole, and one for any pending pact
-- transaction. Upon pact transaction commit, the two 'SQLitePendingData'
-- values are merged together.
data SQLitePendingData = SQLitePendingData
    { _pendingTableCreation :: !SQLitePendingTableCreations
    , _pendingWrites :: !InMemDb.Store
    , _pendingSuccessfulTxs :: !SQLitePendingSuccessfulTxs
    }
    deriving (Eq, Show)

emptySQLitePendingData :: SQLitePendingData
emptySQLitePendingData = SQLitePendingData mempty InMemDb.empty mempty

data BlockHandle = BlockHandle
    { _blockHandleTxId :: !Pact.TxId
    , _blockHandlePending :: !SQLitePendingData
    }
    deriving (Eq, Show)

emptyBlockHandle :: Pact.TxId -> BlockHandle
emptyBlockHandle txid = BlockHandle txid emptySQLitePendingData

-- | The result of a historical lookup which might fail to even find the
-- header the history is being queried for.
data Historical a
    = Historical a
    | NoHistory
    deriving stock (Eq, Foldable, Functor, Generic, Traversable, Show)
    deriving anyclass NFData

throwIfNoHistory :: (HasCallStack, MonadThrow m) => Historical a -> m a
throwIfNoHistory NoHistory = error "missing history"
throwIfNoHistory (Historical a) = return a

makePrisms ''Historical
makeLenses ''BlockHandle
makeLenses ''SQLitePendingData
