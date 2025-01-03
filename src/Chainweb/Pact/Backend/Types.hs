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

module Chainweb.Pact.Backend.Types
    ( Checkpointer(..)
    , SQLiteEnv
    , IntraBlockPersistence(..)
    , BlockHandle(..)
    , blockHandleTxId
    , blockHandlePending
    , emptyBlockHandle
    , SQLitePendingData(..)
    , emptySQLitePendingData
    , pendingWrites
    , pendingSuccessfulTxs
    , pendingTableCreation
    , pendingTxLogMap
    , SQLiteRowDelta(..)
    , Historical(..)
    , _Historical
    , _NoHistory
    , PactDbFor
    ) where

import Control.Lens
import Chainweb.Pact.Backend.DbCache
import Chainweb.Version
import Database.SQLite3.Direct (Database)
import qualified Pact.Types.Persistence as Pact4
import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import GHC.Generics
import qualified Pact.Types.Names as Pact4
import Data.DList (DList)
import Data.Map (Map)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Control.DeepSeq (NFData)

-- | Whether we write rows to the database that were already overwritten
-- in the same block.
data IntraBlockPersistence = PersistIntraBlockWrites | DoNotPersistIntraBlockWrites
    deriving (Eq, Ord, Show)

data Checkpointer logger
    = Checkpointer
    { cpLogger :: logger
    , cpCwVersion :: ChainwebVersion
    , cpChainId :: ChainId
    , cpSql :: SQLiteEnv
    , cpIntraBlockPersistence :: IntraBlockPersistence
    , cpModuleCacheVar :: MVar (DbCache Pact4.PersistModuleData)
    }

type SQLiteEnv = Database

-- | While a block is being run, mutations to the pact database are held
-- in RAM to be written to the DB in batches at @save@ time. For any given db
-- write, we need to record the table name, the current tx id, the row key, and
-- the row value.
--
data SQLiteRowDelta = SQLiteRowDelta
    { _deltaTableName :: !ByteString -- utf8?
    , _deltaTxId :: {-# UNPACK #-} !Pact4.TxId
    , _deltaRowKey :: !ByteString
    , _deltaData :: !ByteString
    } deriving (Show, Generic, Eq)

instance Ord SQLiteRowDelta where
    compare a b = compare aa bb
        where
        aa = (_deltaTableName a, _deltaRowKey a, _deltaTxId a)
        bb = (_deltaTableName b, _deltaRowKey b, _deltaTxId b)
    {-# INLINE compare #-}

-- | A map from table name to a list of 'TxLog' entries. This is maintained in
-- 'BlockState' and is cleared upon pact transaction commit.
type TxLogMap = Map Pact4.TableName (DList Pact4.TxLogJson)

-- | Between a @restore..save@ bracket, we also need to record which tables
-- were created during this block (so the necessary @CREATE TABLE@ statements
-- can be performed upon block save).
type SQLitePendingTableCreations = HashSet ByteString

-- | Pact transaction hashes resolved during this block.
type SQLitePendingSuccessfulTxs = HashSet ByteString

-- | Pending writes to the pact db during a block, to be recorded in 'BlockState'.
-- Structured as a map from table name to a map from rowkey to inserted row delta.
type SQLitePendingWrites = HashMap ByteString (HashMap ByteString (NonEmpty SQLiteRowDelta))

-- Note [TxLogs in SQLitePendingData]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We should really not store TxLogs in SQLitePendingData,
-- because this data structure is specifically for things that
-- can exist both for the whole block and for specific transactions,
-- and txlogs only exist on the transaction level.
-- We don't do this in Pact 5 at all.

-- | A collection of pending mutations to the pact db. We maintain two of
-- these; one for the block as a whole, and one for any pending pact
-- transaction. Upon pact transaction commit, the two 'SQLitePendingData'
-- values are merged together.
data SQLitePendingData = SQLitePendingData
    { _pendingTableCreation :: !SQLitePendingTableCreations
    , _pendingWrites :: !SQLitePendingWrites
    -- See Note [TxLogs in SQLitePendingData]
    , _pendingTxLogMap :: !TxLogMap
    , _pendingSuccessfulTxs :: !SQLitePendingSuccessfulTxs
    }
    deriving (Eq, Show)

makeLenses ''SQLitePendingData

emptySQLitePendingData :: SQLitePendingData
emptySQLitePendingData = SQLitePendingData mempty mempty mempty mempty

data BlockHandle = BlockHandle
    { _blockHandleTxId :: !Pact4.TxId
    , _blockHandlePending :: !SQLitePendingData
    }
    deriving (Eq, Show)
makeLenses ''BlockHandle

emptyBlockHandle :: Pact4.TxId -> BlockHandle
emptyBlockHandle txid = BlockHandle txid emptySQLitePendingData

-- | The result of a historical lookup which might fail to even find the
-- header the history is being queried for.
data Historical a
    = Historical a
    | NoHistory
    deriving stock (Eq, Foldable, Functor, Generic, Traversable, Show)
    deriving anyclass NFData

makePrisms ''Historical

type family PactDbFor logger (pv :: PactVersion)
