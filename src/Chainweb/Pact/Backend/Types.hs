{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Pact.Backend.Types
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Chainweb / Pact Types module for various database backends
module Chainweb.Pact.Backend.Types
    ( RunnableBlock(..)
    , Checkpointer(..)
    , _cpRewindTo
    , ReadCheckpointer(..)
    , CurrentBlockDbEnv(..)
    , Env'(..)
    , EnvPersist'(..)
    , PactDbConfig(..)
    , pdbcGasLimit
    , pdbcGasRate
    , pdbcLogDir
    , pdbcPersistDir
    , pdbcPragmas
    , ChainwebPactDbEnv
    , PactDbEnvPersist(..)
    , pdepEnv
    , pdepPactDb
    , PactDbState(..)
    , pdbsDbEnv

    , SQLiteRowDelta(..)
    , SQLitePendingTableCreations
    , SQLitePendingWrites
    , SQLitePendingData(..)
    , pendingTableCreation
    , pendingWrites
    , pendingTxLogMap
    , pendingSuccessfulTxs
    , emptySQLitePendingData

    , BlockState(..)
    , initBlockState
    , bsMode
    , bsTxId
    , bsPendingBlock
    , bsPendingTx
    , bsModuleCache
    , BlockEnv(..)
    , benvBlockState
    , blockHandlerEnv
    , runBlockEnv
    , SQLiteConnectionType(..)
    , SQLiteEnv(..)
    , Database
    , BlockHandler(..)
    , BlockHandlerEnv(..)
    , mkBlockHandlerEnv
    , blockHandlerBlockHeight
    , blockHandlerModuleNameFix
    , blockHandlerSortedKeys
    , blockHandlerLowerCaseTables
    , blockHandlerDb
    , blockHandlerLogger
    , blockHandlerPersistIntraBlockWrites
    , ParentHash
    , SQLiteFlag(..)

      -- * mempool
    , MemPoolAccess(..)

    , PactServiceException(..)
    ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Exception.Safe hiding (bracket)
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Aeson
import Data.Bits
import Data.ByteString (ByteString)
import Data.DList (DList)
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Vector (Vector)

import Database.SQLite3.Direct as SQ3

import Foreign.C.Types (CInt(..))

import GHC.Generics
import GHC.Stack

import Pact.Interpreter (PactDbEnv(..))
import Pact.Persist.SQLite (Pragma(..))
import Pact.PersistPactDb (DbEnv(..))
import qualified Pact.Types.Hash as P
import Pact.Types.Persistence
import Pact.Types.RowData (RowData)
import Pact.Types.Runtime (TableName)

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Pact.Backend.DbCache
import Chainweb.Pact.Service.Types
import Chainweb.Transaction
import Chainweb.Utils (T2)
import Chainweb.Version
import Chainweb.Version.Guards
import Chainweb.Mempool.Mempool (MempoolPreBlockCheck,TransactionHash,BlockFill)

import Streaming(Stream, Of)

data Env' = forall a. Env' (PactDbEnv (DbEnv a))

data PactDbEnvPersist p = PactDbEnvPersist
    { _pdepPactDb :: !(PactDb (DbEnv p))
    , _pdepEnv :: !(DbEnv p)
    }

makeLenses ''PactDbEnvPersist


data EnvPersist' = forall a. EnvPersist' (PactDbEnvPersist a)

newtype PactDbState = PactDbState { _pdbsDbEnv :: EnvPersist' }

makeLenses ''PactDbState

data PactDbConfig = PactDbConfig
    { _pdbcPersistDir :: !(Maybe FilePath)
    , _pdbcLogDir :: !FilePath
    , _pdbcPragmas :: ![Pragma]
    , _pdbcGasLimit :: !(Maybe Int)
    , _pdbcGasRate :: !(Maybe Int)
    } deriving (Eq, Show, Generic)

instance FromJSON PactDbConfig

makeLenses ''PactDbConfig

-- | While a block is being run, mutations to the pact database are held
-- in RAM to be written to the DB in batches at @save@ time. For any given db
-- write, we need to record the table name, the current tx id, the row key, and
-- the row value.
--
data SQLiteRowDelta = SQLiteRowDelta
    { _deltaTableName :: !ByteString -- utf8?
    , _deltaTxId :: {-# UNPACK #-} !TxId
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
type TxLogMap = Map TableName (DList TxLogJson)

-- | Between a @restore..save@ bracket, we also need to record which tables
-- were created during this block (so the necessary @CREATE TABLE@ statements
-- can be performed upon block save).
type SQLitePendingTableCreations = HashSet ByteString

-- | Pact transaction hashes resolved during this block.
type SQLitePendingSuccessfulTxs = HashSet ByteString

-- | Pending writes to the pact db during a block, to be recorded in 'BlockState'.
-- Structured as a map from table name to a map from rowkey to inserted row delta.
type SQLitePendingWrites = HashMap ByteString (HashMap ByteString (NonEmpty SQLiteRowDelta))

-- | A collection of pending mutations to the pact db. We maintain two of
-- these; one for the block as a whole, and one for any pending pact
-- transaction. Upon pact transaction commit, the two 'SQLitePendingData'
-- values are merged together.
data SQLitePendingData = SQLitePendingData
    { _pendingTableCreation :: !SQLitePendingTableCreations
    , _pendingWrites :: !SQLitePendingWrites
    , _pendingTxLogMap :: !TxLogMap
    , _pendingSuccessfulTxs :: !SQLitePendingSuccessfulTxs
    }
    deriving (Show)

makeLenses ''SQLitePendingData

data SQLiteConnectionType = ReadWrite | ReadOnly deriving Eq
data SQLiteEnv = SQLiteEnv SQLiteConnectionType Database

-- | Monad state for 'BlockHandler.
-- This notably contains all of the information that's being mutated during
-- blocks, notably _bsPendingBlock, the pending writes in the block, and
-- _bsPendingTx, the pending writes in the transaction which will be discarded
-- on tx failure.
data BlockState = BlockState
    { _bsTxId :: !TxId
    , _bsPendingBlock :: !SQLitePendingData
    , _bsPendingTx :: !(Maybe SQLitePendingData)
    , _bsMode :: !(Maybe ExecutionMode)
    , _bsModuleCache :: !(DbCache PersistModuleData)
    }

emptySQLitePendingData :: SQLitePendingData
emptySQLitePendingData = SQLitePendingData mempty mempty mempty mempty

initBlockState
    :: DbCacheLimitBytes
    -- ^ Module Cache Limit (in bytes of corresponding rowdata)
    -> TxId
    -- ^ next tx id (end txid of previous block)
    -> BlockState
initBlockState cl txid = BlockState
    { _bsTxId = txid
    , _bsMode = Nothing
    , _bsPendingBlock = emptySQLitePendingData
    , _bsPendingTx = Nothing
    , _bsModuleCache = emptyDbCache cl
    }

makeLenses ''BlockState

data BlockHandlerEnv logger = BlockHandlerEnv
    { _blockHandlerDb :: !Database
    , _blockHandlerLogger :: !logger
    , _blockHandlerBlockHeight :: !BlockHeight
    , _blockHandlerModuleNameFix :: !Bool
    , _blockHandlerSortedKeys :: !Bool
    , _blockHandlerLowerCaseTables :: !Bool
    , _blockHandlerPersistIntraBlockWrites :: !IntraBlockPersistence
    }

mkBlockHandlerEnv
  :: ChainwebVersion -> ChainId -> BlockHeight
  -> Database -> IntraBlockPersistence -> logger -> BlockHandlerEnv logger
mkBlockHandlerEnv v cid bh sql p logger = BlockHandlerEnv
    { _blockHandlerDb = sql
    , _blockHandlerLogger = logger
    , _blockHandlerBlockHeight = bh
    , _blockHandlerModuleNameFix = enableModuleNameFix v cid bh
    , _blockHandlerSortedKeys = pact42 v cid bh
    , _blockHandlerLowerCaseTables = chainweb217Pact v cid bh
    , _blockHandlerPersistIntraBlockWrites = p
    }


makeLenses ''BlockHandlerEnv

data BlockEnv logger =
  BlockEnv
    { _blockHandlerEnv :: !(BlockHandlerEnv logger)
    , _benvBlockState :: !BlockState -- ^ The current block state.
    }

makeLenses ''BlockEnv


runBlockEnv :: MVar (BlockEnv logger) -> BlockHandler logger a -> IO a
runBlockEnv e m = modifyMVar e $
  \(BlockEnv dbenv bs) -> do
    (!a,!s) <- runStateT (runReaderT (runBlockHandler m) dbenv) bs
    return (BlockEnv dbenv s, a)

-- this monad allows access to the database environment "at" a particular block.
-- unfortunately, this is tied to a useless MVar via runBlockEnv, which will
-- be deleted with pact 5.
newtype BlockHandler logger a = BlockHandler
    { runBlockHandler :: ReaderT (BlockHandlerEnv logger) (StateT BlockState IO) a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadState BlockState
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadIO
        , MonadReader (BlockHandlerEnv logger)
        )

type ChainwebPactDbEnv logger = PactDbEnv (BlockEnv logger)

type ParentHash = BlockHash

-- | The parts of the checkpointer that do not mutate the database.
data ReadCheckpointer logger = ReadCheckpointer
  { _cpReadFrom ::
    !(forall a. Maybe ParentHeader ->
      (CurrentBlockDbEnv logger -> IO a) -> IO a)
    -- ^ rewind to a particular block *in-memory*, producing a read-write snapshot
    -- ^ of the database at that block to compute some value, after which the snapshot
    -- is discarded and nothing is saved to the database.
    -- ^ prerequisite: ParentHeader is an ancestor of the "latest block"
  , _cpGetEarliestBlock :: !(IO (Maybe (BlockHeight, BlockHash)))
    -- ^ get the checkpointer's idea of the earliest block. The block height
    --   is the height of the block of the block hash.
  , _cpGetLatestBlock :: !(IO (Maybe (BlockHeight, BlockHash)))
    -- ^ get the checkpointer's idea of the latest block. The block height is
    -- is the height of the block of the block hash.
    --
    -- TODO: Under which circumstances does this return 'Nothing'?
  , _cpLookupBlockInCheckpointer :: !((BlockHeight, BlockHash) -> IO Bool)
    -- ^ is the checkpointer aware of the given block?
  , _cpGetBlockParent :: !((BlockHeight, BlockHash) -> IO (Maybe BlockHash))
  , _cpGetBlockHistory ::
      !(BlockHeader -> Domain RowKey RowData -> IO BlockTxHistory)
  , _cpGetHistoricalLookup ::
      !(BlockHeader -> Domain RowKey RowData -> RowKey -> IO (Maybe (TxLog RowData)))
  , _cpLogger :: logger
  }

-- | A callback which writes a block's data to the input database snapshot,
-- and knows its parent header (Nothing if it's a genesis block).
-- Reports back its own header and some extra value.
newtype RunnableBlock logger a = RunnableBlock
  { runBlock :: CurrentBlockDbEnv logger -> Maybe ParentHeader -> IO (a, BlockHeader) }

-- | One makes requests to the checkpointer to query the pact state at the
-- current block or any earlier block, to extend the pact state with new blocks, and
-- to rewind the pact state to an earlier block.
data Checkpointer logger = Checkpointer
  { _cpRestoreAndSave ::
    !(forall q r.
      (HasCallStack, Monoid q) =>
      Maybe ParentHeader ->
      Stream (Of (RunnableBlock logger q)) IO r ->
      IO (r, q))
  -- ^ rewind to a particular block, and play a stream of blocks afterward,
  -- extending the chain and saving the result persistently. for example,
  -- to validate a block `vb`, we rewind to the common ancestor of `vb` and
  -- the latest block, and extend the chain with all of the blocks on `vb`'s
  -- fork, including `vb`.
  -- this function takes care of making sure that this is done *atomically*.
  -- TODO: fix with latest type
  -- promises:
  --   - excluding the fact that each _cpRestoreAndSave call is atomic, the
  --     following two expressions should be equivalent:
  --     do
  --       _cpRestoreAndSave cp p1 x
  --         ((,) <$> (bs1 <* Stream.yield p2) <*> bs2) runBlk
  --     do
  --       (r1, q1) <- _cpRestoreAndSave cp p1 x (bs1 <* Stream.yield p2) runBlk
  --       (r2, q2) <- _cpRestoreAndSave cp (Just (x p2)) x bs2 runBlk
  --       return ((r1, r2), q1 <> q2)
  --     i.e. rewinding, extending, then rewinding to the point you extended
  --     to and extending some more, should give the same result as rewinding
  --     once and extending to the same final point.
  --   - no block in the stream is used more than once.
  -- prerequisites:
  --   - the parent being rewound to must be a direct ancestor
  --     of the latest block, i.e. what's returned by _cpLatestBlock.
  --   - the stream must start with a block that is a child of the rewind
  --     target and each block after must be the child of the previous block.
  , _cpReadCp :: !(ReadCheckpointer logger)
  -- ^ access all read-only operations of the checkpointer.
  }

-- the special case where one doesn't want to extend the chain, just rewind it.
_cpRewindTo :: Checkpointer logger -> Maybe ParentHeader -> IO ()
_cpRewindTo cp ancestor = void $ _cpRestoreAndSave cp
    ancestor
    (pure () :: Stream (Of (RunnableBlock logger ())) IO ())

-- this is effectively a read-write snapshot of the Pact state at a block.
data CurrentBlockDbEnv logger = CurrentBlockDbEnv
    { _cpPactDbEnv :: !(ChainwebPactDbEnv logger)
    , _cpRegisterProcessedTx :: !(P.PactHash -> IO ())
    , _cpLookupProcessedTx ::
        !(Vector P.PactHash -> IO (HashMap P.PactHash (T2 BlockHeight BlockHash)))
    }

newtype SQLiteFlag = SQLiteFlag { getFlag :: CInt }
  deriving newtype (Eq, Ord, Bits, Num)

-- TODO: get rid of this shim, it's probably not necessary
data MemPoolAccess = MemPoolAccess
  { mpaGetBlock
        :: !(BlockFill
        -> MempoolPreBlockCheck ChainwebTransaction
        -> BlockHeight
        -> BlockHash
        -> BlockHeader
        -> IO (Vector ChainwebTransaction)
        )
  , mpaSetLastHeader :: !(BlockHeader -> IO ())
  , mpaProcessFork :: !(BlockHeader -> IO ())
  , mpaBadlistTx :: !(Vector TransactionHash -> IO ())
  }

instance Semigroup MemPoolAccess where
  MemPoolAccess f g h i <> MemPoolAccess t u v w =
      MemPoolAccess (f <> t) (g <> u) (h <> v) (i <> w)

instance Monoid MemPoolAccess where
  mempty = MemPoolAccess (\_ _ _ -> mempty) (const mempty) (const mempty) (const mempty)


data PactServiceException = PactServiceIllegalRewind
    { _attemptedRewindTo :: !(Maybe (BlockHeight, BlockHash))
    , _latestBlock :: !(Maybe (BlockHeight, BlockHash))
    } deriving (Generic)

instance Show PactServiceException where
  show (PactServiceIllegalRewind att l)
    = concat [ "illegal rewind attempt to block "
             , show att
             , ", latest was "
             , show l
             ]

instance Exception PactServiceException
