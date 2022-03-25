{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    ( CheckpointEnv(..)
    , cpeCheckpointer
    , cpeLogger
    , Checkpointer(..)
    , Env'(..)
    , EnvPersist'(..)
    , PactDbConfig(..)
    , pdbcGasLimit
    , pdbcGasRate
    , pdbcLogDir
    , pdbcPersistDir
    , pdbcPragmas
    , PactDbEnv'(..)
    , PactDbEnvPersist(..)
    , pdepEnv
    , pdepPactDb
    , PactDbState(..)
    , pdbsDbEnv

    , SQLiteRowDelta(..)
    , SQLiteDeltaKey(..)
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
    , bsBlockHeight
    , bsMode
    , bsTxId
    , bsPendingBlock
    , bsPendingTx
    , bsModuleNameFix
    , bsSortedKeys
    , BlockEnv(..)
    , benvBlockState
    , benvDb
    , runBlockEnv
    , SQLiteEnv(..)
    , sConn
    , sConfig
    , BlockHandler(..)
    , ParentHash
    , BlockDbEnv(..)
    , bdbenvDb
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
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Map.Strict (Map)
import Data.Vector (Vector)

import Database.SQLite3.Direct as SQ3

import Foreign.C.Types (CInt(..))

import GHC.Generics

import Pact.Interpreter (PactDbEnv(..))
import Pact.Persist.SQLite (Pragma(..), SQLiteConfig(..))
import Pact.PersistPactDb (DbEnv(..))
import qualified Pact.Types.Hash as P
import Pact.Types.Logger (Logger(..), Logging(..))
import Pact.Types.Persistence
import Pact.Types.Runtime (TableName)

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Mempool.Mempool (MempoolPreBlockCheck,TransactionHash,BlockFill)
import Chainweb.Pact.Service.Types
import Chainweb.Transaction
import Chainweb.Utils (T2)


data Env' = forall a. Env' (PactDbEnv (DbEnv a))

data PactDbEnvPersist p = PactDbEnvPersist
    { _pdepPactDb :: PactDb (DbEnv p)
    , _pdepEnv :: DbEnv p
    }

makeLenses ''PactDbEnvPersist


data EnvPersist' = forall a. EnvPersist' (PactDbEnvPersist a)

newtype PactDbState = PactDbState { _pdbsDbEnv :: EnvPersist' }

makeLenses ''PactDbState

data PactDbConfig = PactDbConfig
    { _pdbcPersistDir :: Maybe FilePath
    , _pdbcLogDir :: FilePath
    , _pdbcPragmas :: [Pragma]
    , _pdbcGasLimit :: Maybe Int
    , _pdbcGasRate :: Maybe Int
    } deriving (Eq, Show, Generic)

instance FromJSON PactDbConfig

makeLenses ''PactDbConfig

-- | Within a @restore .. save@ block, mutations to the pact database are held
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

-- | When we index 'SQLiteRowDelta' values, we need a lookup key.
data SQLiteDeltaKey = SQLiteDeltaKey
    { _dkTable :: !ByteString
    , _dkRowKey :: !ByteString
    }
  deriving (Show, Generic, Eq, Ord)
  deriving anyclass Hashable

-- | A map from table name to a list of 'TxLog' entries. This is maintained in
-- 'BlockState' and is cleared upon pact transaction commit.
type TxLogMap = Map TableName (DList (TxLog Value))

-- | Between a @restore..save@ bracket, we also need to record which tables
-- were created during this block (so the necessary @CREATE TABLE@ statements
-- can be performed upon block save).
type SQLitePendingTableCreations = HashSet ByteString

-- | Pact transaction hashes resolved during this block.
type SQLitePendingSuccessfulTxs = HashSet ByteString

-- | Pending writes to the pact db during a block, to be recorded in 'BlockState'.
type SQLitePendingWrites = HashMap SQLiteDeltaKey (DList SQLiteRowDelta)

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

data SQLiteEnv = SQLiteEnv
    { _sConn :: !Database
    , _sConfig :: !SQLiteConfig
    }

makeLenses ''SQLiteEnv

-- | Monad state for 'BlockHandler.
data BlockState = BlockState
    { _bsTxId :: !TxId
    , _bsMode :: !(Maybe ExecutionMode)
    , _bsBlockHeight :: !BlockHeight
    , _bsPendingBlock :: !SQLitePendingData
    , _bsPendingTx :: !(Maybe SQLitePendingData)
    , _bsModuleNameFix :: Bool
    , _bsSortedKeys :: Bool
    }
    deriving Show

emptySQLitePendingData :: SQLitePendingData
emptySQLitePendingData = SQLitePendingData mempty mempty mempty mempty

initBlockState :: BlockHeight -> BlockState
initBlockState initialBlockHeight = BlockState
    { _bsTxId = 0
    , _bsMode = Nothing
    , _bsBlockHeight = initialBlockHeight
    , _bsPendingBlock = emptySQLitePendingData
    , _bsPendingTx = Nothing
    , _bsModuleNameFix = False
    , _bsSortedKeys = False
    }

makeLenses ''BlockState

data BlockDbEnv p = BlockDbEnv
    { _bdbenvDb :: !p
    , _logger :: !Logger
    }

makeLenses ''BlockDbEnv

data BlockEnv p = BlockEnv
    { _benvDb :: !(BlockDbEnv p)
    , _benvBlockState :: !BlockState -- ^ The current block state.
    }

makeLenses ''BlockEnv


runBlockEnv :: MVar (BlockEnv SQLiteEnv) -> BlockHandler SQLiteEnv a -> IO a
runBlockEnv e m = modifyMVar e $
  \(BlockEnv dbenv bs) -> do
    (!a,!s) <- runStateT (runReaderT (runBlockHandler m) dbenv) bs
    return (BlockEnv dbenv s, a)

newtype BlockHandler p a = BlockHandler
    { runBlockHandler :: ReaderT (BlockDbEnv p) (StateT BlockState IO) a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadState BlockState
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadIO
        , MonadReader (BlockDbEnv p)
        )

data PactDbEnv' = forall e. PactDbEnv' (PactDbEnv e)

instance Logging (BlockHandler p) where
    log c s = view logger >>= \l -> liftIO $ logLog l c s

type ParentHash = BlockHash

data Checkpointer = Checkpointer
    {
      _cpRestore :: !(Maybe (BlockHeight, ParentHash) -> IO PactDbEnv')
      -- ^ prerequisite: (BlockHeight - 1, ParentHash) is a direct ancestor of
      -- the "latest block"
    , _cpSave :: !(BlockHash -> IO ())
      -- ^ commits pending modifications to block, with the given blockhash
    , _cpDiscard :: IO ()
      -- ^ discard pending block changes
    , _cpGetLatestBlock :: IO (Maybe (BlockHeight, BlockHash))
      -- ^ get the checkpointer's idea of the latest block. The block height is
      -- is the height of the block of the block hash.
      --
      -- TODO: Under which circumstances does this return 'Nothing'?

    , _cpBeginCheckpointerBatch :: IO ()
    , _cpCommitCheckpointerBatch :: IO ()
    , _cpDiscardCheckpointerBatch :: IO ()
    , _cpLookupBlockInCheckpointer :: !((BlockHeight, BlockHash) -> IO Bool)
      -- ^ is the checkpointer aware of the given block?
    , _cpGetBlockParent :: !((BlockHeight, BlockHash) -> IO (Maybe BlockHash))
    , _cpRegisterProcessedTx :: !(P.PactHash -> IO ())

      -- TODO: this would be nicer as a batch lookup :(
    , _cpLookupProcessedTx :: !(P.PactHash -> IO (Maybe (T2 BlockHeight BlockHash)))
    , _cpGetBlockHistory :: !(
        forall k v . (FromJSON v) => BlockHeader -> Domain k v -> IO BlockTxHistory)
    , _cpGetHistoricalLookup :: !(
        forall k v . (FromJSON v) => BlockHeader -> Domain k v -> RowKey -> IO (Maybe (TxLog Value)))
    }

data CheckpointEnv = CheckpointEnv
    { _cpeCheckpointer :: !Checkpointer
    , _cpeLogger :: !Logger
    }

makeLenses ''CheckpointEnv

newtype SQLiteFlag = SQLiteFlag { getFlag :: CInt }
  deriving newtype (Eq, Ord, Bits, Num)

-- TODO: get rid of this shim, it's probably not necessary
data MemPoolAccess = MemPoolAccess
  { mpaGetBlock
        :: BlockFill
        -> MempoolPreBlockCheck ChainwebTransaction
        -> BlockHeight
        -> BlockHash
        -> BlockHeader
        -> IO (Vector ChainwebTransaction)
  , mpaSetLastHeader :: BlockHeader -> IO ()
  , mpaProcessFork :: BlockHeader -> IO ()
  , mpaBadlistTx :: Vector TransactionHash -> IO ()
  }

instance Semigroup MemPoolAccess where
  MemPoolAccess f g h i <> MemPoolAccess t u v w =
      MemPoolAccess (f <> t) (g <> u) (h <> v) (i <> w)

instance Monoid MemPoolAccess where
  mempty = MemPoolAccess (\_ _ _ -> mempty) (const mempty) (const mempty) (const mempty)


data PactServiceException = PactServiceIllegalRewind
    { _attemptedRewindTo :: Maybe (BlockHeight, BlockHash)
    , _latestBlock :: Maybe (BlockHeight, BlockHash)
    } deriving (Generic)

instance Show PactServiceException where
  show (PactServiceIllegalRewind att l)
    = concat [ "illegal rewind attempt to block "
             , show att
             , ", latest was "
             , show l
             ]

instance Exception PactServiceException
