{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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
    , SQLitePendingData
    , emptySQLitePendingData

    , BlockState(..)
    , initBlockState
    , bsBlockHeight
    , bsMode
    , bsTxId
    , bsPendingBlock
    , bsPendingTx
    , BlockEnv(..)
    , benvBlockState
    , benvDb
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

      -- * pact service monad + types
    , PactServiceEnv(..)
    , PactServiceState(..)
    , PactServiceM
    , runPactServiceM

    , PactServiceException(..)

      -- * optics
    , psMempoolAccess
    , psCheckpointEnv
    , psSpvSupport
    , psPublicData
    , psStateValidated
    , psPdb
    , psBlockHeaderDb
    , psMinerRewards
    , psGasModel
    ) where

import Control.Exception
import Control.Exception.Safe hiding (bracket)
import Control.Lens
import Control.Monad.Fail
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
import Data.Tuple.Strict
import Data.Vector (Vector)

import Database.SQLite3.Direct as SQ3

import Foreign.C.Types (CInt(..))

import GHC.Generics

import Pact.Interpreter (PactDbEnv(..))
import Pact.Parse (ParsedDecimal(..))
import Pact.Persist.SQLite (Pragma(..), SQLiteConfig(..))
import Pact.PersistPactDb (DbEnv(..))
import Pact.Types.ChainMeta (PublicData(..))
import qualified Pact.Types.Hash as P
import Pact.Types.Logger (Logger(..), Logging(..))
import Pact.Types.Runtime
    (ExecutionMode(..), PactDb(..), TableName(..), TxId(..),
    TxLog(..))
import Pact.Types.Gas (GasModel)
import Pact.Types.SPV

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Types
import Chainweb.Mempool.Mempool (MempoolPreBlockCheck)
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Transaction


data Env' = forall a. Env' (PactDbEnv (DbEnv a))

data PactDbEnvPersist p = PactDbEnvPersist
    { _pdepPactDb :: PactDb (DbEnv p)
    , _pdepEnv :: DbEnv p
    }

makeLenses ''PactDbEnvPersist


data EnvPersist' = forall a. EnvPersist' (PactDbEnvPersist a)

data PactDbState = PactDbState { _pdbsDbEnv :: EnvPersist' }

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
type SQLitePendingData = ( SQLitePendingTableCreations
                         , SQLitePendingWrites
                         , TxLogMap
                         , SQLitePendingSuccessfulTxs
                         )

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
    }
    deriving Show

emptySQLitePendingData :: SQLitePendingData
emptySQLitePendingData = (mempty, mempty, mempty, mempty)

initBlockState :: BlockState
initBlockState = BlockState 0 Nothing 0 emptySQLitePendingData Nothing

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

newtype BlockHandler p a = BlockHandler
    { runBlockHandler :: ReaderT (BlockDbEnv p) (StateT BlockState IO) a
    } deriving newtype ( Functor
                       , Applicative
                       , Monad
                       , MonadState BlockState
                       , MonadThrow
                       , MonadCatch
                       , MonadMask
                       , MonadIO
                       , MonadReader (BlockDbEnv p)
                       , MonadFail
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
      -- ^ get the checkpointer's idea of the latest block
    , _cpBeginCheckpointerBatch :: IO ()
    , _cpCommitCheckpointerBatch :: IO ()
    , _cpDiscardCheckpointerBatch :: IO ()
    , _cpLookupBlockInCheckpointer :: !((BlockHeight, BlockHash) -> IO Bool)
      -- ^ is the checkpointer aware of the given block?
    , _cpGetBlockParent :: !((BlockHeight, BlockHash) -> IO (Maybe BlockHash))
    , _cpRegisterProcessedTx :: !(P.PactHash -> IO ())

      -- TODO: this would be nicer as a batch lookup :(
    , _cpLookupProcessedTx :: !(P.PactHash -> IO (Maybe (T2 BlockHeight BlockHash)))
    }

data CheckpointEnv = CheckpointEnv
    { _cpeCheckpointer :: !Checkpointer
    , _cpeLogger :: !Logger
    }

makeLenses ''CheckpointEnv

newtype SQLiteFlag = SQLiteFlag { getFlag :: CInt }
  deriving newtype (Eq, Ord, Bits, Num)

data PactServiceEnv cas = PactServiceEnv
    { _psMempoolAccess :: !(Maybe MemPoolAccess)
    , _psCheckpointEnv :: !CheckpointEnv
    , _psSpvSupport :: !SPVSupport
    , _psPublicData :: !PublicData
    , _psPdb :: PayloadDb cas
    , _psBlockHeaderDb :: BlockHeaderDb
    , _psMinerRewards :: HashMap BlockHeight ParsedDecimal
    , _psGasModel :: GasModel
    }

data PactServiceState = PactServiceState
    {_psStateValidated :: Maybe BlockHeader
    }

type PactServiceM cas = ReaderT (PactServiceEnv cas) (StateT PactServiceState IO)

runPactServiceM
    :: (PayloadCas cas)
    => PactServiceState
    -> PactServiceEnv cas
    -> PactServiceM cas a
    -> IO (a, PactServiceState)
runPactServiceM s env action = runStateT (runReaderT action env) s

-- TODO: get rid of this shim, it's probably not necessary
data MemPoolAccess = MemPoolAccess
  { mpaGetBlock
        :: MempoolPreBlockCheck ChainwebTransaction
        -> BlockHeight
        -> BlockHash
        -> BlockHeader
        -> IO (Vector ChainwebTransaction)
  , mpaSetLastHeader :: BlockHeader -> IO ()
  , mpaProcessFork :: BlockHeader -> IO ()
  }

instance Semigroup MemPoolAccess where
  MemPoolAccess f g h <> MemPoolAccess t u v = MemPoolAccess (f <> t) (g <> u) (h <> v)

instance Monoid MemPoolAccess where
  mempty = MemPoolAccess (\_ _ _ -> mempty) (const mempty) (const mempty)

makeLenses ''PactServiceEnv
makeLenses ''PactServiceState

data PactServiceException = PactServiceIllegalRewind {
    _attemptedRewindTo :: Maybe (BlockHeight, BlockHash)
  , _latestBlock :: Maybe (BlockHeight, BlockHash)
  }
  deriving (Generic)

instance Show PactServiceException where
  show (PactServiceIllegalRewind att l)
    = concat [ "illegal rewind attempt to block "
             , show att
             , ", latest was "
             , show l
             ]

instance Exception PactServiceException
