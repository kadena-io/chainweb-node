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
    , cpeGasEnv
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
    , ReorgVersion(..)
    , BlockVersion(..)
    , bvVersion
    , bvBlock
    , BlockState(..)
    , bsBlockVersion
    , bsTxRecord
    , bsMode
    , bsTxId
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

    , PactServiceException(..)

      -- * optics
    , psMempoolAccess
    , psCheckpointEnv
    , psSpvSupport
    , psPublicData
    , psStateValidated
    , psPdb
    , psBlockHeaderDb
    ) where

import Control.DeepSeq
import Control.Exception
import Control.Exception.Safe hiding (bracket)
import Control.Lens
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Aeson
import Data.Bits
import Data.Int
import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Data.Word

import Database.SQLite3.Direct as SQ3

import GHC.Generics

import Pact.Interpreter (PactDbEnv(..))
import Pact.Persist.SQLite (Pragma(..), SQLiteConfig(..))
import Pact.PersistPactDb (DbEnv(..))
import Pact.Types.ChainMeta (PublicData(..))
import Pact.Types.Logger (Logger(..), Logging(..))
import Pact.Types.Runtime
    (ExecutionMode(..), GasEnv(..), PactDb(..), TableName(..), TxId(..),
    TxLog(..))
import Pact.Types.SPV

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Types
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

data SQLiteEnv = SQLiteEnv
  { _sConn :: !Database
  , _sConfig :: !SQLiteConfig
  }

makeLenses ''SQLiteEnv

newtype ReorgVersion = ReorgVersion
  { _getReorgVersion :: Int64
  }
  deriving newtype (Num, Integral, Enum, Real, Ord, Eq)
  deriving stock Show

instance NFData ReorgVersion where
  rnf (ReorgVersion v) = rnf v

data BlockVersion = BlockVersion
  { _bvBlock :: !BlockHeight
  , _bvVersion :: !ReorgVersion
  }
  deriving (Eq, Show)

instance NFData BlockVersion where
  rnf (BlockVersion bvBlock bvVersion) = rnf bvBlock `seq` rnf bvVersion

makeLenses ''BlockVersion

data BlockState = BlockState
  { _bsTxId :: !TxId
  , _bsMode :: !(Maybe ExecutionMode)
  , _bsBlockVersion :: !BlockVersion
  , _bsTxRecord :: !(Map TableName [TxLog Value])
  }
  deriving Show

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
  } deriving ( Functor
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
      restore :: Maybe (BlockHeight, ParentHash) -> IO PactDbEnv'
      -- ^ prerequisite: (BlockHeight - 1, ParentHash) is a direct ancestor of
      -- the "latest block"
    , save :: BlockHash -> IO ()
      -- ^ commits pending modifications to block, with the given blockhash
    , discard :: IO ()
      -- ^ discard pending block changes
    , getLatestBlock :: IO (Maybe (BlockHeight, BlockHash))
      -- ^ get the checkpointer's idea of the latest block
    , withAtomicRewind :: forall a . IO a -> IO a
      -- ^ in the event of rewind we may wish to play through many blocks. In
      -- the event of any of them failing, we should discard the whole
      -- transaction in total.
    , lookupBlockInCheckpointer :: (BlockHeight, BlockHash) -> IO Bool
      -- ^ is the checkpointer aware of the given block?
    }

data CheckpointEnv = CheckpointEnv
    { _cpeCheckpointer :: !Checkpointer
    , _cpeLogger :: !Logger
    , _cpeGasEnv :: !GasEnv
    }

makeLenses ''CheckpointEnv

newtype SQLiteFlag = SQLiteFlag { getFlag :: Word32 }
  deriving (Eq, Ord, Bits, Num)

data PactServiceEnv cas = PactServiceEnv
  { _psMempoolAccess :: !(Maybe MemPoolAccess)
  , _psCheckpointEnv :: !CheckpointEnv
  , _psSpvSupport :: !SPVSupport
  , _psPublicData :: !PublicData
  , _psPdb :: PayloadDb cas
  , _psBlockHeaderDb :: BlockHeaderDb
  }

data PactServiceState = PactServiceState
  {_psStateValidated :: Maybe BlockHeader
  }

type PactServiceM cas = ReaderT (PactServiceEnv cas) (StateT PactServiceState IO)

data MemPoolAccess = MemPoolAccess
  { mpaGetBlock :: BlockHeight -> BlockHash -> BlockHeader -> IO (Vector ChainwebTransaction)
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
