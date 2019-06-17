{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

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
    ) where

import Control.Exception.Safe hiding (bracket)
import Control.Monad.Fail
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Lens
import Control.DeepSeq

import Data.Aeson
import Data.Int
import Data.Map.Strict (Map)

import Database.SQLite3.Direct as SQ3

import GHC.Generics

import Pact.Interpreter (PactDbEnv(..))
import Pact.Persist.SQLite (Pragma(..), SQLiteConfig(..))
import Pact.PersistPactDb (DbEnv(..))
import Pact.Types.Logger (Logger(..), Logging(..))
import Pact.Types.Runtime (PactDb(..), TxId(..), ExecutionMode(..), TableName(..), TxLog(..), GasEnv(..))

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader

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
  { _sConn :: Database
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

makeLenses ''BlockState

data BlockDbEnv p = BlockDbEnv
  { _bdbenvDb :: p
  , _logger :: Logger
  }

makeLenses ''BlockDbEnv


data BlockEnv p = BlockEnv
  { _benvDb :: BlockDbEnv p
  , _benvBlockState :: !BlockState
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
             , MonadIO
             , MonadReader (BlockDbEnv p)
             , MonadFail
             )

data PactDbEnv' = forall e. PactDbEnv' (PactDbEnv e)

instance Logging (BlockHandler p) where
  log c s = view logger >>= \l -> liftIO $ logLog l c s

type ParentHash = BlockHash

-- TODO: pass (Maybe blockHash) (only populated on validateblock) to
-- restore instead of to save make save a noarg function, move history
-- updates to restore

data Checkpointer = Checkpointer
    {
      restore :: Maybe (BlockHeight, ParentHash) -> IO PactDbEnv'
    , save :: BlockHash -> IO ()
    , discard :: IO ()
    }

data CheckpointEnv = CheckpointEnv
    { _cpeCheckpointer :: !Checkpointer
    , _cpeLogger :: Logger
    , _cpeGasEnv :: !GasEnv
    }

makeLenses ''CheckpointEnv
