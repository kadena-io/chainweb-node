{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

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
    , cpeCommandConfig
    , cpeCheckpointer
    , cpeLogger
    , cpeGasEnv
    , CheckpointData(..)
    , cpPactDbEnv
    , cpCommandState
    , Checkpointer(..)
    , Env'(..)
    , EnvPersist'(..)
    , PactDbBackend
    , PactDbConfig(..)
    , pdbcGasLimit
    , pdbcGasRate
    , pdbcLogDir
    , pdbcPersistDir
    , pdbcPragmas
    , PactDbEnvPersist(..)
    , pdepEnv
    , pdepPactDb
    , PactDbState(..)
    , pdbsDbEnv
    , pdbsState
    , usage
    ) where

import Control.Lens

import Data.Aeson

import GHC.Generics

import qualified Pact.Interpreter as P
import qualified Pact.Persist.Pure as P
import qualified Pact.Persist.SQLite as P
import qualified Pact.PersistPactDb as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

-- internal modules
import Chainweb.BlockHeader

class PactDbBackend e

instance PactDbBackend P.PureDb

instance PactDbBackend P.SQLite

data Env' =
    forall a. PactDbBackend a =>
              Env' (P.PactDbEnv (P.DbEnv a))

data PactDbEnvPersist p = PactDbEnvPersist
    { _pdepPactDb :: P.PactDb (P.DbEnv p)
    , _pdepEnv     :: P.DbEnv p
    }
makeLenses ''PactDbEnvPersist

data EnvPersist' = forall a. PactDbBackend a => EnvPersist' (PactDbEnvPersist a)

data PactDbState = PactDbState
    { _pdbsDbEnv :: EnvPersist'
    , _pdbsState :: P.CommandState
    }

makeLenses ''PactDbState

data PactDbConfig = PactDbConfig
    { _pdbcPersistDir :: Maybe FilePath
    , _pdbcLogDir :: FilePath
    , _pdbcPragmas :: [P.Pragma]
    , _pdbcGasLimit :: Maybe Int
    , _pdbcGasRate :: Maybe Int
    } deriving (Eq, Show, Generic)

instance FromJSON PactDbConfig

makeLenses ''PactDbConfig

usage :: String
usage =
    "Config file is YAML format with the following properties: \n\
  \persistDir - Directory for database files. \n\
  \logDir     - Directory for HTTP logs \n\
  \pragmas    - SQLite pragmas to use with persistence DBs \n\
  \gasLimit   - Gas limit for each transaction, defaults to 0 \n\
  \gasRate    - Gas price per action, defaults to 0 \n\
  \\n"

data CheckpointData = CheckpointData
    { _cpPactDbEnv :: EnvPersist'
    , _cpCommandState :: P.CommandState
    }

makeLenses ''CheckpointData

data Checkpointer = Checkpointer
  { restore :: BlockHeight -> BlockPayloadHash -> IO CheckpointData
  , save :: BlockHeight -> BlockPayloadHash -> CheckpointData -> IO ()
  }

-- functions like the ones below need to be implemented internally
-- , prepareForValidBlock :: BlockHeight -> BlockPayloadHash -> IO (Either String CheckpointData)
-- , prepareForNewBlock :: BlockHeight -> BlockPayloadHash -> IO (Either String CheckpointData)

data CheckpointEnv = CheckpointEnv
    { _cpeCheckpointer :: Checkpointer
    , _cpeCommandConfig :: P.CommandConfig
    , _cpeLogger :: P.Logger
    , _cpeGasEnv :: P.GasEnv
    }

makeLenses ''CheckpointEnv
