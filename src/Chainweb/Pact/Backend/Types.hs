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
    , cpRefStore
    , cpPacts
    , Checkpointer(..)
    , cRestore
    , cPrepareForNewBlock
    , cPrepareForValidBlock
    , cSave
    , cDiscard
    , Env'(..)
    , EnvPersist'(..)
    , OpMode(..)
    , PactDbBackend
    , PactDbConfig(..)
    , pdbcGasLimit
    , pdbcGasRate
    , pdbcLogDir
    , pdbcPersistDir
    , pdbcPragmas
    , PactDbEnvPersist(..)
    , pdepPactDb
    , pdepDb
    , pdepPersist
    , pdepLogger
    , pdepTxRecord
    , pdepTxId
    , PactDbState(..)
    , pdbsCommandConfig
    , pdbsDbEnv
    , pdbsState
    , usage
    ) where

{-

data PactDbEnvPersist p = PactDbEnvPersist
    { _pdepPactDb :: P.PactDb p
    , _pdepDb     :: p
    , _pdepPersist :: P.Persister p
    , _pdepLogger :: P.Logger
    , _pdepTxRecord :: M.Map P.TxTable [P.TxLog Value]
    , _pdepTxId :: Maybe P.TxId
    }
makeLenses ''PactDbEnvPersist

data EnvPersist' = forall a. PactDbBackend a => EnvPersist' (PactDbEnvPersist a)
-}
import Control.Lens

import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import GHC.Generics

import qualified Pact.Interpreter as P
import qualified Pact.Persist as P
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
    { _pdepPactDb :: P.PactDb p
    , _pdepDb     :: p
    , _pdepPersist :: P.Persister p
    , _pdepLogger :: P.Logger
    , _pdepTxRecord :: M.Map P.TxTable [P.TxLog Value]
    , _pdepTxId :: Maybe P.TxId
    }
makeLenses ''PactDbEnvPersist

data EnvPersist' = forall a. PactDbBackend a => EnvPersist' (PactDbEnvPersist a)

data PactDbState = PactDbState
    { _pdbsCommandConfig :: P.CommandConfig
    , _pdbsDbEnv :: Env'
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

data OpMode
    = NewBlock
    | Validation

data CheckpointData = CheckpointData
    { _cpPactDbEnv :: Env'
    , _cpRefStore :: P.RefStore
    , _cpPacts :: Map P.TxId P.CommandPact
    }

makeLenses ''CheckpointData

data Checkpointer = Checkpointer
    { _cRestore :: BlockHeight -> BlockPayloadHash -> IO ()
    , _cPrepareForValidBlock :: BlockHeight -> BlockPayloadHash -> IO (Either String CheckpointData)
    , _cPrepareForNewBlock :: BlockHeight -> BlockPayloadHash -> IO (Either String CheckpointData)
    , _cSave :: BlockHeight -> BlockPayloadHash -> CheckpointData -> IO ()
    , _cDiscard :: BlockHeight -> BlockPayloadHash -> CheckpointData -> IO ()
    }

makeLenses ''Checkpointer

data CheckpointEnv = CheckpointEnv
    { _cpeCheckpointer :: Checkpointer
    , _cpeCommandConfig :: P.CommandConfig
    , _cpeLogger :: P.Logger
    , _cpeGasEnv :: P.GasEnv
    }

makeLenses ''CheckpointEnv
