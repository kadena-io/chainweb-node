-- |
-- Module: Chainweb.Pact.Backend.Types
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Chainweb / Pact Types module for various database backends

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Chainweb.Pact.Backend.Types
  ( PactDbConfig(..) , pdbcGasLimit , pdbcGasRate , pdbcLogDir , pdbcPersistDir , pdbcPragmas
  , PactDbState(..) , pdbsCommandConfig , pdbsDbEnv, pdbsState
  , usage
  , CheckpointEnv(..), cpeCheckpointStore , cpeCommandConfig, cpeCheckpointer, cpeLogger, cpeGasEnv
  , CheckpointData(..), cpPactDbEnv, cpRefStore, cpPacts
  , Checkpointer(..), cRestore, cPrepare, cSave
  , Checkpointer'(..)
  , Env'(..)
  , OpMode(..)
  , PactDbBackend
  , Store
  ) where

import qualified Chainweb.BlockHeader as C
import qualified Pact.Types.Runtime as P
import qualified Pact.Interpreter as P
import qualified Pact.Persist.Pure as P
import qualified Pact.Persist.SQLite as P
import qualified Pact.PersistPactDb as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Server as P

import Control.Lens
import Data.Aeson
import GHC.Generics
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict as HMS -- as per Greg's suggestion
import Data.HashMap.Strict (HashMap)
import Data.IORef

class PactDbBackend e where

instance PactDbBackend P.PureDb where
instance PactDbBackend P.SQLite where

type StoreKey = (C.BlockHeight, P.Hash)

data Env' = forall a. PactDbBackend a => Env' (P.PactDbEnv (P.DbEnv a))


data PactDbState = PactDbState
  { _pdbsCommandConfig :: P.CommandConfig
  , _pdbsDbEnv :: Env'
  , _pdbsState :: P.CommandState
  }
makeLenses ''PactDbState

data PactDbConfig = PactDbConfig {
  _pdbcPersistDir :: Maybe FilePath,
  _pdbcLogDir :: FilePath,
  _pdbcPragmas :: [P.Pragma],
  _pdbcGasLimit :: Maybe Int,
  _pdbcGasRate :: Maybe Int
  } deriving (Eq,Show,Generic)
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

data Checkpointer c = Checkpointer
  { _cRestore :: C.BlockHeight -> P.Hash -> CheckpointData -> IORef c -> IO ()
  , _cPrepare :: C.BlockHeight -> P.Hash -> OpMode -> CheckpointData -> IORef c -> IO (Either String c)
  , _cSave :: C.BlockHeight -> P.Hash -> OpMode -> CheckpointData -> IORef c -> IO ()
  }
-- _cGetPactDbState :: Height -> P.Hash -> c -> IO PactDbState' -- MAYBE ADD THIS

makeLenses ''Checkpointer

data CheckpointEnv c = CheckpointEnv
  { _cpeCheckpointer    :: Checkpointer c
  , _cpeCommandConfig   :: P.CommandConfig
  , _cpeCheckpointStore :: IORef c
  , _cpeLogger :: P.Logger
  , _cpeGasEnv :: P.GasEnv
  }

makeLenses ''CheckpointEnv

class CheckpointService c where

instance CheckpointService (HashMap StoreKey CheckpointData) where
instance CheckpointService ([FilePath]) where

data Checkpointer' = forall c. CheckpointService c => Checkpointer' (Checkpointer c)

type Store = HashMap StoreKey CheckpointData
