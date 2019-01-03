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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Chainweb.Pact.Backend.Types
  ( PactDbConfig(..) , pdbcGasLimit , pdbcGasRate , pdbcLogDir , pdbcPersistDir , pdbcPragmas
  , PactDbState(..) , pdbsCommandConfig , pdbsDbEnv , pdbsGasEnv , pdbsLogger, pdbsState
  , PactDbState'(..)
  , usage
  , CheckpointEnv(..), cpeCheckpointStore , cpeCommandConfig, cpeCheckpointer
  , CheckpointData(..), cpPactDbEnv, cpRefStore, cpPacts
  , Checkpointer(..), cRestore, cPrepare, cSave
  , OpMode(..)
  , PactDbBackend
  , initPactCheckpointStore
  , initPactCheckpointer
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

class PactDbBackend e where

instance PactDbBackend P.PureDb where
instance PactDbBackend P.SQLite where

data PactDbState e = PactDbState
  { _pdbsCommandConfig :: P.CommandConfig
  , _pdbsDbEnv :: P.PactDbEnv (P.DbEnv e)
  , _pdbsState :: P.CommandState
  , _pdbsLogger :: P.Logger
  , _pdbsGasEnv :: P.GasEnv
  }
makeLenses ''PactDbState

data PactDbState' = forall a. PactDbBackend a => PactDbState' (PactDbState a)

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

data CheckpointData p = CheckpointData
  { _cpPactDbEnv :: P.PactDbEnv (P.DbEnv p)
  , _cpRefStore :: P.RefStore
  , _cpPacts :: Map P.TxId P.CommandPact
  }

makeLenses ''CheckpointData

data Checkpointer p c = Checkpointer
  { _cRestore :: C.BlockHeight -> P.Hash -> CheckpointData p -> c -> IO ()
  , _cPrepare :: C.BlockHeight -> P.Hash -> OpMode -> CheckpointData p -> c -> IO (Either String c)
  , _cSave :: C.BlockHeight -> P.Hash -> OpMode -> CheckpointData p -> c -> IO ()
  }
-- _cGetPactDbState :: Height -> P.Hash -> c -> IO PactDbState' -- MAYBE ADD THIS


makeLenses ''Checkpointer

data CheckpointEnv p c = CheckpointEnv
  { _cpeCheckpointer :: Checkpointer p c
  , _cpeCommandConfig :: P.CommandConfig
  , _cpeCheckpointStore :: c
  }

makeLenses ''CheckpointEnv

initPactCheckpointer :: IO (Checkpointer p c)
initPactCheckpointer = undefined

initPactCheckpointStore :: IO c
initPactCheckpointStore = undefined
