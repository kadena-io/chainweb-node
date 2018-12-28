-- |
-- Module: Chainweb.Pact.Exec
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact Types module for Chainweb
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Chainweb.Pact.Types
  ( bBlockHeight
  , bHash
  , bParentHash
  , bTransactions
  , Block(..)
  , CheckpointEnv(..)
  , cpeCheckpointStore
  , cpeCommandConfig
  , HashTablePurePactCheckpointStore
  , MapPurePactCheckpointStore
  , OnDiskPactCheckpointStore(..)
  , PactDbBackend
  , PactDbConfig(..)
  , PactDbState(..)
  , PactDbState'(..)
  , PactDbStatePersist(..)
  , PactT
  , pdbcGasLimit
  , pdbcGasRate
  , pdbcLogDir
  , pdbcPersistDir
  , pdbcPragmas
  , pdbsCommandConfig
  , pdbsDbEnv
  , pdbsGasEnv
  , pdbsLogger
  , pdbsPactDbState
  , pdbsRestoreFile
  , pdbsState
  , tCmd
  , tTxId
  , Transaction(..)
  , TransactionCriteria(..)
  , usage
  ) where

import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P
import qualified Pact.Persist.SQLite as P
import qualified Pact.PersistPactDb as P
import qualified Pact.Persist.Pure as P

import Control.Lens
import Control.Monad.Trans.RWS.Lazy
import Data.Aeson
import Data.ByteString (ByteString)
import Data.IORef
import qualified Data.HashTable.IO as H
import Data.Map.Strict (Map)
import GHC.Generics
import GHC.Word (Word64)


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

data Transaction = Transaction
  { _tTxId :: Word64
  , _tCmd :: P.Command ByteString
  }
makeLenses ''Transaction

data Block = Block
  { _bHash :: Maybe P.Hash
  , _bParentHash :: P.Hash
  , _bBlockHeight :: Integer
  , _bTransactions :: [(Transaction, P.CommandResult)]
  }
makeLenses ''Block

data PactDbStatePersist = PactDbStatePersist
  { _pdbsRestoreFile :: Maybe FilePath
  , _pdbsPactDbState :: PactDbState'
  }

makeLenses ''PactDbStatePersist

data PactDbConfig = PactDbConfig {
  _pdbcPersistDir :: Maybe FilePath,
  _pdbcLogDir :: FilePath,
  _pdbcPragmas :: [P.Pragma],
  _pdbcGasLimit :: Maybe Int,
  _pdbcGasRate :: Maybe Int
  } deriving (Eq,Show,Generic)
instance FromJSON PactDbConfig

makeLenses ''PactDbConfig

type MapPurePactCheckpointStore = IORef (Map Integer (P.Hash, PactDbStatePersist))

data CheckpointEnv = CheckpointEnv
  { _cpeCheckpointStore :: MapPurePactCheckpointStore
  , _cpeCommandConfig :: P.CommandConfig
  }

makeLenses ''CheckpointEnv

usage :: String
usage =
  "Config file is YAML format with the following properties: \n\
  \persistDir - Directory for database files. \n\
  \logDir     - Directory for HTTP logs \n\
  \pragmas    - SQLite pragmas to use with persistence DBs \n\
  \gasLimit   - Gas limit for each transaction, defaults to 0 \n\
  \gasRate    - Gas price per action, defaults to 0 \n\
  \\n"

type PactT a = RWST CheckpointEnv () PactDbState' IO a

data TransactionCriteria = TransactionCriteria

type HashTable k v = H.LinearHashTable k v

type HashTablePurePactCheckpointStore = HashTable Integer (P.Hash, PactDbStatePersist)

data OnDiskPactCheckpointStore = OnDiskPactCheckpointStore

-- type OnDiskPactCheckpointStore = IORef (Map Integer (P.Hash, PactDbStatePersist))
-- type MapOnDiskPactCheckPointStore = IORef (Map Integer (P.Hash, FilePath))
-- type HashTableOnDiskPactCheckPointStore = HashTable Integer (P.Hash, FilePath)
