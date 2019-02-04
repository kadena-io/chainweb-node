{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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
    , cpeCommandConfig
    , cpeCheckpointer
    , cpeLogger
    , cpeGasEnv
    , Checkpointer(..)
    , Env'(..)
    , EnvPersist'(..)
    , PactDbBackend(..)
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
    , SaveData(..)
    , sTxRecord
    , sTxId
    , sSQLiteConfig
    , sCommandState
    , usage
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import qualified Control.Monad.State as State

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Serialize

import GHC.Generics

import qualified Pact.Interpreter as P
import qualified Pact.Persist as P
import qualified Pact.Persist.Pure as P
import qualified Pact.Persist.SQLite as P
import qualified Pact.PersistPactDb as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Persistence as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

-- internal modules
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Orphans

class PactDbBackend e where
    openDb :: e -> IO ()
    closeDb :: e -> IO ()
    saveDb :: PactDbEnvPersist e -> P.CommandState -> IO (Maybe FilePath)

data PactDbBackEndException =
    PactDbBackEndException String
    deriving (Show)

instance Exception PactDbBackEndException

instance PactDbBackend P.PureDb where
    openDb = const $ return ()
    closeDb = const $ return ()
    saveDb _ _ = return Nothing

-- withDbEnvPersist :: (a -> b) -> PactDbEnvPersist a -> b
-- withDbEnvPersist f (PactDbEnvPersist {..}) =
--     case _pdepEnv of
--         P.DbEnv {..} -> f _db

instance PactDbBackend P.SQLite where
    openDb = void . liftM2 P.initSQLite P.config (P.constLoggers . P.logger)
    closeDb = either (throwM . PactDbBackEndException) return <=< P.closeSQLite
    saveDb = saveSQLite

saveSQLite :: PactDbEnvPersist P.SQLite -> P.CommandState -> IO (Maybe FilePath)
saveSQLite (PactDbEnvPersist {..}) commandstate =
    case _pdepEnv of
        p@(P.DbEnv {..}) -> do
            let config = P.config (P._db p)
                savedata = encode $ SaveData _txRecord _txId config commandstate
            preparedFileName <- prepare _db savedata
            return (Just $ fromTempFileName preparedFileName)
  where
    prepare = undefined
    fromTempFileName = undefined

data SaveData = SaveData
    { _sTxRecord :: M.Map P.TxTable [P.TxLog A.Value]
    , _sTxId :: Maybe P.TxId
    , _sSQLiteConfig :: P.SQLiteConfig
    , _sCommandState :: P.CommandState
    } deriving (Generic)

instance Serialize SaveData where
    put (SaveData {..}) = do
        put _sTxRecord
        put _sTxId
        put _sSQLiteConfig
        put _sCommandState
    get = do
        _sTxRecord <- get
        _sTxId <- get
        _sSQLiteConfig <- get
        _sCommandState <- get
        return $ SaveData {..}

data Env' =
    forall a. PactDbBackend a =>
              Env' (P.PactDbEnv (P.DbEnv a))

data PactDbEnvPersist p = PactDbEnvPersist
    { _pdepPactDb :: P.PactDb (P.DbEnv p)
    , _pdepEnv :: P.DbEnv p
    }

makeLenses ''PactDbEnvPersist

makeLenses ''SaveData

data EnvPersist' =
    forall a. PactDbBackend a =>
              EnvPersist' (PactDbEnvPersist a)

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

instance A.FromJSON PactDbConfig

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

data Checkpointer = Checkpointer
    { restore :: BlockHeight -> BlockPayloadHash -> IO PactDbState
    , save :: BlockHeight -> BlockPayloadHash -> PactDbState -> IO ()
    }

-- functions like the ones below need to be implemented internally
-- , prepareForValidBlock :: BlockHeight -> BlockPayloadHash -> IO (Either String PactDbState)
-- , prepareForNewBlock :: BlockHeight -> BlockPayloadHash -> IO (Either String PactDbState)
data CheckpointEnv = CheckpointEnv
    { _cpeCheckpointer :: Checkpointer
    , _cpeCommandConfig :: P.CommandConfig
    , _cpeLogger :: P.Logger
    , _cpeGasEnv :: P.GasEnv
    }

makeLenses ''CheckpointEnv
