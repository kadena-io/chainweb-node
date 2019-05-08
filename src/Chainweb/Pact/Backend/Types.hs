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
    , PactDbEnvPersist(..)
    , pdepEnv
    , pdepPactDb
    , PactDbState(..)
    , pdbsDbEnv
    ) where

import Control.Lens

import qualified Data.Aeson as A

import GHC.Generics

import qualified Pact.Interpreter as P
import qualified Pact.Persist.SQLite as P
import qualified Pact.PersistPactDb as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Persistence as P
import qualified Pact.Types.Runtime as P

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader


data Env' = forall a. Env' (P.PactDbEnv (P.DbEnv a))

data PactDbEnvPersist p = PactDbEnvPersist
    { _pdepPactDb :: P.PactDb (P.DbEnv p)
    , _pdepEnv :: P.DbEnv p
    }

makeLenses ''PactDbEnvPersist


data EnvPersist' = forall a. EnvPersist' (PactDbEnvPersist a)

data PactDbState = PactDbState
    { _pdbsDbEnv :: EnvPersist' }

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

data Checkpointer = Checkpointer
    { restore :: BlockHeight -> BlockHash -> IO (Either String PactDbState)
    , restoreInitial ::IO (Either String PactDbState)
    , save :: BlockHeight -> BlockHash -> PactDbState -> IO (Either String ())
    , saveInitial :: PactDbState -> IO (Either String ())
    , discard :: PactDbState -> IO (Either String ())
    }

-- functions like the ones below need to be implemented internally
-- , prepareForValidBlock :: BlockHeight -> BlockHash -> IO (Either String PactDbState)
-- , prepareForNewBlock :: BlockHeight -> BlockHash -> IO (Either String PactDbState)
data CheckpointEnv = CheckpointEnv
    { _cpeCheckpointer :: Checkpointer
    , _cpeLogger :: P.Logger
    , _cpeGasEnv :: P.GasEnv
    }

makeLenses ''CheckpointEnv
