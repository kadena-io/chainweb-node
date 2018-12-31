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

module Chainweb.Pact.Backend.Types
  ( PactDbConfig(..) , pdbcGasLimit , pdbcGasRate , pdbcLogDir , pdbcPersistDir , pdbcPragmas
  , PactDbState(..), pdbsDbEnv ,pdbsState
  , PactDbState'(..)
  , usage
  ) where

import qualified Pact.Interpreter as P
import qualified Pact.Persist.Pure as P
import qualified Pact.Persist.SQLite as P
import qualified Pact.PersistPactDb as P
import qualified Pact.Types.Server as P

import Control.Lens
import Data.Aeson
import GHC.Generics

class PactDbBackend e where

instance PactDbBackend P.PureDb where
instance PactDbBackend P.SQLite where

data PactDbState e = PactDbState
  { _pdbsDbEnv :: P.PactDbEnv (P.DbEnv e)
  , _pdbsState :: P.CommandState
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
