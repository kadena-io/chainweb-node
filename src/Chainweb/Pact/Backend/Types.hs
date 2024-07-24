{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Module: Chainweb.Pact.Backend.Types
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Chainweb / Pact Types module for various database backends
module Chainweb.Pact.Backend.Types
    () where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import Control.Exception.Safe hiding (bracket)
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Aeson
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as SB
import Data.DList (DList)
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Vector (Vector)

import Database.SQLite3.Direct as SQ3

import Foreign.C.Types (CInt(..))

import GHC.Generics
import GHC.Stack

import Pact.Interpreter (PactDbEnv(..))
import Pact.Persist.SQLite (Pragma(..))
import Pact.PersistPactDb (DbEnv(..))
import qualified Pact.Types.Hash as P
import Pact.Types.Persistence
import Pact.Types.RowData (RowData)
import Pact.Types.Runtime (TableName)

import qualified Pact.JSON.Encode as J

import qualified Pact.Core.Builtin as Pact5
import qualified Pact.Core.Persistence as Pact5
import qualified Pact.Core.Info as Pact5
import qualified Pact.Core.Evaluate as Pact5


-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Pact.Backend.DbCache
import qualified Chainweb.Pact4.Transaction as Pact4
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Guards
import Chainweb.Mempool.Mempool (MempoolPreBlockCheck,TransactionHash,BlockFill)

import Streaming(Stream, Of)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HashMap
import Pact.Types.SQLite (SType (..), RType (..), qry, exec_)
import qualified Data.ByteString as BS
import Chainweb.Utils.Serialization
import Data.Foldable
import qualified Data.ByteString.Char8 as B8
import qualified Chainweb.Pact5.Transaction as Pact5
