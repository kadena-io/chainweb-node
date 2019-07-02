{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Pact.InternalTypes
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact Types module for Chainweb
module Chainweb.Pact.InternalTypes
  ( MemPoolAccess(..)
  , PactServiceEnv(..)
  , PactServiceState(..)
    -- * types
  , PactServiceM
    -- * optics
  , noopMemPoolAccess
  , psMempoolAccess
  , psCheckpointEnv
  , psSpvSupport
  , psPublicData
  , psStateValidated
  , psPdb
  , psChainId
  , psBlockHeaderDb
    -- * module exports
  , module Chainweb.Pact.Backend.Types
  ) where

import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Vector (Vector)
import qualified Data.Vector as V

-- internal pact modules

import Pact.Types.ChainMeta (PublicData(..))
import Pact.Types.SPV

-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Types
import Chainweb.ChainId
import Chainweb.Pact.Backend.Types
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Transaction

data PactServiceEnv cas = PactServiceEnv
  { _psChainId :: ChainId
  , _psMempoolAccess :: !(Maybe MemPoolAccess)
  , _psCheckpointEnv :: !CheckpointEnv
  , _psSpvSupport :: !SPVSupport
  , _psPublicData :: !PublicData
  , _psPdb :: PayloadDb cas
  , _psBlockHeaderDb :: BlockHeaderDb
  }

data PactServiceState = PactServiceState
  {_psStateValidated :: Maybe BlockHeader
  }

type PactServiceM cas = ReaderT (PactServiceEnv cas) (StateT PactServiceState IO)

data MemPoolAccess = MemPoolAccess
  { mpaGetBlock :: BlockHeight -> BlockHash -> BlockHeader -> IO (Vector ChainwebTransaction)
  , mpaSetLastHeader :: BlockHeader -> IO ()
  , mpaProcessFork :: BlockHeader -> IO ()
  }

noopMemPoolAccess :: MemPoolAccess
noopMemPoolAccess = MemPoolAccess
    { mpaGetBlock = \_ _ _ -> return V.empty
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }

makeLenses ''PactServiceEnv
makeLenses ''PactServiceState
