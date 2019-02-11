{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.Pact.Types
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact Types module for Chainweb
module Chainweb.Pact.Types
  ( Block(..)
  , PactDbStatePersist(..)
  , PactT
  , Transaction(..)
  , TransactionCriteria(..)
  , TransactionOutput(..)
  , MinerInfo(..)
    -- * optics
  , bBlockHeight
  , bHash
  , bParentHeader
  , bTransactions
  , bMinerInfo
  , pdbspRestoreFile
  , pdbspPactDbState
  , tCmd
  , tTxId
  , minerAccount
  , minerKeys
    -- * defaults
  , defaultMiner
    -- * module exports
  , module Chainweb.Pact.Backend.Types
  ) where

import Control.Lens
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Text (Text)

import GHC.Word

-- internal pact modules

import qualified Pact.Types.Command as P
import qualified Pact.Types.Persistence as P
import Pact.Types.Term (KeySet(..), Name(..))

-- internal chainweb modules

import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types


data Transaction = Transaction
    { _tTxId :: Word64
    , _tCmd :: P.Command ByteString
    }

makeLenses ''Transaction

data TransactionOutput = TransactionOutput
    { _getCommandResult :: P.CommandResult
    , _getTxLogs :: [P.TxLog A.Value]
    }

data MinerInfo = MinerInfo
  { _minerAccount :: Text
  , _minerKeys :: KeySet
  }
makeLenses ''MinerInfo

defaultMiner :: MinerInfo
defaultMiner = MinerInfo "" $ KeySet [] (Name "" def)

data Block = Block
    { _bHash :: Maybe BlockPayloadHash
    , _bParentHeader :: BlockHeader
    , _bBlockHeight :: BlockHeight
    , _bTransactions :: [(Transaction, TransactionOutput)]
    , _bMinerInfo :: MinerInfo
    }

makeLenses ''Block

data PactDbStatePersist = PactDbStatePersist
    { _pdbspRestoreFile :: Maybe FilePath
    , _pdbspPactDbState :: PactDbState
    }

makeLenses ''PactDbStatePersist

type PactT a = ReaderT CheckpointEnv (StateT PactDbState IO) a

data TransactionCriteria =
    TransactionCriteria
