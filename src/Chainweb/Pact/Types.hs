{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Pact.Types
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact Types module for Chainweb
module Chainweb.Pact.Types
    ( PactDbStatePersist(..)
    , pdbspRestoreFile
    , pdbspPactDbState
    , PactT
    , Transaction(..)
    , Transactions(..)
    , tCmd
    , tTxId
    , TransactionCriteria(..)
    , TransactionOutput(..)
    , module Chainweb.Pact.Backend.Types
    ) where

import Control.Lens
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import qualified Data.Aeson as A
import Data.ByteString (ByteString)

import GHC.Word

import qualified Pact.Types.Command as P
import qualified Pact.Types.Persistence as P

import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

data Transaction = Transaction
    { _tTxId :: Word64
    , _tCmd :: P.Command ByteString
    }
makeLenses ''Transaction

newtype Transactions = Transactions { _transactionPair :: [(Transaction, TransactionOutput)] }

data TransactionOutput = TransactionOutput
    { _getCommandResult :: P.CommandResult
    , _getTxLogs :: [P.TxLog A.Value]
    }

data PactDbStatePersist = PactDbStatePersist
    { _pdbspRestoreFile :: Maybe FilePath
    , _pdbspPactDbState :: PactDbState
    }

makeLenses ''PactDbStatePersist

type PactT a = ReaderT CheckpointEnv (StateT PactDbState IO) a

data TransactionCriteria =
    TransactionCriteria
