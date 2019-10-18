{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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
--
module Chainweb.Pact.Types
  ( PactDbStatePersist(..)
  , Transactions(..)
  , toCoinbaseOutput, fromCoinbaseOutput
  , GasSupply(..)
  , GasId(..)
  , PactServiceEnv(..)
  , PactServiceState(..)
    -- * types
  , TransactionM
  , ModuleCache
  , HashCommandResult
    -- * optics
  , pdbspRestoreFile
  , pdbspPactDbState
    -- * defaults
  , emptyPayload
  , noCoinbase
    -- * module exports
  , module Chainweb.Pact.Backend.Types
  ) where

import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Reader

import Data.Aeson
import Data.HashMap.Strict
import Data.Vector (Vector)

-- internal pact modules

import Pact.Parse (ParsedDecimal)
import Pact.Types.Command
import Pact.Types.Exp
import qualified Pact.Types.Hash as H
import Pact.Types.PactValue
import Pact.Types.Runtime (ModuleData)
import Pact.Types.Server (CommandEnv)
import Pact.Types.Term (ModuleName, PactId(..), Ref)

-- internal chainweb modules

import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Payload
import Chainweb.Utils


type HashCommandResult = CommandResult H.Hash

type ModuleCache = HashMap ModuleName (ModuleData Ref, Bool)

data Transactions = Transactions
    { _transactionPairs :: !(Vector (Transaction, HashCommandResult))
    , _transactionCoinbase :: !HashCommandResult
    } deriving (Eq, Show)


emptyPayload :: PayloadWithOutputs
emptyPayload = PayloadWithOutputs mempty miner coinbase h i o
  where
    (BlockPayload h i o) = newBlockPayload miner coinbase mempty
    miner = MinerData $ encodeToByteString noMiner
    coinbase = toCoinbaseOutput noCoinbase

noCoinbase :: CommandResult a
noCoinbase = CommandResult (RequestKey H.pactInitialHash) Nothing
             (PactResult (Right (PLiteral (LString "NO_COINBASE"))))
             0 Nothing Nothing Nothing

toCoinbaseOutput :: HashCommandResult -> CoinbaseOutput
toCoinbaseOutput = CoinbaseOutput . encodeToByteString

fromCoinbaseOutput :: MonadThrow m => CoinbaseOutput -> m HashCommandResult
fromCoinbaseOutput = decodeStrictOrThrow' . _coinbaseOutput

data PactDbStatePersist = PactDbStatePersist
    { _pdbspRestoreFile :: !(Maybe FilePath)
    , _pdbspPactDbState :: !PactDbState
    }

-- | Indicates a computed gas charge (gas amount * gas price)
newtype GasSupply = GasSupply { _gasSupply :: ParsedDecimal }
   deriving (Eq,Ord,Num,Real,Fractional,ToJSON,FromJSON)
instance Show GasSupply where show (GasSupply g) = show g

newtype GasId = GasId PactId deriving (Eq, Show)

type TransactionM p a = ReaderT (CommandEnv p) IO a

makeLenses ''PactDbStatePersist
