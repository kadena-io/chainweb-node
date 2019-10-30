{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
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

    -- * Transaction State
  , TransactionState(..)
  , transactionGasEnv
  , transactionLogs
  , transactionCache

    -- * Transaction Execution Monad
  , TransactionM(..)
  , runTransactionM
  , execTransactionM
  , evalTransactionM

    -- * types
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
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Strict


import Data.Aeson
import Data.HashMap.Strict
import Data.Vector (Vector)

-- internal pact modules

import Pact.Parse (ParsedDecimal)
import Pact.Types.Command
import Pact.Types.Exp
import Pact.Types.Gas
import qualified Pact.Types.Hash as H
import Pact.Types.PactValue
import Pact.Types.Persistence
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
makeLenses ''PactDbStatePersist

-- | Indicates a computed gas charge (gas amount * gas price)
newtype GasSupply = GasSupply { _gasSupply :: ParsedDecimal }
   deriving (Eq,Ord,Num,Real,Fractional,ToJSON,FromJSON)
instance Show GasSupply where show (GasSupply g) = show g

newtype GasId = GasId PactId deriving (Eq, Show)

data TransactionState = TransactionState
    { _transactionGasEnv :: GasEnv
    , _transactionCache :: ModuleCache
    , _transactionLogs :: [TxLog Value]
    }

makeLenses ''TransactionState

-- | The transaction monad used in transaction execute. The reader
-- environment is the a Pact command env, writer is a list of json-ified
-- tx logs, and transaction state consists of a module cache, gas env,
-- and log values.
--
newtype TransactionM db a = TransactionM
    { _runTransactionM
        :: ReaderT (CommandEnv db) (StateT TransactionState IO) a
    } deriving
      ( Functor, Applicative, Monad
      , MonadReader (CommandEnv db)
      , MonadState TransactionState
      , MonadThrow, MonadCatch, MonadMask
      , MonadIO
      , MonadFail
      )

-- | Run a 'TransactionM' computation given some initial
-- reader and state values, returning the full range of
-- results including final computed value, state, and
-- writer values.
--
runTransactionM
    :: forall db a
    . CommandEnv db
      -- ^ initial reader env
    -> TransactionState
      -- ^ initial state
    -> TransactionM db a
      -- ^ computation to execute
    -> IO (a, TransactionState)
runTransactionM cenv txst act
    = runStateT (runReaderT (_runTransactionM act) cenv) txst

-- | Run a 'TransactionM' computation given some initial
-- reader and state values, discarding the final state.
--
evalTransactionM
    :: forall db a
    . CommandEnv db
      -- ^ initial reader env
    -> TransactionState
      -- ^ initial state
    -> TransactionM db a
    -> IO a
evalTransactionM cenv txst act
    = evalStateT (runReaderT (_runTransactionM act) cenv) txst

-- | Run a 'TransactionM' computation given some initial
-- reader and state values, discarding the final value.
--
execTransactionM
    :: forall p a
    . CommandEnv p
      -- ^ initial reader env
    -> TransactionState
      -- ^ initial state
    -> TransactionM p a
      -- ^ computation to execute
    -> IO TransactionState
execTransactionM cenv txst act
    = execStateT (runReaderT (_runTransactionM act) cenv) txst
