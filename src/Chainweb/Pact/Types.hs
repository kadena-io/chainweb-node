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
module Chainweb.Pact.Types
  ( PactDbStatePersist(..)
  , Transactions(..)
  , MemPoolAccess(..)
  , MinerInfo(..)
  , toMinerData, fromMinerData
  , toCoinbaseOutput, fromCoinbaseOutput
  , GasSupply(..)
  , GasId(..)
  , PactServiceEnv(..)
  , PactServiceState(..)
    -- * types
  , PactServiceM
  , ModuleCache
  , HashCommandResult
    -- * optics
  , minerAccount
  , minerKeys
  , pdbspRestoreFile
  , pdbspPactDbState
  , psMempoolAccess
  , psCheckpointEnv
  , psSpvSupport
  , psPublicData
  , psStateDb
  , psStateValidated
    -- * defaults
  , defaultMiner
  , emptyPayload
  , noMiner
  , noCoinbase
    -- * module exports
  , module Chainweb.Pact.Backend.Types
  ) where

import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Aeson
import Data.Default (def)
import Data.HashMap.Strict
import Data.Text (Text)
import Data.Vector (Vector)

-- internal pact modules

import Pact.Parse (ParsedDecimal)
import Pact.Types.ChainMeta (PublicData(..))
import Pact.Types.Command
import Pact.Types.Exp
import qualified Pact.Types.Hash as H
import Pact.Types.PactValue
import Pact.Types.Runtime (ModuleData)
import Pact.Types.SPV
import Pact.Types.Term (KeySet(..), Name(..), PactId(..), Ref, ModuleName)

-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils


type HashCommandResult = CommandResult H.Hash

type ModuleCache = HashMap ModuleName (ModuleData Ref, Bool)

data Transactions = Transactions
    { _transactionPairs :: !(Vector (Transaction, HashCommandResult))
    , _transactionCoinbase :: !HashCommandResult
    } deriving (Eq, Show)

type MinerKeys = KeySet
type MinerId = Text

data MinerInfo = MinerInfo
  { _minerAccount :: !MinerId
  , _minerKeys :: !MinerKeys
  } deriving (Show,Eq)

instance ToJSON MinerInfo where
  toJSON MinerInfo{..} =
    object [ "m" .= _minerAccount
           , "ks" .= _ksKeys _minerKeys
           , "kp" .= _ksPredFun _minerKeys ]
instance FromJSON MinerInfo where
  parseJSON = withObject "MinerInfo" $ \o ->
    MinerInfo <$> o .: "m" <*> (KeySet <$> o .: "ks" <*> o .: "kp")

emptyPayload :: PayloadWithOutputs
emptyPayload = PayloadWithOutputs mempty miner coinbase h i o
  where
    (BlockPayload h i o) = newBlockPayload miner coinbase mempty
    miner = MinerData $ encodeToByteString noMiner
    coinbase = toCoinbaseOutput noCoinbase

noMiner :: MinerInfo
noMiner = MinerInfo "NoMiner" (KeySet [] (Name "<" def))

noCoinbase :: CommandResult a
noCoinbase = CommandResult (RequestKey H.pactInitialHash) Nothing
             (PactResult (Right (PLiteral (LString "NO_COINBASE"))))
             0 Nothing Nothing Nothing

toMinerData :: MinerInfo -> MinerData
toMinerData = MinerData . encodeToByteString

fromMinerData :: MonadThrow m => MinerData -> m MinerInfo
fromMinerData = decodeStrictOrThrow' . _minerData

toCoinbaseOutput :: HashCommandResult -> CoinbaseOutput
toCoinbaseOutput = CoinbaseOutput . encodeToByteString

fromCoinbaseOutput :: MonadThrow m => CoinbaseOutput -> m HashCommandResult
fromCoinbaseOutput = decodeStrictOrThrow' . _coinbaseOutput

-- Keyset taken from cp examples in Pact
-- The private key here was taken from `examples/cp` from the Pact repository
defaultMiner :: MinerInfo
defaultMiner = MinerInfo "miner" $ KeySet
  ["f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
  (Name "keys-all" def)

data PactDbStatePersist = PactDbStatePersist
    { _pdbspRestoreFile :: !(Maybe FilePath)
    , _pdbspPactDbState :: !PactDbState
    }

-- | Indicates a computed gas charge (gas amount * gas price)
newtype GasSupply = GasSupply { _gasSupply :: ParsedDecimal }
   deriving (Eq,Show,Ord,Num,Real,Fractional,ToJSON,FromJSON)

data PactServiceEnv = PactServiceEnv
  { _psMempoolAccess :: !(Maybe MemPoolAccess)
  , _psCheckpointEnv :: !CheckpointEnv
  , _psSpvSupport :: !SPVSupport
  , _psPublicData :: !PublicData
  }

data PactServiceState = PactServiceState
  { _psStateDb :: !PactDbState
  , _psStateValidated :: !(Maybe BlockHeader)
  }

type PactServiceM = ReaderT PactServiceEnv (StateT PactServiceState IO)

data MemPoolAccess = MemPoolAccess
  { mpaGetBlock :: BlockHeight -> BlockHash -> BlockHeader -> IO (Vector ChainwebTransaction)
  , mpaSetLastHeader :: BlockHeader -> IO ()
  , mpaProcessFork :: BlockHeader -> IO ()
  }

instance Semigroup MemPoolAccess where
  MemPoolAccess f g h <> MemPoolAccess t u v = MemPoolAccess (f <> t) (g <> u) (h <> v)

instance Monoid MemPoolAccess where
  mempty = MemPoolAccess (\_ _ _ -> mempty) (const mempty) (const mempty)

newtype GasId = GasId PactId deriving (Eq, Show)

makeLenses ''MinerInfo
makeLenses ''PactDbStatePersist
makeLenses ''PactServiceEnv
makeLenses ''PactServiceState
