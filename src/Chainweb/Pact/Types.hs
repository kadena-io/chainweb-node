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
  ( FullLogTxOutput(..)
  , HashedLogTxOutput(..)
  , PactDbStatePersist(..)
  , Transactions(..)
  , MemPoolAccess
  , MinerInfo(..)
  , toMinerData, fromMinerData
  , toCoinbaseOutput, fromCoinbaseOutput
  , GasSupply(..)
  , PactServiceEnv(..)
  , PactServiceState(..)
    -- * types
  , PactServiceM
    -- * optics
  , flCommandResult
  , flTxLogs
  , hlCommandResult
  , hlTxLogHash
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
  , noMiner
  , noCoinbase
    -- * module exports
  , module Chainweb.Pact.Backend.Types
  , toHashedLogTxOutput
  ) where

import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Aeson as A
import Data.Decimal (Decimal)
import Data.Default (def)
import Data.Text (Text)
import Data.Vector (Vector)

-- internal pact modules

import Pact.Types.ChainMeta (PublicData(..))
import Pact.Types.Command (CommandSuccess(..))
import Pact.Types.Hash (Hash(..))
import Pact.Types.Persistence (TxLog(..))
import Pact.Types.Runtime (SPVSupport(..))
import Pact.Types.Term (KeySet(..), Name(..), Term, tStr)

-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils

data Transactions = Transactions
    { _transactionPairs :: Vector (Transaction, FullLogTxOutput)
    , _transactionCoinbase :: FullLogTxOutput
    } deriving (Eq,Show)


data FullLogTxOutput = FullLogTxOutput
    { _flCommandResult :: A.Value
    , _flTxLogs :: [TxLog A.Value]
    } deriving (Show, Eq)

instance FromJSON FullLogTxOutput where
    parseJSON = withObject "TransactionOutput" $ \o -> FullLogTxOutput
        <$> o .: "flCommandResult"
        <*> o .: "flTxLogs"
    {-# INLINE parseJSON #-}

instance ToJSON FullLogTxOutput where
    toJSON o = object
        [ "flCommandResult" .= _flCommandResult o
        , "flTxLogs" .= _flTxLogs o]
    {-# INLINE toJSON #-}

data HashedLogTxOutput = HashedLogTxOutput
    { _hlCommandResult :: Value
    , _hlTxLogHash :: Hash
    } deriving (Eq, Show)

instance FromJSON HashedLogTxOutput where
    parseJSON = withObject "TransactionOutput" $ \o -> HashedLogTxOutput
        <$> o .: "hlCommandResult"
        <*> o .: "hlTxLogs"
    {-# INLINE parseJSON #-}

instance ToJSON HashedLogTxOutput where
    toJSON o = object
        [ "hlCommandResult" .= _hlCommandResult o
        , "hlTxLogs" .= _hlTxLogHash o]
    {-# INLINE toJSON #-}

toHashedLogTxOutput :: FullLogTxOutput -> HashedLogTxOutput
toHashedLogTxOutput FullLogTxOutput{..} =
    -- let hashed = hash $ encodeToByteString _flTxLogs
    let hashed = undefined
    in HashedLogTxOutput
        { _hlCommandResult = _flCommandResult
        , _hlTxLogHash = hashed
        }

type MinerKeys = KeySet
type MinerId = Text

data MinerInfo = MinerInfo
  { _minerAccount :: MinerId
  , _minerKeys :: MinerKeys
  } deriving (Show,Eq)


instance ToJSON MinerInfo where
  toJSON MinerInfo{..} =
    object [ "m" .= _minerAccount
           , "ks" .= _ksKeys _minerKeys
           , "kp" .= _ksPredFun _minerKeys ]
instance FromJSON MinerInfo where
  parseJSON = withObject "MinerInfo" $ \o ->
    MinerInfo <$> o .: "m" <*> (KeySet <$> o .: "ks" <*> o .: "kp")


noMiner :: MinerInfo
noMiner = MinerInfo "NoMiner" (KeySet [] (Name "<" def))

noCoinbase :: FullLogTxOutput
noCoinbase = FullLogTxOutput (toJSON $ CommandSuccess (tStr "NO_COINBASE" :: Term Name)) []

toMinerData :: MinerInfo -> MinerData
toMinerData = MinerData . encodeToByteString

fromMinerData :: MonadThrow m => MinerData -> m MinerInfo
fromMinerData = decodeStrictOrThrow . _minerData

toCoinbaseOutput :: FullLogTxOutput -> CoinbaseOutput
toCoinbaseOutput = CoinbaseOutput . encodeToByteString . toHashedLogTxOutput

fromCoinbaseOutput :: MonadThrow m => CoinbaseOutput -> m  HashedLogTxOutput
fromCoinbaseOutput = decodeStrictOrThrow . _coinbaseOutput


-- Keyset taken from cp examples in Pact
-- The private key here was taken from `examples/cp` from the Pact repository
defaultMiner :: MinerInfo
defaultMiner = MinerInfo "miner" $ KeySet
  ["f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
  (Name "keys-all" def)

data PactDbStatePersist = PactDbStatePersist
    { _pdbspRestoreFile :: Maybe FilePath
    , _pdbspPactDbState :: PactDbState
    }

newtype GasSupply = GasSupply { _gasSupply :: Decimal }

data PactServiceEnv = PactServiceEnv
  { _psMempoolAccess :: Maybe MemPoolAccess
  , _psCheckpointEnv :: CheckpointEnv
  , _psSpvSupport :: SPVSupport
  , _psPublicData :: PublicData
  }

data PactServiceState = PactServiceState
  { _psStateDb :: PactDbState
  , _psStateValidated :: Maybe BlockHeader
  }

type PactServiceM = ReaderT PactServiceEnv (StateT PactServiceState IO)

type MemPoolAccess = BlockHeight -> BlockHash -> IO (Vector ChainwebTransaction)

makeLenses ''MinerInfo
makeLenses ''PactDbStatePersist
makeLenses ''FullLogTxOutput
makeLenses ''HashedLogTxOutput
makeLenses ''PactServiceEnv
makeLenses ''PactServiceState
