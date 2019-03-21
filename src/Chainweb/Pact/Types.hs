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
  , GasSupply(..)
  , PactServiceEnv(..)
    -- * types
  , PactServiceM
  , TransactionM
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
    -- * defaults
  , defaultMiner
    -- * module exports
  , module Chainweb.Pact.Backend.Types
  ) where

import Control.Lens hiding ((.=))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.Aeson as A
import Data.Decimal (Decimal)
import Data.Default (def)
import Data.Text (Text)
import Data.Vector (Vector)

-- internal pact modules

import Pact.Types.ChainMeta (PublicData(..))
import Pact.Types.Persistence (TxLog(..))
import Pact.Types.Runtime (SPVSupport(..))
import Pact.Types.Server (CommandEnv(..))
import Pact.Types.Term (KeySet(..), Name(..))
import Pact.Types.Util (Hash(..))

-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Transaction
import Chainweb.Payload (Transaction)

newtype Transactions = Transactions
    { _transactionPairs :: Vector (Transaction, FullLogTxOutput)
    } deriving (Eq)

instance ToJSON Transactions where
    toJSON o = object
        ["transactionPairs" .= _transactionPairs o]
    {-# INLINE toJSON #-}

instance FromJSON Transactions where
    parseJSON = withObject "Transactions" $ \o -> Transactions
        <$> o .: "transactionPairs"
    {-# INLINE parseJSON #-}

instance Show Transactions where
    show ts =
        let f x acc = "trans: " ++ show (fst x) ++ "\n out: " ++ show (snd x) ++ acc
        in foldr f "" (_transactionPairs ts)

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


data MinerInfo = MinerInfo
  { _minerAccount :: Text
  , _minerKeys :: KeySet
  }

-- Keyset taken from cp examples in Pact
-- The private key here was taken from `examples/cp` from the Pact repository
defaultMiner :: MinerInfo
defaultMiner = MinerInfo "miner" $ KeySet
  ["f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
  (Name "miner-keyset" def)

data PactDbStatePersist = PactDbStatePersist
    { _pdbspRestoreFile :: Maybe FilePath
    , _pdbspPactDbState :: PactDbState
    }

newtype GasSupply = GasSupply { _gasSupply :: Decimal }

data PactServiceEnv = PactServiceEnv
  { _psMempoolAccess :: MemPoolAccess
  , _psCheckpointEnv :: CheckpointEnv
  , _psSpvSupport :: SPVSupport
  , _psPublicData :: PublicData
  }

type PactServiceM = ReaderT PactServiceEnv (StateT PactDbState IO)

type TransactionM p = ReaderT (CommandEnv p) IO

type MemPoolAccess = BlockHeight -> BlockHash -> IO (Vector ChainwebTransaction)

makeLenses ''MinerInfo
makeLenses ''PactDbStatePersist
makeLenses ''FullLogTxOutput
makeLenses ''HashedLogTxOutput
makeLenses ''PactServiceEnv
