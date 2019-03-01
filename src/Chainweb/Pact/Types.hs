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
  , PactT
  , PactTransaction(..)
  , Transactions(..)
  , MemPoolAccess
  , MinerInfo(..)
    -- * optics
  , flCommandResult
  , flTxLogs
  , hlCommandResult
  , hlTxLogHash
  , minerAccount
  , minerKeys
  , pdbspRestoreFile
  , pdbspPactDbState
  , ptCmd
    -- * defaults
  , defaultMiner
    -- * module exports
  , module Chainweb.Pact.Backend.Types
  ) where

import Control.Lens hiding ((.=))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Text (Text)

import GHC.Word

-- internal pact modules

import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Persistence as P
import Pact.Types.Term (KeySet(..), Name(..))
import qualified Pact.Types.Util as P

-- internal chainweb modules

import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

-- TODO: _ptTxId needs to be removed from the below, and the real TxId needs to be
-- checkpointed & restored
data PactTransaction = PactTransaction
    { _ptTxId :: Word64
    , _ptCmd :: P.Command ByteString
    } deriving (Show, Eq)

instance ToJSON PactTransaction where
    toJSON o = object
        [ "ptTxId" .= _ptTxId o
        , "ptCmd" .= _ptCmd o ]
    {-# INLINE toJSON #-}

instance FromJSON PactTransaction where
    parseJSON = withObject "PactTransaction" $ \o -> PactTransaction
        <$> o .: "ptTxId"
        <*> o .: "ptCmd"
    {-# INLINE parseJSON #-}

newtype Transactions = Transactions
    { _transactionPairs :: [(PactTransaction, FullLogTxOutput)]
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
    , _flTxLogs :: [P.TxLog A.Value]
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
    , _hlTxLogHash :: P.Hash
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

defaultMiner :: MinerInfo
defaultMiner = MinerInfo "" $ KeySet [] (Name "" def)

data PactDbStatePersist = PactDbStatePersist
    { _pdbspRestoreFile :: Maybe FilePath
    , _pdbspPactDbState :: PactDbState
    }

type PactT a = ReaderT CheckpointEnv (StateT PactDbState IO) a

type MemPoolAccess = BlockHeight -> IO [PactTransaction]

makeLenses ''MinerInfo
makeLenses ''PactDbStatePersist
makeLenses ''PactTransaction
makeLenses ''FullLogTxOutput
makeLenses ''HashedLogTxOutput
