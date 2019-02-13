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
  , PactT
  , Transaction(..)
  , Transactions(..)
  , TransactionOutput(..)
  , MemPoolAccess
  , MinerInfo(..)
    -- * optics
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
import qualified Pact.Types.Persistence as P
import Pact.Types.Term (KeySet(..), Name(..))

-- internal chainweb modules

import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types


data Transaction = Transaction
    { _tTxId :: Word64
    , _tCmd :: P.Command ByteString
    } deriving (Show, Eq)
makeLenses ''Transaction

instance ToJSON Transaction where
    toJSON o = object
        [ "tTxId" .= _tTxId o
        , "tCmd" .= _tCmd o ]
    {-# INLINE toJSON #-}

instance FromJSON Transaction where
    parseJSON = withObject "Transaction" $ \o -> Transaction
        <$> o .: "tTxId"
        <*> o .: "tCmd"
    {-# INLINE parseJSON #-}

newtype Transactions = Transactions { _transactionPairs :: [(Transaction, TransactionOutput)] }

instance Eq Transactions where
    (==) a b =
      let tpa = _transactionPairs a
          tpb = _transactionPairs b
      in (fst <$> tpa) == (fst <$> tpb) && (snd <$> tpa) == (snd <$> tpb)

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

data TransactionOutput = TransactionOutput
    { _getCommandResult :: A.Value
    , _getTxLogs :: [P.TxLog A.Value]
    } deriving (Show, Eq)

instance ToJSON TransactionOutput where
    toJSON o = object
        [ "getCommandResult" .= _getCommandResult o
        , "getTxLogs" .= _getTxLogs o]
    {-# INLINE toJSON #-}

data MinerInfo = MinerInfo
  { _minerAccount :: Text
  , _minerKeys :: KeySet
  }
makeLenses ''MinerInfo

defaultMiner :: MinerInfo
defaultMiner = MinerInfo "" $ KeySet [] (Name "" def)



instance FromJSON TransactionOutput where
    parseJSON = withObject "TransactionOutput" $ \o -> TransactionOutput
        <$> o .: "getCommandResult"
        <*> o .: "getTxLogs"
    {-# INLINE parseJSON #-}

data PactDbStatePersist = PactDbStatePersist
    { _pdbspRestoreFile :: Maybe FilePath
    , _pdbspPactDbState :: PactDbState
    }

makeLenses ''PactDbStatePersist

type PactT a = ReaderT CheckpointEnv (StateT PactDbState IO) a

type MemPoolAccess = BlockHeight -> IO [Transaction]
