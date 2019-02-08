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

import Control.Lens hiding ((.=))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.Aeson as A
import Data.ByteString (ByteString)

import GHC.Word

import qualified Pact.Types.Command as P
import qualified Pact.Types.Persistence as P

import Chainweb.Pact.Backend.Types

data Transaction = Transaction
    { _tTxId :: Word64
    , _tCmd :: P.Command ByteString
    } deriving Show
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
    START HERE

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
    } deriving Show

instance ToJSON TransactionOutput where
    toJSON o = object
        [ "getcommandResult" .= _getCommandResult o
        , "getTxLogs" .= _getTxLogs o]
    {-# INLINE toJSON #-}

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

data TransactionCriteria =
    TransactionCriteria

type MemPoolAccess = TransactionCriteria -> IO [Transaction]
