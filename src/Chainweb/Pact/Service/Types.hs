{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module: Chainweb.Pact.Service.Types
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Types module for Pact execution API

module Chainweb.Pact.Service.Types where

import Control.Concurrent.MVar.Strict
import Control.Monad.Catch

import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Tuple.Strict
import Data.Vector (Vector)

import GHC.Generics

-- internal pact modules

import Pact.Types.ChainId as Pact
import Pact.Types.Command
import Pact.Types.Hash

-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Miner.Pact
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils (encodeToText)


data PactException
  = BlockValidationFailure Value
  | PactInternalError Text
  | NoBlockValidatedYet
  | TransactionValidationException [(PactHash, Text)]
  | PactDuplicateTableError Text
  | TransactionDecodeFailure Text
  -- The only argument Text is the duplicate table name.
  deriving (Eq,Generic)

instance Show PactException where
    show = unpack . encodeToText

instance ToJSON PactException
instance FromJSON PactException

instance Exception PactException


internalError :: MonadThrow m => Text -> m a
internalError = throwM . PactInternalError

internalError' :: MonadThrow m => String -> m a
internalError' = internalError . pack

data RequestMsg = NewBlockMsg NewBlockReq
                | ValidateBlockMsg ValidateBlockReq
                | LocalMsg LocalReq
                | LookupPactTxsMsg LookupPactTxsReq
                | CloseMsg
                deriving (Show)

type PactExMVar t = MVar (Either PactException t)

data NewBlockReq = NewBlockReq
    { _newBlockHeader :: BlockHeader
    , _newMiner :: Miner
    , _newCreationTime :: !BlockCreationTime
    , _newResultVar :: PactExMVar PayloadWithOutputs
    }
instance Show NewBlockReq where show NewBlockReq{..} = show (_newBlockHeader, _newMiner)

data ValidateBlockReq = ValidateBlockReq
    { _valBlockHeader :: BlockHeader
    , _valPayloadData :: PayloadData
    , _valResultVar :: PactExMVar PayloadWithOutputs
    }
instance Show ValidateBlockReq where show ValidateBlockReq{..} = show (_valBlockHeader, _valPayloadData)

data LocalReq = LocalReq
    { _localRequest :: ChainwebTransaction
    , _localResultVar :: PactExMVar HashCommandResult
    }
instance Show LocalReq where show LocalReq{..} = show (_localRequest)

data LookupPactTxsReq = LookupPactTxsReq
    { _lookupRestorePoint :: !(Maybe (T2 BlockHeight BlockHash))
        -- here if the restore point is "Nothing" it means "we don't care"
    , _lookupKeys :: !(Vector PactHash)
    , _lookupResultVar :: !(PactExMVar (Vector (Maybe (T2 BlockHeight BlockHash))))
    }
instance Show LookupPactTxsReq where
    show (LookupPactTxsReq m _ _) =
        "LookupPactTxsReq@" ++ show m

data SpvRequest = SpvRequest
    { _spvRequestKey :: RequestKey
    , _spvTargetChainId :: Pact.ChainId
    } deriving (Eq, Show, Generic)

instance ToJSON SpvRequest where
  toJSON (SpvRequest k tid) = object
    [ "requestKey" .= k
    , "targetChainId" .= tid
    ]

instance FromJSON SpvRequest where
  parseJSON = withObject "SpvRequest" $ \o -> SpvRequest
    <$> o .: "requestKey"
    <*> o .: "targetChainId"

newtype TransactionOutputProofB64 = TransactionOutputProofB64 Text
    deriving stock (Eq, Show, Generic)
    deriving newtype (ToJSON, FromJSON)
