{-# LANGUAGE DeriveGeneric #-}
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

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack)

import GHC.Generics

-- internal chainweb modules

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.Miner.Pact
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Transaction

-- internal pact modules

import Pact.Types.Hash (PactHash)

data PactException
  = BlockValidationFailure Text
  | PactInternalError Text
  | NoBlockValidatedYet
  deriving (Eq,Show,Generic)

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
                | SpvMsg SpvReq
                | CloseMsg
                deriving (Show)

data NewBlockReq = NewBlockReq
    { _newBlockHeader :: BlockHeader
    , _newMiner :: Miner
    , _newResultVar :: MVar (Either PactException PayloadWithOutputs)
    }
instance Show NewBlockReq where
  show (NewBlockReq bh m _) = show (bh, m)

data ValidateBlockReq = ValidateBlockReq
    { _valBlockHeader :: BlockHeader
    , _valPayloadData :: PayloadData
    , _valResultVar :: MVar (Either PactException PayloadWithOutputs)
    }
instance Show ValidateBlockReq where
  show (ValidateBlockReq bh pd _) = show (bh, pd)

data LocalReq = LocalReq
    { _localRequest :: ChainwebTransaction
    , _localResultVar :: MVar (Either PactException HashCommandResult)
    }
instance Show LocalReq where
  show = show . _localRequest

data SpvReq = SpvReq
    { _spvTxHash :: PactHash
       -- ^ Transaction hash of tx to do index lookup
    , _spvBlockHeader :: BlockHeader
       -- ^ Most current block header (passed by endpoint)
    , _spvResultVar :: MVar (Either PactException Base64TxOutputProof)
    }
instance Show SpvReq where
  show = show . _spvTxHash
