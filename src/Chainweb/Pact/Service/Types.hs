{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Aeson (FromJSON,ToJSON)
import Data.Text (Text,pack)
import GHC.Generics

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Transaction


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
                | CloseMsg
                deriving (Show)

data NewBlockReq = NewBlockReq
    { _newBlockHeader :: BlockHeader
    , _newMiner :: MinerInfo
    , _newResultVar :: MVar (Either PactException PayloadWithOutputs)
    }
instance Show NewBlockReq where show NewBlockReq{..} = show (_newBlockHeader, _newMiner)

data ValidateBlockReq = ValidateBlockReq
    { _valBlockHeader :: BlockHeader
    , _valPayloadData :: PayloadData
    , _valResultVar :: MVar (Either PactException PayloadWithOutputs)
    }
instance Show ValidateBlockReq where show ValidateBlockReq{..} = show (_valBlockHeader, _valPayloadData)

data LocalReq = LocalReq
    { _localRequest :: ChainwebTransaction
    , _localResultVar :: MVar (Either PactException HashCommandResult)
    }
instance Show LocalReq where show LocalReq{..} = show (_localRequest)
