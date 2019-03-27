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

import Data.Aeson (Value)
import Control.Concurrent.MVar.Strict
import Control.Monad.Catch
import Data.Aeson (FromJSON,ToJSON)
import Data.Text (Text,pack)
import GHC.Generics

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Transaction

import Pact.Types.Command

data RequestMsg = NewBlockMsg NewBlockReq
                | ValidateBlockMsg ValidateBlockReq
                | LocalMsg LocalReq
                | CloseMsg

data NewBlockReq = NewBlockReq
    { _newBlockHeader :: BlockHeader
    , _newMiner :: MinerInfo
    , _newResultVar :: MVar (Either PactException PayloadWithOutputs)
    }

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

data ValidateBlockReq = ValidateBlockReq
    { _valBlockHeader :: BlockHeader
    , _valPayloadData :: PayloadData
    , _valResultVar :: MVar (Either PactException PayloadWithOutputs)
    }

data LocalReq = LocalReq
    { _localRequest :: ChainwebTransaction
    , _localResultVar :: MVar (Either SomeException (CommandSuccess Value))
    }
