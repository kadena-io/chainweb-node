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

import Data.Text (Text)

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.Pact.Types
import Chainweb.Payload

data RequestMsg = NewBlockMsg NewBlockReq
                | ValidateBlockMsg ValidateBlockReq
                | LocalMsg LocalReq
                | CloseMsg

data NewBlockReq = NewBlockReq
    { _newBlockHeader :: BlockHeader
    , _newResultVar :: MVar PayloadWithOutputs
    }

newtype PactValidationErr = PactValidationErr { _pveErrMsg :: Text }

data ValidateBlockReq = ValidateBlockReq
    { _valBlockHeader :: BlockHeader
    , _valPayloadData :: PayloadData
    , _valResultVar :: MVar (Either PactValidationErr PayloadWithOutputs)
    }

data LocalReq = LocalReq
    -- TODO: request type will change to Command (Payload PublicMeta ParsedCode)
    { _localRequest :: BlockHeader
    , _localResultVar :: MVar (Either String Transactions)
    }
