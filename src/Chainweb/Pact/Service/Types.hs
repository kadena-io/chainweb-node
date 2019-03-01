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

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.Pact.Types

data RequestType = ValidateBlock | NewBlock deriving (Show)

data RequestMsg =
    RequestMsg
        { _reqRequestType :: RequestType
        , _reqBlockHeader :: BlockHeader
        , _reqResultVar :: MVar Transactions
        }
    | LocalRequestMsg
        -- TODO: request type will change to Command (Payload PublicMeta ParsedCode)
        { _localRequest :: BlockHeader
        , _localResultVar :: MVar (Either String Transactions)
        }
    | CloseMsg
