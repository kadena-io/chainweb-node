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
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad.Trans.Reader

import Data.Aeson
import Data.Hashable
import qualified Data.HashTable.IO as H
import qualified Data.HashTable.ST.Basic as H
import Data.Int
import Data.String.Conv (toS)

import Safe

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.Pact.Types
import Chainweb.Payload

data RequestMsg =
    NewBlockReq
        { _newBlockHeader :: BlockHeader
        , _newResultVar :: MVar (BlockTransactions, BlockPayloadHash)
        }
    | ValidateBlockReq
        { _valBlockHeader :: BlockHeader
        , _valResultVar :: MVar (BlockTransactions, BlockOutputs)
        }
    | LocalReq
        -- TODO: request type will change to Command (Payload PublicMeta ParsedCode)
        { _localRequest :: BlockHeader
        , _localResultVar :: MVar (Either String Transactions)
        }
    | CloseMsg
