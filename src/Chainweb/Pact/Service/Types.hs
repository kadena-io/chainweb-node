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

data RequestType = ValidateBlock | NewBlock deriving (Show)

data RequestMsg = RequestMsg
    { _reqRequestType :: RequestType
    , _reqBlockHeader :: BlockHeader
    , _reqResultVar :: MVar Transactions
    }
    | CloseMsg
