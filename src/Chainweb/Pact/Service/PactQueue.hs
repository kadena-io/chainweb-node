{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module: Chainweb.Pact.Service.PactQueue
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact execution service queue for Chainweb

module Chainweb.Pact.Service.PactQueue
    ( addRequest
    , addResponse
    , getNextRequest
    , getNextResponse
    , RequestId(..)
    , RequestMsg(..)
    , RequestType(..)
    , ResponseMsg(..)
    ) where

import Data.Aeson
import Data.Int

import Chainweb.BlockHeader

data RequestType = ValidateBlock | NewBlock

newtype RequestId = RequestId { _getInt64 :: Int64 } deriving (FromJSON, ToJSON)

data RequestMsg = RequestMsg
    { _reqRequestType :: RequestType
    , _reqRequestId   :: RequestId
    , _reqBlockHeader :: BlockHeader
    }

data ResponseMsg = ResponseMsg
    { _respRequestType :: RequestType
    , _respRequestId   :: RequestId
    , _respBlockHeader :: BlockPayloadHash
    }

addRequest :: RequestMsg -> IO RequestId
addRequest msg = undefined

getNextRequest :: IO RequestMsg
getNextRequest = undefined

addResponse :: ResponseMsg -> IO RequestId
addResponse msg = undefined

getNextResponse :: IO ResponseMsg
getNextResponse = undefined
