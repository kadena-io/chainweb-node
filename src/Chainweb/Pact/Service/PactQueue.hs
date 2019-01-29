{-# LANGUAGE RecordWildCards #-}

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
    , RequestMsg(..)
    , RequestType(..)
    , ResponseMsg(..)
    ) where

import Chainweb.BlockHeader

data RequestType = ValidateBlock | NewBlock

data RequestMsg = RequestMsg
    { _reqRequestType :: RequestType
    , _reqBlockHeader :: BlockHeader
    }

data ResponseMsg = ResponseMsg
    { _respRequestType :: RequestType
    , _respBlockHeader :: BlockHeader
    }

addRequest :: RequestMsg -> IO ()
addRequest msg = undefined

getNextRequest :: IO RequestMsg
getNextRequest = undefined

addResponse :: ResponseMsg -> IO ()
addResponse msg = undefined

getNextResponse :: IO ResponseMsg
getNextResponse = undefined
