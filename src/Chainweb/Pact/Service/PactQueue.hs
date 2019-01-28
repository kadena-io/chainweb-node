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
    ) where

data RequestType = ValidateBlock | NewBlock

data RequestMsg = RequestMsg
    { requestType :: RequestType
    , blockHeader :: BlockHeader
    }

data ResponseMsg = ResponseMsg
    { requestType :: RequestType
    , blockHeader :: BlockHeader
    }

addRequest :: RequestMsg -> IO ()
addRequest msg = undefined

getNextRequest :: IO RequestMsg
getNextRequest = undefined

addResponse :: ResponseMsg -> IO ()
addResponse msg = undefined

getNextResponse :: IO ResponseMsg
getNextResponse = undefined
