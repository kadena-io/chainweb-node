{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Rosetta.RestAPI
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Rosetta.RestAPI
  ( -- * Endpoints
    RosettaApi_
  , RosettaApi
    -- * Errors
  , RosettaError
  , rosettaError
  ) where

import Data.Text (Text)

import Rosetta

import Servant.API

-- internal modules

import Chainweb.RestAPI.Utils (ChainwebEndpoint(..), Reassoc)
import Chainweb.Version

---

type RosettaApi_ =
    -- Accounts --
    "rosetta" :> "account" :> "balance"
        :> ReqBody '[JSON] AccountBalanceRequest
        :> Post '[JSON] AccountBalanceResponse
    -- Blocks --
    :<|> "rosetta" :> "block" :> "transaction"
        :> ReqBody '[JSON] BlockTransactionRequest
        :> Post '[JSON] BlockTransactionResponse
    :<|> "rosetta" :> "block"
        :> ReqBody '[JSON] BlockRequest
        :> Post '[JSON] BlockResponse
    -- Construction --
    :<|> "rosetta" :> "construction" :> "metadata"
        :> ReqBody '[JSON] ConstructionMetadataRequest
        :> Post '[JSON] ConstructionMetadataResponse
    :<|> "rosetta" :> "construction" :> "submit"
        :> ReqBody '[JSON] ConstructionSubmitRequest
        :> Post '[JSON] ConstructionSubmitResponse
    -- Mempool --
    :<|> "rosetta" :> "mempool" :> "transaction"
        :> ReqBody '[JSON] MempoolTransactionRequest
        :> Post '[JSON] MempoolTransactionResponse
    :<|> "rosetta" :> "mempool"
        :> ReqBody '[JSON] MempoolRequest
        :> Post '[JSON] MempoolResponse
    -- Network --
    :<|> "rosetta" :> "network" :> "list"
        :> ReqBody '[JSON] MetadataRequest
        :> Post '[JSON] NetworkListResponse
    :<|> "rosetta" :> "network" :> "options"
        :> ReqBody '[JSON] NetworkRequest
        :> Post '[JSON] NetworkOptionsResponse
    :<|> "rosetta" :> "network" :> "status"
        :> ReqBody '[JSON] NetworkRequest
        :> Post '[JSON] NetworkStatusResponse

type RosettaApi (v :: ChainwebVersionT) = 'ChainwebEndpoint v :> Reassoc RosettaApi_

data RosettaError

rosettaError :: RosettaError -> (Word, Text)
rosettaError _ = (0, "oops!")
