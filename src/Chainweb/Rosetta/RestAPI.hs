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
  , RosettaFailure(..)
  , rosettaError
  , throwRosetta
  ) where

import Control.Monad.Except (throwError)

import Data.Aeson (encode)
import Data.Text (Text)

import Rosetta

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.RestAPI.Utils (ChainwebEndpoint(..), Reassoc)
import Chainweb.Version

---

type RosettaApi_ =
    -- Accounts --
    "rosetta" :> "account" :> "balance"
        :> ReqBody '[JSON] AccountBalanceReq
        :> Post '[JSON] AccountBalanceResp
    -- Blocks --
    :<|> "rosetta" :> "block" :> "transaction"
        :> ReqBody '[JSON] BlockTransactionReq
        :> Post '[JSON] BlockTransactionResp
    :<|> "rosetta" :> "block"
        :> ReqBody '[JSON] BlockReq
        :> Post '[JSON] BlockResp
    -- Construction --
    :<|> "rosetta" :> "construction" :> "metadata"
        :> ReqBody '[JSON] ConstructionMetadataReq
        :> Post '[JSON] ConstructionMetadataResp
    :<|> "rosetta" :> "construction" :> "submit"
        :> ReqBody '[JSON] ConstructionSubmitReq
        :> Post '[JSON] ConstructionSubmitResp
    -- Mempool --
    :<|> "rosetta" :> "mempool" :> "transaction"
        :> ReqBody '[JSON] MempoolTransactionReq
        :> Post '[JSON] MempoolTransactionResp
    :<|> "rosetta" :> "mempool"
        :> ReqBody '[JSON] MempoolReq
        :> Post '[JSON] MempoolResp
    -- Network --
    :<|> "rosetta" :> "network" :> "list"
        :> ReqBody '[JSON] MetadataReq
        :> Post '[JSON] NetworkListResp
    :<|> "rosetta" :> "network" :> "options"
        :> ReqBody '[JSON] NetworkReq
        :> Post '[JSON] NetworkOptionsResp
    :<|> "rosetta" :> "network" :> "status"
        :> ReqBody '[JSON] NetworkReq
        :> Post '[JSON] NetworkStatusResp

type RosettaApi (v :: ChainwebVersionT) = 'ChainwebEndpoint v :> Reassoc RosettaApi_

data RosettaFailure
    = RosettaChainUnspecified
    | RosettaInvalidChain Text
    | RosettaMempoolBadTx
    | RosettaNotSupported ChainwebVersion

rosettaError :: RosettaFailure -> RosettaError
rosettaError RosettaChainUnspecified = RosettaError 0 "No SubNetwork (chain) specified" False
rosettaError (RosettaInvalidChain cid) = RosettaError 1 ("Invalid chain value: " <> cid) False
rosettaError RosettaMempoolBadTx = RosettaError 2 "Transaction not present in mempool" False
rosettaError (RosettaNotSupported v) = RosettaError 3
  ("Rosetta not supported for this chainweb node version: " <> chainwebVersionToText v) False

throwRosetta :: RosettaFailure -> Handler a
throwRosetta e = throwError err500 { errBody = encode $ rosettaError e }
