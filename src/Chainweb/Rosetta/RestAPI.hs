{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    RosettaApi
  , rosettaApi
    -- * Standalone APIs for client derivation
  , RosettaAccountBalanceApi
  , rosettaAccountBalanceApi
  , RosettaBlockTransactionApi
  , rosettaBlockTransactionApi
  , RosettaBlockApi
  , rosettaBlockApi
  , RosettaConstructionDeriveApi
  , rosettaConstructionDeriveApi
  , RosettaConstructionPreprocessApi
  , rosettaConstructionPreprocessApi
  , RosettaConstructionMetadataApi
  , rosettaConstructionMetadataApi
  , RosettaConstructionPayloadsApi
  , rosettaConstructionPayloadsApi
  , RosettaConstructionParseApi
  , rosettaConstructionParseApi
  , RosettaConstructionCombineApi
  , rosettaConstructionCombineApi
  , RosettaConstructionHashApi
  , rosettaConstructionHashApi
  , RosettaConstructionSubmitApi
  , rosettaConstructionSubmitApi
  , RosettaMempoolTransactionApi
  , rosettaMempoolTransactionApi
  , RosettaMempoolApi
  , rosettaMempoolApi
  , RosettaNetworkListApi
  , rosettaNetworkListApi
  , RosettaNetworkOptionsApi
  , rosettaNetworkOptionsApi
  , RosettaNetworkStatusApi
  , rosettaNetworkStatusApi
    -- * Errors
  , throwRosetta
  , throwRosettaError
  , validateNetwork
  ) where

import Control.Error.Util
import Control.Monad (when)

import Data.Aeson (encode)

import Rosetta

import Servant

-- internal modules

import Chainweb.Rosetta.Utils
import Chainweb.RestAPI.Utils (ChainwebEndpoint(..), Reassoc)
import Chainweb.Utils
import Chainweb.Version

---

-- ------------------------------------------------------------------ --
-- Rosetta Api

type RosettaApi (v :: ChainwebVersionT) = 'ChainwebEndpoint v :> Reassoc RosettaApi_

type RosettaApi_ = "rosetta" :>
    ( -- Accounts --
      RosettaAccountBalanceApi_
      -- Blocks --
    :<|> RosettaBlockTransactionApi_
    :<|> RosettaBlockApi_
      -- Construction --
    :<|> RosettaConstructionDeriveApi_
    :<|> RosettaConstructionPreprocessApi_
    :<|> RosettaConstructionMetadataApi_
    :<|> RosettaConstructionPayloadsApi_
    :<|> RosettaConstructionParseApi_
    :<|> RosettaConstructionCombineApi_
    :<|> RosettaConstructionHashApi_
    :<|> RosettaConstructionSubmitApi_
      -- Mempool --
    :<|> RosettaMempoolTransactionApi_
    :<|> RosettaMempoolApi_
      -- Network --
    :<|> RosettaNetworkListApi_
    :<|> RosettaNetworkOptionsApi_
    :<|> RosettaNetworkStatusApi_
    )

rosettaApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaApi v)
rosettaApi = Proxy

-- ------------------------------------------------------------------ --
-- Standalone Endpoints + Witnesses

type RosettaApiEndpoint (v :: ChainwebVersionT) api
    = 'ChainwebEndpoint v
    :> "rosetta"
    :> api

type RosettaAccountBalanceApi v = RosettaApiEndpoint v RosettaAccountBalanceApi_
type RosettaBlockTransactionApi v = RosettaApiEndpoint v RosettaBlockTransactionApi_
type RosettaBlockApi v = RosettaApiEndpoint v RosettaBlockApi_
type RosettaConstructionDeriveApi v = RosettaApiEndpoint v RosettaConstructionDeriveApi_
type RosettaConstructionPreprocessApi v = RosettaApiEndpoint v RosettaConstructionPreprocessApi_
type RosettaConstructionMetadataApi v = RosettaApiEndpoint v RosettaConstructionMetadataApi_
type RosettaConstructionPayloadsApi v = RosettaApiEndpoint v RosettaConstructionPayloadsApi_
type RosettaConstructionParseApi v = RosettaApiEndpoint v RosettaConstructionParseApi_
type RosettaConstructionCombineApi v = RosettaApiEndpoint v RosettaConstructionCombineApi_
type RosettaConstructionHashApi v = RosettaApiEndpoint v RosettaConstructionHashApi_
type RosettaConstructionSubmitApi v = RosettaApiEndpoint v RosettaConstructionSubmitApi_
type RosettaMempoolTransactionApi v = RosettaApiEndpoint v RosettaMempoolTransactionApi_
type RosettaMempoolApi v = RosettaApiEndpoint v RosettaMempoolApi_
type RosettaNetworkListApi v = RosettaApiEndpoint v RosettaNetworkListApi_
type RosettaNetworkOptionsApi v = RosettaApiEndpoint v RosettaNetworkOptionsApi_
type RosettaNetworkStatusApi v = RosettaApiEndpoint v RosettaNetworkStatusApi_

type RosettaAccountBalanceApi_
    = "account"
    :> "balance"
    :> ReqBody '[JSON] AccountBalanceReq
    :> Post '[JSON] AccountBalanceResp

type RosettaBlockTransactionApi_
    = "block"
    :> "transaction"
    :> ReqBody '[JSON] BlockTransactionReq
    :> Post '[JSON] BlockTransactionResp

type RosettaBlockApi_
    = "block"
    :> ReqBody '[JSON] BlockReq
    :> Post '[JSON] BlockResp

type RosettaConstructionDeriveApi_
    = "construction"
    :> "derive"
    :> ReqBody '[JSON] ConstructionDeriveReq
    :> Post '[JSON] ConstructionDeriveResp

type RosettaConstructionPreprocessApi_
    = "construction"
    :> "preprocess"
    :> ReqBody '[JSON] ConstructionPreprocessReq
    :> Post '[JSON] ConstructionPreprocessResp

type RosettaConstructionMetadataApi_
    = "construction"
    :> "metadata"
    :> ReqBody '[JSON] ConstructionMetadataReq
    :> Post '[JSON] ConstructionMetadataResp

type RosettaConstructionPayloadsApi_
    = "construction"
    :> "payloads"
    :> ReqBody '[JSON] ConstructionPayloadsReq
    :> Post '[JSON] ConstructionPayloadsResp

type RosettaConstructionParseApi_
    = "construction"
    :> "parse"
    :> ReqBody '[JSON] ConstructionParseReq
    :> Post '[JSON] ConstructionParseResp

type RosettaConstructionCombineApi_
    = "construction"
    :> "combine"
    :> ReqBody '[JSON] ConstructionCombineReq
    :> Post '[JSON] ConstructionCombineResp

type RosettaConstructionHashApi_
    = "construction"
    :> "hash"
    :> ReqBody '[JSON] ConstructionHashReq
    :> Post '[JSON] TransactionIdResp

type RosettaConstructionSubmitApi_
    = "construction"
    :> "submit"
    :> ReqBody '[JSON] ConstructionSubmitReq
    :> Post '[JSON] TransactionIdResp

type RosettaMempoolTransactionApi_
    = "mempool"
    :> "transaction"
    :> ReqBody '[JSON] MempoolTransactionReq
    :> Post '[JSON] MempoolTransactionResp

type RosettaMempoolApi_
    = "mempool"
    :> ReqBody '[JSON] NetworkReq
    :> Post '[JSON] MempoolResp

type RosettaNetworkListApi_
    = "network"
    :> "list"
    :> ReqBody '[JSON] MetadataReq
    :> Post '[JSON] NetworkListResp

type RosettaNetworkOptionsApi_
    = "network"
    :> "options"
    :> ReqBody '[JSON] NetworkReq
    :> Post '[JSON] NetworkOptionsResp

type RosettaNetworkStatusApi_
    = "network"
    :> "status"
    :> ReqBody '[JSON] NetworkReq
    :> Post '[JSON] NetworkStatusResp

rosettaAccountBalanceApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaAccountBalanceApi v)
rosettaAccountBalanceApi = Proxy

rosettaBlockTransactionApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaBlockTransactionApi v)
rosettaBlockTransactionApi = Proxy

rosettaBlockApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaBlockApi v)
rosettaBlockApi = Proxy

rosettaConstructionDeriveApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaConstructionDeriveApi v)
rosettaConstructionDeriveApi = Proxy

rosettaConstructionPreprocessApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaConstructionPreprocessApi v)
rosettaConstructionPreprocessApi = Proxy

rosettaConstructionMetadataApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaConstructionMetadataApi v)
rosettaConstructionMetadataApi = Proxy

rosettaConstructionPayloadsApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaConstructionPayloadsApi v)
rosettaConstructionPayloadsApi = Proxy

rosettaConstructionParseApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaConstructionParseApi v)
rosettaConstructionParseApi = Proxy

rosettaConstructionCombineApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaConstructionCombineApi v)
rosettaConstructionCombineApi = Proxy

rosettaConstructionHashApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaConstructionHashApi v)
rosettaConstructionHashApi = Proxy

rosettaConstructionSubmitApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaConstructionSubmitApi v)
rosettaConstructionSubmitApi = Proxy

rosettaMempoolTransactionApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaMempoolTransactionApi v)
rosettaMempoolTransactionApi = Proxy

rosettaMempoolApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaMempoolApi v)
rosettaMempoolApi = Proxy

rosettaNetworkListApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaNetworkListApi v)
rosettaNetworkListApi = Proxy

rosettaNetworkOptionsApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaNetworkOptionsApi v)
rosettaNetworkOptionsApi = Proxy

rosettaNetworkStatusApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaNetworkStatusApi v)
rosettaNetworkStatusApi = Proxy


throwRosetta :: RosettaFailure -> Handler a
throwRosetta e = throwError err500 { errBody = encode $ rosettaError e Nothing }

throwRosettaError :: RosettaError -> Handler a
throwRosettaError e = throwError err500 { errBody = encode e }

-- | Every Rosetta request that requires a `NetworkId` also requires a
-- `SubNetworkId`, at least in the case of Chainweb.
--
-- TODO for requests that concern only a particular block height it should
-- be verified that the chain is is active at that height.
--
validateNetwork :: ChainwebVersion -> NetworkId -> Either RosettaFailure ChainId
validateNetwork v (NetworkId bc n msni) = do
    when (bc /= "kadena") $ Left RosettaInvalidBlockchainName
    when (Just v /= fromText n) $ Left RosettaMismatchNetworkName
    SubNetworkId cid _ <- note RosettaChainUnspecified msni
    note RosettaInvalidChain $ readChainIdText v cid
