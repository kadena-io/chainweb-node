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
  , RosettaConstructionMetadataApi
  , rosettaConstructionMetadataApi
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
  , RosettaFailure(..)
  , rosettaError
  , throwRosetta
  , validateNetwork
  ) where

import Control.Error.Util
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT)

import Data.Aeson (encode, Object)

import Rosetta

import Servant

-- internal modules

import Chainweb.Rosetta.Utils (readChainIdText)
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
    :<|> RosettaConstructionMetadataApi_
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
type RosettaConstructionSubmitApi v = RosettaApiEndpoint v RosettaConstructionSubmitApi_
type RosettaConstructionMetadataApi v = RosettaApiEndpoint v RosettaConstructionMetadataApi_
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

type RosettaConstructionMetadataApi_
    = "construction"
    :> "metadata"
    :> ReqBody '[JSON] ConstructionMetadataReq
    :> Post '[JSON] ConstructionMetadataResp

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

rosettaConstructionMetadataApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (RosettaConstructionMetadataApi v)
rosettaConstructionMetadataApi = Proxy

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

-- ------------------------------------------------------------------ --
-- Rosetta Exceptions

-- TODO: Investigate if Rosetta Erros can be dynamic
data RosettaFailure
    = RosettaChainUnspecified
    | RosettaInvalidChain
    | RosettaMempoolBadTx
    | RosettaUnparsableTx
    | RosettaInvalidTx
    | RosettaInvalidBlockchainName
    | RosettaMismatchNetworkName
    | RosettaPactExceptionThrown
    | RosettaExpectedBalDecimal
    | RosettaInvalidResultMetaData
    | RosettaSubAcctUnsupported
    | RosettaMismatchTxLogs
    | RosettaUnparsableTxLog
    | RosettaInvalidBlockHeight
    | RosettaBlockHashNotFound
    | RosettaUnparsableBlockHash
    | RosettaOrphanBlockHash
    | RosettaMismatchBlockHashHeight
    | RosettaPayloadNotFound
    | RosettaUnparsableTxOut
    | RosettaTxIdNotFound
    | RosettaUnparsableTransactionId
    | RosettaInvalidAccountKey
    deriving (Show, Enum, Bounded, Eq)


-- TODO: Better grouping of rosetta error index?
rosettaError :: RosettaFailure -> Maybe Object -> RosettaError
rosettaError RosettaChainUnspecified = RosettaError 0 "No SubNetwork (chain) specified" False
rosettaError RosettaInvalidChain = RosettaError 1 "Invalid SubNetwork (chain) value" False
rosettaError RosettaMempoolBadTx = RosettaError 2 "Transaction not present in mempool" False
rosettaError RosettaUnparsableTx = RosettaError 3 "Transaction not parsable" False
rosettaError RosettaInvalidTx = RosettaError 4 "Invalid transaction" False
rosettaError RosettaInvalidBlockchainName = RosettaError 5 "Invalid blockchain name" False
rosettaError RosettaMismatchNetworkName = RosettaError 6 "Invalid Chainweb network name" False
rosettaError RosettaPactExceptionThrown =
  RosettaError 7 "A pact exception was thrown" False
rosettaError RosettaExpectedBalDecimal = RosettaError 8 "Expected balance as a decimal" False
rosettaError RosettaInvalidResultMetaData = RosettaError 9 "Invalid meta data field in command result" False
rosettaError RosettaSubAcctUnsupported = RosettaError 10 "Sub account identifier is not supported" False
rosettaError RosettaMismatchTxLogs =
  RosettaError 11 "Unable to match transactions to transaction logs as expected" False
rosettaError RosettaUnparsableTxLog = RosettaError 12 "TxLogs not parsable" False
rosettaError RosettaInvalidBlockHeight = RosettaError 13 "Invalid block height" False -- TODO if retry could succeed
rosettaError RosettaBlockHashNotFound = RosettaError 14 "Block hash was not found" False
rosettaError RosettaUnparsableBlockHash = RosettaError 15 "Block hash not parsable" False
rosettaError RosettaOrphanBlockHash = RosettaError 16 "Block hash not in the latest fork" False
rosettaError RosettaMismatchBlockHashHeight = RosettaError 17 "Block hash and block height did not match" False
rosettaError RosettaPayloadNotFound = RosettaError 18 "Block payload not found" False
rosettaError RosettaUnparsableTxOut = RosettaError 19 "Transaction output not parsable" False
rosettaError RosettaTxIdNotFound = RosettaError 20 "Transaction Id not found in block" False
rosettaError RosettaUnparsableTransactionId = RosettaError 21 "Transaction Id not parsable" False
rosettaError RosettaInvalidAccountKey = RosettaError 22 "Invalid AccountId address" False


throwRosetta :: RosettaFailure -> Handler a
throwRosetta e = throwError err500 { errBody = encode $ rosettaError e Nothing }


-- | Every Rosetta request that requires a `NetworkId` also requires a
-- `SubNetworkId`, at least in the case of Chainweb.
--
-- TODO for requests that concern only a particular block height it should
-- be verified that the chain is is active at that height.
--
validateNetwork :: Monad m => ChainwebVersion -> NetworkId -> ExceptT RosettaFailure m ChainId
validateNetwork v (NetworkId bc n msni) = do
    when (bc /= "kadena") $ throwError RosettaInvalidBlockchainName
    when (Just v /= fromText n) $ throwError RosettaMismatchNetworkName
    SubNetworkId cid _ <- msni ?? RosettaChainUnspecified
    readChainIdText v cid ?? RosettaInvalidChain
