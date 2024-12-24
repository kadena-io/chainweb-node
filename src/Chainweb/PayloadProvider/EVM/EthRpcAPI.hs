{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE  OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM.EthRpcAPI
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.EVM.EthRpcAPI
( mkRpcCtx
, SyncingStatus(..)

-- * RPC Methods
, type Eth_ChainId
, type Eth_BlockNumber
, type Eth_GetBlockByNumber
, type Eth_GetBlockByHash
, type Eth_Syncing
, type Eth_Call
) where

import Chainweb.PayloadProvider.EVM.Header
import Chainweb.PayloadProvider.EVM.JsonRPC
import Chainweb.PayloadProvider.EVM.Utils

import Control.Applicative

import Data.Aeson
import Data.ByteString qualified as B
import Data.Void

import Ethereum.Misc
import Ethereum.Transaction (Transaction)
import Ethereum.Utils

import GHC.Generics

import Network.HTTP.Client qualified as HTTP
import Network.URI.Static (uri)

-- -------------------------------------------------------------------------- --
-- Errors

-- * -39001: Unknown block

-- -------------------------------------------------------------------------- --

-- | Eth Block Number
--
instance JsonRpcMethod "eth_blockNumber" where
    type MethodRequest "eth_blockNumber" = Maybe Void
    type MethodResponse "eth_blockNumber" = BlockNumber
    type ServerErrors "eth_blockNumber" = Int
    type ApplicationErrors "eth_blockNumber" = Int
    responseTimeoutMs = Nothing
    methodErrors = []

type Eth_BlockNumber = "eth_blockNumber"

instance JsonRpcMethod "eth_chainId" where
    type MethodRequest "eth_chainId" = Maybe Void
    type MethodResponse "eth_chainId" = ChainId
    type ServerErrors "eth_chainId" = Int
    type ApplicationErrors "eth_chainId" = Int
    responseTimeoutMs = Nothing
    methodErrors = []

type Eth_ChainId = "eth_chainId"

-- | Get ETH block header by its block number
--
-- The second ('Bool') parameter indicates whether the full payload of the block
-- is returned or just its header.
--
instance JsonRpcMethod "eth_getBlockByNumber" where
    type MethodRequest "eth_getBlockByNumber" = (DefaultBlockParameter, Bool)
    type MethodResponse "eth_getBlockByNumber" = Maybe Header
    type ServerErrors "eth_getBlockByNumber" = Int
    type ApplicationErrors "eth_getBlockByNumber" = Int
    responseTimeoutMs = Nothing
    methodErrors = []

type Eth_GetBlockByNumber = "eth_getBlockByNumber"

instance JsonRpcMethod "eth_getBlockByHash" where
    type MethodRequest "eth_getBlockByHash" = (BlockHash, Bool)
    type MethodResponse "eth_getBlockByHash" = Maybe Header
    type ServerErrors "eth_getBlockByHash" = Int
    type ApplicationErrors "eth_getBlockByHash" = Int
    responseTimeoutMs = Nothing
    methodErrors = []

type Eth_GetBlockByHash = "eth_getBlockByHash"

-- -------------------------------------------------------------------------- --
-- | Returns the current sync status or false

-- | Returns an object with data about the sync status or false
--
instance JsonRpcMethod "eth_syncing" where
    type MethodRequest "eth_syncing" = Maybe Void
    type MethodResponse "eth_syncing" = SyncingStatus
    type ServerErrors "eth_syncing" = Int
    type ApplicationErrors "eth_syncing" = Int
    responseTimeoutMs = Nothing
    methodErrors = []

type Eth_Syncing = "eth_syncing"

data SyncingStatus
    = SyncingStatus
        { _syncingStatusStartingBlock :: BlockNumber
        , _syncingStatusCurrentBlock :: BlockNumber
        , _syncingStatusHighestBlock :: BlockNumber
        }
    | SyncingStatusFalse
    deriving (Show, Eq, Ord, Generic)

instance ToJSON SyncingStatus where
    toEncoding SyncingStatusFalse = toEncoding False
    toEncoding o = pairs
        ( "startingBlock" .= _syncingStatusStartingBlock o
        <> "currentBlock" .= _syncingStatusCurrentBlock o
        <> "highestBlock" .= _syncingStatusHighestBlock o
        )
    toJSON SyncingStatusFalse = toJSON False
    toJSON o = object
        [ "startingBlock" .= _syncingStatusStartingBlock o
        , "currentBlock" .= _syncingStatusCurrentBlock o
        , "highestBlock" .= _syncingStatusHighestBlock o
        ]
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON SyncingStatus where
    parseJSON v = notSyncing v <|> syncStatus v
      where
        notSyncing = withBool "SyncingStatus" $ \b -> if not b
            then return SyncingStatusFalse
            else fail "expected 'false' or object"
        syncStatus = withObject "SyncingStatus" $ \o -> SyncingStatus
            <$> o .: "startingBlock"
            <*> o .: "currentBlock"
            <*> o .: "highestBlock"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --

instance JsonRpcMethod "eth_call" where
    type MethodRequest "eth_call" = (Transaction, DefaultBlockParameter)
    type MethodResponse "eth_call" = HexBytes B.ByteString
    type ServerErrors "eth_call" = Int
    type ApplicationErrors "eth_call" = Int
    responseTimeoutMs = Nothing
    methodErrors = []

type Eth_Call = "eth_call"

-- -------------------------------------------------------------------------- --
-- | Default Engine Context
--

mkRpcCtx :: IO JsonRpcHttpCtx
mkRpcCtx = do
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    return $ JsonRpcHttpCtx
        { _jsonRpcHttpCtxManager = mgr
        , _jsonRpcHttpCtxURI = [uri|http://localhost:8545|]
        , _jsonRpcHttpCtxMakeBearerToken = Nothing
        }

-- -------------------------------------------------------------------------- --
-- Example

-- ghci> rpcCtx <- mkRpcCtx
-- ghci> Just hdr <- callMethodHttp @"eth_getBlockByNumber" rpcCtx (DefaultBlockLatest, False)
--
