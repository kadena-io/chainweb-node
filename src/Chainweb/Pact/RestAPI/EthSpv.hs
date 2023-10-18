{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Pact.RestAPI.EthSpv
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Pact.RestAPI.EthSpv
( EthSpvRequest(..)
, EthSpvResponse(..)
) where

import Data.Aeson
import qualified Data.Text as T

import Ethereum.Block
import Ethereum.Misc
import Ethereum.Receipt

import GHC.Generics

-- -------------------------------------------------------------------------- --
-- Eth SPV Request

data EthSpvRequest = EthSpvRequest
    { _ethSpvReqTransactionHash :: !TransactionHash
        -- ^ The hash of the transactions for which the receipt proof is
        -- created.

    , _ethSpvReqBlocks :: ![RpcBlock]
        -- ^ A list of blocks. This list must contain at least the block that
        -- contains the transaction for which the receipt proof is created. It
        -- may contain a sequence of subsequent blocks, which would be included
        -- in the proof and increase the depth of the proof. The root of the
        -- proof is the hash of the last block in this sequence. Any blocks that
        -- are not in this sequence are ignored.
        --
        -- It is not required that blocks contain the complete transactions,
        -- just the transaction ids are sufficient.

    , _ethSpvReqReceipts :: ![RpcReceipt]
        -- ^ The complete set of receipts of the block that contains the receipt
        -- for which the proof is created. Any additional receipts are ignored.
    }
    deriving (Show, Eq, Generic)

instance ToJSON EthSpvRequest where
    toEncoding = pairs . mconcat . ethSpvRequestProperties
    toJSON = object . ethSpvRequestProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

ethSpvRequestProperties :: KeyValue e kv => EthSpvRequest -> [kv]
ethSpvRequestProperties o =
        [ "transactionHash" .= _ethSpvReqTransactionHash o
        , "blocks" .= _ethSpvReqBlocks o
        , "receipts" .= _ethSpvReqReceipts o
        ]
{-# INLINE ethSpvRequestProperties #-}

instance FromJSON EthSpvRequest where
    parseJSON = withObject "EthSpvRequest" $ \o -> EthSpvRequest
        <$> o .: "transactionHash"
        <*> o .: "blocks"
        <*> o .: "receipts"
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Eth SPV Receipt Proof

newtype EthSpvResponse = EthSpvResponse
    { _ethSpvResponse :: T.Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON EthSpvResponse where
    toEncoding = pairs . mconcat . ethSpvResponseProperties
    toJSON = object . ethSpvResponseProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

ethSpvResponseProperties :: KeyValue e kv => EthSpvResponse -> [kv]
ethSpvResponseProperties o = [ "proof" .= _ethSpvResponse o ]
{-# INLINE ethSpvResponseProperties #-}

instance FromJSON EthSpvResponse where
    parseJSON = withObject "proof" $ \o -> EthSpvResponse
        <$> o .: "proof"
    {-# INLINE parseJSON #-}

