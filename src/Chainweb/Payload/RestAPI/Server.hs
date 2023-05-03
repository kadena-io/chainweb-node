{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Payload.RestAPI.Server
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Server implementation of the block payload REST API
--
module Chainweb.Payload.RestAPI.Server
(
  somePayloadServer
, somePayloadServers

-- * Single Chain Server
, payloadApp
, payloadApiLayout
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Data.Aeson
import Data.Function
import Data.Maybe
import Data.Proxy
import qualified Data.Text.IO as T

import Prelude hiding (lookup)

import Servant

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.Utils (int)
import Chainweb.Version

import Chainweb.Storage.Table

-- -------------------------------------------------------------------------- --
-- Utils

err404Msg :: ToJSON msg  => msg -> ServerError
err404Msg msg = setErrJSON msg err404

-- -------------------------------------------------------------------------- --
-- GET Payload Handler

-- | Query the 'BlockPayload' by its 'BlockPayloadHash'
--
payloadHandler
    :: CanReadablePayloadCas tbl
    => PayloadDb tbl
    -> BlockPayloadHash
    -> Handler PayloadData
payloadHandler db k = run >>= \case
    Nothing -> throwError $ err404Msg $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= k
        ]
    Just e -> return e
  where
    run = runMaybeT $ do
        payload <- MaybeT $ liftIO $ tableLookup
            (_transactionDbBlockPayloads $ _transactionDb db)
            k
        txs <- MaybeT $ liftIO $ tableLookup
            (_transactionDbBlockTransactions $ _transactionDb db)
            (_blockPayloadTransactionsHash payload)
        return $ payloadData txs payload

-- -------------------------------------------------------------------------- --
-- POST Payload Batch Handler

payloadBatchHandler
    :: CanReadablePayloadCas tbl
    => PayloadBatchLimit
    -> PayloadDb tbl
    -> [BlockPayloadHash]
    -> Handler [PayloadData]
payloadBatchHandler batchLimit db ks = liftIO $ do
    payloads <- catMaybes
        <$> tableLookupBatch payloadsDb (take (int batchLimit) ks)
    txs <- zipWith (\a b -> payloadData <$> a <*> pure b)
        <$> tableLookupBatch txsDb (_blockPayloadTransactionsHash <$> payloads)
        <*> pure payloads
    return $ catMaybes txs
  where
    payloadsDb = _transactionDbBlockPayloads $ _transactionDb db
    txsDb = _transactionDbBlockTransactions $ _transactionDb db

-- -------------------------------------------------------------------------- --
-- GET Outputs Handler

-- | Query the 'PayloadWithOutputs' by its 'BlockPayloadHash'
--
outputsHandler
    :: CanReadablePayloadCas tbl
    => PayloadDb tbl
    -> BlockPayloadHash
    -> Handler PayloadWithOutputs
outputsHandler db k = liftIO (tableLookup db k) >>= \case
    Nothing -> throwError $ err404Msg $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= k
        ]
    Just e -> return e

-- -------------------------------------------------------------------------- --
-- POST Outputs Batch Handler

outputsBatchHandler
    :: CanReadablePayloadCas tbl
    => PayloadBatchLimit
    -> PayloadDb tbl
    -> [BlockPayloadHash]
    -> Handler [PayloadWithOutputs]
outputsBatchHandler batchLimit db ks = liftIO
    $ fmap catMaybes
    $ tableLookupBatch db
    $ take (int batchLimit) ks

-- -------------------------------------------------------------------------- --
-- Payload API Server

payloadServer
    :: forall tbl v c
    . CanReadablePayloadCas tbl
    => PayloadBatchLimit
    -> PayloadDb' tbl v c
    -> Server (PayloadApi v c)
payloadServer batchLimit (PayloadDb' db)
    = payloadHandler @tbl db
    :<|> outputsHandler @tbl db
    :<|> payloadBatchHandler @tbl batchLimit db
    :<|> outputsBatchHandler @tbl batchLimit db

-- -------------------------------------------------------------------------- --
-- Application for a single PayloadDb

payloadApp
    :: forall tbl v c
    . CanReadablePayloadCas tbl
    => KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PayloadBatchLimit
    -> PayloadDb' tbl v c
    -> Application
payloadApp batchLimit db = serve (Proxy @(PayloadApi v c)) (payloadServer batchLimit db)

payloadApiLayout
    :: forall tbl v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PayloadDb' tbl v c
    -> IO ()
payloadApiLayout _ = T.putStrLn $ layout (Proxy @(PayloadApi v c))

-- -------------------------------------------------------------------------- --
-- Multichain Server

somePayloadServer
    :: CanReadablePayloadCas tbl
    => PayloadBatchLimit
    -> SomePayloadDb tbl
    -> SomeServer
somePayloadServer batchLimit (SomePayloadDb (db :: PayloadDb' tbl v c))
    = SomeServer (Proxy @(PayloadApi v c)) (payloadServer batchLimit db)

somePayloadServers
    :: CanReadablePayloadCas tbl
    => ChainwebVersion
    -> PayloadBatchLimit
    -> [(ChainId, PayloadDb tbl)]
    -> SomeServer
somePayloadServers v batchLimit
    = mconcat . fmap (somePayloadServer batchLimit . uncurry (somePayloadDbVal v))
