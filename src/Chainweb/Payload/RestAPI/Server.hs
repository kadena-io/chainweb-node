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
  newPayloadServer
, somePayloadServer
, somePayloadServers

-- * Single Chain Server
, payloadApp
, payloadApiLayout
) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Data.Aeson
import Data.Function
import Data.Foldable
import Data.Maybe
import Data.Proxy
import qualified Data.Text.IO as T

import Prelude

import Network.HTTP.Types
import Network.Wai
import Servant

import Web.DeepRoute
import Web.DeepRoute.Wai

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
err404Msg msg = err404 { errBody = encode msg }

-- -------------------------------------------------------------------------- --
-- GET Payload Handler

-- | Query the 'BlockPayload' by its 'BlockPayloadHash'
--
payloadHandler
    :: CanReadablePayloadCas tbl
    => PayloadDb tbl
    -> BlockPayloadHash
    -> IO PayloadData
payloadHandler db k = run >>= \case
    Nothing -> throwM $ err404Msg $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= k
        ]
    Just e -> return e
  where
    run = runMaybeT $ do
        payload <- MaybeT $ tableLookup
            (_transactionDbBlockPayloads $ _transactionDb db)
            k
        txs <- MaybeT $ tableLookup
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
    -> IO [PayloadData]
payloadBatchHandler batchLimit db ks = do
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
    -> IO PayloadWithOutputs
outputsHandler db k = tableLookup db k >>= \case
    Nothing -> throwM $ err404Msg $ object
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
    -> IO [PayloadWithOutputs]
outputsBatchHandler batchLimit db ks =
    fmap catMaybes
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
    = liftIO . payloadHandler @tbl db
    :<|> liftIO . outputsHandler @tbl db
    :<|> liftIO . payloadBatchHandler @tbl batchLimit db
    :<|> liftIO . outputsBatchHandler @tbl batchLimit db

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

newPayloadServer :: CanReadablePayloadCas tbl => PayloadBatchLimit -> Route (PayloadDb tbl -> Application)
newPayloadServer batchLimit = fold
    [ choice "batch" $
        terminus methodGet "application/json" $ \pdb req resp ->
            resp . responseJSON ok200 [] . toJSON =<< payloadBatchHandler batchLimit pdb =<< requestFromJSON req
    , choice "outputs" $
        choice "batch" $
            terminus methodPost "application/json" $ \pdb req resp ->
            resp . responseJSON ok200 [] . toJSON =<< outputsBatchHandler batchLimit pdb =<< requestFromJSON req
    , capture $ fold
        [ choice "outputs" $ terminus methodGet "application/json" $ \k pdb _ resp ->
            resp . responseJSON ok200 [] . toJSON =<< outputsHandler pdb k
        , terminus methodGet "application/json" $ \k pdb _ resp ->
            resp . responseJSON ok200 [] . toJSON =<< payloadHandler pdb k
        ]
    ]

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
