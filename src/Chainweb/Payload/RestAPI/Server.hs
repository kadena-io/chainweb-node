{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
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
import Data.Proxy
import qualified Data.Text.IO as T
import qualified Data.Vector as V

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

import Data.CAS

-- -------------------------------------------------------------------------- --
-- Utils

err404Msg :: ToJSON msg  => msg -> ServerError
err404Msg msg = err404 { errBody = encode msg }

catMaybes :: V.Vector (Maybe a) -> V.Vector a
catMaybes = V.catMaybes

-- -------------------------------------------------------------------------- --
-- GET Payload Handler

-- | Query the 'BlockPayload' by its 'BlockPayloadHash'
--
payloadHandler
    :: forall cas
    . PayloadCasLookup cas
    => PayloadDb cas
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
        payload <- MaybeT $ liftIO $ casLookup
            (_transactionDbBlockPayloads $ _transactionDb db)
            k
        txs <- MaybeT $ liftIO $ casLookup
            (_transactionDbBlockTransactions $ _transactionDb db)
            (_blockPayloadTransactionsHash payload)
        return $ payloadData txs payload

-- -------------------------------------------------------------------------- --
-- POST Payload Batch Handler

payloadBatchHandler
    :: forall cas
    . PayloadCasLookup cas
    => PayloadBatchLimit
    -> PayloadDb cas
    -> [BlockPayloadHash]
    -> Handler [PayloadData]
payloadBatchHandler batchLimit db ks = liftIO $ do
    payloads <- catMaybes
        <$> casLookupBatch payloadsDb (V.fromList $ take (int batchLimit) ks)
    txs <- V.zipWith (\a b -> payloadData <$> a <*> pure b)
        <$> casLookupBatch txsDb (_blockPayloadTransactionsHash <$> payloads)
        <*> pure payloads
    return $ V.toList $ catMaybes txs
  where
    payloadsDb = _transactionDbBlockPayloads $ _transactionDb db
    txsDb = _transactionDbBlockTransactions $ _transactionDb db

-- -------------------------------------------------------------------------- --
-- GET Outputs Handler

-- | Query the 'PayloadWithOutputs' by its 'BlockPayloadHash'
--
outputsHandler
    :: forall cas
    . PayloadCasLookup cas
    => PayloadDb cas
    -> BlockPayloadHash
    -> Handler PayloadWithOutputs
outputsHandler db k = liftIO (casLookup db k) >>= \case
    Nothing -> throwError $ err404Msg $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= k
        ]
    Just e -> return e

-- -------------------------------------------------------------------------- --
-- POST Outputs Batch Handler

outputsBatchHandler
    :: forall cas
    . PayloadCasLookup cas
    => PayloadBatchLimit
    -> PayloadDb cas
    -> [BlockPayloadHash]
    -> Handler [PayloadWithOutputs]
outputsBatchHandler batchLimit db ks = liftIO
    $ fmap (V.toList . catMaybes)
    $ casLookupBatch db
    $ V.fromList
    $ take (int batchLimit) ks

-- -------------------------------------------------------------------------- --
-- Payload API Server

payloadServer
    :: forall cas v (c :: ChainIdT)
    . PayloadCasLookup cas
    => PayloadBatchLimit
    -> PayloadDb' cas v c
    -> Server (PayloadApi v c)
payloadServer batchLimit (PayloadDb' db)
    = payloadHandler @cas db
    :<|> outputsHandler @cas db
    :<|> payloadBatchHandler @cas batchLimit db
    :<|> outputsBatchHandler @cas batchLimit db

-- -------------------------------------------------------------------------- --
-- Application for a single PayloadDb

payloadApp
    :: forall cas v c
    . PayloadCasLookup cas
    => KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PayloadBatchLimit
    -> PayloadDb' cas v c
    -> Application
payloadApp batchLimit db = serve (Proxy @(PayloadApi v c)) (payloadServer batchLimit db)

payloadApiLayout
    :: forall cas v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PayloadDb' cas v c
    -> IO ()
payloadApiLayout _ = T.putStrLn $ layout (Proxy @(PayloadApi v c))

-- -------------------------------------------------------------------------- --
-- Multichain Server

somePayloadServer
    :: PayloadCasLookup cas
    => PayloadBatchLimit
    -> SomePayloadDb cas
    -> SomeServer
somePayloadServer batchLimit (SomePayloadDb (db :: PayloadDb' cas v c))
    = SomeServer (Proxy @(PayloadApi v c)) (payloadServer batchLimit db)

somePayloadServers
    :: PayloadCasLookup cas
    => ChainwebVersion
    -> PayloadBatchLimit
    -> [(ChainId, PayloadDb cas)]
    -> SomeServer
somePayloadServers v batchLimit
    = mconcat . fmap (somePayloadServer batchLimit . uncurry (somePayloadDbVal v))
