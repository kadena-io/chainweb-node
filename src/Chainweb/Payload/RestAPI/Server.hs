{-# LANGUAGE CPP #-}
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
#if ! MIN_VERSION_vector(0,12,2)
import qualified Data.Maybe as M
#endif
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
import Chainweb.Version

import Data.CAS

-- -------------------------------------------------------------------------- --
-- Utils

-- | The maximum number of items that are returned in a batch
--
payloadBatchLimit :: Int
payloadBatchLimit = 100

err404Msg :: ToJSON msg  => msg -> ServerError
err404Msg msg = err404 { errBody = encode msg }

catMaybes :: V.Vector (Maybe a) -> V.Vector a
#if MIN_VERSION_vector(0,12,2)
catMaybes = V.catMaybes
#else
catMaybes = V.fromList . M.catMaybes . V.toList
#endif

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
    => PayloadDb cas
    -> [BlockPayloadHash]
    -> Handler [PayloadData]
payloadBatchHandler db ks = liftIO $ do
    payloads <- catMaybes
        <$> casLookupBatch payloadsDb (V.fromList $ take payloadBatchLimit ks)
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
    => PayloadDb cas
    -> [BlockPayloadHash]
    -> Handler [PayloadWithOutputs]
outputsBatchHandler db ks = liftIO
    $ fmap (V.toList . catMaybes)
    $ casLookupBatch db
    $ V.fromList ks

-- -------------------------------------------------------------------------- --
-- Payload API Server

payloadServer
    :: forall cas v (c :: ChainIdT)
    . PayloadCasLookup cas
    => PayloadDb_ cas v c
    -> Server (PayloadApi v c)
payloadServer (PayloadDb_ db)
    = payloadHandler @cas db
    :<|> outputsHandler @cas db
    :<|> payloadBatchHandler @cas db
    :<|> outputsBatchHandler @cas db

-- -------------------------------------------------------------------------- --
-- Application for a single PayloadDb

payloadApp
    :: forall cas v c
    . PayloadCasLookup cas
    => KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PayloadDb_ cas v c
    -> Application
payloadApp db = serve (Proxy @(PayloadApi v c)) (payloadServer db)

payloadApiLayout
    :: forall cas v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PayloadDb_ cas v c
    -> IO ()
payloadApiLayout _ = T.putStrLn $ layout (Proxy @(PayloadApi v c))

-- -------------------------------------------------------------------------- --
-- Multichain Server

somePayloadServer :: PayloadCasLookup cas => SomePayloadDb cas -> SomeServer
somePayloadServer (SomePayloadDb (db :: PayloadDb_ cas v c))
    = SomeServer (Proxy @(PayloadApi v c)) (payloadServer db)

somePayloadServers
    :: PayloadCasLookup cas
    => ChainwebVersion
    -> [(ChainId, PayloadDb cas)]
    -> SomeServer
somePayloadServers v
    = mconcat . fmap (somePayloadServer . uncurry (somePayloadDbVal v))
