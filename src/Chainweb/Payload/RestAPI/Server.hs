{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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

import Control.Applicative
import Control.Lens (over, _1)
import Control.Monad
import Control.Monad.IO.Class

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

import Chainweb.BlockHeight (BlockHeight)

-- -------------------------------------------------------------------------- --
-- GET Payload Handler

-- | Query the 'BlockPayload' by its 'BlockPayloadHash'
--
payloadHandler
    :: CanReadablePayloadCas tbl
    => PayloadDb tbl
    -> BlockPayloadHash
    -> Maybe BlockHeight
    -> IO PayloadData
payloadHandler db k mh = lookupPayloadDataWithHeight db mh k >>= \case
    Nothing -> jsonErrorWithStatus notFound404 $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= k
        ]
    Just e -> return e

-- -------------------------------------------------------------------------- --
-- POST Payload Batch Handler

payloadBatchHandler
    :: CanReadablePayloadCas tbl
    => PayloadBatchLimit
    -> PayloadDb tbl
    -> BatchBody
    -> IO [PayloadData]
payloadBatchHandler batchLimit db ks
  = catMaybes <$> lookupPayloadDataWithHeightBatch db ks'
  where
      limit = take (int batchLimit)
      ks' | WithoutHeights xs <- ks = limit (fmap (Nothing,) xs)
          | WithHeights    xs <- ks = limit (fmap (over _1 Just) xs)

-- -------------------------------------------------------------------------- --
-- GET Outputs Handler

-- | Query the 'PayloadWithOutputs' by its 'BlockPayloadHash'
--
outputsHandler
    :: CanReadablePayloadCas tbl
    => PayloadDb tbl
    -> BlockPayloadHash
    -> Maybe BlockHeight
    -> IO PayloadWithOutputs
outputsHandler db k mh = liftIO (lookupPayloadWithHeight db mh k) >>= \case
    Nothing -> jsonErrorWithStatus notFound404 $ object
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
    -> BatchBody
    -> IO [PayloadWithOutputs]
outputsBatchHandler batchLimit db ks
  = catMaybes <$> lookupPayloadWithHeightBatch db ks'
  where
      limit = take (int batchLimit)
      ks' | WithoutHeights xs <- ks = limit (fmap (Nothing,) xs)
          | WithHeights    xs <- ks = limit (fmap (over _1 Just) xs)


-- -------------------------------------------------------------------------- --
-- Payload API Server

payloadServer
    :: forall tbl v c
    . CanReadablePayloadCas tbl
    => PayloadBatchLimit
    -> PayloadDb' tbl v c
    -> Server (PayloadApi v c)
payloadServer batchLimit (PayloadDb' db)
    = (liftIO .) . payloadHandler @tbl db
    :<|> (liftIO .) . outputsHandler @tbl db
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
    [ seg "batch" $
        endpoint methodGet ("application/json") $ \pdb req resp ->
            resp . responseJSON ok200 [] . toJSON =<< payloadBatchHandler batchLimit pdb =<< requestFromJSON req
    , seg "outputs" $
        seg "batch" $
            endpoint methodPost ("application/json") $ \pdb req resp ->
            resp . responseJSON ok200 [] . toJSON =<< outputsBatchHandler batchLimit pdb =<< requestFromJSON req
    , capture $ fold
        [ seg "outputs" $ endpoint methodGet ("application/json") $ \k pdb req resp -> do
            mbh <- getParams req $ queryParamMaybe "height"
            resp . responseJSON ok200 [] . toJSON =<< outputsHandler pdb k mbh
        , endpoint methodGet ("application/json") $ \k pdb req resp -> do
            mbh <- getParams req $ queryParamMaybe "height"
            resp . responseJSON ok200 [] . toJSON =<< payloadHandler pdb k mbh
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
