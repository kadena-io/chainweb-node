{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module: Chainweb.Payload.RestAPI.Server
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Server implementation of the block payload REST API
--
-- FIXME: consider to not expose the payload table directly, but only through an
-- API (possibly in the PayloadProvider class or may dedicated
-- PayloadProviderApi class).
-- Or, alternatively, create the apis along with the apis in the provider
-- initialization function internally and not in the resources module.
--
module Chainweb.PayloadProvider.P2P.RestAPI.Server
(
  somePayloadServer

-- * Single Chain Server
, payloadApp
, payloadApiLayout
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

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
import Chainweb.PayloadProvider.P2P.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.Utils (int)
import Chainweb.Version

import Chainweb.BlockHeight (BlockHeight)
import Chainweb.Storage.Table
import Chainweb.BlockPayloadHash

-- -------------------------------------------------------------------------- --
-- Utils

err404Msg :: ToJSON msg  => msg -> ServerError
err404Msg msg = setErrJSON msg err404

-- -------------------------------------------------------------------------- --
-- GET Payload Handler

-- | Query the 'BlockPayload' by its 'BlockPayloadHash'
--
payloadHandler
    :: forall tbl (p :: PayloadProviderType)
    . ReadableTable tbl RankedBlockPayloadHash (PayloadType p)
    => IsPayloadProvider p
    => tbl
    -> BlockHeight
    -> BlockPayloadHash
    -> Handler (RestPayload (PayloadType p))
payloadHandler db h k = liftIO (tableLookup db rh) >>= \case
    Nothing -> throwError $ err404Msg $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= k
        ]
    Just e -> return $ RestPayload e
 where
    rh = RankedBlockPayloadHash h k

-- -------------------------------------------------------------------------- --
-- POST Payload Batch Handler

payloadBatchHandler
    :: forall tbl (p :: PayloadProviderType)
    . ReadableTable tbl RankedBlockPayloadHash (PayloadType p)
    => IsPayloadProvider p
    => PayloadBatchLimit
    -> tbl
    -> BatchBody
    -> Handler (RestPayload (PayloadBatchType p))
payloadBatchHandler 0 _ _ = throwError $ err404Msg @String "payload batch limit is 0"
payloadBatchHandler batchLimit db (BatchBody ks) = liftIO $ do
    r <- batch @p <$> tableLookupBatch db (limit ks)
    return $ RestPayload r
  where
    limit = take $ int (min (p2pPayloadBatchLimit @p) batchLimit)

-- -------------------------------------------------------------------------- --
-- Payload API Server

payloadServer
    :: forall tbl v c p
    . ReadableTable tbl RankedBlockPayloadHash (PayloadType p)
    => IsPayloadProvider p
    => PayloadBatchLimit
    -> tbl
    -> Server (PayloadApi v c p)
payloadServer batchLimit db
    = payloadHandler @tbl @p db
    :<|> payloadBatchHandler @tbl @p batchLimit db

-- -------------------------------------------------------------------------- --
-- Application for a single PayloadDb

payloadApp
    :: forall tbl v c p
    . ReadableTable tbl RankedBlockPayloadHash (PayloadType p)
    => IsPayloadProvider p
    => KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PayloadBatchLimit
    -> tbl
    -> Application
payloadApp batchLimit db = serve
    (Proxy @(PayloadApi v c p))
    (payloadServer @tbl @v @c @p batchLimit db)

payloadApiLayout
    :: forall tbl v c (p :: PayloadProviderType)
    . KnownChainwebVersionSymbol v
    => IsPayloadProvider p
    => KnownChainIdSymbol c
    => tbl
    -> IO ()
payloadApiLayout _ = T.putStrLn $ layout (Proxy @(PayloadApi v c p))

-- -------------------------------------------------------------------------- --
-- Multichain Server

somePayloadServer
    :: forall tbl v c p
    . ReadableTable tbl RankedBlockPayloadHash (PayloadType p)
    => IsPayloadProvider p
    => KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PayloadBatchLimit
    -> tbl
    -> SomeServer
somePayloadServer batchLimit db = SomeServer
    (Proxy @(PayloadApi v c p))
    (payloadServer @tbl @v @c @p batchLimit db)

-- somePayloadServers
--     :: ReadableTable tbl RankedBlockPayloadHash a
--     => ChainwebVersion
--     -> PayloadBatchLimit
--     -> [(ChainId, PayloadDb tbl)]
--     -> SomeServer
-- somePayloadServers v batchLimit
--     = mconcat . fmap (somePayloadServer batchLimit . uncurry (somePayloadDbVal v))
