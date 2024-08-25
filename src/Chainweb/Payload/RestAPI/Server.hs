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
  somePayloadServer
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

import Chainweb.BlockHeight (BlockHeight)
import Chainweb.Logger
import System.LogLevel
import qualified Data.Aeson as A

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
    -> Maybe BlockHeight
    -> Handler PayloadData
payloadHandler db k mh = liftIO (lookupPayloadDataWithHeight db mh k) >>= \case
    Nothing -> throwError $ err404Msg $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= k
        ]
    Just e -> return e

-- -------------------------------------------------------------------------- --
-- POST Payload Batch Handler

payloadBatchHandler
    :: (CanReadablePayloadCas tbl, Logger logger)
    => logger
    -> PayloadBatchLimit
    -> PayloadDb tbl
    -> BatchBody
    -> Handler PayloadDataList
payloadBatchHandler logger _batchLimit _db ks = do
    liftIO (logFunctionJson logger Warn $ A.object [ "batch" .= ks ])
    throwError (ServerError 404 "" "" [])

-- -------------------------------------------------------------------------- --
-- GET Outputs Handler

-- | Query the 'PayloadWithOutputs' by its 'BlockPayloadHash'
--
outputsHandler
    :: CanReadablePayloadCas tbl
    => PayloadDb tbl
    -> BlockPayloadHash
    -> Maybe BlockHeight
    -> Handler PayloadWithOutputs
outputsHandler db k mh = liftIO (lookupPayloadWithHeight db mh k) >>= \case
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
    -> BatchBody
    -> Handler PayloadWithOutputsList
outputsBatchHandler batchLimit db ks
  = liftIO (PayloadWithOutputsList . catMaybes <$> lookupPayloadWithHeightBatch db ks')
  where
      limit = take (int batchLimit)
      ks' | WithoutHeights xs <- ks = limit (fmap (Nothing,) xs)
          | WithHeights    xs <- ks = limit (fmap (over _1 Just) xs)


-- -------------------------------------------------------------------------- --
-- Payload API Server

payloadServer
    :: forall tbl v c logger
    . CanReadablePayloadCas tbl
    => Logger logger
    => logger
    -> PayloadBatchLimit
    -> PayloadDb' tbl v c
    -> Server (PayloadApi v c)
payloadServer logg batchLimit (PayloadDb' db)
    = payloadHandler @tbl db
    :<|> outputsHandler @tbl db
    :<|> payloadBatchHandler @tbl logg batchLimit db
    :<|> outputsBatchHandler @tbl batchLimit db

-- -------------------------------------------------------------------------- --
-- Application for a single PayloadDb

payloadApp
    :: forall tbl v c logger
    . CanReadablePayloadCas tbl
    => KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Logger logger
    => logger
    -> PayloadBatchLimit
    -> PayloadDb' tbl v c
    -> Application
payloadApp logg batchLimit db = serve (Proxy @(PayloadApi v c)) (payloadServer logg batchLimit db)

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
    => Logger logger => logger
    -> PayloadBatchLimit
    -> SomePayloadDb tbl
    -> SomeServer
somePayloadServer logg batchLimit (SomePayloadDb (db :: PayloadDb' tbl v c))
    = SomeServer (Proxy @(PayloadApi v c)) (payloadServer logg batchLimit db)

somePayloadServers
    :: CanReadablePayloadCas tbl
    => Logger logger => logger
    -> ChainwebVersion
    -> PayloadBatchLimit
    -> [(ChainId, PayloadDb tbl)]
    -> SomeServer
somePayloadServers logg v batchLimit
    = mconcat . fmap (somePayloadServer logg batchLimit . uncurry (somePayloadDbVal v))
