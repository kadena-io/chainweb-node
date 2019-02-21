{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Payload.RestAPI.Server
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
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
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Data.Aeson
import Data.Proxy
import qualified Data.Text.IO as T

import Prelude hiding (lookup)

import Servant.Server

-- internal modules

import Chainweb.ChainId
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

import Data.CAS

-- -------------------------------------------------------------------------- --
-- Handler

-- | Query the 'BlockPayload' by its 'BlockPayloadHash'
--
payloadHandler :: PayloadCas cas => PayloadDb cas -> BlockPayloadHash -> Handler PayloadData
payloadHandler db k = run >>= \case
    Nothing -> throwError $ err404Msg $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= (sshow k :: String)
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

err404Msg :: ToJSON msg  => msg -> ServantErr
err404Msg msg = ServantErr
    { errHTTPCode = 404
    , errReasonPhrase = "Not Found"
    , errBody = encode msg
    , errHeaders = []
    }

-- -------------------------------------------------------------------------- --
-- Payload API Server

payloadServer :: PayloadCas cas => PayloadDb_ cas v c -> Server (PayloadApi v c)
payloadServer (PayloadDb_ db) = payloadHandler db

-- -------------------------------------------------------------------------- --
-- Application for a single PayloadDb

payloadApp
    :: forall cas v c
    . PayloadCas cas
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

somePayloadServer :: PayloadCas cas => SomePayloadDb cas -> SomeServer
somePayloadServer (SomePayloadDb (db :: PayloadDb_ cas v c))
    = SomeServer (Proxy @(PayloadApi v c)) (payloadServer db)

somePayloadServers
    :: PayloadCas cas
    => ChainwebVersion
    -> [(ChainId, PayloadDb cas)]
    -> SomeServer
somePayloadServers v = mconcat
    . fmap (somePayloadServer . uncurry (somePayloadDbVal v))
