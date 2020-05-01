{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Rosetta.RestAPI.Server
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Rosetta.RestAPI.Server where

import Control.Error.Util
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString.Short as BSS
import qualified Data.HashMap.Strict as HM
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Pact.Types.Command

import Rosetta

import Servant.API
import Servant.Server

import Text.Read (readMaybe)

-- internal modules

import Chainweb.Mempool.Mempool
import Chainweb.Pact.RestAPI.Server (validateCommand)
import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Rosetta.RestAPI
import Chainweb.Transaction (ChainwebTransaction)
import Chainweb.Version

---

rosettaServer
    :: forall (v :: ChainwebVersionT)
    . [(ChainId, MempoolBackend ChainwebTransaction)]
    -> Server (RosettaApi v)
rosettaServer ms = (const $ error "not yet implemented")
    -- Blocks --
    :<|> (const $ error "not yet implemented")
    :<|> (const $ error "not yet implemented")
    -- Construction --
    :<|> constructionMetadataH
    :<|> constructionSubmitH ms
    -- Mempool --
    :<|> mempoolTransactionH ms
    :<|> mempoolH ms
    -- Network --
    :<|> (const $ error "not yet implemented")
    :<|> (const $ error "not yet implemented")
    :<|> (const $ error "not yet implemented")

someRosettaServer
    :: ChainwebVersion
    -> [(ChainId, MempoolBackend ChainwebTransaction)]
    -> SomeServer
someRosettaServer (FromSingChainwebVersion (SChainwebVersion :: Sing vT)) ms =
    SomeServer (Proxy @(RosettaApi vT)) $ rosettaServer ms

--------------------------------------------------------------------------------
-- Account Handlers

--------------------------------------------------------------------------------
-- Block Handlers

--------------------------------------------------------------------------------
-- Construction Handlers

constructionMetadataH :: ConstructionMetadataReq -> Handler ConstructionMetadataResp
constructionMetadataH (ConstructionMetadataReq (NetworkId _ _ msni) _) =
    runExceptT work >>= either throwRosetta pure
  where
    -- TODO: Extend as necessary.
    work :: ExceptT RosettaFailure Handler ConstructionMetadataResp
    work = do
        SubNetworkId _ _ <- msni ?? RosettaChainUnspecified
        pure $ ConstructionMetadataResp HM.empty

constructionSubmitH
    :: [(ChainId, MempoolBackend ChainwebTransaction)]
    -> ConstructionSubmitReq
    -> Handler ConstructionSubmitResp
constructionSubmitH ms (ConstructionSubmitReq (NetworkId _ _ msni) tx) =
    runExceptT work >>= either throwRosetta pure
  where
    work :: ExceptT RosettaFailure Handler ConstructionSubmitResp
    work = do
        SubNetworkId n _ <- msni ?? RosettaChainUnspecified
        cmd <- command tx ?? RosettaUnparsableTx
        validated <- hoistEither . first (const RosettaInvalidTx) $ validateCommand cmd
        mp <- (readMaybe (T.unpack n) >>= flip lookup ms) ?? RosettaInvalidChain n
        let !vec = V.singleton validated
        liftIO (mempoolInsertCheck mp vec) >>= hoistEither . first (const RosettaInvalidTx)
        liftIO (mempoolInsert mp UncheckedInsert vec)
        let rk = requestKeyToB16Text $ cmdToRequestKey validated
        pure $ ConstructionSubmitResp (TransactionId rk) Nothing

command :: T.Text -> Maybe (Command T.Text)
command = decodeStrict' . T.encodeUtf8

--------------------------------------------------------------------------------
-- Mempool Handlers

mempoolH :: [(ChainId, MempoolBackend a)] -> MempoolReq -> Handler MempoolResp
mempoolH ms (MempoolReq (NetworkId _ _ msni)) = case msni of
    Nothing -> throwRosetta RosettaChainUnspecified
    Just (SubNetworkId n _) ->
        case readMaybe @ChainId (T.unpack n) >>= flip lookup ms of
            Nothing -> throwRosetta $ RosettaInvalidChain n
            Just _ -> do
                error "not yet implemented"  -- TODO!

mempoolTransactionH
    :: [(ChainId, MempoolBackend a)]
    -> MempoolTransactionReq
    -> Handler MempoolTransactionResp
mempoolTransactionH ms mtr = runExceptT work >>= either throwRosetta pure
  where
    MempoolTransactionReq (NetworkId _ _ msni) (TransactionId ti) = mtr
    th = TransactionHash . BSS.toShort $ T.encodeUtf8 ti

    f :: LookupResult a -> Maybe MempoolTransactionResp
    f Missing = Nothing
    f (Pending _) = Just $ MempoolTransactionResp tx Nothing
      where
        tx = Transaction
          { _transaction_transactionId = TransactionId ti
          , _transaction_operations = [] -- TODO!
          , _transaction_metadata = Nothing
          }

    work :: ExceptT RosettaFailure Handler MempoolTransactionResp
    work = do
        SubNetworkId n _ <- msni ?? RosettaChainUnspecified
        mp <- (readMaybe (T.unpack n) >>= flip lookup ms) ?? RosettaInvalidChain n
        lrs <- liftIO . mempoolLookup mp $ V.singleton th
        (lrs V.!? 0 >>= f) ?? RosettaMempoolBadTx

--------------------------------------------------------------------------------
-- Network Handlers
