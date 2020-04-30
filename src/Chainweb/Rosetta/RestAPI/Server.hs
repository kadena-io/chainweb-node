{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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

import qualified Data.ByteString.Short as BSS
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Rosetta

import Servant.API
import Servant.Server

import Text.Read (readMaybe)

-- internal modules

import Chainweb.Mempool.Mempool
import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Rosetta.RestAPI
import Chainweb.Version

---

rosettaServer
    :: forall a (v :: ChainwebVersionT)
    . [(ChainId, MempoolBackend a)]
    -> Server (RosettaApi v)
rosettaServer ms = (const $ error "not yet implemented")
    -- Blocks --
    :<|> (const $ error "not yet implemented")
    :<|> (const $ error "not yet implemented")
    -- Construction --
    :<|> constructionMetadataH
    :<|> (const $ error "not yet implemented")
    -- Mempool --
    :<|> flip mempoolTransactionH ms
    :<|> flip mempoolH ms
    -- Network --
    :<|> (const $ error "not yet implemented")
    :<|> (const $ error "not yet implemented")
    :<|> (const $ error "not yet implemented")

someRosettaServer
    :: ChainwebVersion
    -> [(ChainId, MempoolBackend a)]
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
constructionMetadataH _ = error "not yet implemented"

--------------------------------------------------------------------------------
-- Mempool Handlers

mempoolH :: MempoolReq -> [(ChainId, MempoolBackend a)] -> Handler MempoolResp
mempoolH (MempoolReq (NetworkId _ _ msni)) ms = case msni of
    Nothing -> throwRosetta RosettaChainUnspecified
    Just (SubNetworkId n _) ->
        case readMaybe @ChainId (T.unpack n) >>= flip lookup ms of
            Nothing -> throwRosetta $ RosettaInvalidChain n
            Just _ -> do
                error "not yet implemented"  -- TODO!

mempoolTransactionH
    :: MempoolTransactionReq
    -> [(ChainId, MempoolBackend a)]
    -> Handler MempoolTransactionResp
mempoolTransactionH mtr ms = runExceptT work >>= either throwRosetta pure
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
