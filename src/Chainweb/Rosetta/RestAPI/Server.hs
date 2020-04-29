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

import qualified Data.ByteString.Short as BSS
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import qualified Data.HashSet as HS

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
    -> ChainwebVersion
    -> Server (RosettaApi v)
rosettaServer ms v = (const $ error "not yet implemented")
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
    :<|> flip networkListH v
    :<|> (const $ error "not yet implemented")
    :<|> (const $ error "not yet implemented")

someRosettaServer
    :: ChainwebVersion
    -> [(ChainId, MempoolBackend a)]
    -> SomeServer
someRosettaServer v@(FromSingChainwebVersion (SChainwebVersion :: Sing vT)) ms =
    SomeServer (Proxy @(RosettaApi vT)) $ rosettaServer ms v

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

networkListH :: MetadataReq -> ChainwebVersion -> Handler NetworkListResp
networkListH _ v
  | (isRosettaEnabled v) = return $ NetworkListResp (networkIds v)
  | otherwise = throwRosetta $ RosettaNotSupported v

--------------------------------------------------------------------------------
-- Utils

rosettaBlockchainName :: T.Text
rosettaBlockchainName = "kadena"

-- Rosetta only enabled for production versions
-- NOTE: Explicit pattern match to trigger compile warning when new versions added
isRosettaEnabled :: ChainwebVersion -> Bool
isRosettaEnabled (Test _) = False
isRosettaEnabled (TimedConsensus _) = False
isRosettaEnabled (PowConsensus _) = False
isRosettaEnabled (TimedCPM _) = False
isRosettaEnabled (FastTimedCPM _) = False
isRosettaEnabled Development = True
isRosettaEnabled Testnet04 = True
isRosettaEnabled Mainnet01 = True

-- Unique Rosetta network ids for each of the chainweb version's chain ids
networkIds :: ChainwebVersion -> [NetworkId]
networkIds v = map f (HS.toList (chainIds v))
  where
    f :: ChainId -> NetworkId
    f cid =  NetworkId
      { _networkId_blockchain = rosettaBlockchainName
      , _networkId_network = (chainwebVersionToText v)
      , _networkId_subNetworkId = Just (SubNetworkId (chainIdToText cid) Nothing)
      }
