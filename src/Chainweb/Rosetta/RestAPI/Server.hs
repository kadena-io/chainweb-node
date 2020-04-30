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

import Control.Error.Safe (assertMay)
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

import Chainweb.Cut.CutHashes (cutToCutHashes)
import Chainweb.CutDB
import Chainweb.Mempool.Mempool
import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Rosetta.RestAPI
import Chainweb.Version

---

rosettaServer
    :: forall a cas (v :: ChainwebVersionT)
    . [(ChainId, MempoolBackend a)]
    -> ChainwebVersion
    -> CutDb cas
    -> Server (RosettaApi v)
rosettaServer ms v cutDb = (const $ error "not yet implemented")
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
    -> CutDb cas
    -> SomeServer
someRosettaServer v@(FromSingChainwebVersion (SChainwebVersion :: Sing vT)) ms cdb =
    SomeServer (Proxy @(RosettaApi vT)) $ rosettaServer ms v cdb

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
        mp <- (readChainId n >>= flip lookup ms) ?? RosettaInvalidChain n
        lrs <- liftIO . mempoolLookup mp $ V.singleton th
        (lrs V.!? 0 >>= f) ?? RosettaMempoolBadTx

--------------------------------------------------------------------------------
-- Network Handlers

networkListH :: MetadataReq -> ChainwebVersion -> Handler NetworkListResp
networkListH _ v
  | (isRosettaEnabled v) = pure $ NetworkListResp (networkIds v)
  | otherwise = throwRosetta $ RosettaNotSupported v


{--
data NetworkStatusResponse = NetworkStatusResponse
  { _NetworkStatusResponse_currentBlockIdentifier :: BlockIdentifier
  , _NetworkStatusResponse_currentBlockTimestamp :: Word64
  -- ^ Timestamp of the block in milliseconds since the Unix Epoch.
  , _NetworkStatusResponse_genesisBlockIdentifier :: BlockIdentifier
  , _NetworkStatusResponse_peers :: [Peer]
  }
--}

networkStatusH :: NetworkReq -> ChainwebVersion -> CutDb cas -> Handler NetworkStatusResp
networkStatusH (NetworkReq nid _) v db = do
  cid <- runExceptT (enforceValidNetworkId v nid)
         >>= either throwRosetta pure
  cuts <- liftIO $ _cut db
  undefined

--------------------------------------------------------------------------------
-- Utils

blockchainName :: T.Text
blockchainName = "kadena"

readChainId :: T.Text -> Maybe ChainId
readChainId = chainIdFromText

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

isValidChainId :: ChainwebVersion -> ChainId -> Bool
isValidChainId v cid = HS.member cid (chainIds v)

-- Unique Rosetta network ids for each of the chainweb version's chain ids
networkIds :: ChainwebVersion -> [NetworkId]
networkIds v = map f (HS.toList (chainIds v))
  where
    f :: ChainId -> NetworkId
    f cid =  NetworkId
      { _networkId_blockchain = blockchainName
      , _networkId_network = (chainwebVersionToText v)
      , _networkId_subNetworkId = Just (SubNetworkId (chainIdToText cid) Nothing)
      }

enforceValidNetworkId
  :: ChainwebVersion
  -> NetworkId
  -> ExceptT RosettaFailure Handler ChainId
enforceValidNetworkId v (NetworkId bn rv sid) = do
  enforce (isRosettaEnabled v) (RosettaNotSupported v)
  enforce isValidBlockchainName (RosettaInvalidBlockchainName bn)
  enforce isValidNetworkVersion (RosettaMismatchNetworkName v rv)
  SubNetworkId cidt _ <- sid ?? RosettaChainUnspecified
  cid <- (readChainId cidt) ?? (RosettaInvalidChain cidt)
  enforce (isValidChainId v cid) (RosettaInvalidChain cidt)
  pure cid

  where
    enforce :: Bool -> RosettaFailure -> ExceptT RosettaFailure Handler ()
    enforce p e = assertMay p ?? e

    isValidBlockchainName = bn == blockchainName
    isValidNetworkVersion = rv == (chainwebVersionToText v)
