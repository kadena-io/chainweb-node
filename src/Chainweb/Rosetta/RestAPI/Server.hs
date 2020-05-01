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
import Data.Aeson ((.=))
import Data.Proxy (Proxy(..))
import Data.Word (Word64)

import qualified Data.ByteString.Short as BSS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Memory.Endian as BA
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Rosetta

import Servant.API
import Servant.Server

import Text.Read (readMaybe)

-- internal modules

import Chainweb.BlockCreationTime (BlockCreationTime(..))
import Chainweb.BlockHash (blockHashToText)
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Cut (_cutMap)
import Chainweb.CutDB
import Chainweb.HostAddress
import Chainweb.Mempool.Mempool
import qualified Chainweb.RestAPI.NetworkID as ChainwebNetId
import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Rosetta.RestAPI
import Chainweb.Time
import Chainweb.Utils (int)
import Chainweb.Utils.Paging
import Chainweb.Version

import P2P.Node.PeerDB
import P2P.Node.RestAPI.Server (peerGetHandler)
import P2P.Peer (PeerInfo(..))

---

rosettaServer
    :: forall a cas (v :: ChainwebVersionT)
    . [(ChainId, MempoolBackend a)]
    -> PeerDb
    -> ChainwebVersion
    -> CutDb cas
    -> Server (RosettaApi v)
rosettaServer ms peerDb v cutDb = (const $ error "not yet implemented")
    -- Blocks --
    :<|> (const $ error "not yet implemented")
    :<|> (const $ error "not yet implemented")
    -- Construction --
    :<|> constructionMetadataH
    :<|> (const $ error "not yet implemented")
    -- Mempool --
    :<|> flip (mempoolTransactionH v) ms
    :<|> flip (mempoolH v) ms
    -- Network --
    :<|> networkListH v
    :<|> (const $ error "not yet implemented")
    :<|> (networkStatusH v cutDb peerDb)

someRosettaServer
    :: ChainwebVersion
    -> [(ChainId, MempoolBackend a)]
    -> PeerDb
    -> CutDb cas
    -> SomeServer
someRosettaServer v@(FromSingChainwebVersion (SChainwebVersion :: Sing vT)) ms pdb cdb =
    SomeServer (Proxy @(RosettaApi vT)) $ rosettaServer ms pdb v cdb

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

mempoolH
    :: ChainwebVersion
    -> MempoolReq
    -> [(ChainId, MempoolBackend a)]
    -> Handler MempoolResp
mempoolH v (MempoolReq (NetworkId _ _ msni)) ms = case msni of
    Nothing -> throwRosetta RosettaChainUnspecified
    Just (SubNetworkId n _) ->
        case readChainIdText v n >>= flip lookup ms of
            Nothing -> throwRosetta $ RosettaInvalidChain n
            Just _ -> do
                error "not yet implemented"  -- TODO!

mempoolTransactionH
    :: ChainwebVersion
    -> MempoolTransactionReq
    -> [(ChainId, MempoolBackend a)]
    -> Handler MempoolTransactionResp
mempoolTransactionH v mtr ms = runExceptT work >>= either throwRosetta pure
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
        mp <- (readChainIdText v n >>= flip lookup ms) ?? RosettaInvalidChain n
        lrs <- liftIO . mempoolLookup mp $ V.singleton th
        (lrs V.!? 0 >>= f) ?? RosettaMempoolBadTx

--------------------------------------------------------------------------------
-- Network Handlers

networkListH :: ChainwebVersion -> MetadataReq -> Handler NetworkListResp
networkListH v _ = pure $ NetworkListResp networkIds
  where
    -- Unique Rosetta network ids for each of the chainweb version's chain ids
    networkIds = map f (HS.toList (chainIds v))
    f :: ChainId -> NetworkId
    f cid =  NetworkId
      { _networkId_blockchain = blockchainName
      , _networkId_network = (chainwebVersionToText v)
      , _networkId_subNetworkId = Just (SubNetworkId (chainIdToText cid) Nothing)
      }

networkStatusH
    :: ChainwebVersion
    -> CutDb cas
    -> PeerDb
    -> NetworkReq
    -> Handler NetworkStatusResp
networkStatusH v cutDb peerDb (NetworkReq nid _) = do
  cid <- handle (enforceValidNetworkId v nid)
  cut' <- liftIO $ _cut cutDb
  bh <- handle (getBlockHeader cut' cid)
  let genesisBh = genesisBlockHeader v cid

  -- TODO: Will this throw Handler error? How to wrap as Rosetta Error?
  peers <- _pageItems <$>
    peerGetHandler
    peerDb
    ChainwebNetId.CutNetwork
    (Just (Limit maxRosettaNodePeerLimit)) -- TODO: document max number of peers returned
    Nothing

  pure $ resp bh genesisBh peers
  where
    getBlockHeader c i =
      (HM.lookup i (_cutMap c)) ??
      (RosettaInvalidChain (chainIdToText i))
    resp bh genesis ps = NetworkStatusResp
      { _networkStatusResp_currentBlockId = blockId bh
      , _networkStatusResp_currentBlockTimestamp = currTimestamp bh
      , _networkStatusResp_genesisBlockId = blockId genesis
      , _networkStatusResp_peers = rosettaNodePeers ps
      }

    blockId :: BlockHeader -> BlockId
    blockId bh = BlockId
      { _blockId_index = _height (_blockHeight bh)
      , _blockId_hash = blockHashToText (_blockHash bh)
      }

    -- Timestamp of the block in milliseconds since the Unix Epoch.
    -- NOTE: Chainweb provides this timestamp in microseconds.
    currTimestamp :: BlockHeader -> Word64
    currTimestamp bh = BA.unLE . BA.toLE $ fromInteger msTime
      where
        msTime = int $ microTime `div` ms
        (TimeSpan ms) = millisecond
        microTime = encodeTimeToWord64 $ _bct (_blockCreationTime bh)

    rosettaNodePeers :: [PeerInfo] -> [RosettaNodePeer]
    rosettaNodePeers ps = map f ps
      where
        f p = RosettaNodePeer
          { _peer_peerId = hostAddressToText (_peerAddr p)
          , _peer_metadata = Just $ HM.fromList (metaPairs p)
          }
        metaPairs p =
          addrPairs (_peerAddr p) ++ someCertPair (_peerId p)
        addrPairs addr =
          [ "address_hostname" .= hostnameToText (_hostAddressHost addr)
          , "address_port" .= portToText (_hostAddressPort addr)
          -- TODO: document that port is string represation of Word16
          ]
        someCertPair (Just i) = ["certificate_id" .= i]
        someCertPair Nothing = []

--------------------------------------------------------------------------------
-- Utils

maxRosettaNodePeerLimit :: Num a => a
maxRosettaNodePeerLimit = 64

blockchainName :: T.Text
blockchainName = "kadena"

handle :: ExceptT RosettaFailure Handler a -> Handler a
handle et = runExceptT et >>= either throwRosetta pure

readChainIdText :: ChainwebVersion -> T.Text -> Maybe ChainId
readChainIdText v c = do
  cid :: Word64 <- readMaybe (T.unpack c)
  mkChainId v cid

enforceValidNetworkId
  :: ChainwebVersion
  -> NetworkId
  -> ExceptT RosettaFailure Handler ChainId
enforceValidNetworkId v (NetworkId bn rv sid) = do
  enforce isValidBlockchainName (RosettaInvalidBlockchainName bn)
  enforce isValidNetworkVersion (RosettaMismatchNetworkName v rv)
  SubNetworkId cidt _ <- sid ?? RosettaChainUnspecified
  (readChainIdText v cidt) ?? (RosettaInvalidChain cidt)

  where
    enforce :: Bool -> RosettaFailure -> ExceptT RosettaFailure Handler ()
    enforce p e = assertMay p ?? e

    isValidBlockchainName = bn == blockchainName
    isValidNetworkVersion = rv == (chainwebVersionToText v)
