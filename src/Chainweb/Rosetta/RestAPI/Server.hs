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

import Control.Error.Safe (assertMay)
import Control.Error.Util
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Bifunctor
import Data.Proxy (Proxy(..))
import Data.Word (Word64)

import qualified Data.ByteString.Short as BSS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Memory.Endian as BA
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Pact.Types.Command

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
import Chainweb.Pact.RestAPI.Server (validateCommand)
import Chainweb.RestAPI.Utils
import Chainweb.Rosetta.RestAPI
import Chainweb.Time
import Chainweb.Utils (int)
import Chainweb.Utils.Paging
import Chainweb.Transaction (ChainwebTransaction)
import Chainweb.Version

import P2P.Node.PeerDB
import P2P.Node.RestAPI.Server (peerGetHandler)
import P2P.Peer (PeerInfo(..))

---

rosettaServer
    :: forall cas (v :: ChainwebVersionT)
    . [(ChainId, MempoolBackend ChainwebTransaction)]
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
    :<|> constructionSubmitH ms
    -- Mempool --
    :<|> mempoolTransactionH v ms
    :<|> mempoolH v ms
    -- Network --
    :<|> networkListH v
    :<|> networkOptionsH v
    :<|> (networkStatusH v cutDb peerDb)

someRosettaServer
    :: ChainwebVersion
    -> [(ChainId, MempoolBackend ChainwebTransaction)]
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

mempoolH
    :: ChainwebVersion
    -> [(ChainId, MempoolBackend a)]
    -> MempoolReq
    -> Handler MempoolResp
mempoolH v ms (MempoolReq (NetworkId _ _ msni)) = case msni of
    Nothing -> throwRosetta RosettaChainUnspecified
    Just (SubNetworkId n _) ->
        case readChainIdText v n >>= flip lookup ms of
            Nothing -> throwRosetta $ RosettaInvalidChain n
            Just _ -> do
                error "not yet implemented"  -- TODO!

mempoolTransactionH
    :: ChainwebVersion
    -> [(ChainId, MempoolBackend a)]
    -> MempoolTransactionReq
    -> Handler MempoolTransactionResp
mempoolTransactionH v ms mtr = runExceptT work >>= either throwRosetta pure
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

networkOptionsH :: ChainwebVersion -> NetworkReq -> Handler NetworkOptionsResp
networkOptionsH v (NetworkReq nid _) = do
  _ <- handle (enforceValidNetworkId v nid)
  pure $ NetworkOptionsResp version allow
  where
    version = RosettaNodeVersion
      { _version_rosettaVersion = "1.3.1"   -- TODO: Make this a variable in /rosetta repo
      , _version_nodeVersion = chainwebNodeVersionHeaderValue :: T.Text
      , _version_middlewareVersion = Nothing
      , _version_metadata = Just $ HM.fromList metaPairs
      }
    -- TODO: Document this meta data
    metaPairs =
      [ "node-api-version" .= prettyApiVersion
      , "chainweb-version" .= (chainwebVersionToText v)
      ]
    allow = Allow
      { _allow_operationStatuses = [] -- TODO
      , _allow_operationTypes = [] -- TODO
      , _allow_errors = errExamples
      }
    errExamples = map rosettaError [minBound..maxBound]

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
        -- TODO: document this meta data
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
