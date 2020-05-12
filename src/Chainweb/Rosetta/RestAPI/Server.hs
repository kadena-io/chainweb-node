{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Bifunctor
import Data.Decimal
import Data.String
import Data.Proxy (Proxy(..))
import Data.Word (Word64)

import qualified Data.ByteString.Short as BSS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Memory.Endian as BA
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Numeric.Natural

import Pact.Types.ChainMeta (PublicMeta(..))
import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.PactValue (PactValue(..))
import Pact.Types.Exp (Literal(..))

import Rosetta

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.BlockCreationTime (BlockCreationTime(..))
import Chainweb.BlockHash (blockHashToText)
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Chainweb.ChainResources (ChainResources(..))
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.HostAddress
import Chainweb.Mempool.Mempool
import Chainweb.Pact.RestAPI.Server
import qualified Chainweb.RestAPI.NetworkID as ChainwebNetId
import Chainweb.RestAPI.Utils
import Chainweb.Rosetta.RestAPI
import Chainweb.Time
import Chainweb.Transaction (ChainwebTransaction)
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version
import Chainweb.WebPactExecutionService (PactExecutionService(..))

import P2P.Node.PeerDB
import P2P.Node.RestAPI.Server (peerGetHandler)
import P2P.Peer

---

rosettaServer
    :: forall cas a (v :: ChainwebVersionT)
    . ChainwebVersion
    -> [(ChainId, MempoolBackend ChainwebTransaction)]
    -> PeerDb
    -> CutDb cas
    -> [(ChainId, ChainResources a)]
    -> Server (RosettaApi v)
rosettaServer v ms peerDb cutDb cr =
    -- Account --
    accountBalanceH v cr
    -- Blocks --
    :<|> (const $ error "not yet implemented")
    :<|> (const $ error "not yet implemented")
    -- Construction --
    :<|> constructionMetadataH v
    :<|> constructionSubmitH v ms
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
    -> [(ChainId, ChainResources a)]
    -> CutDb cas
    -> SomeServer
someRosettaServer v@(FromSingChainwebVersion (SChainwebVersion :: Sing vT)) ms pdb crs cdb =
    SomeServer (Proxy @(RosettaApi vT)) $ rosettaServer v ms pdb cdb crs

--------------------------------------------------------------------------------
-- Account Handlers
accountBalanceH
    :: ChainwebVersion
    -> [(ChainId, ChainResources a)]
    -> AccountBalanceReq
    -> Handler AccountBalanceResp
accountBalanceH _ _ (AccountBalanceReq _ _ (Just _)) = throwRosetta RosettaHistBalCheckUnsupported
accountBalanceH _ _ (AccountBalanceReq _ (AccountId _ (Just _) _) _) = throwRosetta RosettaSubAcctUnsupported
accountBalanceH v crs (AccountBalanceReq net (AccountId acct _ _) _) =
  runExceptT work >>= either throwRosetta pure
  where
    readBal :: PactValue -> Maybe Decimal
    readBal (PLiteral (LDecimal d)) = Just d
    readBal _ = Nothing

    readMeta :: Maybe Value -> Maybe (HM.HashMap T.Text Value)
    readMeta (Just (Object hm)) = Just hm
    readMeta _ = Nothing

    readBlockHeight :: Maybe Value -> Maybe Word64
    readBlockHeight Nothing = Nothing
    readBlockHeight (Just a) = case (fromJSON a) of
      Success w -> Just w
      Error _ -> Nothing

    readBlockHash :: Maybe Value -> Maybe T.Text
    readBlockHash (Just (String hsh)) = Just hsh
    readBlockHash _ = Nothing

    balCheckCmd :: ChainId -> IO (Command T.Text)
    balCheckCmd cid = do
      cmd <- mkCommand [] meta "nonce" Nothing rpc
      return $ T.decodeUtf8 <$> cmd
      where
        tableName = "coin"
        rpc = Exec $ ExecMsg code Null
        code = mconcat
          ["(at \"balance\" ",
           "(", tableName, ".details ",
           "\"", acct, "\"))"]
        meta = PublicMeta
          (fromString $ show $ chainIdToText cid)
          "someSender"
          10000
          0.0001
          300
          0

    work :: ExceptT RosettaFailure Handler AccountBalanceResp
    work = do
      cid <- validateNetwork v net
      cr <- lookup cid crs ?? RosettaInvalidChain
      cmd <- do
        c <- liftIO $ balCheckCmd cid
        (hush $ validateCommand c) ?? RosettaInvalidTx
      cRes <- do
        r <- liftIO $ _pactLocal (_chainResPact cr) cmd
        (hush r) ?? RosettaPactExceptionThrown
      let (PactResult pRes) = _crResult cRes
      pv <- (hush pRes) ?? RosettaPactErrorThrown
      balKDA <- readBal pv ?? RosettaExpectedBalDecimal
      meta <- (readMeta $ _crMetaData cRes) ?? RosettaInvalidResultMetaData

      blockHeight <- (readBlockHeight $ HM.lookup "blockHeight" meta) ?? RosettaInvalidResultMetaData
      blockHash <- (readBlockHash $ HM.lookup "prevBlockHash" meta) ?? RosettaInvalidResultMetaData

      pure $ AccountBalanceResp
        { _accountBalanceResp_blockId = BlockId blockHeight blockHash
        , _accountBalanceResp_balances = [ kdaToRosettaAmount balKDA ]
        , _accountBalanceResp_metadata = Nothing }

--------------------------------------------------------------------------------
-- Block Handlers

--------------------------------------------------------------------------------
-- Construction Handlers

constructionMetadataH
    :: ChainwebVersion
    -> ConstructionMetadataReq
    -> Handler ConstructionMetadataResp
constructionMetadataH v (ConstructionMetadataReq net _) =
    runExceptT work >>= either throwRosetta pure
  where
    -- TODO: Extend as necessary.
    work :: ExceptT RosettaFailure Handler ConstructionMetadataResp
    work = do
        void $ validateNetwork v net
        pure $ ConstructionMetadataResp HM.empty

constructionSubmitH
    :: ChainwebVersion
    -> [(ChainId, MempoolBackend ChainwebTransaction)]
    -> ConstructionSubmitReq
    -> Handler ConstructionSubmitResp
constructionSubmitH v ms (ConstructionSubmitReq net tx) =
    runExceptT work >>= either throwRosetta pure
  where
    work :: ExceptT RosettaFailure Handler ConstructionSubmitResp
    work = do
        cid <- validateNetwork v net
        cmd <- command tx ?? RosettaUnparsableTx
        validated <- hoistEither . first (const RosettaInvalidTx) $ validateCommand cmd
        mp <- lookup cid ms ?? RosettaInvalidChain
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
mempoolH v ms (MempoolReq net) = runExceptT work >>= either throwRosetta pure
  where
    work = do
        cid <- validateNetwork v net
        _ <- lookup cid ms ?? RosettaInvalidChain
        error "not yet implemented"  -- TODO!

mempoolTransactionH
    :: ChainwebVersion
    -> [(ChainId, MempoolBackend a)]
    -> MempoolTransactionReq
    -> Handler MempoolTransactionResp
mempoolTransactionH v ms mtr = runExceptT work >>= either throwRosetta pure
  where
    MempoolTransactionReq net (TransactionId ti) = mtr
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
        cid <- validateNetwork v net
        mp <- lookup cid ms ?? RosettaInvalidChain
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
      { _networkId_blockchain = "kadena"
      , _networkId_network = chainwebVersionToText v
      , _networkId_subNetworkId = Just (SubNetworkId (chainIdToText cid) Nothing)
      }

networkOptionsH :: ChainwebVersion -> NetworkReq -> Handler NetworkOptionsResp
networkOptionsH v (NetworkReq nid _) = runExceptT work >>= either throwRosetta pure
  where
    work :: ExceptT RosettaFailure Handler NetworkOptionsResp
    work = do
        void $ validateNetwork v nid
        pure $ NetworkOptionsResp version allow

    version = RosettaNodeVersion
      { _version_rosettaVersion = rosettaSpecVersion
      , _version_nodeVersion = chainwebNodeVersionHeaderValue
      , _version_middlewareVersion = Nothing
      , _version_metadata = Just $ HM.fromList metaPairs }

    -- TODO: Document this meta data
    metaPairs =
      [ "node-api-version" .= prettyApiVersion
      , "chainweb-version" .= chainwebVersionToText v ]

    allow = Allow
      { _allow_operationStatuses = [] -- TODO
      , _allow_operationTypes = [] -- TODO
      , _allow_errors = errExamples }

    errExamples :: [RosettaError]
    errExamples = map rosettaError [minBound .. maxBound]

networkStatusH
    :: ChainwebVersion
    -> CutDb cas
    -> PeerDb
    -> NetworkReq
    -> Handler NetworkStatusResp
networkStatusH v cutDb peerDb (NetworkReq nid _) =
    runExceptT work >>= either throwRosetta pure
  where
    work :: ExceptT RosettaFailure Handler NetworkStatusResp
    work = do
        cid <- validateNetwork v nid
        c <- liftIO $ _cut cutDb
        bh <- getBlockHeader c cid
        let genesisBh = genesisBlockHeader v cid
        -- TODO: Will this throw Handler error? How to wrap as Rosetta Error?
        peers <- lift $ _pageItems <$>
          peerGetHandler
          peerDb
          ChainwebNetId.CutNetwork
          -- TODO: document max number of peers returned
          (Just $ Limit maxRosettaNodePeerLimit)
          Nothing
        pure $ resp bh genesisBh peers

    getBlockHeader :: Cut -> ChainId -> ExceptT RosettaFailure Handler BlockHeader
    getBlockHeader c i = HM.lookup i (_cutMap c) ?? RosettaInvalidChain

    resp :: BlockHeader -> BlockHeader -> [PeerInfo] -> NetworkStatusResp
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
        TimeSpan ms = millisecond
        microTime = encodeTimeToWord64 $ _bct (_blockCreationTime bh)

    rosettaNodePeers :: [PeerInfo] -> [RosettaNodePeer]
    rosettaNodePeers ps = map f ps
      where
        f :: PeerInfo -> RosettaNodePeer
        f p = RosettaNodePeer
          { _peer_peerId = hostAddressToText $ _peerAddr p
          , _peer_metadata = Just . HM.fromList $ metaPairs p }

        -- TODO: document this meta data
        metaPairs :: PeerInfo -> [(T.Text, Value)]
        metaPairs p = addrPairs (_peerAddr p) ++ someCertPair (_peerId p)

        addrPairs :: HostAddress -> [(T.Text, Value)]
        addrPairs addr =
          [ "address_hostname" .= hostnameToText (_hostAddressHost addr)
          , "address_port" .= portToText (_hostAddressPort addr)
          -- TODO: document that port is string represation of Word16
          ]

        someCertPair :: Maybe PeerId -> [(T.Text, Value)]
        someCertPair (Just i) = ["certificate_id" .= i]
        someCertPair Nothing = []

--------------------------------------------------------------------------------
-- Utils

maxRosettaNodePeerLimit :: Natural
maxRosettaNodePeerLimit = 64
