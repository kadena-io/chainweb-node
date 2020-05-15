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
import Data.Map (Map)
import Data.Proxy (Proxy(..))
import Data.Tuple.Strict (T2(..))
import Data.Word (Word64)

import qualified Data.ByteString.Short as BSS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Memory.Endian as BA
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Numeric.Natural

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Runtime (TxId(..), TxLog(..))

import Rosetta

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.BlockCreationTime (BlockCreationTime(..))
import Chainweb.BlockHash (blockHashToText)
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.HostAddress
import Chainweb.Mempool.Mempool
import Chainweb.Pact.RestAPI.Server (validateCommand)
import qualified Chainweb.RestAPI.NetworkID as ChainwebNetId
import Chainweb.RestAPI.Utils
import Chainweb.Rosetta.RestAPI
import Chainweb.Time
import Chainweb.Transaction (ChainwebTransaction)
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

import P2P.Node.PeerDB
import P2P.Node.RestAPI.Server (peerGetHandler)
import P2P.Peer

---

rosettaServer
    :: forall cas (v :: ChainwebVersionT)
    . ChainwebVersion
    -> [(ChainId, MempoolBackend ChainwebTransaction)]
    -> PeerDb
    -> CutDb cas
    -> Server (RosettaApi v)
rosettaServer v ms peerDb cutDb = (const $ error "not yet implemented")
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
    -> CutDb cas
    -> SomeServer
someRosettaServer v@(FromSingChainwebVersion (SChainwebVersion :: Sing vT)) ms pdb cdb =
    SomeServer (Proxy @(RosettaApi vT)) $ rosettaServer v ms pdb cdb

--------------------------------------------------------------------------------
-- Account Handlers

--------------------------------------------------------------------------------
-- Block Handlers

type CoinbaseCommandResult = CommandResult Hash

newtype FundTxLogs = FundTxLogs (T2 TxId [TxLog Value])
newtype GasPaymentLogs = GasPaymentLogs (T2 TxId [TxLog Value])

data RosettaOperationStatus =
    Success
  | LockedInPact -- TODO: Think about.
  | UnlockedReverted -- TODO: Think about in case of rollback (same chain pacts)?
  | UnlockedTransfer -- TOOD: pacts finished, cross-chain?
  deriving (Enum, Bounded, Show)

data OperationType =
    CoinbaseReward
  | FundTx
  | GasRefundAndPayment
  | GasReward
  | TransferOrCreateAcct
  deriving (Enum, Bounded, Show)

blockH :: ChainwebVersion -> BlockReq -> Handler BlockResp
blockH v (BlockReq net (PartialBlockId bheight bhash)) =
  runExceptT work >>= either throwRosetta pure
  where
    block :: BlockHeader -> [Transaction] -> Block
    block bh txs = Block
      { _block_blockId = blockId bh
      , _block_parentBlockId = parentBlockId bh
      , _block_timestamp = timestamp bh
      , _block_transactions = txs
      , _block_metadata = Nothing
      }

    getTxLogs :: BlockHeader -> IO (Map TxId [TxLog Value])
    getTxLogs = undefined

    work :: ExceptT RosettaFailure Handler BlockResp
    work = do
      cid <- validateNetwork v net
      bh <- findBlockHeaderInCurrFork v cid bheight bhash
      (coinbaseOut, txsOut) <- getBlockOutputs bh
      logs <- liftIO $ getTxLogs bh

      undefined

txLogToOperation
    :: Word64
    -> TxId
    -> TxLog Value
    -> OperationType
    -> RosettaOperationStatus
    -> ExceptT RosettaFailure Handler Operation
txLogToOperation idx txid txlog otype ostatus = do
  pure $ Operation
    { _operation_operationId = OperationId idx Nothing
    , _operation_relatedOperations = Nothing -- TODO
    , _operation_type = sshow otype
    , _operation_status = sshow ostatus
    , _operation_account = Just undefined
    , _operation_amount = Just undefined
    , _operation_metadata = Just undefined
    }

getBlockOutputs
    :: BlockHeader
    -> ExceptT RosettaFailure Handler (CoinbaseCommandResult, Map RequestKey (CommandResult Hash))
getBlockOutputs = undefined

-- /cut to get current fork's chain hash
-- chain/1/header/branch?minheight=567820&maxheight=567820
-- /chain/1/payload/3x3f6kTQMBywYF8uBcDXHH7DW7Ah_NCHcEzhnI4FHjc=/outputs   (to get the outputs)
-- In the current fork
findBlockHeaderInCurrFork
    :: ChainwebVersion
    -> ChainId
    -> Maybe Word64
    -> Maybe T.Text
    -> ExceptT RosettaFailure Handler BlockHeader
findBlockHeaderInCurrFork = undefined

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
      , _networkStatusResp_currentBlockTimestamp = timestamp bh
      , _networkStatusResp_genesisBlockId = blockId genesis
      , _networkStatusResp_peers = rosettaNodePeers ps
      }

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

-- | If its the genesis block, Rosetta wants the parent block to be itself.
--   Otherwise, fetch the parent header from the block.
parentBlockId :: BlockHeader -> BlockId
parentBlockId bh
  | (_blockHeight bh == 0) = blockId bh  -- genesis
  | otherwise = parent
  where parent = BlockId
          { _blockId_index = _height (pred $ _blockHeight bh)
          , _blockId_hash = blockHashToText (_blockParent bh)
          }

blockId :: BlockHeader -> BlockId
blockId bh = BlockId
  { _blockId_index = _height (_blockHeight bh)
  , _blockId_hash = blockHashToText (_blockHash bh)
  }

-- Timestamp of the block in milliseconds since the Unix Epoch.
-- NOTE: Chainweb provides this timestamp in microseconds.
timestamp :: BlockHeader -> Word64
timestamp bh = BA.unLE . BA.toLE $ fromInteger msTime
  where
    msTime = int $ microTime `div` ms
    TimeSpan ms = millisecond
    microTime = encodeTimeToWord64 $ _bct (_blockCreationTime bh)
