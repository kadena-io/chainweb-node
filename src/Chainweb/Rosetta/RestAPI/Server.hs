{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
import Control.Monad (void, (<$!>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Bifunctor
import Data.IORef
import Data.List (sort)
import Data.Proxy (Proxy(..))

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

import Pact.Types.Command
import Pact.Types.Util (fromText')

import Rosetta

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.Chainweb.ChainResources (ChainResources(..))
import Chainweb.Cut (_cutMap)
import Chainweb.CutDB
import Chainweb.HostAddress
import Chainweb.Mempool.Mempool
import Chainweb.Pact.RestAPI.Server
import Chainweb.Payload.PayloadStore
import qualified Chainweb.RestAPI.NetworkID as ChainwebNetId
import Chainweb.RestAPI.Utils
import Chainweb.Rosetta.Internal
import Chainweb.Rosetta.RestAPI
import Chainweb.Rosetta.Utils
import Chainweb.Transaction (ChainwebTransaction)
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

import P2P.Node.PeerDB
import P2P.Node.RestAPI.Server (peerGetHandler)
import P2P.Peer

---

rosettaServer
    :: forall cas a (v :: ChainwebVersionT)
    . PayloadCasLookup cas
    => ChainwebVersion
    -> [(ChainId, PayloadDb cas)]
    -> [(ChainId, MempoolBackend ChainwebTransaction)]
    -> PeerDb
    -> CutDb cas
    -> [(ChainId, ChainResources a)]
    -> Server (RosettaApi v)
rosettaServer v ps ms peerDb cutDb cr =
    -- Account --
    accountBalanceH v cutDb cr
    -- Blocks --
    :<|> blockTransactionH v cutDb ps cr
    :<|> blockH v cutDb ps cr
    -- Construction --
    :<|> constructionDeriveH v
    :<|> constructionPreprocessH v
    :<|> constructionMetadataH v
    :<|> constructionPayloadsH v
    :<|> constructionParseH v
    :<|> constructionCombineH
    :<|> constructionHashH
    :<|> constructionSubmitH v ms
    -- Mempool --
    :<|> mempoolTransactionH v ms
    :<|> mempoolH v ms
    -- Network --
    :<|> (networkListH v cutDb)
    :<|> networkOptionsH v
    :<|> (networkStatusH v cutDb peerDb)

someRosettaServer
    :: PayloadCasLookup cas
    => ChainwebVersion
    -> [(ChainId, PayloadDb cas)]
    -> [(ChainId, MempoolBackend ChainwebTransaction)]
    -> PeerDb
    -> [(ChainId, ChainResources a)]
    -> CutDb cas
    -> SomeServer
someRosettaServer v@(FromSingChainwebVersion (SChainwebVersion :: Sing vT)) ps ms pdb crs cdb =
    SomeServer (Proxy @(RosettaApi vT)) $ rosettaServer v ps ms pdb cdb crs

--------------------------------------------------------------------------------
-- Account Handlers

accountBalanceH
    :: ChainwebVersion
    -> CutDb cas
    -> [(ChainId, ChainResources a)]
    -> AccountBalanceReq
    -> Handler AccountBalanceResp
accountBalanceH _ _ _ (AccountBalanceReq _ (AccountId _ (Just _) _) _) = throwRosetta RosettaSubAcctUnsupported
accountBalanceH v cutDb crs (AccountBalanceReq net (AccountId acct _ _) pbid) = do
  runExceptT work >>= either throwRosetta pure
  where
    acctBalResp bid bal = AccountBalanceResp
      { _accountBalanceResp_blockId = bid
      , _accountBalanceResp_balances = [ kdaToRosettaAmount bal ]
      , _accountBalanceResp_coins = Nothing
      , _accountBalanceResp_metadata = Nothing
      }

    work :: ExceptT RosettaFailure Handler AccountBalanceResp
    work = do
      cid <- hoistEither $ validateNetwork v net
      cr <- lookup cid crs ?? RosettaInvalidChain
      bh <- findBlockHeaderInCurrFork cutDb cid
        (get _partialBlockId_index pbid) (get _partialBlockId_hash pbid)
      bal <- getHistoricalLookupBalance (_chainResPact cr) bh acct
      pure $ acctBalResp (blockId bh) bal
      where
        get _ Nothing = Nothing
        get f (Just b) = f b

--------------------------------------------------------------------------------
-- Block Handlers

blockH
    :: forall a cas
    . PayloadCasLookup cas
    => ChainwebVersion
    -> CutDb cas
    -> [(ChainId, PayloadDb cas)]
    -> [(ChainId, ChainResources a)]
    -> BlockReq
    -> Handler BlockResp
blockH v cutDb ps crs (BlockReq net (PartialBlockId bheight bhash)) =
  runExceptT work >>= either throwRosetta pure
  where
    block :: BlockHeader -> [Transaction] -> Block
    block bh txs = Block
      { _block_blockId = blockId bh
      , _block_parentBlockId = parentBlockId bh
      , _block_timestamp = rosettaTimestamp bh
      , _block_transactions = txs
      , _block_metadata = Nothing
      }

    work :: ExceptT RosettaFailure Handler BlockResp
    work = do
      cid <- hoistEither $ validateNetwork v net
      cr <- lookup cid crs ?? RosettaInvalidChain
      payloadDb <- lookup cid ps ?? RosettaInvalidChain
      bh <- findBlockHeaderInCurrFork cutDb cid bheight bhash
      (coinbase, txs) <- getBlockOutputs payloadDb bh
      logs <- getTxLogs (_chainResPact cr) bh
      trans <- matchLogs FullLogs bh logs coinbase txs
      pure $ BlockResp
        { _blockResp_block = Just $ block bh trans
        , _blockResp_otherTransactions = Nothing
        }

blockTransactionH
    :: forall a cas
    . PayloadCasLookup cas
    => ChainwebVersion
    -> CutDb cas
    -> [(ChainId, PayloadDb cas)]
    -> [(ChainId, ChainResources a)]
    -> BlockTransactionReq
    -> Handler BlockTransactionResp
blockTransactionH v cutDb ps crs (BlockTransactionReq net bid t) = do
  runExceptT work >>= either throwRosetta pure
  where
    BlockId bheight bhash = bid
    TransactionId rtid = t

    work :: ExceptT RosettaFailure Handler BlockTransactionResp
    work = do
      cid <- hoistEither $ validateNetwork v net
      cr <- lookup cid crs ?? RosettaInvalidChain
      payloadDb <- lookup cid ps ?? RosettaInvalidChain
      bh <- findBlockHeaderInCurrFork cutDb cid (Just bheight) (Just bhash)
      rkTarget <- (hush $ fromText' rtid) ?? RosettaUnparsableTransactionId
      (coinbase, txs) <- getBlockOutputs payloadDb bh
      logs <- getTxLogs (_chainResPact cr) bh
      tran <- matchLogs (SingleLog rkTarget) bh logs coinbase txs

      pure $ BlockTransactionResp tran


--------------------------------------------------------------------------------
-- Construction Handlers
-- NOTE: all Construction API endpoints except /metadata and /submit must
-- operate in "offline" mode.

constructionDeriveH
    :: ChainwebVersion
    -> ConstructionDeriveReq
    -> Handler ConstructionDeriveResp
constructionDeriveH _ _ =
   -- NOTE: Blockchains that require an on-chain action to create
   -- an account should not implement this method
  throwRosetta RosettaConstructionDeriveNotSupported


constructionPreprocessH
    :: ChainwebVersion
    -> ConstructionPreprocessReq
    -> Handler ConstructionPreprocessResp
constructionPreprocessH v req = do
    either throwRosetta pure (void $ validateNetwork v net)
    either throwRosettaError pure work
  where
    ConstructionPreprocessReq net ops someMeta someMaxFee someMult = req
    xchainMeta = _constructionPreprocessReqMetaData_crossChainTxMetaData
    
    work :: Either RosettaError ConstructionPreprocessResp
    work = do
      meta <- note (rosettaError RosettaMissingMetaData Nothing) someMeta
      parsedMeta <- extractMetaData meta
      tx <- parseOps net (xchainMeta parsedMeta) ops
      let (gasLimit, gasPrice, fee) = getSuggestedFee tx someMaxFee someMult
          respMeta = toObject $ ConstructionPreprocessRespMetaData
            { _constructionPreprocessRespMetaData_preprocessMetaData = parsedMeta
            , _constructionPreprocessRespMetaData_tx = tx
            , _constructionPreprocessRespMetaData_suggestedFee = fee
            , _constructionPreprocessRespMetaData_gasLimit = gasLimit
            , _constructionPreprocessRespMetaData_gasPrice = gasPrice
            }
      pure $ ConstructionPreprocessResp (Just respMeta)


constructionMetadataH
    :: ChainwebVersion
    -> ConstructionMetadataReq
    -> Handler ConstructionMetadataResp
constructionMetadataH v (ConstructionMetadataReq net opts) =
    runExceptT work >>= either throwRosettaError pure
  where
    validateNetwork' = annotate (\f -> rosettaError f Nothing)
                       (validateNetwork v net)
    
    work :: ExceptT RosettaError Handler ConstructionMetadataResp
    work = do
      cid <- hoistEither validateNetwork'
      (ConstructionPreprocessRespMetaData meta tx fee gasLimit gasPrice) <-
        hoistEither $ extractMetaData opts
      let (ConstructionPreprocessReqMetaData xchainMeta payer someNonce) = meta

      tx' <- (liftIO $ txWithSPVProofIfNeeded tx) >>= hoistEither
          
      
      let pubMeta = createCmdPublicMeta cid payer gasLimit gasPrice
          nonce = getCmdNonce someNonce
          payloadMeta = ConstructionPayloadsReqMetaData
            { _constructionPayloadsReqMetaData_signers = undefined -- :: ![Signer]
            , _constructionPayloadsReqMetaData_nonce = nonce
            , _constructionPayloadsReqMetaData_publicMeta = pubMeta
            , _constructionPayloadsReqMetaData_tx = tx'
            }
      
      pure $ ConstructionMetadataResp HM.empty (Just [fee])

constructionPayloadsH
    :: ChainwebVersion
    -> ConstructionPayloadsReq
    -> Handler ConstructionPayloadsResp
constructionPayloadsH = undefined
  where
    resp parsedMeta = ConstructionPayloadsResp
      { _constructionPayloadsResp_unsignedTransaction = encodedEnrichedUnsignedCmd
      , _constructionPayloadsResp_payloads = rosettaSignPayloads
      }
      where
        signers = _constructionPayloadsReqMetaData_signers parsedMeta
        rosettaSignPayloads = createSigningPayloads enrichedUnsignedCmd signers
        enrichedUnsignedCmd = createUnsignedCmd parsedMeta
        encodedEnrichedUnsignedCmd = enrichedCommandToText $! enrichedUnsignedCmd

constructionParseH
    :: ChainwebVersion
    -> ConstructionParseReq
    -> Handler ConstructionParseResp
constructionParseH = undefined

constructionCombineH
    :: ConstructionCombineReq
    -> Handler ConstructionCombineResp
constructionCombineH (ConstructionCombineReq _ unsignedTx sigs) =
  runExceptT work >>= either throwRosetta pure
  where
    work :: ExceptT RosettaFailure Handler ConstructionCombineResp
    work = do
      (EnrichedCommand unsignedCmd meta) <- textToEnrichedCommand unsignedTx ?? RosettaUnparsableTx
      let userSigs = map (UserSig . _rosettaSignature_hexBytes) sigs
          signedCmd = unsignedCmd { _cmdSigs = userSigs }
          --  Assumes list of signatures are in correct order to match
          --  their respective Signers in the signed payload.
          --  No rearrangement of signatures done.
          --  No validity checks run.
          -- TOOD: Add check to put sigs in correct order.
          -- TODO: check public keys signing type?
          signedTx = enrichedCommandToText (EnrichedCommand signedCmd meta)
      pure $ ConstructionCombineResp signedTx

constructionHashH
    :: ConstructionHashReq
    -> Handler TransactionIdResp
constructionHashH (ConstructionHashReq _ signedTx) =
  runExceptT work >>= either throwRosetta pure
  where
    work :: ExceptT RosettaFailure Handler TransactionIdResp
    work = do
      (EnrichedCommand cmd _) <- textToEnrichedCommand signedTx ?? RosettaUnparsableTx
      pure $ TransactionIdResp (cmdToTransactionId cmd) Nothing

constructionSubmitH
    :: ChainwebVersion
    -> [(ChainId, MempoolBackend ChainwebTransaction)]
    -> ConstructionSubmitReq
    -> Handler TransactionIdResp
constructionSubmitH v ms (ConstructionSubmitReq net tx) =
    runExceptT work >>= either throwRosetta pure
  where
    work :: ExceptT RosettaFailure Handler TransactionIdResp
    work = do
        cid <- hoistEither $ validateNetwork v net
        (EnrichedCommand cmd _) <- textToEnrichedCommand tx ?? RosettaUnparsableTx
        validated <- hoistEither . first (const RosettaInvalidTx) $ validateCommand cmd
        mp <- lookup cid ms ?? RosettaInvalidChain
        let !vec = V.singleton validated
        liftIO (mempoolInsertCheck mp vec) >>= hoistEither . first (const RosettaInvalidTx)
        liftIO (mempoolInsert mp UncheckedInsert vec)
        pure $ TransactionIdResp (cmdToTransactionId cmd) Nothing

--------------------------------------------------------------------------------
-- Mempool Handlers

mempoolH
    :: ChainwebVersion
    -> [(ChainId, MempoolBackend a)]
    -> NetworkReq
    -> Handler MempoolResp
mempoolH v ms (NetworkReq net _) = work >>= \case
    Left !e -> throwRosetta e
    Right !a -> pure a
  where
    f :: TransactionHash -> TransactionId
    f !h = TransactionId $ toText h

    work = runExceptT $! do
        cid <- hoistEither $ validateNetwork v net
        mp <- lookup cid ms ?? RosettaInvalidChain
        r <- liftIO $ newIORef mempty
        -- TODO: This will need to be revisited once we can add
        -- pagination + streaming the mempool
        void $! liftIO $ mempoolGetPendingTransactions mp Nothing $ \hs -> do
          modifyIORef' r (<> hs)

        txs <- liftIO $! readIORef r
        let !ts = V.toList $ f <$!> txs
        return $ MempoolResp ts

mempoolTransactionH
    :: ChainwebVersion
    -> [(ChainId, MempoolBackend a)]
    -> MempoolTransactionReq
    -> Handler MempoolTransactionResp
mempoolTransactionH v ms mtr = runExceptT work >>= either throwRosetta pure
  where
    MempoolTransactionReq net (TransactionId ti) = mtr

    f :: LookupResult a -> Maybe MempoolTransactionResp
    f Missing = Nothing
    f (Pending _) = Just $ MempoolTransactionResp tx Nothing
      where
        tx = Transaction
          { _transaction_transactionId = TransactionId ti
          , _transaction_operations = [] -- Can't even know who will pay for gas at this moment
          , _transaction_metadata = Nothing
          }

    work :: ExceptT RosettaFailure Handler MempoolTransactionResp
    work = do
        cid <- hoistEither $ validateNetwork v net
        mp <- lookup cid ms ?? RosettaInvalidChain
        th <- (hush $ fromText ti) ?? RosettaUnparsableTransactionId
        lrs <- liftIO . mempoolLookup mp $ V.singleton th
        (lrs V.!? 0 >>= f) ?? RosettaMempoolBadTx

--------------------------------------------------------------------------------
-- Network Handlers

networkListH :: ChainwebVersion -> CutDb cas -> MetadataReq -> Handler NetworkListResp
networkListH v cutDb _ = runExceptT work >>= either throwRosetta pure
  where
    work = do
      c <- liftIO $ _cut cutDb

      -- Unique Rosetta network ids for each of the Chainweb Version's chain ids at
      -- the current cut.
      -- NOTE: This ensures only returning chains that are "active" at
      -- the current time.
      let networkIds = map f $! sort $! (HM.keys (_cutMap c))
      pure $ NetworkListResp networkIds

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
        void $ hoistEither $ validateNetwork v nid
        pure $ NetworkOptionsResp version allow

    version = RosettaNodeVersion
      { _version_rosettaVersion = rosettaSpecVersion
      , _version_nodeVersion = chainwebNodeVersionHeaderValue
      , _version_middlewareVersion = Nothing
      , _version_metadata = Just $ HM.fromList metaPairs }

    -- TODO: Document this meta data
    metaPairs =
      [ "node-api-version" .= prettyApiVersion
      , "chainweb-version" .= chainwebVersionToText v
      , "rosetta-chainweb-version" .= rosettaImplementationVersion
      -- ^ The version of the rosetta implementation.
      --   Meant to capture if something about the internal
      --   implementation has changed.
      ]

    rosettaImplementationVersion = "1.0.0" :: T.Text

    allow = Allow
      { _allow_operationStatuses = opStatuses
      , _allow_operationTypes = opTypes
      , _allow_errors = errExamples
      , _allow_historicalBalanceLookup = True }

    errExamples :: [RosettaError]
    errExamples = map (\e -> rosettaError e Nothing) [minBound .. maxBound]

    opStatuses :: [OperationStatus]
    opStatuses = map operationStatus [minBound .. maxBound]

    opTypes :: [T.Text]
    opTypes = map sshow ([minBound .. maxBound] :: [OperationType])

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
        cid <- hoistEither $ validateNetwork v nid
        bh <- getLatestBlockHeader cutDb cid
        let genesisBh = genesisBlockHeader v cid
        peers <- lift $ _pageItems <$>
          peerGetHandler
          peerDb
          ChainwebNetId.CutNetwork
          -- TODO: document max number of peers returned
          (Just $ Limit maxRosettaNodePeerLimit)
          Nothing
        pure $ resp bh genesisBh peers

    resp :: BlockHeader -> BlockHeader -> [PeerInfo] -> NetworkStatusResp
    resp bh genesis ps = NetworkStatusResp
      { _networkStatusResp_currentBlockId = blockId bh
      , _networkStatusResp_currentBlockTimestamp = rosettaTimestamp bh
      , _networkStatusResp_genesisBlockId = blockId genesis
      , _networkStatusResp_oldestBlockIdentifier = Nothing
      , _networkStatusResp_syncStatus = Nothing
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
