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
module Chainweb.Rosetta.RestAPI.Server
( someRosettaServer
) where

import Control.Error.Util
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
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
import Chainweb.WebPactExecutionService

import P2P.Node.PeerDB
import P2P.Node.RestAPI.Server (peerGetHandler)
import P2P.Peer

---

rosettaServer
    :: forall cas (v :: ChainwebVersionT)
    . PayloadCasLookup cas
    => ChainwebVersion
    -> [(ChainId, PayloadDb cas)]
    -> [(ChainId, MempoolBackend ChainwebTransaction)]
    -> PeerDb
    -> CutDb cas
    -> [(ChainId, PactExecutionService)]
    -> Server (RosettaApi v)
rosettaServer v ps ms peerDb cutDb pacts =
    -- Account --
    accountBalanceH v cutDb pacts
    -- Blocks --
    :<|> blockTransactionH v cutDb ps pacts
    :<|> blockH v cutDb ps pacts
    -- Construction --
    :<|> constructionDeriveH v
    :<|> constructionPreprocessH v
    :<|> constructionMetadataH v cutDb pacts
    :<|> constructionPayloadsH v
    :<|> constructionParseH v
    :<|> constructionCombineH
    :<|> constructionHashH
    :<|> constructionSubmitH v ms
    -- Mempool --
    :<|> mempoolTransactionH v ms
    :<|> mempoolH v ms
    -- Network --
    :<|> networkListH v cutDb
    :<|> networkOptionsH v
    :<|> networkStatusH v cutDb peerDb

someRosettaServer
    :: PayloadCasLookup cas
    => ChainwebVersion
    -> [(ChainId, PayloadDb cas)]
    -> [(ChainId, MempoolBackend ChainwebTransaction)]
    -> PeerDb
    -> [(ChainId, PactExecutionService)]
    -> CutDb cas
    -> SomeServer
someRosettaServer v@(FromSingChainwebVersion (SChainwebVersion :: Sing vT)) ps ms pdb pacts cdb =
    SomeServer (Proxy @(RosettaApi vT)) $ rosettaServer v ps ms pdb cdb pacts

--------------------------------------------------------------------------------
-- Account Handlers

accountBalanceH
    :: ChainwebVersion
    -> CutDb cas
    -> [(ChainId, PactExecutionService)]
    -> AccountBalanceReq
    -> Handler AccountBalanceResp
accountBalanceH _ _ _ (AccountBalanceReq _ (AccountId _ (Just _) _) _) = throwRosetta RosettaSubAcctUnsupported
accountBalanceH v cutDb pacts (AccountBalanceReq net (AccountId acct _ _) pbid) = do
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
      pact <- lookup cid pacts ?? RosettaInvalidChain
      bh <- findBlockHeaderInCurrFork cutDb cid
        (get _partialBlockId_index pbid) (get _partialBlockId_hash pbid)
      bal <- getHistoricalLookupBalance pact bh acct
      pure $ acctBalResp (blockId bh) bal
      where
        get _ Nothing = Nothing
        get f (Just b) = f b

--------------------------------------------------------------------------------
-- Block Handlers

blockH
    :: forall cas
    . PayloadCasLookup cas
    => ChainwebVersion
    -> CutDb cas
    -> [(ChainId, PayloadDb cas)]
    -> [(ChainId, PactExecutionService)]
    -> BlockReq
    -> Handler BlockResp
blockH v cutDb ps pacts (BlockReq net (PartialBlockId bheight bhash)) =
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
      pact <- lookup cid pacts ?? RosettaInvalidChain
      payloadDb <- lookup cid ps ?? RosettaInvalidChain
      bh <- findBlockHeaderInCurrFork cutDb cid bheight bhash
      (coinbase, txs) <- getBlockOutputs payloadDb bh
      logs <- getTxLogs pact bh
      trans <- matchLogs FullLogs bh logs coinbase txs
      pure $ BlockResp
        { _blockResp_block = Just $ block bh trans
        , _blockResp_otherTransactions = Nothing
        }

blockTransactionH
    :: forall cas
    . PayloadCasLookup cas
    => ChainwebVersion
    -> CutDb cas
    -> [(ChainId, PayloadDb cas)]
    -> [(ChainId, PactExecutionService)]
    -> BlockTransactionReq
    -> Handler BlockTransactionResp
blockTransactionH v cutDb ps pacts (BlockTransactionReq net bid t) = do
  runExceptT work >>= either throwRosetta pure
  where
    BlockId bheight bhash = bid
    TransactionId rtid = t

    work :: ExceptT RosettaFailure Handler BlockTransactionResp
    work = do
      cid <- hoistEither $ validateNetwork v net
      pact <- lookup cid pacts ?? RosettaInvalidChain
      payloadDb <- lookup cid ps ?? RosettaInvalidChain
      bh <- findBlockHeaderInCurrFork cutDb cid (Just bheight) (Just bhash)
      rkTarget <- hush (fromText' rtid) ?? RosettaUnparsableTransactionId
      (coinbase, txs) <- getBlockOutputs payloadDb bh
      logs <- getTxLogs pact bh
      tran <- matchLogs (SingleLog rkTarget) bh logs coinbase txs

      pure $ BlockTransactionResp tran


--------------------------------------------------------------------------------
-- Construction Handlers
-- NOTE: all Construction API endpoints except /metadata and /submit must
-- operate in "offline" mode.

-- | Given an ED25519 Public Key, returns the k: account name associated with it.
constructionDeriveH
    :: ChainwebVersion
    -> ConstructionDeriveReq
    -> Handler ConstructionDeriveResp
constructionDeriveH v req =
  either throwRosettaError pure work
  where
    ConstructionDeriveReq net rosettaPubKey _ = req

    work :: Either RosettaError ConstructionDeriveResp
    work = do
      _ <- annotate rosettaError' (validateNetwork v net)
      T2 kAccount ownership <- rosettaPubKeyTokAccount rosettaPubKey
      pure $! ConstructionDeriveResp
        { _constructionDeriveResp_address = Nothing
        , _constructionDeriveResp_accountIdentifier = Just $! accountId kAccount
        , _constructionDeriveResp_metadata = Just $! toObject $! DeriveRespMetaData
          { _deriveRespMetaData_ownership = ownership }
        }

constructionPreprocessH
    :: ChainwebVersion
    -> ConstructionPreprocessReq
    -> Handler ConstructionPreprocessResp
constructionPreprocessH v req = do
    either throwRosettaError pure work
  where
    ConstructionPreprocessReq net ops someMeta someMaxFee someMult = req
    
    work :: Either RosettaError ConstructionPreprocessResp
    work = do
      _ <- annotate rosettaError' (validateNetwork v net)
      meta <- note (rosettaError' RosettaMissingMetaData) someMeta
      parsedMeta :: PreprocessReqMetaData <- extractMetaData meta

      let PreprocessReqMetaData gasPayer _ = parsedMeta

      -- Maps the intended operations to an intermediary dats type
      -- that will facilitate creating pact code later on in the workflow.
      tx <- opsToConstructionTx ops

      -- The suggested cost of the transaction
      (gasLimit, gasPrice, fee) <- getSuggestedFee tx someMaxFee someMult

      -- The accounts that need to sign the transaction
      let expectedAccts = neededAccounts tx gasPayer

      pure $! ConstructionPreprocessResp
        { _constructionPreprocessResp_options = Just $! toObject $! PreprocessRespMetaData
            { _preprocessRespMetaData_reqMetaData = parsedMeta
            , _preprocessRespMetaData_tx = tx
            , _preprocessRespMetaData_suggestedFee = fee
            , _preprocessRespMetaData_gasLimit = gasLimit
            , _preprocessRespMetaData_gasPrice = gasPrice
            }
        , _constructionPreprocessResp_requiredPublicKeys = Just $! expectedAccts
        }


constructionMetadataH
    :: ChainwebVersion
    -> CutDb cas
    -> [(ChainId, PactExecutionService)]
    -> ConstructionMetadataReq
    -> Handler ConstructionMetadataResp
constructionMetadataH v cutDb pacts (ConstructionMetadataReq net opts someKeys) =
    runExceptT work >>= either throwRosettaError pure
  where
    
    work :: ExceptT RosettaError Handler ConstructionMetadataResp
    work = do
      cid <- hoistEither $ annotate rosettaError' (validateNetwork v net)
      availableSigners <- someKeys ?? rosettaError' RosettaMissingPublicKeys
                          >>= hoistEither . rosettaPubKeysToSignerMap
      meta :: PreprocessRespMetaData <- hoistEither $ extractMetaData opts
      let PreprocessRespMetaData reqMeta tx fee gLimit gPrice = meta
          PreprocessReqMetaData payer someNonce = reqMeta

      pubMeta <- liftIO $ toPublicMeta cid payer gLimit gPrice
      let nonce = toNonce someNonce pubMeta

      expectedAccts <- toSignerAcctsMap tx payer cid pacts cutDb
      signersAndAccts <- hoistEither $!
                         createSigners availableSigners expectedAccts
      
      pure $! ConstructionMetadataResp
        { _constructionMetadataResp_metadata = toObject $! PayloadsMetaData
            { _payloadsMetaData_signers = signersAndAccts
            , _payloadsMetaData_nonce = nonce
            , _payloadsMetaData_publicMeta = pubMeta
            , _payloadsMetaData_tx = tx
            }
        , _constructionMetadataResp_suggestedFee = Just [fee]
        }


constructionPayloadsH
    :: ChainwebVersion
    -> ConstructionPayloadsReq
    -> Handler ConstructionPayloadsResp
constructionPayloadsH v req =
  runExceptT work >>= either throwRosettaError pure
  where
    (ConstructionPayloadsReq net _ someMeta _) = req

    work :: ExceptT RosettaError Handler ConstructionPayloadsResp
    work = do
      void $ hoistEither $ annotate rosettaError' (validateNetwork v net)
      meta :: PayloadsMetaData <- hoistEither $ note
              (rosettaError' RosettaMissingMetaData) someMeta >>=
              extractMetaData
      unsigned :: EnrichedCommand <- liftIO $ createUnsignedCmd v meta
      let encoded = enrichedCommandToText $! unsigned
          signingPayloads = createSigningPayloads unsigned
                            (_payloadsMetaData_signers meta)

      pure $ ConstructionPayloadsResp
        { _constructionPayloadsResp_unsignedTransaction = encoded
        , _constructionPayloadsResp_payloads = signingPayloads
        }


constructionParseH
    :: ChainwebVersion
    -> ConstructionParseReq
    -> Handler ConstructionParseResp
constructionParseH v (ConstructionParseReq net isSigned tx) =
  either throwRosettaError pure work
  where
    work :: Either RosettaError ConstructionParseResp
    work = do
      void $ annotate rosettaError' (validateNetwork v net)

      (EnrichedCommand cmd txInfo signAccts) <- note
        (rosettaError' RosettaUnparsableTx)
        $ textToEnrichedCommand tx
      signers <- getRosettaSigners cmd signAccts
      let ops = txToOps txInfo

      pure $ ConstructionParseResp
        { _constructionParseResp_operations = ops
        , _constructionParseResp_signers = Nothing
        , _constructionParseResp_accountIdentifierSigners = Just signers
        , _constructionParseResp_metadata = Nothing
        }

    getRosettaSigners cmd expectedSignerAccts
      | isSigned = do
          _ <- toRosettaError RosettaInvalidTx $ validateCommand cmd
          pure expectedSignerAccts
          -- If transaction signatures successfully validates,
          -- it was signed correctly with all of the account public
          -- keys needed.
          -- NOTE: Might contain repetitions.
      | otherwise = pure []


constructionCombineH
    :: ConstructionCombineReq
    -> Handler ConstructionCombineResp
constructionCombineH (ConstructionCombineReq _ unsignedTx sigs) =
  either throwRosettaError pure work
  where
    work :: Either RosettaError ConstructionCombineResp
    work = do
      (EnrichedCommand unsignedCmd meta signAccts) <- note
        (rosettaError' RosettaUnparsableTx)
        $ textToEnrichedCommand unsignedTx
      payload <- getCmdPayload unsignedCmd
      userSigs <- matchSigs sigs (_pSigners $! payload)

      let signedCmd = unsignedCmd { _cmdSigs = userSigs }
          signedTx = enrichedCommandToText (EnrichedCommand signedCmd meta signAccts)
      pure $ ConstructionCombineResp signedTx


constructionHashH
    :: ConstructionHashReq
    -> Handler TransactionIdResp
constructionHashH (ConstructionHashReq _ signedTx) =
  either throwRosetta pure work
  where
    work :: Either RosettaFailure TransactionIdResp
    work = do
      (EnrichedCommand cmd _ _) <- note RosettaUnparsableTx
        $ textToEnrichedCommand signedTx
      pure $ TransactionIdResp (cmdToTransactionId cmd) Nothing


-- Note (linda): This code simulates the logic of `sendHandler` closely.
constructionSubmitH
    :: ChainwebVersion
    -> [(ChainId, MempoolBackend ChainwebTransaction)]
    -> ConstructionSubmitReq
    -> Handler TransactionIdResp
constructionSubmitH v ms (ConstructionSubmitReq net tx) =
    runExceptT work >>= either throwRosettaError pure
  where
    checkResult
        :: Either (T2 TransactionHash InsertError) ()
        -> ExceptT RosettaError Handler ()
    checkResult (Right _) = pure ()
    checkResult (Left (T2 hsh insErr)) =
      throwE $ stringRosettaError RosettaInvalidTx
      $ "Validation failed for hash "
      ++ (show $! hsh) ++ ": "
      ++ show insErr
    
    work :: ExceptT RosettaError Handler TransactionIdResp
    work = do
        cid <- hoistEither $ annotate rosettaError' (validateNetwork v net)
        mempool <- hoistEither $
          note (rosettaError' RosettaInvalidChain)
          $ lookup cid ms
        (EnrichedCommand cmd _ _) <- hoistEither $
          note (rosettaError' RosettaUnparsableTx)
          $ textToEnrichedCommand tx

        case validateCommand cmd of
          Right validated -> do
            let txs = V.fromList [validated]
            -- If any of the txs in the batch fail validation, we reject them all.
            liftIO (mempoolInsertCheck mempool txs) >>= checkResult
            liftIO (mempoolInsert mempool UncheckedInsert txs)
            pure $ TransactionIdResp (cmdToTransactionId cmd) Nothing
          Left e -> throwE $ stringRosettaError RosettaInvalidTx
            $ "Validation failed: " ++ show e

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
        th <- hush (fromText ti) ?? RosettaUnparsableTransactionId
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
      let networkIds = map f $! sort $! HM.keys (_cutMap c)
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
      --  The version of the rosetta implementation.
      --  Meant to capture if something about the internal
      --  implementation has changed.
      ]

    rosettaImplementationVersion = "2.0.0" :: T.Text

    allow = Allow
      { _allow_operationStatuses = opStatuses
      , _allow_operationTypes = opTypes
      , _allow_errors = errExamples
      , _allow_historicalBalanceLookup = True }

    errExamples :: [RosettaError]
    errExamples = map (`rosettaError` Nothing) [minBound .. maxBound]

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
