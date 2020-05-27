{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import Control.Monad (void, foldM, (<$!>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Bifunctor
import Data.Map (Map)
import Data.Decimal
import Data.String
import Data.IORef
import Data.Proxy (Proxy(..))
import Data.Tuple.Strict (T2(..))
import Data.Word (Word64)

import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Pact.Types.ChainMeta (PublicMeta(..))
import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Runtime (TxId(..))
import Pact.Types.RPC
import Pact.Types.PactValue (PactValue(..))
import Pact.Types.Pretty (renderCompactText)
import Pact.Types.Exp (Literal(..))
import Pact.Types.Util (fromText')

import Rosetta

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.Chainweb.ChainResources (ChainResources(..))
import Chainweb.CutDB
import Chainweb.HostAddress
import Chainweb.Mempool.Mempool
import Chainweb.Pact.RestAPI.Server
import Chainweb.Pact.Templates
import Chainweb.Payload.PayloadStore
import qualified Chainweb.RestAPI.NetworkID as ChainwebNetId
import Chainweb.RestAPI.Utils
import Chainweb.Rosetta.RestAPI
import Chainweb.Rosetta.Util
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
    accountBalanceH v cr
    -- Blocks --
    :<|> blockTransactionH v cutDb ps cr
    :<|> blockH v cutDb ps cr
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

    readBlock :: Maybe Value -> Maybe (Word64, T.Text)
    readBlock (Just (Object meta)) = do
      hi <- (HM.lookup "blockHeight" meta) >>= (hushResult . fromJSON)
      hsh <- (HM.lookup "prevBlockHash" meta) >>= (hushResult . fromJSON)
      pure $ (pred hi, hsh)
    readBlock _ = Nothing

    balCheckCmd :: ChainId -> IO (Command T.Text)
    balCheckCmd cid = do
      cmd <- mkCommand [] meta "nonce" Nothing rpc
      return $ T.decodeUtf8 <$> cmd
      where
        rpc = Exec $ ExecMsg code Null
        code = renderCompactText $
          app (bn "at")
            [ strLit "balance"
            , app (qn "coin" "details") [ strLit acct ]
            ]
        meta = PublicMeta
          (fromString $ show $ chainIdToText cid)
          "someSender"
          10000   -- gas limit
          0.0001  -- gas price
          300     -- ttl
          0       -- creation time

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
      (blockHeight, blockHash) <- (readBlock $ _crMetaData cRes) ?? RosettaInvalidResultMetaData

      pure $ AccountBalanceResp
        { _accountBalanceResp_blockId = BlockId blockHeight blockHash
        , _accountBalanceResp_balances = [ kdaToRosettaAmount balKDA ]
        , _accountBalanceResp_metadata = Nothing }

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
    -- TODO: Max limit of tx to return at once.
    --       When to do pagination using /block/transaction?
    -- | Retrieve the coin contract logs for all transactions in a block.
    getBlockTxs
        :: BlockHeader
        -> Map TxId [AccountLog]
        -> CoinbaseCommandResult
        -> V.Vector (CommandResult Hash)
        -> Maybe [Transaction]
    getBlockTxs bh logs coinbase rest
      | (_blockHeight bh == 0) = pure $ V.toList $ V.map (getGenesisLog logs) rest
      | otherwise = hush $ nonGenesisTransactions
                    logs _crTxId rosettaTransaction coinbase rest
 
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
      cid <- validateNetwork v net
      cr <- lookup cid crs ?? RosettaInvalidChain
      payloadDb <- lookup cid ps ?? RosettaInvalidChain
      bh <- findBlockHeaderInCurrFork cutDb cid bheight bhash
      (coinbaseOut, txsOut) <- getBlockOutputs payloadDb bh
      logs <- getTxLogs (_chainResPact cr) bh
      trans <- (getBlockTxs bh logs coinbaseOut txsOut) ?? RosettaMismatchTxLogs
      pure $ BlockResp
        { _blockResp_block = block bh trans
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
blockTransactionH v cutDb ps crs (BlockTransactionReq net bid t) =
  runExceptT work >>= either throwRosetta pure
  where
    BlockId bheight bhash = bid
    TransactionId rtid = t

    -- | Find target transaction in the block and,
    --   if it is present in the block, retrieve its coin contract logs.
    getBlockTx
        :: BlockHeader
        -> Map TxId [AccountLog]
        -> CoinbaseCommandResult
        -> V.Vector (CommandResult Hash)
        -> RequestKey
        -> Either RosettaFailure Transaction
    getBlockTx bh logs coinbase rest target
      | (_blockHeight bh == 0) = genesisTransaction logs rest target
      | otherwise = nonGenesisTransaction
                    logs _crReqKey _crTxId rosettaTransaction coinbase rest target

    -- | Matches a single genesis transaction to its coin contract logs.
    genesisTransaction
        :: Map TxId [AccountLog]
        -> V.Vector (CommandResult Hash)
        -> RequestKey
        -- ^ target tx
        -> Either RosettaFailure Transaction
    genesisTransaction logs rest target = do
      cr <- note RosettaTxIdNotFound $
            V.find (\c -> (_crReqKey c) == target) rest
      pure $ getGenesisLog logs cr

    work :: ExceptT RosettaFailure Handler BlockTransactionResp
    work = do
      cid <- validateNetwork v net
      tagetReqKey <- (hush $ fromText' rtid) ?? RosettaUnparsableTransactionId
      cr <- lookup cid crs ?? RosettaInvalidChain
      payloadDb <- lookup cid ps ?? RosettaInvalidChain
      bh <- findBlockHeaderInCurrFork cutDb cid (Just bheight) (Just bhash)
      (coinbaseOut, txsOut) <- getBlockOutputs payloadDb bh
      logs <- getTxLogs (_chainResPact cr) bh
      tran <- hoistEither $ getBlockTx bh logs coinbaseOut txsOut tagetReqKey
      pure $ BlockTransactionResp tran


-- | Genesis transactions do not have coinbase or gas payments.
getGenesisLog
    :: Map TxId [AccountLog]
    -> CommandResult Hash
    -> Transaction
getGenesisLog logs cr =
  case (_crTxId cr) of
    Just tid -> case (M.lookup tid logs) of
      Just l -> rosettaTransaction cr $ makeOps tid l
      Nothing -> rosettaTransaction cr []  -- not a coin contract tx
    Nothing -> rosettaTransaction cr [] -- all genesis tx should have a txid
  where
      makeOps tid l = indexedOperations $
        map (operation Successful TransferOrCreateAcct tid) l


-- | Matches the first coin contract logs to the coinbase tx
nonGenesisCoinbase
    :: V.Vector (TxId, [AccountLog])
    -> (a -> Maybe TxId)
    -> (a -> [Operation] -> b)
    -> a
    -> Either String (T2 Int b)
nonGenesisCoinbase logs getTxId f cr = do
  let idx = 0
  expectedTid <- note "No Coinbase TxId found" (getTxId cr)
  (tid, l) <- note
    ("initial logs missing with txId=" ++ show idx)
    $ logs V.!? idx
  if (expectedTid == tid)
    then let ops = indexedOperations $
                   map (operation Successful CoinbaseReward tid) l
         in pure $ T2 idx (f cr ops)
    else Left "First log's txId does not match coinbase tx's TxId"


-- | Matches all transactions in a non-genesis block to their coin contract logs.
-- The first transaction in non-genesis blocks is the coinbase transaction.
-- Each transactions that follows has (1) logs that fund the transaction,
-- (2) optinal tx specific coin contract logs, and (3) logs that pay gas.
nonGenesisTransactions
    :: Map TxId [AccountLog]
    -> (a -> Maybe TxId)
    -> (a -> [Operation] -> b)
    -> a
    -> V.Vector a
    -> Either String [b]
nonGenesisTransactions logs getTxId f initial rest = do
  T2 initIdx initTx <- nonGenesisCoinbase logsVector getTxId f initial
  T2 _ ts <- foldM matchLogs (defAcc initIdx initTx) rest
  pure $ DList.toList ts
  where
    logsVector = V.fromList $ M.toAscList logs   -- O(1) lookup by index
    matchLogs = gasTransactionAcc logsVector getTxId f DList.snoc
    defAcc i tx = T2 (succ i) (DList.singleton tx)

-- | Matches a single non-genesis transaction to its coin contract logs
nonGenesisTransaction
    :: Map TxId [AccountLog]
    -> (a -> RequestKey)
    -> (a -> Maybe TxId)
    -> (a -> [Operation] -> b)
    -> a
    -> V.Vector a
    -> RequestKey
    -- ^ Lookup target
    -> Either RosettaFailure b
nonGenesisTransaction logs getRk getTxId f initial rest target = do
  T2 initIdx initTx <- (note RosettaMismatchTxLogs) . hush $ getCoinbaseLogs
  if (getRk initial == target)
    then pure $ initTx
    else (work initIdx initTx)

  where
    getCoinbaseLogs = nonGenesisCoinbase logsVector getTxId f initial
    logsVector = V.fromList $ M.toAscList logs
    matchLogs = gasTransactionAcc logsVector getTxId f overwriteLastTx
    overwriteLastTx _ curr = curr

    -- | Traverse list matching transactions to their logs.
    -- If target's logs found or if error throw by matching function,
    -- short circuit.
    work initIdx initTx = do
      let acc = T2 (succ initIdx) initTx
      hoistToRosettaFailure $ foldM findTxAndLogs acc rest

    hoistToRosettaFailure
        :: Either (Either String c) (T2 Int c)
        -> Either RosettaFailure c
    hoistToRosettaFailure (Right _) = Left RosettaTxIdNotFound
    hoistToRosettaFailure (Left (Left _)) = Left RosettaMismatchTxLogs
    hoistToRosettaFailure (Left (Right tx)) = pure tx

    hoistToShortCircuit
        :: Either String (T2 Int c)
        -> Either (Either String c) (T2 Int c)
    hoistToShortCircuit (Left e) = Left $ Left e  -- short-circuit if matching function threw error
    hoistToShortCircuit (Right r) = Right r

    findTxAndLogs acc cr = do
      T2 nextIdx nextTx <- hoistToShortCircuit (matchLogs acc cr)
      if (getRk cr == target)
        then Left $ Right nextTx   -- short-circuit if find target's logs
        else pure $ (T2 nextIdx nextTx)  -- continue matching other txs' logs until find target
      

-- | Algorithm:
-- Assumes logs at current index (i.e. idx) funded the tx.
-- Attempts to get the coin contract logs the tx
-- might have. If the tx succeeded (i.e. the tx has a TxId),
-- it peeks at the next logs in the list (i.e. idx + 1).
-- If the log's txId matches the tx's txId, then these
-- logs are associated with the tx and the logs that
-- paid for gas are retrieved from following index (i.e. idx + 1 + 1).
-- If the txIds don't match OR if the tx failed, the gas logs
-- are retrieved from idx + 1 instead.
gasTransactionAcc
    :: V.Vector (TxId, [AccountLog])
    -> (a -> Maybe TxId)
    -> (a -> [Operation] -> b)
    -> (c -> b -> c)
    -- ^ Accumulator function
    -> T2 Int c
    -> a
    -> Either String (T2 Int c)
gasTransactionAcc logs getTxId f acc (T2 idx txs) cr = do
  T2 transferIdx fund <- getLogs idx FundTx
  T2 gasIdx transfer <- getTransferLogs transferIdx (getTxId cr)
  T2 nextIdx gas <- getLogs gasIdx GasPayment
  let ops = indexedOperations $ fund <> transfer <> gas
      tx = f cr ops
  pure $ T2 nextIdx (acc txs tx)
  where
    peek i = note
      ("no logs found at txIdx=" ++ show i)
      $ logs V.!? i

    -- Returns logs at given index and the next index to try
    getLogs
        :: Int
        -> OperationType
        -> Either String (T2 Int [UnindexedOperation])
    getLogs i otype = do
      (tid,l) <- peek i
      let opsF = map (operation Successful otype tid) l
      pure $ T2 (succ i) opsF

    getTransferLogs
        :: Int
        -> Maybe TxId
        -> Either String (T2 Int [UnindexedOperation])
    getTransferLogs expected actual =
      case actual of
        Nothing -> noTransferLogs
        Just actualTid -> peekNextTxId >>= (isCoinTx actualTid)
      where
        noTransferLogs = pure $ T2 expected []
        transferLogs = getLogs expected TransferOrCreateAcct
        peekNextTxId = fmap fst (peek expected)
        isCoinTx aTid eTid
          | (eTid == aTid) = transferLogs
          | otherwise = noTransferLogs

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
mempoolH v ms (MempoolReq net) = work >>= \case
    Left !e -> throwRosetta e
    Right !a -> pure a
  where
    f :: TransactionHash -> TransactionId
    f !h = TransactionId (encodeToText h)

    work = runExceptT $! do
        cid <- validateNetwork v net
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
    th = fromTransactionId (TransactionId ti)

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
      { _allow_operationStatuses = opStatuses
      , _allow_operationTypes = opTypes
      , _allow_errors = errExamples }

    errExamples :: [RosettaError]
    errExamples = map rosettaError [minBound .. maxBound]

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
        cid <- validateNetwork v nid
        bh <- getLatestBlockHeader cutDb cid
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

    resp :: BlockHeader -> BlockHeader -> [PeerInfo] -> NetworkStatusResp
    resp bh genesis ps = NetworkStatusResp
      { _networkStatusResp_currentBlockId = blockId bh
      , _networkStatusResp_currentBlockTimestamp = rosettaTimestamp bh
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
