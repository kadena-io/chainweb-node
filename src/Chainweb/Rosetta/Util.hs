{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Rosetta.Util
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Linda Ortega <linda@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Rosetta.Util where

import Control.Error.Util
import Control.Lens ((^?))
import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Map (Map)
import Data.Decimal
import Data.CAS
import Data.Tuple.Strict (T2(..))
import Data.Word (Word64)


import qualified Data.ByteString.Short as BSS
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Memory.Endian as BA
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Vector as V

import Numeric.Natural

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Runtime (TxId(..), TxLog(..), Domain(..))
import Pact.Types.PactValue (PactValue(..))
import Pact.Types.Persistence (RowKey(..))
import Pact.Types.Exp (Literal(..))

import Rosetta
import Servant.Server

-- internal modules

import Chainweb.BlockCreationTime (BlockCreationTime(..))
import Chainweb.BlockHash
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Mempool.Mempool (TransactionHash(..))
import Chainweb.Pact.Service.Types (Domain'(..), BlockTxHistory(..))
import Chainweb.Payload hiding (Transaction(..))
import Chainweb.Payload.PayloadStore
import Chainweb.Rosetta.RestAPI
import Chainweb.Time
import Chainweb.TreeDB (seekAncestor)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService (PactExecutionService(..))

---

--------------------------------------------------------------------------------
-- Rosetta Helper Types --
--------------------------------------------------------------------------------

type CoinbaseCommandResult = CommandResult Hash
type AccountLog = (T.Text, Decimal, Value)
type UnindexedOperation = (Word64 -> Operation)

data ChainwebOperationStatus = Successful | Remediation
  deriving (Enum, Bounded, Show)

data OperationType =
    CoinbaseReward
  | FundTx
  | GasPayment
  | TransferOrCreateAcct
  deriving (Enum, Bounded, Show)


--------------------------------------------------------------------------------
-- Functions to create Rosetta types --
--------------------------------------------------------------------------------

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

rosettaTransactionFromCmd :: Command a -> [Operation] -> Transaction
rosettaTransactionFromCmd cmd ops =
  Transaction
    { _transaction_transactionId = pactHashToTransactionId (_cmdHash cmd)
    , _transaction_operations = ops
    , _transaction_metadata = Nothing
    }

rosettaTransaction :: CommandResult Hash -> [Operation] -> Transaction
rosettaTransaction cr ops =
  Transaction
    { _transaction_transactionId = rkToTransactionId (_crReqKey cr)
    , _transaction_operations = ops
    , _transaction_metadata = txMeta
    }
  where
    -- Include information on related transactions (i.e. continuations)
    txMeta
      | enableMetaData =
          case _crContinuation cr of
            Nothing -> Nothing
            Just pe -> Just $ HM.fromList
              [("related-transaction", toJSON pe)] -- TODO: document, make nicer?
      | otherwise = Nothing


pactHashToTransactionId :: PactHash -> TransactionId
pactHashToTransactionId hsh = TransactionId $ hashToText $ toUntypedHash hsh

rkToTransactionId :: RequestKey -> TransactionId
rkToTransactionId rk = TransactionId $ requestKeyToB16Text rk

fromTransactionId :: TransactionId -> TransactionHash
fromTransactionId (TransactionId ti) = TransactionHash . BSS.toShort $ T.encodeUtf8 ti

indexedOperations :: [UnindexedOperation] -> [Operation]
indexedOperations logs = zipWith (\f i -> f i) logs [(0 :: Word64)..]

operationStatus :: ChainwebOperationStatus -> OperationStatus
operationStatus s@Successful =
  OperationStatus
    { _operationStatus_status = sshow s
    , _operationStatus_successful = True
    }
operationStatus s@Remediation =
  OperationStatus
    { _operationStatus_status = sshow s
    , _operationStatus_successful = True
    }

operation
    :: ChainwebOperationStatus
    -> OperationType
    -> TxId
    -> AccountLog
    -> Word64
    -> Operation
operation ostatus otype txid (key, bal, guard) idx =
  Operation
    { _operation_operationId = OperationId idx Nothing
    , _operation_relatedOperations = Nothing -- TODO: implement
    , _operation_type = sshow otype
    , _operation_status = sshow ostatus
    , _operation_account = Just accountId
    , _operation_amount = Just $ kdaToRosettaAmount bal
    , _operation_metadata = opMeta
    }
  where
    opMeta
      | enableMetaData = Just $ HM.fromList [("txId", toJSON txid)] -- TODO: document
      | otherwise = Nothing
    accountId = AccountId
      { _accountId_address = key
      , _accountId_subAccount = Nothing  -- assumes coin acct contract only
      , _accountId_metadata = accountIdMeta
      }
    accountIdMeta
      | enableMetaData = Just $ HM.fromList [("ownership", guard)]  -- TODO: document
      | otherwise = Nothing


-- Timestamp of the block in milliseconds since the Unix Epoch.
-- NOTE: Chainweb provides this timestamp in microseconds.
rosettaTimestamp :: BlockHeader -> Word64
rosettaTimestamp bh = BA.unLE . BA.toLE $ fromInteger msTime
  where
    msTime = int $ microTime `div` ms
    TimeSpan ms = millisecond
    microTime = encodeTimeToWord64 $ _bct (_blockCreationTime bh)

kdaToRosettaAmount :: Decimal -> Amount
kdaToRosettaAmount k = Amount (sshow amount) currency Nothing
  where
    -- Value in atomic units represented as an arbitrary-sized signed integer.
    amount :: Integer
    amount = floor $ k * (realToFrac ((10 :: Integer) ^ numDecimals))

    -- How to convert from atomic units to standard units
    numDecimals = 12 :: Word

    currency = Currency "KDA" numDecimals Nothing

--------------------------------------------------------------------------------
-- Chainweb Helper Functions --
--------------------------------------------------------------------------------

getLatestBlockHeader
    :: CutDb cas
    -> ChainId
    -> ExceptT RosettaFailure Handler BlockHeader
getLatestBlockHeader cutDb cid = do
  c <- liftIO $ _cut cutDb
  HM.lookup cid (_cutMap c) ?? RosettaInvalidChain


findBlockHeaderInCurrFork
    :: CutDb cas
    -> ChainId
    -> Maybe Word64
    -- ^ Block Height
    -> Maybe T.Text
    -- ^ Block Hash
    -> ExceptT RosettaFailure Handler BlockHeader
findBlockHeaderInCurrFork cutDb cid someHeight someHash = do
  latestBlock <- getLatestBlockHeader cutDb cid
  chainDb <- (cutDb ^? cutDbBlockHeaderDb cid) ?? RosettaInvalidChain

  case (someHeight, someHash) of
    (Nothing, Nothing) -> pure latestBlock
    (Just hi, Nothing) -> byHeight chainDb latestBlock hi
    (Just hi, Just hsh) -> do
      bh <- byHeight chainDb latestBlock hi
      bhashExpected <- (blockHashFromText hsh) ?? RosettaUnparsableBlockHash
      if (_blockHash bh == bhashExpected)
        then pure bh
        else throwError RosettaMismatchBlockHashHeight
    (Nothing, Just hsh) -> do
      bhash <- (blockHashFromText hsh) ?? RosettaUnparsableBlockHash
      somebh <- liftIO $ (casLookup chainDb bhash)
      bh <- somebh ?? RosettaBlockHashNotFound
      isInCurrFork <- liftIO $ memberOfHeader cutDb cid bhash latestBlock
      if isInCurrFork
        then pure bh
        else throwError RosettaOrphanBlockHash
  where
    byHeight db latest hi = do
      somebh <- liftIO $ seekAncestor db latest (int hi)
      somebh ?? RosettaInvalidBlockHeight


getBlockOutputs
    :: forall cas
    . PayloadCasLookup cas
    => PayloadDb cas
    -> BlockHeader
    -> ExceptT RosettaFailure Handler (CoinbaseCommandResult, V.Vector (CommandResult Hash))
getBlockOutputs payloadDb bh = do
  someOut <- liftIO $ casLookup payloadDb (_blockPayloadHash bh)
  outputs <- someOut ?? RosettaPayloadNotFound
  txsOut <- decodeTxsOut outputs ?? RosettaUnparsableTxOut
  coinbaseOut <- decodeCoinbaseOut outputs ?? RosettaUnparsableTxOut
  pure $ (coinbaseOut, txsOut)

  where
    decodeCoinbaseOut :: PayloadWithOutputs -> Maybe (CommandResult Hash)
    decodeCoinbaseOut = decodeStrictOrThrow . _coinbaseOutput . _payloadWithOutputsCoinbase

    decodeTxsOut :: PayloadWithOutputs -> Maybe (V.Vector (CommandResult Hash))
    decodeTxsOut pwo = mapM (decodeStrictOrThrow . _transactionOutputBytes . snd)
                       (_payloadWithOutputsTransactions pwo)

getTxLogs
    :: PactExecutionService
    -> BlockHeader
    -> ExceptT RosettaFailure Handler (Map TxId [AccountLog])
getTxLogs cr bh = do
  someHist <- liftIO $ (_pactBlockTxHistory cr) bh d
  (BlockTxHistory hist) <- (hush someHist) ?? RosettaPactExceptionThrown
  case (M.size hist) of
    0 -> throwError RosettaUnparsableTxLog
    _ -> pure ()
  let histParsed = M.mapMaybe (mapM txLogToAccountInfo) hist
  if (M.size histParsed == M.size hist)
    then pure histParsed  -- all logs successfully parsed
    else throwError RosettaUnparsableTxLog
  where
    d = (Domain' (UserTables "coin_coin-table"))


getHistoricalLookupBalance
    :: PactExecutionService
    -> BlockHeader
    -> T.Text
    -> ExceptT RosettaFailure Handler Decimal
getHistoricalLookupBalance cr bh k = do
  someHist <- liftIO $ (_pactHistoricalLookup cr) bh d key
  hist <- (hush someHist) ?? RosettaPactExceptionThrown
  case hist of
    Nothing -> pure 0.0
    Just h -> do
      (_,bal,_) <- (txLogToAccountInfo h) ?? RosettaUnparsableTxLog
      pure bal
  where
    d = (Domain' (UserTables "coin_coin-table"))
    key = RowKey k


--------------------------------------------------------------------------------
-- Transaction Log Matching Functions --
--------------------------------------------------------------------------------

newtype GenesisMatchFunction a = GenesisMatchFunction
  { _genesisMatchFunction :: Either RosettaFailure a }
newtype RemediationMatchFunction a = RemediationMatchFunction
  { _remediationMatchFunction :: Either RosettaFailure a }
newtype NonGenesisMatchFunction a = NonGenesisMatchFunction
  { _nonGenesisMatchFunction :: Either RosettaFailure a }

-- | Retrieve the coin contract logs for transaction(s) in a block.
matchLogs
    :: BlockHeader
    -> GenesisMatchFunction a
    -> RemediationMatchFunction a
    -> NonGenesisMatchFunction a
    -> Either RosettaFailure a
matchLogs bh g r f
  | bheight == 0 = _genesisMatchFunction g
  | coinV2Upgrade v cid bheight = _remediationMatchFunction r
  | otherwise = _nonGenesisMatchFunction f
  where
    bheight = _blockHeight bh
    cid = _blockChainId bh
    v = _blockChainwebVersion bh


---------------------
-- Genesis Helpers --
---------------------

-- |
-- | Genesis transactions do not have coinbase or gas payments.
-- |
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


-- |
-- | Matches all genesis transactions to their coin contract logs.
-- |
genesisTransactions
    :: Map TxId [AccountLog]
    -> V.Vector (CommandResult Hash)
    -> GenesisMatchFunction [Transaction]
genesisTransactions logs txs =
  GenesisMatchFunction $
  pure $ V.toList $ V.map (getGenesisLog logs) txs


-- |
-- | Matches a single genesis transaction to its coin contract logs.
-- |
genesisTransaction
    :: Map TxId [AccountLog]
    -> V.Vector (CommandResult Hash)
    -> RequestKey
    -- ^ target tx
    -> GenesisMatchFunction Transaction
genesisTransaction logs rest target = GenesisMatchFunction $ do
  cr <- note RosettaTxIdNotFound $
        V.find (\c -> (_crReqKey c) == target) rest
  pure $ getGenesisLog logs cr


------------------------
-- Coinbase Helpers --
------------------------

-- | Matches the first coin contract logs to the coinbase tx
nonGenesisCoinbase
    :: (TxId, [AccountLog])
    -> (a -> Maybe TxId)
    -> (a -> [Operation] -> b)
    -> a
    -> Either String b
nonGenesisCoinbase (tid,l) getTxId f cr = do
  expectedTid <- note "No Coinbase TxId found" (getTxId cr)
  if (expectedTid == tid)
    then let ops = indexedOperations $
                   map (operation Successful CoinbaseReward tid) l
         in pure $ (f cr ops)
    else Left "First log's txId does not match coinbase tx's TxId"


------------------------
-- NonGenesis Helpers --
------------------------

-- | Algorithm:
--   Assumes logs at current index (i.e. idx) funded the tx.
--   Attempts to get the coin contract logs the tx
--   might have. If the tx succeeded (i.e. the tx has a TxId),
--   it peeks at the next logs in the list (i.e. idx + 1).
--   If the log's txId matches the tx's txId, then these
--   logs are associated with the tx and the logs that
--   paid for gas are retrieved from following index (i.e. idx + 1 + 1).
--   If the txIds don't match OR if the tx failed, the gas logs
--   are retrieved from idx + 1 instead.
-- |
gasTransactionAcc
    :: V.Vector (TxId, [AccountLog])
    -> (a -> Maybe TxId)
    -> (a -> [Operation] -> b)
    -> (c -> b -> c)
    -- ^ Accumulator function
    -> T2 Int c
    -- ^ Index to start and accumulator
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
    -- Returns logs at given index and the next index to try
    getLogs
        :: Int
        -> OperationType
        -> Either String (T2 Int [UnindexedOperation])
    getLogs i otype = do
      (tid,l) <- peekLog i logs
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
        peekNextTxId = fmap fst (peekLog expected logs)
        isCoinTx aTid eTid
          | (eTid == aTid) = transferLogs
          | otherwise = noTransferLogs


-- TODO: Max limit of tx to return at once.
--       When to do pagination using /block/transaction?
-- |
-- | Matches all transactions in a non-genesis block to their coin contract logs.
--   The first transaction in non-genesis blocks is the coinbase transaction.
--   Each transactions that follows has (1) logs that fund the transaction,
--   (2) optional tx specific coin contract logs, and (3) logs that pay gas.
-- |
nonGenesisTransactions
    :: Map TxId [AccountLog]
    -> CoinbaseCommandResult
    -> V.Vector (CommandResult Hash)
    -> NonGenesisMatchFunction [Transaction]
nonGenesisTransactions logs initial rest =
  NonGenesisMatchFunction $
    overwriteError RosettaMismatchTxLogs $
      nonGenesisTransactions'
      logs _crTxId rosettaTransaction initial rest

nonGenesisTransactions'
    :: Map TxId [AccountLog]
    -> (a -> Maybe TxId)
    -> (a -> [Operation] -> b)
    -> a
    -> V.Vector a
    -> Either String [b]
nonGenesisTransactions' logs getTxId f initial rest = do
  let initIdx = 0
  initLog <- peekLog initIdx logsVector
  initTx <- nonGenesisCoinbase initLog getTxId f initial
  T2 _ ts <- foldM match (defAcc initIdx initTx) rest
  pure $ DList.toList ts
  where
    logsVector = V.fromList $ M.toAscList logs   -- O(1) lookup by index
    match = gasTransactionAcc logsVector getTxId f DList.snoc
    defAcc i tx = T2 (succ i) (DList.singleton tx)


-- |
-- | Matches a single non-genesis transaction to its coin contract logs
-- |
nonGenesisTransaction
    :: Map TxId [AccountLog]
    -> CoinbaseCommandResult
    -> V.Vector (CommandResult Hash)
    -> RequestKey
    -> NonGenesisMatchFunction Transaction
nonGenesisTransaction logs initial rest target =
  NonGenesisMatchFunction $
    (noteOptional RosettaTxIdNotFound .
     overwriteError RosettaMismatchTxLogs) $
       nonGenesisTransaction'
       logs _crReqKey _crTxId rosettaTransaction initial rest target

nonGenesisTransaction'
    :: Map TxId [AccountLog]
    -> (a -> RequestKey)
    -> (a -> Maybe TxId)
    -> (a -> [Operation] -> b)
    -> a
    -> V.Vector a
    -> RequestKey
    -- ^ Lookup target
    -> Either String (Maybe b)
nonGenesisTransaction' logs getRk getTxId f initial rest target = do
  let initIdx = 0
  initLog <- peekLog initIdx logsVector
  initTx <- getCoinbaseLogs initLog
  if (getRk initial == target)
    then pure $ Just initTx
    else (work initIdx initTx)

  where
    -- | Traverse list matching transactions to their logs.
    -- If target's logs found or if error throw by matching function,
    -- short circuit.
    work initIdx initTx = do
      let acc = T2 (succ initIdx) initTx
      fromShortCircuit $ foldM findTxAndLogs acc rest

    getCoinbaseLogs l = nonGenesisCoinbase l getTxId f initial
    logsVector = V.fromList $ M.toAscList logs

    match = gasTransactionAcc logsVector getTxId f overwriteLastTx
    overwriteLastTx _ curr = curr

    fromShortCircuit
        :: Either (Either String c) (T2 Int c)
        -> Either String (Maybe c)
    fromShortCircuit (Right _) = pure Nothing -- RosettaTxIdNotFound
    fromShortCircuit (Left (Left s)) = Left s
    fromShortCircuit (Left (Right tx)) = pure (Just tx)

    hoistToShortCircuit
        :: Either String (T2 Int c)
        -> Either (Either String c) (T2 Int c)
    hoistToShortCircuit (Left e) = Left $ Left e  -- short-circuit if matching function threw error
    hoistToShortCircuit (Right r) = Right r

    findTxAndLogs acc cr = do
      T2 nextIdx nextTx <- hoistToShortCircuit (match acc cr)
      if (getRk cr == target)
        then Left $ Right nextTx   -- short-circuit if find target's logs
        else pure $ (T2 nextIdx nextTx)  -- continue matching other txs' logs until find target


-------------------------
-- Remediation Helpers --
-------------------------

remediationRequestKey :: RequestKey
remediationRequestKey = RequestKey $ pactHash "remediation"


-- Group the rest of the logs into a single transaction id because
-- remediations all have different txIds and we don't have access to
-- their command results.
-- Thus, if a normal transaction occurs in this block, it will be grouped with
-- the remediation changes.
getRemediationsTx
    :: [(TxId, [AccountLog])]
    -> Transaction
getRemediationsTx logsList =
  let t = rkToTransactionId remediationRequestKey
      f (tid, ali) = map (operation Successful TransferOrCreateAcct tid) ali
      ops = indexedOperations $ concat $ map f logsList
  in (Transaction t ops Nothing)


-- TODO: Do all the remediations touch the coin contract? This might help
-- with connecting a remediation Command with a TxId log.
-- TODO: Are we loading "pact/coin-contract/v2/load-fungible-asset-v2.yaml"
-- "pact/coin-contract/v2/load-coin-contract-v2.yaml" every time a remediation is
-- run? Is it always 3 commands that are run for each remediation?
remediations
    :: Map TxId [AccountLog]
    -> CoinbaseCommandResult
    -> RemediationMatchFunction [Transaction]
remediations logs initial =
  RemediationMatchFunction $
    overwriteError RosettaMismatchTxLogs work
  where
    work = do
      T2 initLog restLogs <- splitAtFirst logs
      initTx <- nonGenesisCoinbase initLog _crTxId rosettaTransaction initial
      pure $ [initTx, getRemediationsTx restLogs]

singleRemediation
    :: Map TxId [AccountLog]
    -> CoinbaseCommandResult
    -> RequestKey
    -- ^ target
    -> RemediationMatchFunction Transaction
singleRemediation logs initial target =
  RemediationMatchFunction $
    (noteOptional RosettaTxIdNotFound .
     overwriteError RosettaMismatchTxLogs)
    work
  where
    work
      | _crReqKey initial == target = do
          T2 initLog _ <- splitAtFirst logs
          Just <$> nonGenesisCoinbase initLog _crTxId rosettaTransaction initial
      | target == remediationRequestKey = do
          T2 _ restLogs <- splitAtFirst logs
          pure $ Just $ getRemediationsTx restLogs
      | otherwise = pure Nothing -- Target not found


--------------------------------------------------------------------------------
-- Misc Helper Functions --
--------------------------------------------------------------------------------

-- BUG: validator throws error when writing (some?) unstructured json to db.
-- Disable filling in metadata for now.
enableMetaData :: Bool
enableMetaData = False

-- TODO: document
maxRosettaNodePeerLimit :: Natural
maxRosettaNodePeerLimit = 64

-- | Parse TxLog Value into fungible asset account
txLogToAccountInfo :: TxLog Value -> Maybe AccountLog
txLogToAccountInfo (TxLog _ key (Object row)) = do
  guard :: Value <- (HM.lookup "guard" row) >>= (hushResult . fromJSON)
  (PLiteral (LDecimal bal)) <- (HM.lookup "balance" row) >>= (hushResult . fromJSON)
  pure $ (key, bal, guard)
txLogToAccountInfo _ = Nothing

hushResult :: Result a -> Maybe a
hushResult (Success w) = Just w
hushResult (Error _) = Nothing

overwriteError :: a -> Either b c -> Either a c
overwriteError e (Left _) = Left e
overwriteError _ (Right r) = Right r

noteOptional :: a -> Either a (Maybe c) -> Either a c
noteOptional e (Right Nothing) = Left e
noteOptional _ (Right (Just c)) = pure c
noteOptional _ (Left oe) = Left oe

peekLog
    :: Int
    -> V.Vector (TxId, [AccountLog])
    -> Either String (TxId, [AccountLog])
peekLog i logs = note
  ("no logs found at txIdx=" ++ show i)
  $ logs V.!? i

splitAtFirst
    :: Map a b
    -> Either String (T2 (a,b) [(a,b)])
splitAtFirst m = case (M.toAscList m) of
  [] -> Left "Received empty map"
  x:xs -> Right $ T2 x xs
