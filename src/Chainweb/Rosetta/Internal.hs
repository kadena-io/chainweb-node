{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module: Chainweb.Rosetta.Internal
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Linda Ortega <linda@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Rosetta.Internal where

import Control.Error.Util
import Control.Lens hiding ((??), from, to)
import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Map (Map)
import Data.List (foldl', find)
import Data.Default (def)
import Data.Decimal
import Data.Word (Word64)


import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Set as S

import qualified Pact.Parse as P
import qualified Pact.Types.Capability as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Runtime as P

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Runtime (TxId(..), Domain(..), TxLog(..))
import Pact.Types.Persistence (RowKey(..))
import Pact.Types.RowData (RowData)
import Pact.Types.PactValue

import Rosetta
import Servant.Server

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Pact.Service.Types (BlockTxHistory(..))
import Chainweb.Payload hiding (Transaction(..))
import Chainweb.Payload.PayloadStore
import Chainweb.Rosetta.Utils
import Chainweb.TreeDB (seekAncestor)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService (PactExecutionService(..))

import Chainweb.Storage.Table

---

--------------------------------------------------------------------------------
-- Internal Helper Types and Typeclasses --
--------------------------------------------------------------------------------

data LogType tx where
  FullLogs :: LogType [Transaction]
    -- ^ Signals wanting all Rosetta Transactions
  SingleLog :: RequestKey -> LogType Transaction
    -- ^ Signals wanting only a single Rosetta Transaction

data TxAccumulator rosettaTx = TxAccumulator
  { _txAccumulator_logsLeft :: ![(TxId, [AccountLog])]
    -- ^ Logs left to be matched
  , _txAccumulator_lastSeen :: !rosettaTx
    -- ^ Last Rosetta Transaction(s) seen so far
  }

data AccumulatorType rosettaTx where
  AppendTx :: AccumulatorType (TxAccumulator (DList.DList Transaction))
    -- ^ Signals wanting to keep track of all the Rosetta Transactions seen so far.
  Overwrite :: AccumulatorType (TxAccumulator Transaction)
    -- ^ Signals wanting to only keep track of the latest Rosetta Transaction seen so far.

accumulatorFunction
    :: AccumulatorType (TxAccumulator rosettaTx)
    -> TxAccumulator rosettaTx
    -> [(TxId, [AccountLog])]
    -> Transaction
    -> TxAccumulator rosettaTx
accumulatorFunction typ prevAcc logsLeft lastSeen = newAcc
  where
    TxAccumulator _ prevLastSeen = prevAcc
    f = case typ of
      AppendTx -> DList.snoc
      Overwrite -> (\_last next -> next)
    newAcc = TxAccumulator logsLeft (f prevLastSeen lastSeen)

--------------------------------------------------------------------------------
-- Transaction Log Matching Functions --
--------------------------------------------------------------------------------

-- | Retrieve the coin contract logs for transaction(s) in a block.
matchLogs
    :: LogType tx
    -> BlockHeader
    -> M.Map TxId [AccountLog]
    -> CoinbaseTx (CommandResult Hash)
    -> V.Vector (CommandResult Hash)
    -> ExceptT RosettaFailure Handler tx
matchLogs typ bh logs coinbase txs
  | bheight == genesisHeight v cid = matchGenesis
  | Just upg <- v ^? versionUpgrades . onChain cid . at bheight . _Just = matchRemediation upg
  | otherwise = matchRest
  where
    bheight = _blockHeight bh
    cid = _blockChainId bh
    v = _chainwebVersion bh

    matchGenesis = hoistEither $ case typ of
      FullLogs -> genesisTransactions logs cid txs
      SingleLog rk -> genesisTransaction logs cid txs rk

    matchRemediation upg = do
      hoistEither $ case typ of
        FullLogs ->
          overwriteError RosettaMismatchTxLogs $!
            remediations logs cid coinbase (_upgradeTransactions upg) txs
        SingleLog rk ->
          (noteOptional RosettaTxIdNotFound .
            overwriteError RosettaMismatchTxLogs) $
              singleRemediation logs cid coinbase (_upgradeTransactions upg) txs rk

    matchRest = hoistEither $ case typ of
      FullLogs ->
        overwriteError RosettaMismatchTxLogs $
          nonGenesisTransactions logs cid coinbase txs
      SingleLog rk ->
        (noteOptional RosettaTxIdNotFound .
          overwriteError RosettaMismatchTxLogs) $
            nonGenesisTransaction logs cid coinbase txs rk

---------------------
-- Genesis Helpers --
---------------------

-- | Using its TxId, lookup a genesis transaction's coin table logs (if any) in block's
--   map of all coin table logs.
--   NOTE: Genesis transactions do not have coinbase or gas payments.
getGenesisLog
    :: Map TxId [AccountLog]
    -> ChainId
    -> CommandResult Hash
    -> Transaction
getGenesisLog logs cid cr =
  case _crTxId cr of
    Just tid -> case M.lookup tid logs of
      Just l -> rosettaTransaction cr cid $! makeOps l
      Nothing -> rosettaTransaction cr cid []  -- not a coin contract tx
    Nothing -> rosettaTransaction cr cid [] -- all genesis tx should have a txid
  where
    makeOps l = indexedOperations $!
      UnindexedOperations
      { _unindexedOperation_fundOps = []
      , _unindexedOperation_transferOps =
          map (operation Successful TransferOrCreateAcct) l
      , _unindexedOperation_gasOps = []
      }

-- | Matches all genesis transactions to their coin contract logs.
genesisTransactions
    :: Map TxId [AccountLog]
    -> ChainId
    -> V.Vector (CommandResult Hash)
    -> Either RosettaFailure [Transaction]
genesisTransactions logs cid txs =
  pure $ V.toList $ V.map (getGenesisLog logs cid) txs

-- | Matches a single genesis transaction to its coin contract logs.
genesisTransaction
    :: Map TxId [AccountLog]
    -> ChainId
    -> V.Vector (CommandResult Hash)
    -> RequestKey
    -- ^ target tx
    -> Either RosettaFailure Transaction
genesisTransaction logs cid rest target = do
  cr <- note RosettaTxIdNotFound $
        V.find (\c -> _crReqKey c == target) rest
  pure $ getGenesisLog logs cid cr

------------------------
-- Coinbase Helpers --
------------------------

-- | Matches the first coin contract logs to the coinbase tx
nonGenesisCoinbaseLog
    :: PendingRosettaTx chainwebTx
    => [(TxId, [AccountLog])]
    -> ChainId
    -> CoinbaseTx chainwebTx
    -> Either String (TxAccumulator Transaction)
nonGenesisCoinbaseLog logs cid cr = case getSomeTxId cr of
  Nothing -> makeAcc logs []
  Just tid -> case logs of
    (coinbaseTid,coinbaseLog):restLogs
      | tid == coinbaseTid ->
        makeAcc restLogs
        (map (operation Successful CoinbaseReward) coinbaseLog)
      | otherwise -> Left "First log's TxId does not match coinbase tx's TxId"
    _ -> Left "Expected coinbase log: Received empty logs list"

  where
    makeAcc restLogs ops =
      let tx = makeRosettaTx cr cid $! indexedOperations $!
            UnindexedOperations
            { _unindexedOperation_fundOps = []
            , _unindexedOperation_transferOps = ops
            , _unindexedOperation_gasOps = []
            }
      in pure $ TxAccumulator restLogs tx

------------------------
-- NonGenesis Helpers --
------------------------

-- Motivation: Facilitate testing matching functions with non-CommandResult types.
class PendingRosettaTx chainwebTx where
  getSomeTxId :: chainwebTx -> Maybe TxId
  getRequestKey :: chainwebTx -> RequestKey
  makeRosettaTx :: chainwebTx -> ChainId -> [Operation] -> Transaction

instance PendingRosettaTx (CommandResult a) where
  getSomeTxId = _crTxId
  getRequestKey = _crReqKey
  makeRosettaTx = rosettaTransaction

-- | For a given tx, accumulates a triple of logs representing said tx's bracketing gas logs
--   and any coin contract logs caused by the tx itself (i.e. transfers, create-accounts).
--   Algorithm:
--   (1) Peek at the first two logs in the logs list.
--   (2) Assume the first log out of the two peeked funded the tx.
--   (3) Label as "unknown" the second log peeked for now.
--       It could be a gas or transfer logs, but more information is needed.
--   (4) Check if the tx has a TxId (i.e. was the tx successful or not).
--   (5) If the tx is unsuccessful and thus does not have a TxId, then it
--       wouldn't have any transfer logs. Thus, label the peeked "unknown" log (1)
--       as a gas payment log.
--   (6) But if the tx is successful and thus has a TxId, check whether this TxId
--       matches the TxId associated with the "unknown" peeked log (1).
--   (7) If the TxIds match, then label the "unknown" peeked log (1) as a transfer log.
--       And peek at the next log in the logs list and label it as a gas payment log.
--   (8) If the TxIds don't match, then the tx did not interact with the coin contract
--       and thus the "unknown" peeked logs (1) are gas payment logs.
gasTransactionAcc
    :: PendingRosettaTx chainwebTx
    => AccumulatorType (TxAccumulator rosettaTxAcc)
    -> ChainId
    -> TxAccumulator rosettaTxAcc
    -> chainwebTx
    -> Either String (TxAccumulator rosettaTxAcc)
gasTransactionAcc accTyp cid acc ctx = combine (_txAccumulator_logsLeft acc)
  where
    combine (fundLog:someLog:restLogs) =
      case getSomeTxId ctx of
        Nothing -> -- tx was unsuccessful
          makeAcc restLogs
            (makeOps FundTx fundLog)
            [] -- no transfer logs
            (makeOps GasPayment someLog)
        Just tid   -- tx was successful
          | tid /= txId someLog -> -- if tx didn't touch coin table
            makeAcc restLogs
              (makeOps FundTx fundLog)
              [] -- no transfer logs
              (makeOps GasPayment someLog)
          | otherwise -> case restLogs of
              gasLog:restLogs' -> -- if tx DID touch coin table
                makeAcc restLogs'
                  (makeOps FundTx fundLog)
                  (makeOps TransferOrCreateAcct someLog)
                  (makeOps GasPayment gasLog)
              l -> listErr "No gas logs found after transfer logs" l
    combine [f] = listErr "Only fund logs found" f
    combine [] = listErr "No logs found" ([] :: [(TxId, [AccountLog])])

    makeAcc restLogs fund transfer gas = pure $!
      accumulatorFunction accTyp acc restLogs tx
      where
        tx = makeRosettaTx ctx cid $! indexedOperations $!
          UnindexedOperations
          { _unindexedOperation_fundOps = fund
          , _unindexedOperation_transferOps = transfer
          , _unindexedOperation_gasOps = gas
          }

    txId (tid,_) = tid

    makeOps ot (_, als) =
      map (operation Successful ot) als

    listErr expectedMsg logs = Left $
      expectedMsg ++ ": Received logs list " ++ show logs


-- | Matches all transactions in a non-genesis block to their coin contract logs.
--   The first transaction in non-genesis blocks is the coinbase transaction.
--   Each transactions that follows has (1) logs that fund the transaction,
--   (2) optional tx specific coin contract logs, and (3) logs that pay gas.
--   TODO: Max limit of tx to return at once.
--         When to do pagination using /block/transaction?
nonGenesisTransactions
    :: PendingRosettaTx chainwebTx
    => Map TxId [AccountLog]
    -> ChainId
    -> CoinbaseTx chainwebTx
    -> V.Vector chainwebTx
    -> Either String [Transaction]
nonGenesisTransactions logs cid initial rest = do
  TxAccumulator restLogs initTx <- nonGenesisCoinbaseLog logsList cid initial
  TxAccumulator _ ts <- foldM match (defAcc restLogs initTx) rest
  pure $ DList.toList ts
  where
    logsList = M.toAscList logs
    match = gasTransactionAcc AppendTx cid
    defAcc li tx = TxAccumulator li (DList.singleton tx)


-- | Matches a single non-genesis transaction to its coin contract logs
-- if it exists in the given block.
nonGenesisTransaction
    :: PendingRosettaTx chainwebTx
    => Map TxId [AccountLog]
    -> ChainId
    -> CoinbaseTx chainwebTx
    -> V.Vector chainwebTx
    -> RequestKey
    -- ^ Lookup target
    -> Either String (Maybe Transaction)
nonGenesisTransaction logs cid initial rest target
  | getRequestKey initial == target = do
      -- Looking for coinbase tx
      TxAccumulator _ initTx <- nonGenesisCoinbaseLog logsList cid initial
      pure $ Just initTx
  | otherwise = do
      -- Traverse list matching transactions to their logs.
      -- If target's logs found or if error throw by matching function,
      -- short circuit.
      TxAccumulator restLogs initTx <- nonGenesisCoinbaseLog logsList cid initial
      let acc = TxAccumulator restLogs initTx
      fromShortCircuit $ foldM findTxAndLogs acc rest

  where
    logsList = M.toAscList logs
    match = gasTransactionAcc Overwrite cid

    findTxAndLogs acc cr = do
      TxAccumulator logsLeft lastSeenTx <- shortCircuit (match acc cr)
      if getRequestKey cr == target
        then Left $ Right lastSeenTx
          -- short-circuit if find target tx's logs
        else pure (TxAccumulator logsLeft lastSeenTx)
          -- continue matching other txs' logs until find target

    shortCircuit
        :: Either String (TxAccumulator Transaction)
        -> Either (Either String Transaction) (TxAccumulator Transaction)
    shortCircuit (Left e) = Left $ Left e
      -- short-circuit if matching function threw error
    shortCircuit (Right r) = Right r

    fromShortCircuit
        :: Either (Either String Transaction) (TxAccumulator Transaction)
        -> Either String (Maybe Transaction)
    fromShortCircuit (Right _) = pure Nothing
        -- Tx not found
    fromShortCircuit (Left (Left s)) = Left s
    fromShortCircuit (Left (Right tx)) = pure (Just tx)
        -- Tx found

-------------------------
-- Remediation Helpers --
-------------------------

-- | Given a remediation Command and its assumed TxId:
--     Query the latest coin table log in the list.
--     If the TxId of this coin table log matches the remediation's TxId,
--       then assumes that this log corresponds to the given remediation.
--     Otherwise, a rosetta transaction is created for the given
--       remediation with an empty operations list.
-- NOTE: Assumes that each remediation must have been successful and thus
--       would have a TxId associated with it.
-- NOTE: Not all remediations transactions will output coin table logs.
remediationAcc
    :: AccumulatorType (TxAccumulator rosettaTx)
    -> TxAccumulator rosettaTx
    -> (Command payload, TxId)
    -> TxAccumulator rosettaTx
remediationAcc accTyp acc (remTx, remTid) =
  case _txAccumulator_logsLeft acc of
    (logTid,logs):rest
      | logTid == remTid -> -- remediation touched coin table
        let ops = indexedOperations $!
              UnindexedOperations
              { _unindexedOperation_fundOps = []
              , _unindexedOperation_transferOps = makeOps logs
              , _unindexedOperation_gasOps = []
              }
            rosettaTx = rosettaTransactionFromCmd remTx $! ops
        in makeAcc rest rosettaTx
    rest -> -- list of logs empty or remediation didn't touch coin table
      makeAcc rest $!
      rosettaTransactionFromCmd remTx []
  where
    makeAcc restLogs rosettaTx =
      accumulatorFunction accTyp acc restLogs rosettaTx
    makeOps logs =
      map (operation Remediation TransferOrCreateAcct) logs

-- | Matches all transactions in a remediation block
--   (including coinbase, remediations, and user transactions)
--   to their coin table logs.
--  Coinbase logs are matched first, followed by remediations txs,
--  and finally user transactions are matched using the same algorithm
--  as non-genesis transactions.
-- NOTE: Assumes that each remediation transaction incremented
--       the TxId counter by one.
-- NOTE: Assumes remediations don't occur in blocks where coinbase transaction
--       could have failed. It needs to know the TxId of the coinbase transaction in
--       order to derive the remediation's TxIds.
remediations
    :: Map TxId [AccountLog]
    -> ChainId
    -> CoinbaseTx (CommandResult Hash)
    -- Remediation transactions.
    -- NOTE: No CommandResult available for these.
    -> [Command payload]
    -- User transactions in the same block as remediations
    -> V.Vector (CommandResult Hash)
    -> Either String [Transaction]
remediations logs cid coinbase remTxs txs = do
  TxAccumulator restLogs coinbaseTx <- nonGenesisCoinbaseLog logsList cid coinbase
  coinbaseTxId <- note "remediations: No TxId found for Coinbase" (_crTxId coinbase)

  let remWithTxIds = zip remTxs [(succ coinbaseTxId)..]
      -- Assumes that each remediation transaction gets its own TxId.
      -- Assumes that TxIds are going to be sequential for each remediation.

      -- Note (linda and emily): This assumption holds, since the remediation txs are
      -- applied directly after coinbase at a particular height, as part of applyCoinbase.
      -- We construct the blocks, hence, the txids are not random.
      -- See for more details:
      -- https://github.com/kadena-io/chainweb-node/blob/c0c300a64040390d603f1183eac126e3bbfebe8d/src/Chainweb/Pact/TransactionExec.hs#L328

      accWithCoinbase = TxAccumulator restLogs (DList.singleton coinbaseTx)
      accWithRems = foldl' matchRem accWithCoinbase remWithTxIds

  TxAccumulator _ ts <- foldM matchOtherTxs accWithRems txs
  pure $ DList.toList ts

  where
    logsList = M.toAscList logs
    matchRem = remediationAcc AppendTx
    matchOtherTxs = gasTransactionAcc AppendTx cid


-- | Matches a single request key to its coin table logs in a block
--   with remediations.
singleRemediation
    :: Map TxId [AccountLog]
    -> ChainId
    -> CoinbaseTx (CommandResult Hash)
    -> [Command payload]
    -- ^ Remediation transactions.
    -- ^ NOTE: No CommandResult available for these.
    -> V.Vector (CommandResult Hash)
    -- ^ User transactions in the same block as remediations
    -> RequestKey
    -- ^ target
    -> Either String (Maybe Transaction)
singleRemediation logs cid coinbase remTxs txs rkTarget = do
  rosettaTxs <- remediations logs cid coinbase remTxs txs
  pure $ find isTargetTx rosettaTxs
  -- TODO: Make searching for tx and its logs more efficient.
  where
    isTargetTx rtx =
      rkToTransactionId rkTarget == _transaction_transactionId rtx

--------------------------------------------------------------------------------
-- Chainweb Helper Functions --
--------------------------------------------------------------------------------

getLatestBlockHeader
    :: CutDb tbl
    -> ChainId
    -> ExceptT RosettaFailure Handler BlockHeader
getLatestBlockHeader cutDb cid = do
  c <- liftIO $ _cut cutDb
  HM.lookup cid (_cutMap c) ?? RosettaInvalidChain

findBlockHeaderInCurrFork
    :: CutDb tbl
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
      bhashExpected <- blockHashFromText hsh ?? RosettaUnparsableBlockHash
      if _blockHash bh == bhashExpected
        then pure bh
        else throwError RosettaMismatchBlockHashHeight
    (Nothing, Just hsh) -> do
      bhash <- blockHashFromText hsh ?? RosettaUnparsableBlockHash
      somebh <- liftIO (tableLookup chainDb bhash)
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
    :: forall tbl
    . CanReadablePayloadCas tbl
    => PayloadDb tbl
    -> BlockHeader
    -> ExceptT RosettaFailure Handler (CoinbaseTx (CommandResult Hash), V.Vector (CommandResult Hash))
getBlockOutputs payloadDb bh = do
  someOut <- liftIO $ tableLookup payloadDb (_blockPayloadHash bh)
  outputs <- someOut ?? RosettaPayloadNotFound
  txsOut <- decodeTxsOut outputs ?? RosettaUnparsableTxOut
  coinbaseOut <- decodeCoinbaseOut outputs ?? RosettaUnparsableTxOut
  pure (coinbaseOut, txsOut)

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
  someHist <- liftIO $ _pactBlockTxHistory cr bh d
  (BlockTxHistory hist prevTxs) <- hush someHist ?? RosettaPactExceptionThrown
  lastBalSeen <- hoistEither $ parsePrevTxs prevTxs
  histAcctRow <- hoistEither $ parseHist hist
  pure $ getBalanceDeltas histAcctRow lastBalSeen
  where
    d = UserTables "coin_coin-table"

    parseHist
        :: Map TxId [TxLog RowData]
        -> Either RosettaFailure (Map TxId [AccountRow])
    parseHist m
      | M.size parsed == M.size m = pure $! parsed
      | otherwise = throwError RosettaUnparsableTxLog
      where
        parsed = M.mapMaybe (mapM txLogToAccountRow) m

    parsePrevTxs
        :: Map RowKey (TxLog RowData)
        -> Either RosettaFailure (Map RowKey AccountRow)
    parsePrevTxs m
      | M.size parsed == M.size m = pure $! parsed
      | otherwise = throwError RosettaUnparsableTxLog
      where
        parsed = M.mapMaybe txLogToAccountRow m

getBalanceDeltas
    :: Map TxId [AccountRow]
    -> Map RowKey AccountRow
    -> Map TxId [AccountLog]
getBalanceDeltas hist lastBalsSeenDef =
  snd $! M.mapAccumWithKey f lastBalsSeenDef hist
  where
    -- | For given txId and the rows it affected, calculate
    -- | how each row key has changed since previously seen.
    -- | Adds or updates map of previously seen rows with each
    -- | of this txId's rows.
    f
      :: Map RowKey AccountRow
      -> TxId
      -> [AccountRow]
      -> (Map RowKey AccountRow, [AccountLog])
    f lastBals _txId currRows = (updatedBals, reverse logs)
      where
        (updatedBals, logs) = foldl' helper (lastBals, []) currRows
        helper (bals, li) row = (bals', li')
          where
            (bals', acctLog) = lookupAndUpdate bals row
            li' = acctLog:li -- needs to be reversed at the end

    -- | Lookup current row key in map of previous seen rows
    -- | to calculate how row has changed.
    -- | Adds or updates the map of previously seen rows with
    -- | the current row.
    lookupAndUpdate
        :: Map RowKey AccountRow
        -> AccountRow
        -> (Map RowKey AccountRow, AccountLog)
    lookupAndUpdate lastBals currRow = (lastBals', acctLog)
      where
        (key, _, _) = currRow
        (prevRow, lastBals') =
          M.insertLookupWithKey
          lookupAndReplace
          (RowKey key)
          currRow
          lastBals
        acctLog = rowDataToAccountLog currRow prevRow
        lookupAndReplace _key new _old = new


-- | Lookup the row value of a coin-contract key
-- at a given block.
getHistoricalLookupBalance'
    :: PactExecutionService
    -> BlockHeader
    -> T.Text
    -> ExceptT RosettaFailure Handler (Maybe AccountRow)
getHistoricalLookupBalance' cr bh k = do
  someHist <- liftIO $ _pactHistoricalLookup cr bh d key
  hist <- hush someHist ?? RosettaPactExceptionThrown
  case hist of
    Nothing -> pure Nothing
    Just h -> do
      row <- txLogToAccountRow h ?? RosettaUnparsableTxLog
      pure $ Just row
  where
    d = UserTables "coin_coin-table"
    key = RowKey k -- TODO: How to sanitize this further

getHistoricalLookupBalance
    :: PactExecutionService
    -> BlockHeader
    -> T.Text
    -> ExceptT RosettaFailure Handler Decimal
getHistoricalLookupBalance cr bh k = do
  someRow <- getHistoricalLookupBalance' cr bh k
  case someRow of
    Nothing -> pure 0.0 -- key not present
    Just (_,bal,_) -> pure bal


rosettaErrorT
    :: Maybe String
    -> ExceptT RosettaFailure Handler a
    -> ExceptT RosettaError Handler a
rosettaErrorT someMsg = mapExceptT f
  where
    f :: Handler (Either RosettaFailure b)
      -> Handler (Either RosettaError b)
    f run = do
      eitherRes <- run
      case eitherRes of
        Left failure ->
          case someMsg of
            Nothing -> pure $ Left $ rosettaError' failure
            Just msg -> pure $ Left $ stringRosettaError failure msg
        Right r -> pure $ Right r


neededAccounts
    :: ConstructionTx
    -> AccountId
    -> [AccountId]
neededAccounts txInfo payerAcct = S.toList $
  case txInfo of
    ConstructTransfer from _ _ _ _ ->
      S.insert from m
  where
    -- Uses Sets to avoid repeating accounts
    -- (i.e. the gas payer is the same as the transfer)
    m = S.singleton payerAcct


-- | Iterates through the `ConstructionTx` to determine
-- the accounts needed for signing, determines their required
-- capabilities, and queries the blockchain to determine the keys
-- associated with said accounts.
toSignerAcctsMap
    :: ConstructionTx
    -> AccountId
    -> ChainId
    -> [(ChainId, PactExecutionService)]
    -> CutDb tbl
    -> ExceptT RosettaError Handler
       (HM.HashMap AccountId ([P.SigCapability], [T.Text]))
toSignerAcctsMap txInfo payerAcct cid pacts cutDb = do
  bhCurr <- rosettaErrorT Nothing $
            getLatestBlockHeader cutDb cid
  peCurr <- rosettaErrorT Nothing $
            lookup cid pacts ?? RosettaInvalidChain

  -- GAS
  someGasOwner <- getOwnership peCurr bhCurr payerAcct
  gasOwner <- enforceAcctPresent payerAcct someGasOwner
  let gasCaps = [ mkGasCap ]
      mapWithGas = HM.singleton
        payerAcct
        (gasCaps, gasOwner)

  -- TRANSACTION
  case txInfo of
    ConstructTransfer from fromGuard to toGuard (P.ParsedDecimal amt) -> do
      let expectedFrom = ksToPubKeys fromGuard
          -- `to` acount could be getting created
          expectedTo = ksToPubKeys toGuard

      someActualFrom <- getOwnership peCurr bhCurr from
      someActualTo <- getOwnership peCurr bhCurr to

      _ <- enforceAcctPresent from someActualFrom
      checkExpectedOwnership from expectedFrom someActualFrom
      checkExpectedOwnership to expectedTo someActualTo

      let capsFrom = [ mkTransferCap from to amt ]

      pure $ insertWith' from (capsFrom, expectedFrom) mapWithGas
  where
    getOwnership cr bh k = do
      someRow <- rosettaErrorT Nothing $
        getHistoricalLookupBalance' cr bh (_accountId_address k)
      case someRow of
        Nothing -> pure Nothing
        Just (_,_,g) -> hoistEither $ Just <$> parsePubKeys (_accountId_address k) g

    insertWith'
        :: AccountId
        -> ([P.SigCapability], [T.Text])
        -> HM.HashMap AccountId ([P.SigCapability], [T.Text])
        -> HM.HashMap AccountId ([P.SigCapability], [T.Text])
    insertWith' acct sigs m = HM.insertWith f acct sigs m
      where
        f (newSigs, _) (oldSigs, oldKeys) =
          (oldSigs <> newSigs, oldKeys) -- keys wouldn't change

    -- Cap smart constructor.
    mkCapability
        :: P.ModuleName
        -> T.Text
        -> [PactValue]
        -> P.SigCapability
    mkCapability mn cap args =
      P.SigCapability (P.QualifiedName mn cap def) args

    -- Convenience to make caps like TRANSFER, GAS etc.
    mkCoinCap
        :: T.Text
        -> [PactValue]
        -> P.SigCapability
    mkCoinCap n = mkCapability "coin" n

    mkTransferCap
        :: AccountId
        -> AccountId
        -> Decimal
        -> P.SigCapability
    mkTransferCap sender receiver amount = mkCoinCap "TRANSFER"
      [ pString (_accountId_address sender),
        pString (_accountId_address receiver),
        pDecimal amount ]

    mkGasCap :: P.SigCapability
    mkGasCap = mkCoinCap "GAS" []

    -- Make PactValue from text
    pString :: T.Text -> PactValue
    pString = PLiteral . P.LString

    -- Make PactValue from decimal
    pDecimal :: Decimal -> PactValue
    pDecimal = PLiteral . P.LDecimal

enforceAcctPresent
    :: AccountId
    -> Maybe [T.Text]
    -> ExceptT RosettaError Handler [T.Text]
enforceAcctPresent k actualOwnership =
  case actualOwnership of
    Just pks -> pure pks
    Nothing -> -- key missing (not expected)
      hoistEither $ Left $
      stringRosettaError RosettaInvalidAccountProvided $
      "Account=" ++ show k ++ " doesn't exists"

checkExpectedOwnership
    :: AccountId
    -> [T.Text]
    -> Maybe [T.Text]
    -> ExceptT RosettaError Handler ()
checkExpectedOwnership _ _ Nothing = pure ()
checkExpectedOwnership acct expected (Just actual)
  | expected == actual = pure ()
  | otherwise = hoistEither $ Left $
      stringRosettaError RosettaInvalidAccountProvided $
      "Account=" ++ show acct ++ ": Provided public key addresses "
      ++ show expected ++
      " doesn't match the account's actual public key addresses "
      ++ show actual

-- | Maps a Pact Address (derived from PublicKey) to a
--   function to create a Signer.
rosettaPubKeysToSignerMap
    :: [RosettaPublicKey]
    -> Either RosettaError (HM.HashMap T.Text ([P.SigCapability] -> Signer))
rosettaPubKeysToSignerMap pubKeys = HM.fromList <$> mapM f pubKeys
  where
    f (RosettaPublicKey pk ct) = do
      sk <- getScheme ct
      addr <- toPactPubKeyAddr pk
      let signerWithoutCap = P.Signer (Just sk) pk (Just addr)
      pure (addr, signerWithoutCap)

createSigners
    :: HM.HashMap T.Text ([P.SigCapability] -> Signer)
    -> HM.HashMap AccountId ([P.SigCapability], [T.Text])
    -> Either RosettaError [(Signer, AccountId)]
createSigners addrToSignerMap acctToCapMap =
  -- NOTE: There might be duplicates signers but that's okay
  concat <$> mapM f (HM.toList acctToCapMap)
  where
    f (acct, (caps, pubKeyAddrs)) =
      mapM (lookupSigner acct caps) pubKeyAddrs

    lookupSigner :: AccountId -> [P.SigCapability] -> T.Text -> Either RosettaError (Signer, AccountId)
    lookupSigner acct caps pkAddr = do
      mkSigner <- toRosettaError RosettaMissingExpectedPublicKey $
                  note ("No Rosetta Public Key found for pact public key address="
                        ++ show pkAddr ++ " for AccountId=" ++ show acct)
                  (HM.lookup pkAddr addrToSignerMap)
      pure (mkSigner caps, acct)
