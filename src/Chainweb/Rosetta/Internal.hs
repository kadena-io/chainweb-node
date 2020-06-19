{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
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
import Control.Lens ((^?))
import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Map (Map)
import Data.Decimal
import Data.CAS
import Data.Word (Word64)


import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Runtime (TxId(..), Domain(..))
import Pact.Types.Persistence (RowKey(..))

import Rosetta
import Servant.Server

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Pact.Service.Types (Domain'(..), BlockTxHistory(..))
import Chainweb.Payload hiding (Transaction(..))
import Chainweb.Payload.PayloadStore
import Chainweb.Rosetta.RestAPI
import Chainweb.Rosetta.Utils
import Chainweb.TreeDB (seekAncestor)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService (PactExecutionService(..))

---

--------------------------------------------------------------------------------
-- Internal Helper Types and Typeclasses --
--------------------------------------------------------------------------------

data LogType tx where
  FullLogs :: LogType [Transaction]
  -- ^ Signals wanting all Rosetta Transactions
  SingleLog :: RequestKey -> LogType Transaction
  -- ^ Signals wanting only a single Rosetta Transaction

class PendingTx chainwebTx where
  getSomeTxId :: chainwebTx -> Maybe TxId
  getRequestKey :: chainwebTx -> RequestKey
  makeRosettaTx :: chainwebTx -> [Operation] -> Transaction

instance PendingTx (CommandResult a) where
  getSomeTxId = _crTxId
  getRequestKey = _crReqKey
  makeRosettaTx = rosettaTransaction

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
    -> Either RosettaFailure tx
matchLogs typ bh logs coinbase txs
  | bheight == genesisHeight v cid = matchGenesis
  | coinV2Upgrade v cid bheight = matchRemediation
  | otherwise = matchRest
  where
    bheight = _blockHeight bh
    cid = _blockChainId bh
    v = _blockChainwebVersion bh

    matchGenesis = case typ of
      FullLogs -> genesisTransactions logs txs
      SingleLog rk -> genesisTransaction logs txs rk

    matchRemediation = case typ of
      FullLogs -> remediations logs coinbase
      SingleLog rk -> singleRemediation logs coinbase rk

    matchRest = case typ of
      FullLogs ->
        overwriteError RosettaMismatchTxLogs $
          nonGenesisTransactions logs coinbase txs
      SingleLog rk ->
        (noteOptional RosettaTxIdNotFound .
          overwriteError RosettaMismatchTxLogs) $
            nonGenesisTransaction logs coinbase txs rk

---------------------
-- Genesis Helpers --
---------------------

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


-- | Matches all genesis transactions to their coin contract logs.
genesisTransactions
    :: Map TxId [AccountLog]
    -> V.Vector (CommandResult Hash)
    -> Either RosettaFailure [Transaction]
genesisTransactions logs txs =
  pure $ V.toList $ V.map (getGenesisLog logs) txs


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


------------------------
-- Coinbase Helpers --
------------------------

-- | Matches the first coin contract logs to the coinbase tx
nonGenesisCoinbaseLog
    :: PendingTx chainwebTx
    => [(TxId, [AccountLog])]
    -> CoinbaseTx chainwebTx
    -> Either String (TxAccumulator Transaction)
nonGenesisCoinbaseLog logs cr = case (getSomeTxId cr) of
  Nothing -> makeAcc logs []
  Just tid -> case logs of
    (coinbaseTid,coinbaseLog):restLogs
      | tid == coinbaseTid ->
        makeAcc restLogs
        (map (operation Successful CoinbaseReward tid) coinbaseLog)
      | otherwise -> Left $ "First log's TxId does not match coinbase tx's TxId"
    _ -> Left "Expected coinbase log: Received empty logs list"

  where
    makeAcc restLogs ops =
      let tx = makeRosettaTx cr $ indexedOperations $ ops
      in pure $ TxAccumulator restLogs tx

------------------------
-- NonGenesis Helpers --
------------------------

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
    :: PendingTx chainwebTx
    => AccumulatorType (TxAccumulator rosettaTxAcc)
    -> TxAccumulator rosettaTxAcc
    -> chainwebTx
    -> Either String (TxAccumulator rosettaTxAcc)
gasTransactionAcc accTyp txa@(TxAccumulator logs' _) ctx = combine logs'
  where
    combine (fundLog:someLog:restLogs) =
      case (getSomeTxId ctx) of
        Nothing -> -- tx was unsuccessful
          makeAcc restLogs
          (makeOps FundTx fundLog)
          [] -- no transfer logs
          (makeOps GasPayment someLog)
        Just tid   -- tx was successful
          | tid /= (fst someLog) ->  -- tx didn't touch coin table
            makeAcc restLogs
            (makeOps FundTx fundLog)
            [] -- no transfer logs
            (makeOps GasPayment someLog)
          | otherwise -> case restLogs of
              gasLog:restLogs' -> makeAcc restLogs'
                (makeOps FundTx fundLog)
                (makeOps TransferOrCreateAcct someLog)
                (makeOps GasPayment gasLog)
              l -> listErr "No gas logs found after transfer logs" l
    combine l = listErr "No fund and gas logs found" l

    makeAcc restLogs fund transfer gas = pure $
      accumulatorFunction accTyp txa restLogs tx
      where
        tx = makeRosettaTx ctx $ indexedOperations $
             fund <> transfer <> gas

    makeOps ot (tid, als) =
      map (operation Successful ot tid) als

    listErr expectedMsg logs = Left $
      expectedMsg ++ ": Received logs list " ++ show logs


-- TODO: Max limit of tx to return at once.
--       When to do pagination using /block/transaction?
-- | Matches all transactions in a non-genesis block to their coin contract logs.
--   The first transaction in non-genesis blocks is the coinbase transaction.
--   Each transactions that follows has (1) logs that fund the transaction,
--   (2) optional tx specific coin contract logs, and (3) logs that pay gas.
nonGenesisTransactions
    :: PendingTx chainwebTx
    => Map TxId [AccountLog]
    -> CoinbaseTx chainwebTx
    -> V.Vector chainwebTx
    -> Either String [Transaction]
nonGenesisTransactions logs initial rest = do
  TxAccumulator restLogs initTx <- nonGenesisCoinbaseLog logsList initial
  TxAccumulator _ ts <- foldM match (defAcc restLogs initTx) rest
  pure $ DList.toList ts
  where
    logsList = M.toAscList logs
    match = gasTransactionAcc AppendTx
    defAcc li tx = TxAccumulator li (DList.singleton tx)


-- | Matches a single non-genesis transaction to its coin contract logs
-- if it exists in the given block.
nonGenesisTransaction
    :: PendingTx chainwebTx
    => Map TxId [AccountLog]
    -> CoinbaseTx chainwebTx
    -> V.Vector chainwebTx
    -> RequestKey
    -- ^ Lookup target
    -> Either String (Maybe Transaction)
nonGenesisTransaction logs initial rest target
  | (getRequestKey initial == target) = do
      -- | Looking for coinbase tx
      TxAccumulator _ initTx <- nonGenesisCoinbaseLog logsList initial
      pure $ Just initTx
  | otherwise = do
      -- | Traverse list matching transactions to their logs.
      -- If target's logs found or if error throw by matching function,
      -- short circuit.
      TxAccumulator restLogs initTx <- nonGenesisCoinbaseLog logsList initial
      let acc = TxAccumulator restLogs initTx
      fromShortCircuit $ foldM findTxAndLogs acc rest

  where
    logsList = M.toAscList logs
    match = gasTransactionAcc Overwrite

    findTxAndLogs acc cr = do
      TxAccumulator logsLeft lastSeenTx <- shortCircuit (match acc cr)
      if (getRequestKey cr == target)
        then Left $ Right lastSeenTx
        -- ^ short-circuit if find target tx's logs
        else pure $ (TxAccumulator logsLeft lastSeenTx)
        -- ^ continue matching other txs' logs until find target

    shortCircuit
        :: Either String (TxAccumulator Transaction)
        -> Either (Either String Transaction) (TxAccumulator Transaction)
    shortCircuit (Left e) = Left $ Left e
    -- ^ short-circuit if matching function threw error
    shortCircuit (Right r) = Right r

    fromShortCircuit
        :: Either (Either String Transaction) (TxAccumulator Transaction)
        -> Either String (Maybe Transaction)
    fromShortCircuit (Right _) = pure Nothing
    -- ^ Tx not found
    fromShortCircuit (Left (Left s)) = Left s
    fromShortCircuit (Left (Right tx)) = pure (Just tx)
    -- ^ Tx found


-------------------------
-- Remediation Helpers --
-------------------------

remediationRequestKey :: RequestKey
remediationRequestKey = RequestKey $ pactHash "remediation"


-- | Group the rest of the logs into a single transaction id because
-- remediations all have different txIds and we don't have access to
-- their command results.
-- NOTE: If a normal transaction occurs in this block, it will be grouped with
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
    -> CoinbaseTx (CommandResult Hash)
    -> Either RosettaFailure [Transaction]
remediations logs initial =
    overwriteError RosettaMismatchTxLogs work
  where
    logsList = M.toAscList logs
    work = do
      TxAccumulator restLogs initTx <- nonGenesisCoinbaseLog logsList initial
      pure $! [initTx, getRemediationsTx restLogs]

singleRemediation
    :: Map TxId [AccountLog]
    -> CoinbaseTx (CommandResult Hash)
    -> RequestKey
    -- ^ target
    -> Either RosettaFailure Transaction
singleRemediation logs initial target =
    (noteOptional RosettaTxIdNotFound .
     overwriteError RosettaMismatchTxLogs)
    work
  where
    logsList = M.toAscList logs
    work = do
      TxAccumulator restLogs initTx <- nonGenesisCoinbaseLog logsList initial
      if (_crReqKey initial == target)
        then pure $ Just initTx
        else if (target == remediationRequestKey)
          then pure $ Just $ getRemediationsTx restLogs
          else pure Nothing -- Tx not found

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
    -> ExceptT RosettaFailure Handler (CoinbaseTx (CommandResult Hash), V.Vector (CommandResult Hash))
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
    key = RowKey k  -- TODO: How to sanitize this further
