{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Pact.PactService.ExecBlock
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Lars Kuhtz, Emily Pillmore, Stuart Popejoy
-- Stability: experimental
--
-- Functionality for playing block transactions.
--
module Chainweb.Pact.PactService.ExecBlock
    ( setParentHeader
    , execBlock
    , execTransactions
    , toHashCommandResult
    , minerReward
    , toPayloadWithOutputs
    , validateChainwebTxs
    , validateHashes
    ) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Aeson as A
import qualified Data.ByteString.Short as SB
import Data.Decimal
import Data.Default (def)
import Data.DList (DList(..))
import qualified Data.DList as DL
import Data.Either
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tuple.Strict (T2(..))
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.IO

import Prelude hiding (lookup)

import Pact.Compile (compileExps)
import qualified Pact.Interpreter as P
import qualified Pact.Parse as P
import qualified Pact.Types.Command as P
import Pact.Types.Exp (ParsedCode(..))
import Pact.Types.ExpParser (mkTextInfo)
import qualified Pact.Types.Hash as P
import Pact.Types.RPC
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.SPV as P

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Mempool.Mempool as Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.NoCoinbase
import Chainweb.Pact.Service.Types
import Chainweb.Pact.SPV
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Pact.Utils
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Transaction
import Chainweb.Utils hiding (check)
import Chainweb.Version


-- | Set parent header in state and spv support (using parent hash)
setParentHeader :: String -> ParentHeader -> PactServiceM cas ()
setParentHeader msg ph@(ParentHeader bh) = do
  logDebug $ "setParentHeader: " ++ msg ++ ": " ++ show (_blockHash bh,_blockHeight bh)
  modify' $ set psParentHeader ph
  bdb <- view psBlockHeaderDb
  modify' $ set psSpvSupport $! pactSPV bdb (_blockHash bh)

-- | Execute a block -- only called in validate either for replay or for validating current block.
--
-- /NOTE:/
--
-- Any call of this function must occur within a dedicated call to
-- 'withChwithCheckpointerRewind', 'withCurrentCheckpointer' or
-- 'withCheckPointerWithoutRewind'.
--
execBlock
    :: (PayloadCasLookup cas)
    => BlockHeader
        -- ^ this is the current header. We may consider changing this to the parent
        -- header to avoid confusion with new block and prevent using data from this
        -- header when we should use the respective values from the parent header
        -- instead.
    -> PayloadData
    -> PactDbEnv'
    -> PactServiceM cas (T2 Miner Transactions)
execBlock currHeader plData pdbenv = do

    unlessM ((> 0) <$> asks _psCheckpointerDepth) $ do
        error $ "Code invariant violation: execBlock must be called with withCheckpointer. Please report this as a bug."

    miner <- decodeStrictOrThrow' (_minerData $ _payloadDataMiner plData)
    trans <- liftIO $ transactionsFromPayload plData
    cp <- getCheckpointer

    -- The reference time for tx timings validation.
    --
    -- The legacy behavior is to use the creation time of the /current/ header.
    -- The new default behavior is to use the creation time of the /parent/ header.
    --
    txValidationTime <- if isGenesisBlockHeader currHeader
      then
        return (ParentCreationTime $ _blockCreationTime currHeader)
      else
         ParentCreationTime . _blockCreationTime . _parentHeader <$> use psParentHeader

    -- prop_tx_ttl_validate
    valids <- liftIO $ V.zip trans <$>
        validateChainwebTxs v cid cp txValidationTime
            (_blockHeight currHeader) trans skipDebitGas

    case foldr handleValids [] valids of
      [] -> return ()
      errs -> throwM $ TransactionValidationException $ errs

    !results <- go miner trans
    modify' $ set psStateValidated $ Just currHeader

    -- Validate hashes if requested
    asks _psValidateHashesOnReplay >>= \x -> when x $
        either throwM (void . return) $!
        validateHashes currHeader plData miner results

    return $! T2 miner results

  where

    handleValids (tx,Left e) es = (P._cmdHash tx, sshow e):es
    handleValids _ es = es

    v = _chainwebVersion currHeader
    cid = _chainId currHeader

    isGenesisBlock = isGenesisBlockHeader currHeader

    go m txs = if isGenesisBlock
      then do
        -- GENESIS VALIDATE COINBASE: Reject bad coinbase, use date rule for precompilation
        execTransactions True m txs
          (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled False) pdbenv
      else do
        -- VALIDATE COINBASE: back-compat allow failures, use date rule for precompilation
        execTransactions False m txs
          (EnforceCoinbaseFailure False) (CoinbaseUsePrecompiled False) pdbenv


-- | The principal validation logic for groups of Pact Transactions.
--
-- Skips validation for genesis transactions, since gas accounts, etc. don't
-- exist yet.
--
validateChainwebTxs
    :: ChainwebVersion
    -> ChainId
    -> Checkpointer
    -> ParentCreationTime
        -- ^ reference time for tx validation.
    -> BlockHeight
        -- ^ Current block height
    -> Vector ChainwebTransaction
    -> RunGas
    -> IO ValidateTxs
validateChainwebTxs v cid cp txValidationTime bh txs doBuyGas
  | bh == genesisHeight v cid = pure $! V.map Right txs
  | V.null txs = pure V.empty
  | otherwise = go
  where
    go = V.mapM validations initTxList >>= doBuyGas

    validations t = runValid checkUnique t
      >>= runValid checkTimes
      >>= runValid (return . checkCompile)

    checkUnique :: ChainwebTransaction -> IO (Either InsertError ChainwebTransaction)
    checkUnique t = do
      found <- _cpLookupProcessedTx cp (P._cmdHash t)
      case found of
        Nothing -> pure $ Right t
        Just _ -> pure $ Left InsertErrorDuplicate

    checkTimes :: ChainwebTransaction -> IO (Either InsertError ChainwebTransaction)
    checkTimes t
        | skipTxTimingValidation v bh = return $ Right t
        | timingsCheck txValidationTime $ fmap payloadObj t = return $ Right t
        | otherwise = return $ Left InsertErrorInvalidTime

    initTxList :: ValidateTxs
    initTxList = V.map Right txs

    runValid :: Monad m => (a -> m (Either e a)) -> Either e a -> m (Either e a)
    runValid f (Right r) = f r
    runValid _ l@Left{} = pure l


type ValidateTxs = Vector (Either InsertError ChainwebTransaction)
type RunGas = ValidateTxs -> IO ValidateTxs

checkCompile :: ChainwebTransaction -> Either InsertError ChainwebTransaction
checkCompile tx = case payload of
  Exec (ExecMsg parsedCode _) ->
    case compileCode parsedCode of
      Left perr -> Left $ InsertErrorCompilationFailed (sshow perr)
      Right _ -> Right tx
  _ -> Right tx
  where
    payload = P._pPayload $ payloadObj $ P._cmdPayload tx
    compileCode p =
      compileExps (mkTextInfo (P._pcCode p)) (P._pcExps p)

skipDebitGas :: RunGas
skipDebitGas = return

execTransactions
    :: Bool
    -> Miner
    -> Vector ChainwebTransaction
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> PactDbEnv'
    -> PactServiceM cas Transactions
execTransactions isGenesis miner ctxs enfCBFail usePrecomp (PactDbEnv' pactdbenv) = do
    mc <- use psInitCache
    coinOut <- runCoinbase isGenesis pactdbenv miner enfCBFail usePrecomp mc
    txOuts <- applyPactCmds isGenesis pactdbenv ctxs miner mc
    return $! Transactions (V.zip ctxs txOuts) coinOut

runCoinbase
    :: Bool
    -> P.PactDbEnv p
    -> Miner
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> ModuleCache
    -> PactServiceM cas (P.CommandResult [P.TxLog A.Value])
runCoinbase True _ _ _ _ _ = return noCoinbase
runCoinbase False dbEnv miner enfCBFail usePrecomp mc = do
    logger <- view (psCheckpointEnv . cpeLogger)
    rs <- view psMinerRewards
    v <- view chainwebVersion
    pd <- getTxContext def

    let !bh = ctxCurrentBlockHeight pd

    reward <- liftIO $! minerReward v rs bh

    (T2 cr upgradedCacheM) <-
      liftIO $! applyCoinbase v logger dbEnv miner reward pd enfCBFail usePrecomp mc
    mapM_ upgradeInitCache upgradedCacheM
    debugResult "runCoinbase" cr
    return $! cr

  where

    upgradeInitCache newCache = do
      logInfo "Updating init cache for upgrade"
      modify' $ over psInitCache (HM.union newCache)


-- | Apply multiple Pact commands, incrementing the transaction Id for each.
-- The output vector is in the same order as the input (i.e. you can zip it
-- with the inputs.)
applyPactCmds
    :: Bool
    -> P.PactDbEnv p
    -> Vector ChainwebTransaction
    -> Miner
    -> ModuleCache
    -> PactServiceM cas (Vector (P.CommandResult [P.TxLog A.Value]))
applyPactCmds isGenesis env cmds miner mc =
    V.fromList . toList . sfst <$> V.foldM f (T2 mempty mc) cmds
  where
    f  (T2 dl mcache) cmd = applyPactCmd isGenesis env cmd miner mcache dl

-- | Apply a single Pact command
applyPactCmd
    :: Bool
    -> P.PactDbEnv p
    -> ChainwebTransaction
    -> Miner
    -> ModuleCache
    -> DList (P.CommandResult [P.TxLog A.Value])
    -> PactServiceM cas (T2 (DList (P.CommandResult [P.TxLog A.Value])) ModuleCache)
applyPactCmd isGenesis dbEnv cmdIn miner mcache dl = do
    logger <- view (psCheckpointEnv . cpeLogger)
    gasModel <- view psGasModel
    v <- view psVersion

    T2 result mcache' <- if isGenesis
      then liftIO $! applyGenesisCmd logger dbEnv P.noSPVSupport (payloadObj <$> cmdIn)
      else do
        pd <- getTxContext (publicMetaOf $ payloadObj <$> cmdIn)
        spv <- use psSpvSupport
        liftIO $! applyCmd v logger dbEnv miner gasModel pd spv cmdIn mcache
        {- the following can be used instead of above to nerf transaction execution
        return $! T2 (P.CommandResult (P.cmdToRequestKey cmdIn) Nothing
                      (P.PactResult (Right (P.PLiteral (P.LInteger 1))))
                      0 Nothing Nothing Nothing)
                      mcache -}

    when isGenesis $
      psInitCache <>= mcache'

    unless isGenesis $ debugResult "applyPactCmd" result

    cp <- getCheckpointer
    -- mark the tx as processed at the checkpointer.
    liftIO $ _cpRegisterProcessedTx cp (P._cmdHash cmdIn)
    pure $! T2 (DL.snoc dl result) mcache'

toHashCommandResult :: P.CommandResult [P.TxLog A.Value] -> P.CommandResult P.Hash
toHashCommandResult = over (P.crLogs . _Just) $ P.pactHash . encodeToByteString

transactionsFromPayload :: PayloadData -> IO (Vector ChainwebTransaction)
transactionsFromPayload plData = do
    vtrans <- fmap V.fromList $
              mapM toCWTransaction $
              toList (_payloadDataTransactions plData)
    let (theLefts, theRights) = partitionEithers $ V.toList vtrans
    unless (null theLefts) $ do
        let ls = map T.pack theLefts
        throwM $ TransactionDecodeFailure $ "Failed to decode pact transactions: "
            <> T.intercalate ". " ls
    return $! V.fromList theRights
  where
    toCWTransaction bs = evaluate (force (codecDecode chainwebPayloadCodec $
                                          _transactionBytes bs))

debugResult :: A.ToJSON a => Text -> a -> PactServiceM cas ()
debugResult msg result =
  logDebug $ T.unpack $ trunc $ msg <> " result: " <> encodeToText result
  where
    trunc t | T.length t < limit = t
            | otherwise = T.take limit t <> " [truncated]"
    limit = 5000


-- | Calculate miner reward. We want this to error hard in the case where
-- block times have finally exceeded the 120-year range. Rewards are calculated
-- at regular blockheight intervals.
--
-- See: 'rewards/miner_rewards.csv'
--
minerReward
    :: ChainwebVersion
    -> MinerRewards
    -> BlockHeight
    -> IO P.ParsedDecimal
minerReward v (MinerRewards rs) bh =
    case Map.lookupGE bh rs of
      Nothing -> err
      Just (_, m) -> pure $! P.ParsedDecimal (roundTo 8 (m / n))
  where
    !n = int . order $ chainGraphAt v bh
    err = internalError "block heights have been exhausted"
{-# INLINE minerReward #-}


data CRLogPair = CRLogPair P.Hash [P.TxLog A.Value]
instance A.ToJSON CRLogPair where
  toJSON (CRLogPair h logs) = A.object
    [ "hash" A..= h
    , "rawLogs" A..= logs ]

validateHashes
    :: BlockHeader
        -- ^ Current Header
    -> PayloadData
    -> Miner
    -> Transactions
    -> Either PactException PayloadWithOutputs
validateHashes bHeader pData miner transactions =
    if newHash == prevHash
    then Right pwo
    else Left $ BlockValidationFailure $ A.object
         [ "mismatch" A..= errorMsg "Payload hash" prevHash newHash
         , "details" A..= details
         ]
    where

      pwo = toPayloadWithOutputs miner transactions

      newHash = _payloadWithOutputsPayloadHash pwo
      prevHash = _blockPayloadHash bHeader

      newTransactions = V.map fst (_payloadWithOutputsTransactions pwo)
      prevTransactions = _payloadDataTransactions pData

      newMiner = _payloadWithOutputsMiner pwo
      prevMiner = _payloadDataMiner pData

      newTransactionsHash = _payloadWithOutputsTransactionsHash pwo
      prevTransactionsHash = _payloadDataTransactionsHash pData

      newOutputsHash = _payloadWithOutputsOutputsHash pwo
      prevOutputsHash = _payloadDataOutputsHash pData

      check desc extra expect actual
        | expect == actual = []
        | otherwise =
          [A.object $ "mismatch" A..= errorMsg desc expect actual :  extra]

      errorMsg desc expect actual = A.object
        [ "type" A..= (desc :: Text)
        , "actual" A..= actual
        , "expected" A..= expect
        ]

      checkTransactions prev new =
        ["txs" A..= concatMap (uncurry (check "Tx" [])) (V.zip prev new)]

      addOutputs (Transactions pairs coinbase) =
        [ "outputs" A..= A.object
         [ "coinbase" A..= toPairCR coinbase
         , "txs" A..= (addTxOuts <$> pairs)
         ]
        ]

      addTxOuts :: (ChainwebTransaction, P.CommandResult [P.TxLog A.Value]) -> A.Value
      addTxOuts (tx,cr) = A.object
        [ "tx" A..= fmap (fmap _pcCode . payloadObj) tx
        , "result" A..= toPairCR cr
        ]

      toPairCR cr = over (P.crLogs . _Just)
        (CRLogPair (fromJuste $ P._crLogs (toHashCommandResult cr))) cr

      details = concat
        [ check "Miner" [] prevMiner newMiner
        , check "TransactionsHash" (checkTransactions prevTransactions newTransactions)
          prevTransactionsHash newTransactionsHash
        , check "OutputsHash" (addOutputs transactions)
          prevOutputsHash newOutputsHash
        ]


toTransactionBytes :: P.Command Text -> Transaction
toTransactionBytes cwTrans =
    let plBytes = encodeToByteString cwTrans
    in Transaction { _transactionBytes = plBytes }


toOutputBytes :: P.CommandResult P.Hash -> TransactionOutput
toOutputBytes cr =
    let outBytes = A.encode cr
    in TransactionOutput { _transactionOutputBytes = toS outBytes }

toPayloadWithOutputs :: Miner -> Transactions -> PayloadWithOutputs
toPayloadWithOutputs mi ts =
    let oldSeq = _transactionPairs ts
        trans = cmdBSToTx . fst <$> oldSeq
        transOuts = toOutputBytes . toHashCommandResult . snd <$> oldSeq

        miner = toMinerData mi
        cb = CoinbaseOutput $ encodeToByteString $ toHashCommandResult $ _transactionCoinbase ts
        blockTrans = snd $ newBlockTransactions miner trans
        cmdBSToTx = toTransactionBytes
          . fmap (T.decodeUtf8 . SB.fromShort . payloadBytes)
        blockOuts = snd $ newBlockOutputs cb transOuts

        blockPL = blockPayload blockTrans blockOuts
        plData = payloadData blockTrans blockPL
     in payloadWithOutputs plData cb transOuts
