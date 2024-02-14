{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    ( execBlock
    , execTransactions
    , execTransactionsOnly
    , toHashCommandResult
    , minerReward
    , toPayloadWithOutputs
    , validateChainwebTxs
    , validateHashes
    , throwOnGasFailure
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
import Data.Either
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.IO
import System.Timeout

import Prelude hiding (lookup)

import Pact.Compile (compileExps)
import qualified Pact.JSON.Encode as J
import qualified Pact.Parse as P
import qualified Pact.Types.Command as P
import Pact.Types.Exp (ParsedCode(..))
import Pact.Types.ExpParser (mkTextInfo, ParseEnv(..))
import qualified Pact.Types.Hash as P
import Pact.Types.RPC
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.SPV as P

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Mempool.Mempool as Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.NoCoinbase
import Chainweb.Pact.Service.Types
import Chainweb.Pact.SPV
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Pact.Validations (assertTxTimeRelativeToParent, assertValidateSigs)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Version.Guards

-- | Execute a block -- only called in validate either for replay or for validating current block.
--
execBlock
    :: (CanReadablePayloadCas tbl, Logger logger)
    => BlockHeader
        -- ^ this is the current header. We may consider changing this to the parent
        -- header to avoid confusion with new block and prevent using data from this
        -- header when we should use the respective values from the parent header
        -- instead.
    -> PayloadData
    -> PactBlockM logger tbl (P.Gas, PayloadWithOutputs)
execBlock currHeader plData = do
    dbEnv <- view psBlockDbEnv
    miner <- decodeStrictOrThrow' (_minerData $ _payloadDataMiner plData)
    trans <- liftIO $ transactionsFromPayload
      (pactParserVersion v (_blockChainId currHeader) (_blockHeight currHeader))
      plData
    logger <- view (psServiceEnv . psLogger)

    -- The reference time for tx timings validation.
    --
    -- The legacy behavior is to use the creation time of the /current/ header.
    -- The new default behavior is to use the creation time of the /parent/ header.
    --
    txValidationTime <- if isGenesisBlockHeader currHeader
      then return (ParentCreationTime $ _blockCreationTime currHeader)
      else ParentCreationTime . _blockCreationTime . _parentHeader <$> view psParentHeader

    -- prop_tx_ttl_validate
    valids <- liftIO $ V.zip trans <$>
        validateChainwebTxs logger v cid dbEnv txValidationTime
            (_blockHeight currHeader) trans skipDebitGas

    case foldr handleValids [] valids of
      [] -> return ()
      errs -> throwM $ TransactionValidationException errs

    logInitCache

    !results <- go miner trans >>= throwOnGasFailure

    let !totalGasUsed = sumOf (folded . to P._crGas) results

    pwo <- either throwM return $
      validateHashes currHeader plData miner results
    return (totalGasUsed, pwo)
  where
    blockGasLimit =
      fromIntegral <$> maxBlockGasLimit v (_blockHeight currHeader)

    logInitCache = liftPactServiceM $ do
      mc <- fmap (fmap instr . _getModuleCache) <$> use psInitCache
      logDebug $ "execBlock: initCache: " <> sshow mc

    instr (md,_) = preview (P._MDModule . P.mHash) $ P._mdModule md

    handleValids (tx,Left e) es = (P._cmdHash tx, sshow e):es
    handleValids _ es = es

    v = _chainwebVersion currHeader
    cid = _chainId currHeader

    isGenesisBlock = isGenesisBlockHeader currHeader

    go m txs = if isGenesisBlock
      then do
        -- GENESIS VALIDATE COINBASE: Reject bad coinbase, use date rule for precompilation
        execTransactions True m txs
          (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled False) blockGasLimit Nothing
      else do
        -- VALIDATE COINBASE: back-compat allow failures, use date rule for precompilation
        execTransactions False m txs
          (EnforceCoinbaseFailure False) (CoinbaseUsePrecompiled False) blockGasLimit Nothing

throwOnGasFailure
    :: Transactions (Either GasPurchaseFailure a)
    -> PactBlockM logger tbl (Transactions a)
throwOnGasFailure = (transactionPairs . traverse . _2) throwGasFailure
  where
    throwGasFailure (Left e) = throwM $! BuyGasFailure e
    throwGasFailure (Right r) = pure r

-- | The principal validation logic for groups of Pact Transactions.
--
-- Skips validation for genesis transactions, since gas accounts, etc. don't
-- exist yet.
--
validateChainwebTxs
    :: (Logger logger)
    => logger
    -> ChainwebVersion
    -> ChainId
    -> CurrentBlockDbEnv logger
    -> ParentCreationTime
        -- ^ reference time for tx validation.
    -> BlockHeight
        -- ^ Current block height
    -> Vector ChainwebTransaction
    -> RunGas
    -> IO ValidateTxs
validateChainwebTxs logger v cid dbEnv txValidationTime bh txs doBuyGas
  | bh == genesisHeight v cid = pure $! V.map Right txs
  | V.null txs = pure V.empty
  | otherwise = go
  where
    go = V.mapM validations initTxList >>= doBuyGas

    validations t =
      runValid checkUnique t
      >>= runValid checkTxHash
      >>= runValid checkTxSigs
      >>= runValid checkTimes
      >>= runValid (return . checkCompile v cid bh)

    checkUnique :: ChainwebTransaction -> IO (Either InsertError ChainwebTransaction)
    checkUnique t = do
      found <- HashMap.lookup (P._cmdHash t) <$> _cpLookupProcessedTx dbEnv (V.singleton $ P._cmdHash t)
      case found of
        Nothing -> pure $ Right t
        Just _ -> pure $ Left InsertErrorDuplicate

    checkTimes :: ChainwebTransaction -> IO (Either InsertError ChainwebTransaction)
    checkTimes t
        | skipTxTimingValidation v cid bh = return $ Right t
        | assertTxTimeRelativeToParent txValidationTime $ fmap payloadObj t = return $ Right t
        | otherwise = return $ Left InsertErrorInvalidTime

    checkTxHash :: ChainwebTransaction -> IO (Either InsertError ChainwebTransaction)
    checkTxHash t =
        case P.verifyHash (P._cmdHash t) (SB.fromShort $ payloadBytes $ P._cmdPayload t) of
            Left _
                | doCheckTxHash v cid bh -> return $ Left InsertErrorInvalidHash
                | otherwise -> do
                    logDebug_ logger "ignored legacy tx-hash failure"
                    return $ Right t
            Right _ -> pure $ Right t


    checkTxSigs :: ChainwebTransaction -> IO (Either InsertError ChainwebTransaction)
    checkTxSigs t
      | assertValidateSigs validSchemes webAuthnPrefixLegal hsh signers sigs = pure $ Right t
      | otherwise = return $ Left InsertErrorInvalidSigs
      where
        hsh = P._cmdHash t
        sigs = P._cmdSigs t
        signers = P._pSigners $ payloadObj $ P._cmdPayload t
        validSchemes = validPPKSchemes v cid bh
        webAuthnPrefixLegal = isWebAuthnPrefixLegal v cid bh

    initTxList :: ValidateTxs
    initTxList = V.map Right txs

    runValid :: Monad m => (a -> m (Either e a)) -> Either e a -> m (Either e a)
    runValid f (Right r) = f r
    runValid _ l@Left{} = pure l


type ValidateTxs = Vector (Either InsertError ChainwebTransaction)
type RunGas = ValidateTxs -> IO ValidateTxs

checkCompile
  :: ChainwebVersion
  -> ChainId
  -> BlockHeight
  -> ChainwebTransaction
  -> Either InsertError ChainwebTransaction
checkCompile v cid bh tx = case payload of
  Exec (ExecMsg parsedCode _) ->
    case compileCode parsedCode of
      Left perr -> Left $ InsertErrorCompilationFailed (sshow perr)
      Right _ -> Right tx
  _ -> Right tx
  where
    payload = P._pPayload $ payloadObj $ P._cmdPayload tx
    compileCode p =
      let e = ParseEnv (chainweb216Pact v cid bh)
      in compileExps e (mkTextInfo (P._pcCode p)) (P._pcExps p)

skipDebitGas :: RunGas
skipDebitGas = return



execTransactions
    :: (Logger logger)
    => Bool
    -> Miner
    -> Vector ChainwebTransaction
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> Maybe P.Gas
    -> Maybe Micros
    -> PactBlockM logger tbl (Transactions (Either GasPurchaseFailure (P.CommandResult [P.TxLogJson])))
execTransactions isGenesis miner ctxs enfCBFail usePrecomp gasLimit timeLimit = do
    mc <- getCache
    coinOut <- runCoinbase isGenesis miner enfCBFail usePrecomp mc
    txOuts <- applyPactCmds isGenesis ctxs miner mc gasLimit timeLimit
    return $! Transactions (V.zip ctxs txOuts) coinOut
  where
    getCache = get >>= \PactServiceState{..} -> do
      pbh <- views psParentHeader (_blockHeight . _parentHeader)
      case Map.lookupLE pbh _psInitCache of
        Nothing -> if isGenesis
          then return mempty
          else do
            l <- view (psServiceEnv . psLogger)
            dbEnv <- view psBlockDbEnv
            txCtx <- getTxContext def
            mc <- liftIO (readInitModules l (_cpPactDbEnv dbEnv) txCtx)
            updateInitCacheM mc
            return mc
        Just (_,mc) -> return mc


execTransactionsOnly
    :: (Logger logger)
    => Miner
    -> Vector ChainwebTransaction
    -> Maybe Micros
    -> PactBlockM logger tbl
       (Vector (ChainwebTransaction, Either GasPurchaseFailure (P.CommandResult [P.TxLogJson])))
execTransactionsOnly miner ctxs txTimeLimit = do
    mc <- getInitCache
    txOuts <- applyPactCmds False ctxs miner mc Nothing txTimeLimit
    return $! V.force (V.zip ctxs txOuts)

runCoinbase
    :: (Logger logger)
    => Bool
    -> Miner
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> ModuleCache
    -> PactBlockM logger tbl (P.CommandResult [P.TxLogJson])
runCoinbase True _ _ _ _ = return noCoinbase
runCoinbase False miner enfCBFail usePrecomp mc = do
    logger <- view (psServiceEnv . psLogger)
    rs <- view (psServiceEnv . psMinerRewards)
    v <- view chainwebVersion
    txCtx <- getTxContext def

    let !bh = ctxCurrentBlockHeight txCtx

    reward <- liftIO $! minerReward v rs bh
    dbEnv <- view psBlockDbEnv

    T2 cr upgradedCacheM <-
        liftIO $ applyCoinbase v logger (_cpPactDbEnv dbEnv) miner reward txCtx enfCBFail usePrecomp mc
    mapM_ upgradeInitCache upgradedCacheM
    liftPactServiceM $ debugResult "runCoinbase" (P.crLogs %~ fmap J.Array $ cr)
    return $! cr

  where

    upgradeInitCache newCache = do
      liftPactServiceM $ logInfo "Updating init cache for upgrade"
      updateInitCacheM newCache


-- | Apply multiple Pact commands, incrementing the transaction Id for each.
-- The output vector is in the same order as the input (i.e. you can zip it
-- with the inputs.)
applyPactCmds
    :: (Logger logger)
    => Bool
    -> Vector ChainwebTransaction
    -> Miner
    -> ModuleCache
    -> Maybe P.Gas
    -> Maybe Micros
    -> PactBlockM logger tbl (Vector (Either GasPurchaseFailure (P.CommandResult [P.TxLogJson])))
applyPactCmds isGenesis cmds miner mc blockGas txTimeLimit = do
    let txsGas txs = fromIntegral $ sumOf (traversed . _Right . to P._crGas) txs
    txs <- tracePactBlockM' "applyPactCmds" () txsGas $
      evalStateT (V.mapM (applyPactCmd isGenesis miner txTimeLimit) cmds) (T2 mc blockGas)
    return txs

applyPactCmd
  :: (Logger logger)
  => Bool
  -> Miner
  -> Maybe Micros
  -> ChainwebTransaction
  -> StateT
      (T2 ModuleCache (Maybe P.Gas))
      (PactBlockM logger tbl)
      (Either GasPurchaseFailure (P.CommandResult [P.TxLogJson]))
applyPactCmd isGenesis miner txTimeLimit cmd = StateT $ \(T2 mcache maybeBlockGasRemaining) -> do
  env <- view psBlockDbEnv
  logger <- view (psServiceEnv . psLogger)
  gasLogger <- view (psServiceEnv . psGasLogger)
  gasModel <- view (psServiceEnv . psGasModel)
  v <- view chainwebVersion
  let
    onBuyGasFailure (BuyGasFailure f) = pure $! (Left f, T2 mcache maybeBlockGasRemaining)
    onBuyGasFailure e = throwM e
    requestedTxGasLimit = view cmdGasLimit (payloadObj <$> cmd)
    -- notice that we add 1 to the remaining block gas here, to distinguish the
    -- cases "tx used exactly as much gas remained in the block" (which is fine)
    -- and "tx attempted to use more gas than remains in the block" (which is
    -- illegal). for example: tx has a tx gas limit of 10000. the block has 5000
    -- remaining gas. therefore the tx is applied with a tx gas limit of 5001.
    -- if it uses 5001, that's illegal; if it uses 5000 or less, that's legal.
    newTxGasLimit = case maybeBlockGasRemaining of
      Nothing -> requestedTxGasLimit
      Just blockGasRemaining -> min (fromIntegral (succ blockGasRemaining)) requestedTxGasLimit
    gasLimitedCmd =
      set cmdGasLimit newTxGasLimit (payloadObj <$> cmd)
    initialGas = initialGasOf (P._cmdPayload cmd)
  let !hsh = P._cmdHash cmd
  handle onBuyGasFailure $ do
    T2 result mcache' <- do
      txCtx <- getTxContext (publicMetaOf gasLimitedCmd)
      if isGenesis
      then liftIO $! applyGenesisCmd logger (_cpPactDbEnv env) P.noSPVSupport txCtx gasLimitedCmd
      else do
        bhdb <- view (psServiceEnv . psBlockHeaderDb)
        parent <- view psParentHeader
        let spv = pactSPV bhdb (_parentHeader parent)
        let
          !timeoutError = TxTimeout (requestKeyToTransactionHash $ P.cmdToRequestKey cmd)
          txTimeout = case txTimeLimit of
            Nothing -> id
            Just limit ->
               maybe (throwM timeoutError) return <=< timeout (fromIntegral limit)
        let txGas (T3 r _ _) = fromIntegral $ P._crGas r
        T3 r c _warns <-
          tracePactBlockM' "applyCmd" (J.toJsonViaEncode hsh) txGas $
            liftIO $ txTimeout $ applyCmd v logger gasLogger (_cpPactDbEnv env) miner (gasModel txCtx) txCtx spv gasLimitedCmd initialGas mcache ApplySend
        pure $ T2 r c

    if isGenesis
    then updateInitCacheM mcache'
    else liftPactServiceM $ debugResult "applyPactCmd" (P.crLogs %~ fmap J.Array $ result)

    -- mark the tx as processed at the checkpointer.
    liftIO $ _cpRegisterProcessedTx env hsh
    case maybeBlockGasRemaining of
      Just blockGasRemaining ->
        when (P._crGas result >= succ blockGasRemaining) $
          -- this tx attempted to consume more gas than remains in the
          -- block, so the block is invalid. we don't know how much gas it
          -- would've consumed, because we stop early, so we guess that it
          -- needed its entire original gas limit.
          throwM $ BlockGasLimitExceeded (blockGasRemaining - fromIntegral requestedTxGasLimit)
      Nothing -> return ()
    let maybeBlockGasRemaining' = (\g -> g - P._crGas result) <$> maybeBlockGasRemaining
    pure (Right result, T2 mcache' maybeBlockGasRemaining')

toHashCommandResult :: P.CommandResult [P.TxLogJson] -> P.CommandResult P.Hash
toHashCommandResult = over (P.crLogs . _Just) $ P.pactHash . P.encodeTxLogJsonArray

transactionsFromPayload
    :: PactParserVersion
    -> PayloadData
    -> IO (Vector ChainwebTransaction)
transactionsFromPayload ppv plData = do
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
    toCWTransaction bs = evaluate (force (codecDecode (chainwebPayloadCodec ppv) $
                                          _transactionBytes bs))

debugResult :: J.Encode a => Logger logger => Text -> a -> PactServiceM logger tbl ()
debugResult msg result =
  logDebug $ trunc $ msg <> " result: " <> J.encodeText result
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


data CRLogPair = CRLogPair P.Hash [P.TxLogJson]



instance J.Encode CRLogPair where
  build (CRLogPair h logs) = J.object
    [ "hash" J..= h
    , "rawLogs" J..= J.Array logs
    ]
  {-# INLINE build #-}

validateHashes
    :: BlockHeader
        -- ^ Current Header
    -> PayloadData
    -> Miner
    -> Transactions (P.CommandResult [P.TxLogJson])
    -> Either PactException PayloadWithOutputs
validateHashes bHeader pData miner transactions =
    if newHash == prevHash
      then Right pwo
      else Left $ BlockValidationFailure $ BlockValidationFailureMsg $
        J.encodeJsonText $ J.object
            [ "header" J..= J.encodeWithAeson (ObjectEncoded bHeader)
            , "mismatch" J..= errorMsg "Payload hash" prevHash newHash
            , "details" J..= details
            ]
  where

    pwo = toPayloadWithOutputs miner transactions

    newHash = _payloadWithOutputsPayloadHash pwo
    prevHash = _blockPayloadHash bHeader

    newTransactions = toList $ fst <$> (_payloadWithOutputsTransactions pwo)
    prevTransactions = toList $ _payloadDataTransactions pData

    newMiner = _payloadWithOutputsMiner pwo
    prevMiner = _payloadDataMiner pData

    newTransactionsHash = _payloadWithOutputsTransactionsHash pwo
    prevTransactionsHash = _payloadDataTransactionsHash pData

    newOutputsHash = _payloadWithOutputsOutputsHash pwo
    prevOutputsHash = _payloadDataOutputsHash pData

    -- The following JSON encodings are used in the BlockValidationFailure message

    check :: Eq a => A.ToJSON a => T.Text -> [Maybe J.KeyValue] -> a -> a -> Maybe J.Builder
    check desc extra expect actual
        | expect == actual = Nothing
        | otherwise = Just $ J.object
            $ "mismatch" J..= errorMsg desc expect actual
            : extra

    errorMsg :: A.ToJSON a => T.Text -> a -> a -> J.Builder
    errorMsg desc expect actual = J.object
        [ "type" J..= J.text desc
        , "actual" J..= J.encodeWithAeson actual
        , "expected" J..= J.encodeWithAeson expect
        ]

    checkTransactions :: [Transaction] -> [Transaction] -> [Maybe J.KeyValue]
    checkTransactions prev new =
        [ "txs" J..?=
            (J.Array <$> traverse (uncurry $ check "Tx" []) (zip prev new))
        ]

    addOutputs (Transactions pairs coinbase) =
        [ "outputs" J..= J.object
            [ "coinbase" J..=  toPairCR coinbase
            , "txs" J..= J.array (addTxOuts <$> pairs)
            ]
        ]

    addTxOuts :: (ChainwebTransaction, P.CommandResult [P.TxLogJson]) -> J.Builder
    addTxOuts (tx,cr) = J.object
        [ "tx" J..= fmap (fmap _pcCode . payloadObj) tx
        , "result" J..= toPairCR cr
        ]

    toPairCR cr = over (P.crLogs . _Just)
        (CRLogPair (fromJuste $ P._crLogs (toHashCommandResult cr))) cr

    details = J.Array $ catMaybes
        [ check "Miner" [] prevMiner newMiner
        , check "TransactionsHash" (checkTransactions prevTransactions newTransactions)
            prevTransactionsHash newTransactionsHash
        , check "OutputsHash" (addOutputs transactions)
            prevOutputsHash newOutputsHash
        ]

toTransactionBytes :: P.Command Text -> Transaction
toTransactionBytes cwTrans =
    let plBytes = J.encodeStrict cwTrans
    in Transaction { _transactionBytes = plBytes }


toOutputBytes :: P.CommandResult P.Hash -> TransactionOutput
toOutputBytes cr =
    let outBytes = J.encodeStrict cr
    in TransactionOutput { _transactionOutputBytes = outBytes }

toPayloadWithOutputs :: Miner -> Transactions (P.CommandResult [P.TxLogJson]) -> PayloadWithOutputs
toPayloadWithOutputs mi ts =
    let oldSeq = _transactionPairs ts
        trans = cmdBSToTx . fst <$> oldSeq
        transOuts = toOutputBytes . toHashCommandResult . snd <$> oldSeq

        miner = toMinerData mi
        cb = CoinbaseOutput $ J.encodeStrict $ toHashCommandResult $ _transactionCoinbase ts
        blockTrans = snd $ newBlockTransactions miner trans
        cmdBSToTx = toTransactionBytes
          . fmap (T.decodeUtf8 . SB.fromShort . payloadBytes)
        blockOuts = snd $ newBlockOutputs cb transOuts

        blockPL = blockPayload blockTrans blockOuts
        plData = payloadData blockTrans blockPL
     in payloadWithOutputs plData cb transOuts
