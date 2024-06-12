{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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
    , throwCommandInvalidError
    , initModuleCacheForBlock
    , runCoinbase
    , CommandInvalidError(..)
    ) where

import Control.Concurrent.MVar
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
import Data.List qualified as List
import Data.Default (def)
import Data.Either
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.IO
import System.Timeout

import Prelude hiding (lookup)
import qualified Data.Map.Strict as M

import Pact.Compile (compileExps)
import Pact.Interpreter(PactDbEnv(..))
import qualified Pact.JSON.Encode as J
import qualified Pact.Parse as P
import qualified Pact.Types.Command as P
import Pact.Types.Exp (ParsedCode(..))
import Pact.Types.ExpParser (mkTextInfo, ParseEnv(..))
import qualified Pact.Types.Hash as P
import Pact.Types.RPC
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.SPV as P

import qualified Pact.Core.Names as PCore
import qualified Pact.Core.Command as PCore

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Mempool.Mempool as Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.NoCoinbase
import Chainweb.Pact.Service.Types
import Chainweb.Pact.SPV
import Chainweb.Pact.TransactionExec.Pact4
import qualified Chainweb.Pact.TransactionExec.Pact5 as Pact5
import Chainweb.Pact.Types
import Chainweb.Pact.Validations
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
    -> CheckablePayload
    -> PactBlockM logger tbl (P.Gas, PayloadWithOutputs)
execBlock currHeader payload = do
    let plData = checkablePayloadToPayloadData payload
    dbEnv <- view psBlockDbEnv
    miner <- decodeStrictOrThrow' (_minerData $ _payloadDataMiner plData)

    -- if

    trans <- liftIO $ pact4TransactionsFromPayload
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

    !results <- go miner trans >>= throwCommandInvalidError

    let !totalGasUsed = sumOf (folded . to P._crGas) results

    pwo <- either throwM return $
      validateHashes currHeader payload miner results
    return (totalGasUsed, pwo)
  where
    blockGasLimit =
      fromIntegral <$> maxBlockGasLimit v (_blockHeight currHeader)

    logInitCache = liftPactServiceM $ do
      mc <- fmap (fmap instr . _getModuleCache . fst) <$> use psInitCache
      logDebug $ "execBlock: initCache: " <> sshow mc

    instr (md,_) = preview (P._MDModule . P.mHash) $ P._mdModule md

    handleValids (tx,Left e) es = (P._cmdHash tx, sshow e):es
    handleValids _ es = es

    v = _chainwebVersion currHeader
    cid = _chainId currHeader

    isGenesisBlock = isGenesisBlockHeader currHeader

    go m txs = if isGenesisBlock
      then
        -- GENESIS VALIDATE COINBASE: Reject bad coinbase, use date rule for precompilation
        execTransactions True m txs
          (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled False) blockGasLimit Nothing
      else
        -- VALIDATE COINBASE: back-compat allow failures, use date rule for precompilation
        execTransactions False m txs
          (EnforceCoinbaseFailure False) (CoinbaseUsePrecompiled False) blockGasLimit Nothing

throwCommandInvalidError
    :: Transactions (Either CommandInvalidError a)
    -> PactBlockM logger tbl (Transactions a)
throwCommandInvalidError = (transactionPairs . traverse . _2) throwGasFailure
  where
    throwGasFailure = \case
      Left (CommandInvalidGasPurchaseFailure e) -> throwM (BuyGasFailure e)

      -- this should be impossible because we don't
      -- set tx time limits in validateBlock
      Left (CommandInvalidTxTimeout t) -> throwM t

      Right r -> pure r

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
    -> Vector Pact4Transaction
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

    checkUnique :: Pact4Transaction -> IO (Either InsertError Pact4Transaction)
    checkUnique t = do
      found <- HashMap.lookup (P._cmdHash t) <$> _cpLookupProcessedTx dbEnv (V.singleton $ P._cmdHash t)
      case found of
        Nothing -> pure $ Right t
        Just _ -> pure $ Left InsertErrorDuplicate

    checkTimes :: Pact4Transaction -> IO (Either InsertError Pact4Transaction)
    checkTimes t
        | skipTxTimingValidation v cid bh =
          return $ Right t
        | not (assertTxNotInFuture txValidationTime (payloadObj <$> t)) =
          return $ Left InsertErrorTimeInFuture
        | not (assertTxTimeRelativeToParent txValidationTime (payloadObj <$> t)) =
          return $ Left InsertErrorTTLExpired
        | otherwise =
          return $ Right t

    checkTxHash :: Pact4Transaction -> IO (Either InsertError Pact4Transaction)
    checkTxHash t =
        case P.verifyHash (P._cmdHash t) (SB.fromShort $ payloadBytes $ P._cmdPayload t) of
            Left _
                | doCheckTxHash v cid bh -> return $ Left InsertErrorInvalidHash
                | otherwise -> do
                    logDebug_ logger "ignored legacy tx-hash failure"
                    return $ Right t
            Right _ -> pure $ Right t


    checkTxSigs :: Pact4Transaction -> IO (Either InsertError Pact4Transaction)
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


type ValidateTxs = Vector (Either InsertError Pact4Transaction)
type RunGas = ValidateTxs -> IO ValidateTxs

checkCompile
  :: ChainwebVersion
  -> ChainId
  -> BlockHeight
  -> Pact4Transaction
  -> Either InsertError Pact4Transaction
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
    -> Vector Pact4Transaction
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> Maybe P.Gas
    -> Maybe Micros
    -> PactBlockM logger tbl (Transactions (Either CommandInvalidError (P.CommandResult [P.TxLogJson])))
execTransactions isGenesis miner ctxs enfCBFail usePrecomp gasLimit timeLimit = do
    mc <- initModuleCacheForBlock isGenesis
    -- for legacy reasons (ask Emily) we don't use the module cache resulting
    -- from coinbase to run the pact cmds
    coinOut <- runCoinbase isGenesis miner enfCBFail usePrecomp mc
    T3 txOuts _mcOut _cmcOut <- applyPactCmds isGenesis ctxs miner mc gasLimit timeLimit
    return $! Transactions (V.zip ctxs txOuts) coinOut

execTransactionsOnly
    :: (Logger logger)
    => Miner
    -> Vector Pact4Transaction
    -> (ModuleCache, CoreModuleCache)
    -> Maybe Micros
    -> PactBlockM logger tbl
       (T3 (Vector (Pact4Transaction, Either CommandInvalidError (P.CommandResult [P.TxLogJson]))) ModuleCache CoreModuleCache)
execTransactionsOnly miner ctxs (mc, cmc) txTimeLimit = do
    T3 txOuts mcOut cmcOut <- applyPactCmds False ctxs miner (mc, cmc) Nothing txTimeLimit
    return $! T3 (V.force (V.zip ctxs txOuts)) mcOut cmcOut

initModuleCacheForBlock :: (Logger logger) => Bool -> PactBlockM logger tbl (ModuleCache, CoreModuleCache)
initModuleCacheForBlock isGenesis = do
  PactServiceState{..} <- get
  pbh <- views psParentHeader (_blockHeight . _parentHeader)
  l <- view (psServiceEnv . psLogger)
  dbEnv <- view psBlockDbEnv
  txCtx <- getTxContext def
  case Map.lookupLE pbh _psInitCache of
    Nothing -> if isGenesis
      then return (mempty, mempty)
      else do
        mc <- readInitModules
        updateInitCacheM mc
        return mc
    Just (_,(mc, cmc)) -> pure (mc, cmc)

runCoinbase
    :: (Logger logger)
    => Bool
    -> Miner
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> (ModuleCache, CoreModuleCache)
    -> PactBlockM logger tbl (Either (P.CommandResult [P.TxLogJson]) PCore.CommandResult)
runCoinbase True _ _ _ _ = return noCoinbase
runCoinbase False miner enfCBFail usePrecomp (mc, cmc) = do
    logger <- view (psServiceEnv . psLogger)
    rs <- view (psServiceEnv . psMinerRewards)
    v <- view chainwebVersion
    txCtx <- getTxContext def

    let !bh = ctxCurrentBlockHeight txCtx

    reward <- liftIO $! minerReward v rs bh
    dbEnv <- view psBlockDbEnv

    T2 cr upgradedCacheM <-
        liftIO $ applyCoinbase v logger (_cpPactDbEnv dbEnv, _cpPactCoreDbEnv dbEnv) miner reward txCtx enfCBFail usePrecomp (mc, cmc)
    mapM_ upgradeInitCache upgradedCacheM
    liftPactServiceM $ debugResult "runCoinbase" (P.crLogs %~ fmap J.Array $ cr)
    return $! cr

  where

    upgradeInitCache newCache = do
      liftPactServiceM $ logInfo "Updating init cache for upgrade"
      updateInitCacheM newCache

data CommandInvalidError
  = CommandInvalidGasPurchaseFailure !GasPurchaseFailure
  | CommandInvalidTxTimeout !TxTimeout

-- | Apply multiple Pact commands, incrementing the transaction Id for each.
-- The output vector is in the same order as the input (i.e. you can zip it
-- with the inputs.)
applyPactCmds
    :: forall logger tbl. (Logger logger)
    => Bool
    -> Vector Pact4Transaction
    -> Miner
    -> (ModuleCache, CoreModuleCache)
    -> Maybe P.Gas
    -> Maybe Micros
    -> PactBlockM logger tbl (T3 (Vector (Either CommandInvalidError (P.CommandResult [P.TxLogJson]))) ModuleCache CoreModuleCache)
applyPactCmds isGenesis cmds miner (mc, cmc) blockGas txTimeLimit = do
    let txsGas txs = fromIntegral $ sumOf (traversed . _Right . to P._crGas) txs
    (txOuts, T3 mcOut cmcOut _) <- tracePactBlockM' "applyPactCmds" () (txsGas . fst) $
      flip runStateT (T3 mc cmc blockGas) $
        go [] (V.toList cmds)
    return $! T3 (V.fromList . List.reverse $ txOuts) mcOut cmcOut
  where
    go
      :: [Either CommandInvalidError (P.CommandResult [P.TxLogJson])]
      -> [Pact4Transaction]
      -> StateT
          (T3 ModuleCache CoreModuleCache (Maybe P.Gas))
          (PactBlockM logger tbl)
          [Either CommandInvalidError (P.CommandResult [P.TxLogJson])]
    go !acc = \case
        [] -> do
            pure acc
        tx : rest -> do
            r <- applyPactCmd isGenesis miner txTimeLimit tx
            case r of
                Left e@(CommandInvalidTxTimeout _) -> do
                    pure (Left e : acc)
                Left e@(CommandInvalidGasPurchaseFailure _) -> do
                    go (Left e : acc) rest
                Right a -> do
                    go (Right a : acc) rest

applyPactCmd
  :: (Logger logger)
  => Bool
  -> Miner
  -> Maybe Micros
  -> Pact4Transaction
  -> StateT
      (T3 ModuleCache CoreModuleCache (Maybe P.Gas))
      (PactBlockM logger tbl)
      (Either CommandInvalidError (P.CommandResult [P.TxLogJson]))
applyPactCmd isGenesis miner txTimeLimit cmd = StateT $ \(T3 mcache cmcache maybeBlockGasRemaining) -> do
  dbEnv <- view psBlockDbEnv
  prevBlockState <- liftIO $ fmap _benvBlockState $
    readMVar $ pdPactDbVar $ _cpPactDbEnv dbEnv
  logger <- view (psServiceEnv . psLogger)
  gasLogger <- view (psServiceEnv . psGasLogger)
  gasModel <- view (psServiceEnv . psGasModel)
  gasModelCore <- view (psServiceEnv . psGasModelCore)
  v <- view chainwebVersion
  let
    -- for errors so fatal that the tx doesn't make it in the block
    onFatalError e
      | Just (BuyGasFailure f) <- fromException e = pure (Left (CommandInvalidGasPurchaseFailure f), T3 mcache cmcache maybeBlockGasRemaining)
      | Just t@(TxTimeout {}) <- fromException e = do
        -- timeouts can occur at any point during the transaction, even after
        -- gas has been bought (or even while gas is being redeemed, after the
        -- transaction proper is done). therefore we need to revert the block
        -- state ourselves if it happens.
        liftIO $ P.modifyMVar'
          (pdPactDbVar $ _cpPactDbEnv dbEnv)
          (benvBlockState .~ prevBlockState)
        pure (Left (CommandInvalidTxTimeout t), T3 mcache cmcache maybeBlockGasRemaining)
      | otherwise = throwM e
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

  handle onFatalError $ do
    T2 result (mcache', cmcache') <- do
      txCtx <- getTxContext (publicMetaOf gasLimitedCmd)
      if isGenesis
      then liftIO $! applyGenesisCmd logger (_cpPactDbEnv dbEnv, _cpPactCoreDbEnv dbEnv) P.noSPVSupport txCtx gasLimitedCmd
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
          txGas (T4 r _ _ _) = fromIntegral $ P._crGas r
        T4 r c cc _warns <- do
          -- TRACE.traceShowM ("applyPactCmd.CACHE: ", LHM.keys $ _getModuleCache mcache, M.keys $ _getCoreModuleCache cmcache)
          tracePactBlockM' "applyCmd" (J.toJsonViaEncode hsh) txGas $ do
            liftIO $ txTimeout $ applyCmd v logger gasLogger (_cpPactDbEnv dbEnv, _cpPactCoreDbEnv dbEnv) miner (gasModel txCtx, gasModelCore txCtx) txCtx spv gasLimitedCmd initialGas (mcache, cmcache) ApplySend
        pure $ T2 r (c, cc)

    if isGenesis
    then updateInitCacheM (mcache', cmcache')
    else liftPactServiceM $ debugResult "applyPactCmd" (P.crLogs %~ fmap J.Array $ result)

    -- mark the tx as processed at the checkpointer.
    liftIO $ _cpRegisterProcessedTx dbEnv hsh
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
    pure (Right result, T3 mcache' cmcache' maybeBlockGasRemaining')

toHashCommandResult :: P.CommandResult [P.TxLogJson] -> P.CommandResult P.Hash
toHashCommandResult = over (P.crLogs . _Just) $ P.pactHash . P.encodeTxLogJsonArray

pact4TransactionsFromPayload
    :: PactParserVersion
    -> PayloadData
    -> IO (Vector Pact4Transaction)
pact4TransactionsFromPayload ppv plData = do
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
    toCWTransaction bs = evaluate (force (codecDecode (pact4PayloadCodec ppv) $
                                          _transactionBytes bs))

pact5TransactionsFromPayload
    :: PactParserVersion
    -> PayloadData
    -> IO (Vector Pact5Transaction)
pact5TransactionsFromPayload ppv plData = do
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
    toCWTransaction bs = evaluate (force (codecDecode (pact5PayloadCodec ppv) $
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
    -> CheckablePayload
    -> Miner
    -> Transactions (P.CommandResult [P.TxLogJson])
    -> Either PactException PayloadWithOutputs
validateHashes bHeader payload miner transactions =
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

    details = case payload of
        CheckablePayload pData -> J.Array $ catMaybes
            [ check "Miner"
                []
                (_payloadDataMiner pData)
                (_payloadWithOutputsMiner pwo)
            , check "TransactionsHash"
                [ "txs" J..?=
                    (J.Array <$> traverse (uncurry $ check "Tx" []) (zip
                      (toList $ fst <$> _payloadWithOutputsTransactions pwo)
                      (toList $ _payloadDataTransactions pData)
                    ))
                ]
                (_payloadDataTransactionsHash pData)
                (_payloadWithOutputsTransactionsHash pwo)
            , check "OutputsHash"
                [ "outputs" J..= J.object
                    [ "coinbase" J..= toPairCR (_transactionCoinbase transactions)
                    , "txs" J..= J.array (addTxOuts <$> _transactionPairs transactions)
                    ]
                ]
                (_payloadDataOutputsHash pData)
                (_payloadWithOutputsOutputsHash pwo)
            ]

        CheckablePayloadWithOutputs localPwo -> J.Array $ catMaybes
            [ check "Miner"
                []
                (_payloadWithOutputsMiner localPwo)
                (_payloadWithOutputsMiner pwo)
            , Just $ J.object
              [ "transactions" J..= J.object
                  [ "txs" J..?=
                      (J.Array <$> traverse (uncurry $ check "Tx" []) (zip
                        (toList $ _payloadWithOutputsTransactions pwo)
                        (toList $ _payloadWithOutputsTransactions localPwo)
                      ))
                  , "coinbase" J..=
                      check "Coinbase" []
                        (_payloadWithOutputsCoinbase pwo)
                        (_payloadWithOutputsCoinbase localPwo)
                  ]
              ]
            ]

    addTxOuts :: (Pact4Transaction, P.CommandResult [P.TxLogJson]) -> J.Builder
    addTxOuts (tx,cr) = J.object
        [ "tx" J..= fmap (fmap _pcCode . payloadObj) tx
        , "result" J..= toPairCR cr
        ]

    toPairCR cr = over (P.crLogs . _Just)
        (CRLogPair (fromJuste $ P._crLogs (toHashCommandResult cr))) cr

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
