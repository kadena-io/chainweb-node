{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
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

import "semialign" Data.Zip (align)
import Data.These (These(..))
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SB
import Data.Decimal
import Data.List qualified as List
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
import Pact.Interpreter(PactDbEnv(..))
import qualified Pact.JSON.Encode as J
import qualified Pact.Parse as P
import qualified Pact.Types.Command as P
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
    miner <- decodeStrictOrThrow' (_minerData $ view payloadDataMiner plData)
    trans <- liftIO $ transactionsFromPayload
      (pactParserVersion v (view blockChainId currHeader) (view blockHeight currHeader))
      plData
    logger <- view (psServiceEnv . psLogger)

    -- The reference time for tx timings validation.
    --
    -- The legacy behavior is to use the creation time of the /current/ header.
    -- The new default behavior is to use the creation time of the /parent/ header.
    --
    txValidationTime <- if isGenesisBlockHeader currHeader
      then return (ParentCreationTime $ view blockCreationTime currHeader)
      else ParentCreationTime . view blockCreationTime . _parentHeader <$> view psParentHeader

    -- prop_tx_ttl_validate
    valids <- liftIO $ V.zip trans <$>
        validateChainwebTxs logger v cid dbEnv txValidationTime
            (view blockHeight currHeader) trans skipDebitGas

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
      fromIntegral <$> maxBlockGasLimit v (view blockHeight currHeader)

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
        | skipTxTimingValidation v cid bh =
          return $ Right t
        | not (assertTxNotInFuture txValidationTime (payloadObj <$> t)) =
          return $ Left InsertErrorTimeInFuture
        | not (assertTxTimeRelativeToParent txValidationTime (payloadObj <$> t)) =
          return $ Left InsertErrorTTLExpired
        | otherwise =
          return $ Right t

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
    -> PactBlockM logger tbl (Transactions (Either CommandInvalidError (P.CommandResult [P.TxLogJson])))
execTransactions isGenesis miner ctxs enfCBFail usePrecomp gasLimit timeLimit = do
    mc <- initModuleCacheForBlock isGenesis
    -- for legacy reasons (ask Emily) we don't use the module cache resulting
    -- from coinbase to run the pact cmds
    coinOut <- runCoinbase isGenesis miner enfCBFail usePrecomp mc
    T2 txOuts _mcOut <- applyPactCmds isGenesis ctxs miner mc gasLimit timeLimit
    return $! Transactions (V.zip ctxs txOuts) coinOut

execTransactionsOnly
    :: (Logger logger)
    => Miner
    -> Vector ChainwebTransaction
    -> ModuleCache
    -> Maybe Micros
    -> PactBlockM logger tbl
       (T2 (Vector (ChainwebTransaction, Either CommandInvalidError (P.CommandResult [P.TxLogJson]))) ModuleCache)
execTransactionsOnly miner ctxs mc txTimeLimit = do
    T2 txOuts mcOut <- applyPactCmds False ctxs miner mc Nothing txTimeLimit
    return $! T2 (V.force (V.zip ctxs txOuts)) mcOut

initModuleCacheForBlock :: (Logger logger) => Bool -> PactBlockM logger tbl ModuleCache
initModuleCacheForBlock isGenesis = do
  PactServiceState{..} <- get
  pbh <- views psParentHeader (view blockHeight . _parentHeader)
  case Map.lookupLE pbh _psInitCache of
    Nothing -> if isGenesis
      then return mempty
      else do
        mc <- readInitModules
        updateInitCacheM mc
        return mc
    Just (_,mc) -> return mc

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

data CommandInvalidError
  = CommandInvalidGasPurchaseFailure !GasPurchaseFailure
  | CommandInvalidTxTimeout !TxTimeout

-- | Apply multiple Pact commands, incrementing the transaction Id for each.
-- The output vector is in the same order as the input (i.e. you can zip it
-- with the inputs.)
applyPactCmds
    :: forall logger tbl. (Logger logger)
    => Bool
    -> Vector ChainwebTransaction
    -> Miner
    -> ModuleCache
    -> Maybe P.Gas
    -> Maybe Micros
    -> PactBlockM logger tbl (T2 (Vector (Either CommandInvalidError (P.CommandResult [P.TxLogJson]))) ModuleCache)
applyPactCmds isGenesis cmds miner startModuleCache blockGas txTimeLimit = do
    let txsGas txs = fromIntegral $ sumOf (traversed . _Right . to P._crGas) txs
    (txOuts, T2 mcOut _) <- tracePactBlockM' "applyPactCmds" () (txsGas . fst) $
      flip runStateT (T2 startModuleCache blockGas) $
        go [] (V.toList cmds)
    return $! T2 (V.fromList . List.reverse $ txOuts) mcOut
  where
    go
      :: [Either CommandInvalidError (P.CommandResult [P.TxLogJson])]
      -> [ChainwebTransaction]
      -> StateT
          (T2 ModuleCache (Maybe P.Gas))
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
  -> ChainwebTransaction
  -> StateT
      (T2 ModuleCache (Maybe P.Gas))
      (PactBlockM logger tbl)
      (Either CommandInvalidError (P.CommandResult [P.TxLogJson]))
applyPactCmd isGenesis miner txTimeLimit cmd = StateT $ \(T2 mcache maybeBlockGasRemaining) -> do
  dbEnv <- view psBlockDbEnv
  prevBlockState <- liftIO $ fmap _benvBlockState $
    readMVar $ pdPactDbVar $ _cpPactDbEnv dbEnv
  logger <- view (psServiceEnv . psLogger)
  gasLogger <- view (psServiceEnv . psGasLogger)
  gasModel <- view (psServiceEnv . psGasModel)
  txFailuresCounter <- view (psServiceEnv . psTxFailuresCounter)
  v <- view chainwebVersion
  let
    -- for errors so fatal that the tx doesn't make it in the block
    onFatalError e
      | Just (BuyGasFailure f) <- fromException e = pure (Left (CommandInvalidGasPurchaseFailure f), T2 mcache maybeBlockGasRemaining)
      | Just t@(TxTimeout {}) <- fromException e = do
        -- timeouts can occur at any point during the transaction, even after
        -- gas has been bought (or even while gas is being redeemed, after the
        -- transaction proper is done). therefore we need to revert the block
        -- state ourselves if it happens.
        liftIO $ P.modifyMVar'
          (pdPactDbVar $ _cpPactDbEnv dbEnv)
          (benvBlockState .~ prevBlockState)
        pure (Left (CommandInvalidTxTimeout t), T2 mcache maybeBlockGasRemaining)
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
    T2 result mcache' <- do
      txCtx <- getTxContext (publicMetaOf gasLimitedCmd)
      if isGenesis
      then liftIO $! applyGenesisCmd logger (_cpPactDbEnv dbEnv) P.noSPVSupport txCtx gasLimitedCmd
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
          tracePactBlockM' "applyCmd" (J.toJsonViaEncode hsh) txGas $ do
            liftIO $ txTimeout $
              applyCmd v logger gasLogger txFailuresCounter (_cpPactDbEnv dbEnv) miner (gasModel txCtx) txCtx spv gasLimitedCmd initialGas mcache ApplySend
        pure $ T2 r c

    if isGenesis
    then updateInitCacheM mcache'
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
    pure (Right result, T2 mcache' maybeBlockGasRemaining')

transactionsFromPayload
    :: PactParserVersion
    -> PayloadData
    -> IO (Vector ChainwebTransaction)
transactionsFromPayload ppv plData = do
    vtrans <- fmap V.fromList $
              mapM toCWTransaction $
              toList (view payloadDataTransactions plData)
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
    -> CheckablePayload
    -> Miner
    -> Transactions (P.CommandResult [P.TxLogJson])
    -> Either PactException PayloadWithOutputs
validateHashes bHeader payload miner transactions =
    if newHash == prevHash
      then Right actualPwo
      else Left $ BlockValidationFailure $ BlockValidationFailureMsg $
        prettyJson $ J.encodeText $ J.object
            [ "header" J..= J.encodeWithAeson (ObjectEncoded bHeader)
            , "mismatch" J..= errorMsg "Payload hash" prevHash newHash
            , "details" J..= difference
            ]
  where

    prettyJson txt = case A.eitherDecodeStrict @A.Value (T.encodeUtf8 txt) of
      Right obj -> T.cons '\n' $ T.decodeUtf8 $ BL.toStrict $ A.encodePretty obj
      Left err -> error $ "validateHashes: impossible JSON decode failure: " <> show err

    actualPwo = toPayloadWithOutputs miner transactions

    newHash = _payloadWithOutputsPayloadHash actualPwo
    prevHash = view blockPayloadHash bHeader

    checkWithMsg :: (Eq a, A.ToJSON a) => T.Text -> [Maybe J.KeyValue] -> a -> a -> Maybe J.Builder
    checkWithMsg desc extra expect actual
        | expect == actual = Nothing
        | otherwise = Just $ J.object
            $ "mismatch" J..= errorMsg desc expect actual
            : extra

    errorMsg :: (A.ToJSON a) => T.Text -> a -> a -> J.Builder
    errorMsg desc expect actual = J.object
        [ "type" J..= J.text desc
        , "actual" J..= J.encodeWithAeson actual
        , "expected" J..= J.encodeWithAeson expect
        ]

    checkEncode :: (Eq a, J.Encode a) => T.Text -> Word -> [Maybe J.KeyValue] -> These a a -> Maybe J.Builder
    checkEncode desc idx extra = \case
        This expect -> Just $ J.object
            $ "mismatch" J..= errorMsgEncode desc expect (Nothing @Bool)
            : "index" J..= sshow @_ @T.Text idx
            : extra
        That actual -> Just $ J.object
            $ "mismatch" J..= errorMsgEncode desc (Nothing @Bool) actual
            : "index" J..= sshow @_ @T.Text idx
            : extra
        These expect actual -> do
          guard (expect /= actual)
          Just $ J.object
            $ "mismatch" J..= errorMsgEncode desc expect actual
            : "index" J..= sshow @_ @T.Text idx
            : extra

    errorMsgEncode :: (J.Encode a, J.Encode b) => T.Text -> a -> b -> J.Builder
    errorMsgEncode desc expect actual = J.object
      [ "type" J..= J.text desc
      , "actual" J..= J.build actual
      , "expected" J..= J.build expect
      ]

    encodeTuple :: (J.Encode a, J.Encode b) => (a, b) -> J.Array [T.Text]
    encodeTuple (a, b) = J.Array [J.encodeText a, J.encodeText b]

    transactionBytesToCommand :: Chainweb.Payload.Transaction -> P.Command T.Text
    transactionBytesToCommand txBytes = case A.decodeStrict' (_transactionBytes txBytes) of
      Just cmd -> cmd
      Nothing -> error $ "validateHashes.transactionBytesToCommand: Failed to decode transaction bytes as Command Text"

    transactionOutputsToCommandResult :: Chainweb.Payload.TransactionOutput -> P.CommandResult A.Value
    transactionOutputsToCommandResult txOuts = case A.decodeStrict' (_transactionOutputBytes txOuts) of
      Just cmdRes -> cmdRes
      Nothing -> error $ "validateHashes.transactionOutputsToJson: Failed to decode transaction output bytes as CommandResult Text"

    addTxOuts :: (ChainwebTransaction, P.CommandResult [P.TxLogJson]) -> J.Builder
    addTxOuts (tx,cr) = J.object
        [ "tx" J..= fmap (fmap P._pcCode . payloadObj) tx
        , "result" J..= toPairCR cr
        ]

    toPairCR cr = over (P.crLogs . _Just)
        (CRLogPair (fromJuste $ P._crLogs (toHashCommandResult cr))) cr

    payloadDataToJSON pData = J.Array $ catMaybes
        [ checkWithMsg "Miner"
            []
            (pData ^. payloadDataMiner)
            (_payloadWithOutputsMiner actualPwo)
        , checkWithMsg "TransactionsHash"
            [ "txs" J..=
                (J.array $ catMaybes $ map (\(i, tx) -> checkEncode "Tx" i [] tx) $ zip [0..] (align
                  (toList $ fmap (transactionBytesToCommand . fst) $ _payloadWithOutputsTransactions actualPwo)
                  (toList $ fmap transactionBytesToCommand $ pData ^. payloadDataTransactions)
                ))
            ]
            (pData ^. payloadDataTransactionsHash)
            (_payloadWithOutputsTransactionsHash actualPwo)
        , checkWithMsg "OutputsHash"
            [ "outputs" J..= J.object
                [ "coinbase" J..= toPairCR (_transactionCoinbase transactions)
                , "txs" J..= J.array (addTxOuts <$> _transactionPairs transactions)
                ]
            ]
            (pData ^. payloadDataOutputsHash)
            (_payloadWithOutputsOutputsHash actualPwo)
        ]

    payloadWithOutputsToJSON localPwo = J.Array $ catMaybes
        [ checkWithMsg "Miner"
            []
            (_payloadWithOutputsMiner localPwo)
            (_payloadWithOutputsMiner actualPwo)
        , Just $ J.object
          [ "transactions" J..= J.object
              [ "txs" J..=
                  (J.array $ catMaybes $ map (\(i, tx) -> checkEncode "Tx" i [] tx) $ zip [0..] (align
                    (toList $ fmap (encodeTuple . bimap transactionBytesToCommand transactionOutputsToCommandResult) $ _payloadWithOutputsTransactions actualPwo)
                    (toList $ fmap (encodeTuple . bimap transactionBytesToCommand transactionOutputsToCommandResult) $ _payloadWithOutputsTransactions localPwo)
                  ))
              , "coinbase" J..=
                  checkWithMsg "Coinbase" []
                    (_payloadWithOutputsCoinbase actualPwo)
                    (_payloadWithOutputsCoinbase localPwo)
              ]
          ]
        ]

    expectedJSON = case payload of
      CheckablePayloadWithOutputs expected ->
        payloadWithOutputsToJSON expected
      CheckablePayload expected ->
        payloadDataToJSON expected

    actualJSON = payloadWithOutputsToJSON actualPwo

    difference = J.object
        [ "expected" J..= expectedJSON
        , "actual" J..= actualJSON
        ]