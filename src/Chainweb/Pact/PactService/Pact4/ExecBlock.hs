{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module: Chainweb.Pact.PactService.Pact4.ExecBlock
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Lars Kuhtz, Emily Pillmore, Stuart Popejoy
-- Stability: experimental
--
-- Functionality for playing block transactions.
--
module Chainweb.Pact.PactService.Pact4.ExecBlock
    ( execBlock
    , execTransactions
    , toPayloadWithOutputs
    , validateParsedChainwebTx
    , validateRawChainwebTx
    , validateHashes
    , initModuleCacheForBlock
    , runCoinbase
    , checkParse
    ) where

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.MinerReward
import Chainweb.Pact.Mempool.Mempool as Mempool
import Chainweb.Pact.Payload
import Chainweb.Pact.Payload.PayloadStore
import Chainweb.Pact.Types (ServiceEnv(..), Transactions (..), bctxParentCreationTime, bctxParentHeight, _bctxIsGenesis, _bctxCurrentBlockHeight, BlockCtx, BlockInvalidError (..), TxInvalidError (..))
import Chainweb.Pact4.Backend.ChainwebPactDb
import Chainweb.Pact4.ModuleCache
import Chainweb.Pact4.NoCoinbase
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Pact4.TransactionExec qualified as Pact4
import Chainweb.Pact4.Types
import Chainweb.Pact4.Validations qualified as Pact4
import Chainweb.Parent
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Version.Guards
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.ByteString.Short qualified as SB
import Data.Coerce
import Data.Either
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Pact.Compile (compileExps)
import Pact.Core.Command.Types qualified as Pact5
import Pact.Core.Hash qualified as Pact5
import Pact.JSON.Encode qualified as J
import Pact.Parse qualified as Pact4 hiding (parsePact)
import Pact.Types.Command qualified as Pact4
import Pact.Types.ExpParser (mkTextInfo, ParseEnv(..))
import Pact.Types.Hash qualified as Pact4
import Pact.Types.RPC
import Pact.Types.Runtime qualified as Pact4
import Pact.Types.SPV qualified as Pact4
import Prelude hiding (lookup)
import System.LogLevel (LogLevel(..))

-- | Update init cache at adjusted parent block height (APBH).
-- Contents are merged with cache found at or before APBH.
-- APBH is 0 for genesis and (parent block height + 1) thereafter.
updateInitCache :: HasVersion => BlockCtx -> ModuleCache -> StateT ModuleInitCache IO ()
updateInitCache bctx mc = get >>= \initCache -> do
    let bf 0 = 0
        bf h = succ h
    let pbh = bf (view (bctxParentHeight . _Parent) bctx)

    let cid = view chainId bctx

    put $ case M.lookupLE pbh initCache of
        Nothing -> M.singleton pbh mc
        Just (_,before)
          | cleanModuleCache cid pbh ->
            M.insert pbh mc initCache
          | otherwise -> M.insert pbh (before <> mc) initCache

-- | Execute a block -- only called in validate either for replay or for validating current block.
--
execBlock
    :: (CanReadablePayloadCas tbl, Logger logger1, Logger logger2)
    => HasVersion
    => logger1
    -> ServiceEnv tbl
    -> BlockEnv logger2
    -> CheckablePayload
    -> ExceptT BlockInvalidError IO (Pact4.Gas, PayloadWithOutputs)
execBlock logger serviceEnv blockEnv payload = do
    let plData = checkablePayloadToPayloadData payload
    miner <- decodeStrictOrThrow' (_minerData $ view payloadDataMiner plData)
    let currentHeight = blockEnv ^. benvBlockCtx . to _bctxCurrentBlockHeight

    trans <- pact4TransactionsFromPayload
      (pact4ParserVersion (blockEnv ^. benvBlockCtx . chainId) currentHeight)
      plData

    -- The reference time for tx timings validation.
    --
    -- The legacy behavior is to use the creation time of the /current/ header.
    -- The new default behavior is to use the creation time of the /parent/ header.
    --
    let txValidationTime =
          if blockEnv ^. benvBlockCtx . to _bctxIsGenesis
          then Parent $ implicitVersion ^?! versionGenesis . genesisTime . atChain (view chainId serviceEnv)
          else blockEnv ^. benvBlockCtx . bctxParentCreationTime

    -- prop_tx_ttl_validate
    errorsIfPresent <- liftIO $
        forM (V.toList trans) $ \tx ->
          fmap (Pact4._cmdHash tx,) $
            runExceptT $
              validateParsedChainwebTx logger cid (blockEnv ^. benvDbEnv) txValidationTime
                currentHeight tx

    case NE.nonEmpty
        [ (Pact5.RequestKey $ Pact5.Hash hsh, err)
        | (Pact4.toUntypedHash -> Pact4.Hash hsh, Left err) <- errorsIfPresent ]
      of
      Nothing -> return ()
      Just errs -> throwError $ BlockInvalidDueToInvalidTxs (coerce errs)

    liftIO logInitCache

    startModuleInitCache <- liftIO $ readMVar (_psModuleInitCacheVar serviceEnv)

    !(results, finalModuleInitCache) <- runStateT (go miner trans) startModuleInitCache

    _ <- liftIO $ swapMVar (_psModuleInitCacheVar serviceEnv) finalModuleInitCache

    let !totalGasUsed = sumOf (folded . to Pact4._crGas) results

    pwo <- either throwM return $
      validateHashes (blockEnv ^. benvBlockCtx) payload miner results
    return (totalGasUsed, pwo)
  where
    blockGasLimit =
      fromIntegral <$> maxBlockGasLimit (blockEnv ^. benvBlockCtx . to _bctxCurrentBlockHeight)

    logInitCache = do
      mc <- fmap (fmap instr . _getModuleCache) <$> readMVar (_psModuleInitCacheVar serviceEnv)
      liftIO $ logFunctionText logger Debug $ "execBlock: initCache: " <> sshow mc

    instr (md,_) = preview (Pact4._MDModule . Pact4.mHash) $ Pact4._mdModule md

    cid = blockEnv ^. benvBlockCtx . chainId

    isGenesisBlock = blockEnv ^. benvBlockCtx . to _bctxIsGenesis

    go m txs = if isGenesisBlock
      then
        -- GENESIS VALIDATE COINBASE: Reject bad coinbase, use date rule for precompilation
        execTransactions logger serviceEnv blockEnv m txs
          (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled False) blockGasLimit
      else
        -- VALIDATE COINBASE: back-compat allow failures, use date rule for precompilation
        execTransactions logger serviceEnv blockEnv m txs
          (EnforceCoinbaseFailure False) (CoinbaseUsePrecompiled False) blockGasLimit

-- | The validation logic for Pact Transactions that have not had their
-- code parsed yet. This is used by the mempool to estimate tx validity
-- before inclusion into blocks, but it's also used by ExecBlock to check
-- if all of the txs in a block are valid.
--
-- Skips validation for genesis transactions, since gas accounts, etc. don't
-- exist yet.
--
validateRawChainwebTx
    :: forall logger
    . (Logger logger)
    => HasVersion
    => logger
    -> ChainId
    -> CurrentBlockDbEnv logger
    -> Parent BlockCreationTime
        -- ^ reference time for tx validation.
    -> BlockHeight
        -- ^ Current block height
    -> Pact4.UnparsedTransaction
    -> ExceptT InsertError IO Pact4.Transaction
validateRawChainwebTx logger cid dbEnv txValidationTime bh tx = do
  parsed <- checkParse logger cid bh tx
  validateParsedChainwebTx logger cid dbEnv txValidationTime bh parsed
  return parsed

-- | The principal validation logic for groups of Pact Transactions.
-- This is used by the mempool to estimate tx validity
-- before inclusion into blocks, but it's also used by ExecBlock to check
-- if all of the txs in a block are valid.
--
-- Skips validation for genesis transactions, since gas accounts, etc. don't
-- exist yet.
--
validateParsedChainwebTx
    :: (Logger logger1, Logger logger2)
    => HasVersion
    => logger1
    -> ChainId
    -> CurrentBlockDbEnv logger2
    -> Parent BlockCreationTime
        -- ^ reference time for tx validation.
    -> BlockHeight
        -- ^ Current block height
    -> Pact4.Transaction
    -> ExceptT InsertError IO ()
validateParsedChainwebTx logger cid dbEnv txValidationTime bh tx
  | bh == genesisHeight cid = pure ()
  | otherwise = do
      checkUnique logger dbEnv tx
      checkTxHash logger cid bh tx
      checkChain cid tx
      checkTxSigs logger cid bh tx
      checkTimes logger cid bh txValidationTime tx
      _ <- checkCompile logger cid bh tx
      return ()

checkChain
  :: ChainId -> Pact4.Transaction -> ExceptT InsertError IO ()
checkChain cid tx =
  unless (Pact4.assertChainId cid txCid) $
    throwError $ InsertErrorWrongChain (chainIdToText cid) (Pact4._chainId txCid)
  where
  txCid = view (Pact4.cmdPayload . to Pact4.payloadObj . Pact4.pMeta . Pact4.pmChainId) tx

checkUnique
  :: (Logger logger1, Logger logger2)
  => logger1
  -> CurrentBlockDbEnv logger2
  -> Pact4.Command (Pact4.PayloadWithText meta code)
  -> ExceptT InsertError IO ()
checkUnique logger dbEnv t = do
  liftIO $ logFunctionText logger Debug $ "Pact4.checkUnique: " <> sshow (Pact4._cmdHash t)
  found <- liftIO $
    HashMap.lookup (coerce $ Pact4.toUntypedHash $ Pact4._cmdHash t) <$>
      _cpLookupProcessedTx dbEnv
        (V.singleton $ coerce $ Pact4.toUntypedHash $ Pact4._cmdHash t)
  case found of
    Nothing -> pure ()
    Just _ -> throwError InsertErrorDuplicate

checkTimes
  :: (Logger logger)
  => HasVersion
  => logger
  -> ChainId
  -> BlockHeight
  -> Parent BlockCreationTime
  -> Pact4.Command (Pact4.PayloadWithText Pact4.PublicMeta code)
  -> ExceptT InsertError IO ()
checkTimes logger cid bh txValidationTime t = do
    liftIO $ logFunctionText logger Debug $ "Pact4.checkTimes: " <> sshow (Pact4._cmdHash t)
    if | skipTxTimingValidation cid bh ->
           return ()
       | not (Pact4.assertTxNotInFuture txValidationTime (Pact4.payloadObj <$> t)) ->
           throwError InsertErrorTimeInFuture
       | not (Pact4.assertTxTimeRelativeToParent txValidationTime (Pact4.payloadObj <$> t)) ->
           throwError InsertErrorTTLExpired
       | otherwise ->
           return ()

checkTxHash
  :: (Logger logger)
  => HasVersion
  => logger
  -> ChainId
  -> BlockHeight
  -> Pact4.Command (Pact4.PayloadWithText Pact4.PublicMeta code)
  -> ExceptT InsertError IO ()
checkTxHash logger cid bh t = do
    liftIO $ logFunctionText logger Debug $ "Pact4.checkTxHash: " <> sshow (Pact4._cmdHash t)
    case Pact4.verifyHash (Pact4._cmdHash t) (SB.fromShort $ Pact4.payloadBytes $ Pact4._cmdPayload t) of
        Left _
            | doCheckTxHash cid bh -> throwError InsertErrorInvalidHash
            | otherwise -> pure ()
        Right _ -> pure ()

checkTxSigs
  :: (MonadIO f, MonadError InsertError f, Logger logger)
  => HasVersion
  => logger
  -> ChainId
  -> BlockHeight
  -> Pact4.Command (Pact4.PayloadWithText m c)
  -> f ()
checkTxSigs logger cid bh t = do
  liftIO $ logFunctionText logger Debug $ "Pact4.checkTxSigs: " <> sshow (Pact4._cmdHash t)
  case Pact4.assertValidateSigs validSchemes webAuthnPrefixLegal hsh signers sigs of
      Right _ -> do
          pure ()
      Left err -> do
        throwError $ InsertErrorInvalidSigs (Pact4.displayAssertValidateSigsError err)
  where
    hsh = Pact4._cmdHash t
    sigs = Pact4._cmdSigs t
    signers = Pact4._pSigners $ Pact4.payloadObj $ Pact4._cmdPayload t
    validSchemes = validPPKSchemes cid bh
    webAuthnPrefixLegal = isWebAuthnPrefixLegal cid bh

checkCompile
  :: (Logger logger)
  => HasVersion
  => logger
  -> ChainId
  -> BlockHeight
  -> Pact4.Command (Pact4.PayloadWithText Pact4.PublicMeta Pact4.ParsedCode)
  -> ExceptT InsertError IO Pact4.Transaction
checkCompile logger cid bh tx = do
  liftIO $ logFunctionText logger Debug $ "Pact4.checkCompile: " <> sshow (Pact4._cmdHash tx)
  case payload of
    Exec (ExecMsg parsedCode _) ->
      case compileCode parsedCode of
        Left perr -> throwError $ InsertErrorCompilationFailed (sshow perr)
        Right _ -> return tx
    _ -> return tx
  where
    payload = Pact4._pPayload $ Pact4.payloadObj $ Pact4._cmdPayload tx
    compileCode p =
      let e = ParseEnv (chainweb216Pact cid bh)
      in compileExps e (mkTextInfo (Pact4._pcCode p)) (Pact4._pcExps p)

checkParse
  :: (Logger logger)
  => HasVersion
  => logger
  -> ChainId
  -> BlockHeight
  -> Pact4.Command (Pact4.PayloadWithText Pact4.PublicMeta Text)
  -> ExceptT InsertError IO Pact4.Transaction
checkParse logger cid bh tx = do
  liftIO $ logFunctionText logger Debug $ "Pact4.checkParse: " <> sshow (Pact4._cmdHash tx)
  forMOf (traversed . traversed) tx
    (either (throwError . InsertErrorPactParseError . T.pack) return . Pact4.parsePact (pact4ParserVersion cid bh))

execTransactions
    :: (Logger logger1, Logger logger2)
    => HasVersion
    => logger1
    -> ServiceEnv tbl
    -> BlockEnv logger2
    -> Miner
    -> Vector Pact4.Transaction
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> Maybe Pact4.Gas
    -> StateT ModuleInitCache (ExceptT BlockInvalidError IO) (Transactions Pact4.Transaction ((Pact4.CommandResult [Pact4.TxLogJson])))
execTransactions logger serviceEnv blockEnv miner ctxs enfCBFail usePrecomp gasLimit = do
    mc <- mapStateT lift $ initModuleCacheForBlock logger blockEnv
    -- for legacy reasons (ask Emily) we don't use the module cache resulting
    -- from coinbase to run the pact cmds
    coinOut <- mapStateT lift $ runCoinbase logger enfCBFail usePrecomp blockEnv miner mc
    T2 txOuts _mcOut <- lift $ applyPactCmds logger serviceEnv blockEnv ctxs miner mc gasLimit
    return $! Transactions (V.zipWith T2 ctxs txOuts) coinOut

initModuleCacheForBlock
  :: (Logger logger1, Logger logger2)
  => HasVersion
  => logger1
  -> BlockEnv logger2
  -> StateT ModuleInitCache IO ModuleCache
initModuleCacheForBlock logger blockEnv = do
  moduleInitCache <- get
  let pbh = blockEnv ^. benvBlockCtx . bctxParentHeight . _Parent
  let isGenesis = blockEnv ^. benvBlockCtx . to _bctxIsGenesis
  case Map.lookupLE pbh moduleInitCache of
    Nothing -> if isGenesis
      then return mempty
      else do
        mc <- liftIO $ Pact4.readInitModules logger blockEnv
        updateInitCache (blockEnv ^. benvBlockCtx) mc
        return mc
    Just (_,mc) -> pure mc

runCoinbase
    :: (Logger logger1, Logger logger2)
    => HasVersion
    => logger1
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> BlockEnv logger2
    -> Miner
    -> ModuleCache
    -> StateT ModuleInitCache IO (Pact4.CommandResult [Pact4.TxLogJson])
runCoinbase logger enfCBFail usePrecomp blockEnv miner mc = do
    if blockEnv ^. benvBlockCtx . to _bctxIsGenesis
    then return noCoinbase
    else do
      let !bh = _bctxCurrentBlockHeight (blockEnv ^. benvBlockCtx)

      let reward = minerReward bh

      T2 cr upgradedCacheM <-
          liftIO $ Pact4.applyCoinbase logger blockEnv miner reward enfCBFail usePrecomp mc
      mapM_ upgradeInitCache upgradedCacheM
      return $! cr

  where
    upgradeInitCache newCache = do
      liftIO $ logFunctionText logger Info "Updating init cache for upgrade"
      updateInitCache (blockEnv ^. benvBlockCtx) newCache


-- | Apply multiple Pact commands, incrementing the transaction Id for each.
-- The output vector is in the same order as the input (i.e. you can zip it
-- with the inputs.)
applyPactCmds
    :: forall logger1 logger2 tbl. (Logger logger1, Logger logger2)
    => HasVersion
    => logger1
    -> ServiceEnv tbl
    -> BlockEnv logger2
    -> Vector Pact4.Transaction
    -> Miner
    -> ModuleCache
    -> Maybe Pact4.Gas
    -> ExceptT BlockInvalidError IO (T2 (Vector ((Pact4.CommandResult [Pact4.TxLogJson]))) ModuleCache)
applyPactCmds logger serviceEnv blockEnv cmds miner startModuleCache blockGas = do
    (txOuts, T2 mcOut _) <-
      flip runStateT (T2 startModuleCache blockGas) $
        go [] (zip [0..] $ V.toList cmds)
    return $! T2 (V.fromList . List.reverse $ txOuts) mcOut
  where
    go
      :: [Pact4.CommandResult [Pact4.TxLogJson]]
      -> [(Word, Pact4.Transaction)]
      -> StateT
          (T2 ModuleCache (Maybe Pact4.Gas))
          (ExceptT BlockInvalidError IO)
          [Pact4.CommandResult [Pact4.TxLogJson]]
    go !acc = \case
        [] -> do
            pure acc
        (txIdxInBlock, tx) : rest -> do
            r <- applyPactCmd logger serviceEnv blockEnv (TxBlockIdx txIdxInBlock) miner tx
            go (r : acc) rest

applyPactCmd
  :: (Logger logger1, Logger logger2)
  => HasVersion
  => logger1
  -> ServiceEnv tbl
  -> BlockEnv logger2
  -> TxIdxInBlock
  -> Miner
  -> Pact4.Transaction
  -> StateT
      (T2 ModuleCache (Maybe Pact4.Gas))
      (ExceptT BlockInvalidError IO)
      (Pact4.CommandResult [Pact4.TxLogJson])
applyPactCmd logger serviceEnv blockEnv txIdxInBlock miner cmd = StateT $ \(T2 mcache maybeBlockGasRemaining) -> do
  let pactDb = blockEnv ^. benvDbEnv . cpPactDbEnv
  let
    requestedTxGasLimit = view Pact4.cmdGasLimit (Pact4.payloadObj <$> cmd)
    -- notice that we add 1 to the remaining block gas here, to distinguish the
    -- cases "tx used exactly as much gas remained in the block" (which is fine)
    -- and "tx attempted to use more gas than remains in the block" (which is
    -- illegal). for example: tx has a tx gas limit of 10000. the block has 5000
    -- remaining gas. therefore the tx is applied with a tx gas limit of 5001.
    -- if it uses 5001, that's illegal; if it uses 5000 or less, that's legal.
    newTxGasLimit = case maybeBlockGasRemaining of
      Nothing -> requestedTxGasLimit
      Just blockGasRemaining -> min (fromIntegral blockGasRemaining) requestedTxGasLimit
    gasLimitedCmd =
      set Pact4.cmdGasLimit newTxGasLimit (Pact4.payloadObj <$> cmd)
    initialGas = Pact4.initialGasOf (Pact4._cmdPayload cmd)
  let !hsh = Pact4._cmdHash cmd

  T2 result mcache' <- do
    let gasModel = getGasModel bCtx
    if _bctxIsGenesis bCtx
    then liftIO $! Pact4.applyGenesisCmd logger pactDb Pact4.noSPVSupport bCtx gasLimitedCmd
    else do
      -- FIXME
      -- let bhdb = view psBlockHeaderDb serviceEnv
      -- let spv = Pact4.pactSPV bhdb (_parentHeader parent)
      let
        txTimeout io = do
            logFunctionText logger Debug $ "txTimeLimit was not set - defaulting to a function of the block gas limit"
            io
      T3 r c _warns <- do
        liftIO $ txTimeout $
          Pact4.applyCmd logger
            -- FIXME spv
            blockEnv miner gasModel txIdxInBlock undefined gasLimitedCmd initialGas mcache
      pure $ T2 r c

  if _bctxIsGenesis bCtx
  then return ()
  -- FIXME: what? why are we doing this here? is this a bug?
  -- _k $ updateInitCache (bCtx ^. tcParentHeader) mcache'
  else liftIO $ debugResult logger "applyPactCmd" (Pact4.crLogs %~ fmap J.Array $ result)

  -- mark the tx as processed at the checkpointer.
  liftIO $ _cpRegisterProcessedTx (blockEnv ^. benvDbEnv) (coerce $ Pact4.toUntypedHash hsh)
  case maybeBlockGasRemaining of
    Just blockGasRemaining
      | Left _ <- Pact4._pactResult (Pact4._crResult result)
      , blockGasRemaining < fromIntegral requestedTxGasLimit
      -> throwError $ BlockInvalidDueToInvalidTxAtRuntime TxExceedsBlockGasLimit
        -- ^ this tx attempted to consume more gas than remains in the
        -- block, so the block is invalid. we know this because failing
        -- transactions consume their entire gas limit.
    _ -> return ()
  let maybeBlockGasRemaining' = (\g -> g - Pact4._crGas result) <$> maybeBlockGasRemaining
  pure (result, T2 mcache' maybeBlockGasRemaining')
  where
  bCtx = blockEnv ^. benvBlockCtx

pact4TransactionsFromPayload
    :: Pact4.PactParserVersion
    -> PayloadData
    -> ExceptT BlockInvalidError IO (Vector Pact4.Transaction)
pact4TransactionsFromPayload ppv plData = do
    vtrans <- fmap V.fromList $
              mapM toCWTransaction $
              toList (view payloadDataTransactions plData)
    let (theLefts, theRights) = partitionEithers $ V.toList vtrans
    unless (null theLefts) $ do
        let ls = map T.pack theLefts
        throwError $ BlockInvalidDueToTxDecodeFailure ls
    return $! V.fromList theRights
  where
    toCWTransaction bs =
      liftIO $ evaluate $
        codecDecode (Pact4.payloadCodec ppv) (_transactionBytes bs)

debugResult :: J.Encode a => Logger logger => logger -> Text -> a -> IO ()
debugResult logger msg result =
  logFunctionText logger Debug $ trunc $ msg <> " result: " <> J.encodeText result
  where
    trunc t | T.length t < limit = t
            | otherwise = T.take limit t <> " [truncated]"
    limit = 5000


-- | Calculate miner reward.
--
-- See: 'rewards/miner_rewards.csv'
--
minerReward
    :: HasVersion
    => BlockHeight
    -> Pact4.ParsedDecimal
minerReward = Pact4.ParsedDecimal
    . _kda
    . minerRewardKda
    . blockMinerReward
{-# INLINE minerReward #-}

data CRLogPair = CRLogPair Pact4.Hash [Pact4.TxLogJson]

instance J.Encode CRLogPair where
  build (CRLogPair h logs) = J.object
    [ "hash" J..= h
    , "rawLogs" J..= J.Array logs
    ]
  {-# INLINE build #-}

validateHashes
    :: HasVersion
    => BlockCtx
        -- ^ Current Header
    -> CheckablePayload
    -> Miner
    -> Transactions Pact4.Transaction (Pact4.CommandResult [Pact4.TxLogJson])
    -> Either PactInternalError PayloadWithOutputs
validateHashes bctx payload miner transactions =
    if newHash == expectedPayloadHash
      then Right actualPwo
      else Left $ BlockValidationFailure $
              J.encodeText $ J.object
                [ "header" J..= sshow @_ @Text bctx
                , "actual" J..= J.encodeWithAeson actualPwo
                , "expected" J..?= case payload of
                    CheckablePayload _ -> Nothing
                    CheckablePayloadWithOutputs pwo -> Just $ J.encodeWithAeson pwo
                ]
  where

    actualPwo = toPayloadWithOutputs miner transactions

    newHash = _payloadWithOutputsPayloadHash actualPwo
    expectedPayloadHash = checkablePayloadExpectedHash payload
