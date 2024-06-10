{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module      :  Chainweb.Pact.TransactionExec
-- Copyright   :  Copyright © 2018 Kadena LLC.
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mark Nichols <mark@kadena.io>, Emily Pillmore <emily@kadena.io>
-- Stability   :  experimental
--
-- Pact command execution and coin-contract transaction logic for Chainweb
--
module Chainweb.Pact.TransactionExec.Pact5
( -- * Transaction Execution
  applyCmd
, applyLocal
, applyExec
, applyExec'
, applyContinuation
, applyContinuation'
, runPayload
, enablePactEvents'
, enforceKeysetFormats'
, disableReturnRTC

  -- * Gas Execution
, buyGas

  -- * Coinbase Execution
, applyCoinbase
, EnforceCoinbaseFailure(..)

  -- * Command Helpers
, publicMetaOf
, networkIdOf
, gasSupplyOf

  -- * Utilities
, buildExecParsedCode
, mkMagicCapSlot
, listErrMsg
, initialGasOf

) where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Parallel.Strategies(using, rseq)

import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SB
import Data.Coerce (coerce)
import Data.Decimal (Decimal, roundTo)
import Data.Default (def)
import Data.Foldable (fold, for_)
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.LogLevel as L

-- internal Pact modules

import Pact.Eval (eval, liftTerm)
import Pact.Gas (freeGasEnv)
import Pact.Interpreter
import qualified Pact.JSON.Decode as J
import qualified Pact.JSON.Encode as J
import Pact.JSON.Legacy.Value
import Pact.Native.Capabilities (evalCap)
import Pact.Native.Internal (appToCap)
import Pact.Parse (ParsedDecimal(..), ParsedInteger(..))
import Pact.Runtime.Capabilities (popCapStack)
import Pact.Runtime.Utils (lookupModule)
import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Hash as Pact
import Pact.Types.KeySet
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.RPC
import Pact.Types.Runtime hiding (catchesPactError)
import Pact.Types.Server
import Pact.Types.SPV
import Pact.Types.Verifier

import Pact.Types.Util as PU

import Pact.Core.Serialise.LegacyPact ()
import qualified Pact.Core.Compile as PCore
import qualified Pact.Core.Evaluate as PCore
import qualified Pact.Core.Capabilities as PCore
import qualified Pact.Core.Names as PCore
import qualified Pact.Core.Namespace as PCore
import qualified Pact.Core.Persistence as PCore
import qualified Pact.Core.Pretty as PCore
import qualified Pact.Core.Gas as PCore
import qualified Pact.Core.Hash as PCore
import qualified Pact.Core.PactValue as PCore
import qualified Pact.Core.Environment as PCore
import qualified Pact.Core.Builtin as PCore
import qualified Pact.Core.Syntax.ParseTree as PCore
import qualified Pact.Core.DefPacts.Types as PCore
import qualified Pact.Core.Scheme as PCore
import qualified Pact.Core.StableEncoding as PCore
import qualified Pact.Core.SPV as PCore
import qualified Pact.Core.Verifiers as PCore
import qualified Pact.Core.Info as PCore

-- internal Chainweb modules
import qualified Chainweb.Pact.Transactions.CoinCoreV4Transactions as CoinCoreV4

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import qualified Chainweb.ChainId as Chainweb
import Chainweb.Mempool.Mempool (requestKeyToTransactionHash)
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.Types
import qualified Chainweb.Pact.Templates.Pact5 as Pact5
import Chainweb.Pact.Templates.Pact4
import Chainweb.Pact.Utils
import qualified Chainweb.Pact.Conversion as PactConversion
import Chainweb.Pact.Types hiding (logError)
import Chainweb.Pact.Backend.Types
import Chainweb.Transaction
import Chainweb.VerifierPlugin
import Chainweb.Utils (encodeToByteString, sshow, tryAllSynchronous, T2(..), T4(..))
import Chainweb.Version as V
import Chainweb.Version.Guards as V
import Chainweb.Version.Utils as V
import Pact.JSON.Encode (toJsonViaEncode)

import qualified Debug.Trace as TRACE

-- Note [Throw out verifier proofs eagerly]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We try to discard verifier proofs eagerly so that we don't hang onto them in
-- the liveset. This implies that we also try to discard `Command`s for the same
-- reason, because they contain the verifier proofs and other data we probably
-- don't need.

-- -------------------------------------------------------------------------- --

-- | "Magic" capability 'COINBASE' used in the coin contract to
-- constrain coinbase calls.
--
magic_COINBASE :: CapSlot SigCapability
magic_COINBASE = mkMagicCapSlot "COINBASE"

-- | "Magic" capability 'GAS' used in the coin contract to
-- constrain gas buy/redeem calls.
--
magic_GAS :: CapSlot SigCapability
magic_GAS = mkMagicCapSlot "GAS"

-- | "Magic" capability 'GENESIS' used in the coin contract to
-- constrain genesis-only allocations
--
magic_GENESIS :: CapSlot SigCapability
magic_GENESIS = mkMagicCapSlot "GENESIS"

debitCap :: Text -> SigCapability
debitCap s = mkCoinCap "DEBIT" [PLiteral (LString s)]

core_magic_COINBASE :: PCore.CapSlot PCore.QualifiedName PCore.PactValue
core_magic_COINBASE = mkMagicCoreCapSlot "COINBASE"

core_magic_GAS :: PCore.CapSlot PCore.QualifiedName PCore.PactValue
core_magic_GAS = mkMagicCoreCapSlot "GAS"

-- core_magic_GENESIS :: PCore.CapSlot PCore.QualifiedName PCore.PactValue
-- core_magic_GENESIS = mkMagicCoreCapSlot "GENESIS"

onChainErrorPrintingFor :: TxContext -> UnexpectedErrorPrinting
onChainErrorPrintingFor txCtx =
  if guardCtx chainweb219Pact txCtx
  then CensorsUnexpectedError
  else PrintsUnexpectedError

-- | The main entry point to executing transactions. From here,
-- 'applyCmd' assembles the command environment for a command and
-- orchestrates gas buys/redemption, and executing payloads.
--
applyCmd
    :: (Logger logger)
    => ChainwebVersion
    -> logger
      -- ^ Pact logger
    -> Maybe logger
      -- ^ Pact gas logger
    -> (PactDbEnv p, CoreDb)
      -- ^ Pact db environment
    -> Miner
      -- ^ The miner chosen to mine the block
    -> (GasModel, PCore.GasModel PCore.CoreBuiltin)
      -- ^ Gas model (pact Service config)
    -> TxContext
      -- ^ tx metadata and parent header
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command (Payload PublicMeta ParsedCode)
      -- ^ command with payload to execute
    -> Gas
      -- ^ initial gas used
    -> (ModuleCache, CoreModuleCache)
      -- ^ cached module state
    -> ApplyCmdExecutionContext
      -- ^ is this a local or send execution context?
    -> IO (T4 (CommandResult [TxLogJson]) ModuleCache CoreModuleCache (S.Set PactWarning))
applyCmd v logger gasLogger (pdbenv, coreDb) miner (gasModel, gasModelCore) txCtx spv cmd initialGas (mcache0, mccache0) callCtx = do
    T2 cr st <- runTransactionM cenv txst applyBuyGas

    let cache = _txCache st
        warns = _txWarnings st

    pure $ T4 cr cache undefined warns
  where
    stGasModel
      | chainweb217Pact' = gasModel
      | otherwise = _geGasModel freeGasEnv

    stGasModelCore
      | chainweb217Pact' = gasModelCore
      | otherwise = PCore.freeGasModel
    txst = TransactionState mcache0 mempty 0 Nothing (Right stGasModelCore) mempty
    quirkGasFee = v ^? versionQuirks . quirkGasFees . ix requestKey

    executionConfigNoHistory = ExecutionConfig
      $ S.singleton FlagDisableHistoryInTransactionalMode
      <> S.fromList
        ([ FlagOldReadOnlyBehavior | isPactBackCompatV16 ]
        ++ [ FlagPreserveModuleNameBug | not isModuleNameFix ]
        ++ [ FlagPreserveNsModuleInstallBug | not isModuleNameFix2 ])
      <> flagsFor v (ctxChainId txCtx) (ctxCurrentBlockHeight txCtx)

    cenv = TransactionEnv Transactional (Right coreDb) logger gasLogger (ctxToPublicData txCtx) spv nid gasPrice
      requestKey (fromIntegral gasLimit) executionConfigNoHistory quirkGasFee

    !requestKey = cmdToRequestKey cmd
    !gasPrice = view cmdGasPrice cmd
    !gasLimit = view cmdGasLimit cmd
    !nid = networkIdOf cmd
    currHeight = ctxCurrentBlockHeight txCtx
    cid = ctxChainId txCtx
    isModuleNameFix = enableModuleNameFix v cid currHeight
    isModuleNameFix2 = enableModuleNameFix2 v cid currHeight
    isPactBackCompatV16 = pactBackCompat_v16 v cid currHeight
    chainweb213Pact' = guardCtx chainweb213Pact txCtx
    chainweb217Pact' = guardCtx chainweb217Pact txCtx
    chainweb219Pact' = guardCtx chainweb219Pact txCtx
    chainweb223Pact' = guardCtx chainweb223Pact txCtx
    allVerifiers = verifiersAt v cid currHeight
    usePactTng = chainweb223Pact v cid currHeight
    toEmptyPactError (PactError errty _ _ _) = PactError errty def [] mempty

    toOldListErr pe = pe { peDoc = listErrMsg }
    isOldListErr = \case
      PactError EvalError _ _ doc -> "Unknown primitive" `T.isInfixOf` renderCompactText' doc
      _ -> False

    redeemAllGas r = do
      txGasUsed .= fromIntegral gasLimit
      applyRedeem r

    applyBuyGas =
      -- TRACE.traceShowM ("applyBuyGas.CACHE: ", LHM.keys $ _getModuleCache mcache0, M.keys $ _getCoreModuleCache mccache0)
      catchesPactError logger (onChainErrorPrintingFor txCtx) (buyGas txCtx cmd miner) >>= \case
        Left e -> view txRequestKey >>= \rk ->
          throwM $ BuyGasFailure $ GasPurchaseFailure (requestKeyToTransactionHash rk) e
        Right _ -> checkTooBigTx initialGas gasLimit applyVerifiers redeemAllGas

    displayPactError e = do
      r <- failTxWith e "tx failure for request key when running cmd"
      redeemAllGas r

    stripPactError e = do
      let e' = case callCtx of
            ApplyLocal -> e
            ApplySend -> toEmptyPactError e
      r <- failTxWith e' "tx failure for request key when running cmd"
      redeemAllGas r

    applyVerifiers = do
      if chainweb223Pact'
      then do
        gasUsed <- use txGasUsed
        let initGasRemaining = fromIntegral gasLimit - gasUsed
        verifierResult <- liftIO $ runVerifierPlugins (ctxVersion txCtx, cid, currHeight) logger allVerifiers initGasRemaining cmd
        case verifierResult of
          Left err -> do
            let errMsg = "Tx verifier error: " <> getVerifierError err
            cmdResult <- failTxWith
              (PactError TxFailure def [] (pretty errMsg))
              errMsg
            redeemAllGas cmdResult
          Right verifierGasRemaining -> do
            txGasUsed += initGasRemaining - verifierGasRemaining
            applyPayload
      else applyPayload

    applyPayload = do
      txGasModel .= (Right gasModelCore)
      if chainweb217Pact' then txGasUsed += initialGas
      else txGasUsed .= initialGas

      cr <- catchesPactError logger (onChainErrorPrintingFor txCtx) $! runPayload cmd managedNamespacePolicy
      case cr of
        Left e
          -- 2.19 onwards errors return on chain
          | chainweb219Pact' -> displayPactError e
          -- 2.17 errors were removed
          | chainweb217Pact' -> stripPactError e
          | chainweb213Pact' || not (isOldListErr e) -> displayPactError e
          | otherwise -> do
              r <- failTxWith (toOldListErr e) "tx failure for request key when running cmd"
              redeemAllGas r
        Right r -> applyRedeem r

    applyRedeem cr = do
      txGasModel .= (Right gasModelCore)

      r <- catchesPactError logger (onChainErrorPrintingFor txCtx) $! redeemGas txCtx cmd miner
      case r of
        Left e ->
          -- redeem gas failure is fatal (block-failing) so miner doesn't lose coins
          fatal $ "tx failure for request key while redeeming gas: " <> sshow e
        Right es -> do
          logs <- use txLogs

          -- /local requires enriched results with metadata, while /send strips them.
          -- when ?preflight=true is set, make sure that metadata occurs in result.

          let !cr' = case callCtx of
                ApplySend -> set crLogs (Just logs) $ over crEvents (es ++) cr
                ApplyLocal -> set crMetaData (Just $ J.toJsonViaEncode $ ctxToPublicData' txCtx)
                  $ set crLogs (Just logs)
                  $ over crEvents (es ++) cr

          return cr'

listErrMsg :: Doc
listErrMsg =
    "Unknown primitive \"list\" in determining cost of GUnreduced\nCallStack (from HasCallStack):\n  error, called at src/Pact/Gas/Table.hs:209:22 in pact-4.2.0-fe223ad86f1795ba381192792f450820557e59c2926c747bf2aa6e398394bee6:Pact.Gas.Table"

flagsFor :: ChainwebVersion -> V.ChainId -> BlockHeight -> S.Set ExecutionFlag
flagsFor v cid bh = S.fromList $ concat
  [ enablePactEvents' v cid bh
  , enablePact40 v cid bh
  , enablePact42 v cid bh
  , enforceKeysetFormats' v cid bh
  , enablePactModuleMemcheck v cid bh
  , enablePact43 v cid bh
  , enablePact431 v cid bh
  , enablePact44 v cid bh
  , enablePact45 v cid bh
  , enableNewTrans v cid bh
  , enablePact46 v cid bh
  , enablePact47 v cid bh
  , enablePact48 v cid bh
  , disableReturnRTC v cid bh
  , enablePact49 v cid bh
  , enablePact410 v cid bh
  , enablePact411 v cid bh
  , enablePact412 v cid bh
  ]

applyCoinbase
    :: (Logger logger)
    => ChainwebVersion
    -> logger
      -- ^ Pact logger
    -> (PactDbEnv p, CoreDb)
      -- ^ Pact db environment
    -> Miner
      -- ^ The miner chosen to mine the block
    -> ParsedDecimal
      -- ^ Miner reward
    -> TxContext
      -- ^ tx metadata and parent header
    -> EnforceCoinbaseFailure
      -- ^ enforce coinbase failure or not
    -> CoinbaseUsePrecompiled
      -- ^ always enable precompilation
    -> (ModuleCache, CoreModuleCache)
    -> IO (T2 (CommandResult [TxLogJson]) (Maybe (ModuleCache, CoreModuleCache)))
applyCoinbase v logger (dbEnv, coreDb) (Miner mid mks@(MinerKeys mk)) reward@(ParsedDecimal d) txCtx
  (EnforceCoinbaseFailure enfCBFailure) (CoinbaseUsePrecompiled enablePC) (mc, cmc)
  | fork1_3InEffect || enablePC = do
    when chainweb213Pact' $ enforceKeyFormats
        (\k -> throwM $ CoinbaseFailure $ "Invalid miner key: " <> sshow k)
        (validKeyFormats v (ctxChainId txCtx) (ctxCurrentBlockHeight txCtx))
        mk
    let (cterm, cexec) = mkCoinbaseTerm mid mks reward
        interp = Interpreter $ \_ -> do put initState; fmap pure (eval cterm)
        coreState = setCoreModuleCache cmc $ initCoreCapabilities [core_magic_COINBASE]
        (coinbaseTerm, _) = Pact5.mkCoinbaseTerm mid mks (GasSupply reward)
    go interp coreState cexec (Just coinbaseTerm)
  | otherwise = do
    cexec <- mkCoinbaseCmd mid mks reward
    let interp = initStateInterpreter initState
    let coreState = setCoreModuleCache cmc $ initCoreCapabilities [core_magic_COINBASE]
    go interp coreState cexec Nothing
  where
    chainweb213Pact' = chainweb213Pact v cid bh
    fork1_3InEffect = vuln797Fix v cid bh
    throwCritical = fork1_3InEffect || enfCBFailure
    ec = ExecutionConfig $ S.delete FlagEnforceKeyFormats $ fold
      [ S.singleton FlagDisableModuleInstall
      , S.singleton FlagDisableHistoryInTransactionalMode
      , flagsFor v (ctxChainId txCtx) (ctxCurrentBlockHeight txCtx)
      ]
    usePactTng = chainweb223Pact v (ctxChainId txCtx) bh
    tenv = TransactionEnv Transactional (Right coreDb) logger Nothing (ctxToPublicData txCtx) noSPVSupport
           Nothing 0.0 rk 0 ec Nothing
    txst = TransactionState mc mempty 0 Nothing (Right $ PCore.freeGasModel) mempty
    initState = setModuleCache mc $ initCapabilities [magic_COINBASE]
    rk = RequestKey chash
    parent = _tcParentHeader txCtx

    bh = ctxCurrentBlockHeight txCtx
    cid = Chainweb._chainId parent
    chash = Pact.Hash $ SB.toShort $ encodeToByteString $ _blockHash $ _parentHeader parent
        -- NOTE: it holds that @ _pdPrevBlockHash pd == encode _blockHash@
        -- NOTE: chash includes the /quoted/ text of the parent header.

    go interp evState cexec@(ExecMsg _ execData) mCoinbaseTerm = evalTransactionM tenv txst $! do
      case mCoinbaseTerm of
        Just coinbaseTerm -> do
          evalEnv <- mkCoreEvalEnv managedNamespacePolicy (MsgData execData Nothing chash mempty [])
          cr <- liftIO $ PCore.evalTermExec evalEnv evState coinbaseTerm

          case cr of
            Right er -> do
              debug
                $! "successful coinbase of "
                <> T.take 18 (sshow d)
                <> " to "
                <> sshow mid

              upgradedModuleCache <- applyUpgrades v cid bh

              coreCr <- mkCommandResultFromCoreResult er

              return $! T2
                coreCr
                upgradedModuleCache

            Left e
              | throwCritical -> throwM $ CoinbaseFailure $ sshow e
              | otherwise -> (`T2` Nothing) <$> failTxWith (PactError EvalError (Info Nothing) [] mempty) "coinbase tx failure"
        _ -> do
          -- TODO: what to do if no coinbase term?
          pure undefined


applyLocal
    :: (Logger logger)
    => logger
      -- ^ Pact logger
    -> Maybe logger
      -- ^ Pact gas logger
    -> (PactDbEnv p, CoreDb)
      -- ^ Pact db environment
    -> (GasModel, PCore.GasModel PCore.CoreBuiltin)
      -- ^ Gas model (pact Service config)
    -> TxContext
      -- ^ tx metadata and parent header
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command PayloadWithText
      -- ^ command with payload to execute
    -> (ModuleCache, CoreModuleCache)
    -> ExecutionConfig
    -> IO (CommandResult [TxLogJson])
applyLocal logger gasLogger (dbEnv, coreDb) (gasModel, gasModelCore) txCtx spv cmdIn (mc, cmc) execConfig =
    evalTransactionM tenv txst go
  where
    !cmd = payloadObj <$> cmdIn `using` traverse rseq
    !rk = cmdToRequestKey cmd
    !nid = networkIdOf cmd
    !chash = toUntypedHash $ _cmdHash cmd
    !signers = _pSigners $ _cmdPayload cmd
    !verifiers = fromMaybe [] $ _pVerifiers $ _cmdPayload cmd
    !gasPrice = view cmdGasPrice cmd
    !gasLimit = view cmdGasLimit cmd
    tenv = TransactionEnv Local (Right coreDb) logger gasLogger (ctxToPublicData txCtx) spv nid gasPrice
           rk (fromIntegral gasLimit) execConfig Nothing
    txst = TransactionState mc mempty 0 Nothing (Right gasModelCore) mempty
    gas0 = initialGasOf (_cmdPayload cmdIn)
    currHeight = ctxCurrentBlockHeight txCtx
    cid = V._chainId txCtx
    v = _chainwebVersion txCtx
    allVerifiers = verifiersAt v cid currHeight
    usePactTng = chainweb223Pact (ctxVersion txCtx) (ctxChainId txCtx) currHeight
    -- Note [Throw out verifier proofs eagerly]
    !verifiersWithNoProof =
        (fmap . fmap) (\_ -> ()) verifiers
        `using` (traverse . traverse) rseq

    applyVerifiers m = do
      let initGasRemaining = fromIntegral gasLimit - gas0
      verifierResult <- liftIO $ runVerifierPlugins (v, cid, currHeight) logger allVerifiers initGasRemaining cmd
      case verifierResult of
        Left err -> do
          let errMsg = "Tx verifier error: " <> getVerifierError err
          failTxWith
            (PactError TxFailure def [] (pretty errMsg))
            errMsg
        Right verifierGasRemaining -> do
          let gas1 = (initGasRemaining - verifierGasRemaining) + gas0
          applyPayload gas1 m

    applyPayload gas1 m = do
      interp <- gasInterpreter gas1
      let coreState = def
      cr <- catchesPactError logger PrintsUnexpectedError $! case m of
        Exec em -> applyExec gas1 coreState em signers chash managedNamespacePolicy
        Continuation cm -> applyContinuation gas1 coreState cm signers chash managedNamespacePolicy


      case cr of
        Left e -> failTxWith e "applyLocal"
        Right r -> return $! r { _crMetaData = Just (J.toJsonViaEncode $ ctxToPublicData' txCtx) }

    go = checkTooBigTx gas0 gasLimit (applyVerifiers $ _pPayload $ _cmdPayload cmd) return

-- | Apply (forking) upgrade transactions and module cache updates
-- at a particular blockheight.
--
-- This is the place where we consistently /introduce/ new transactions
-- into the blockchain along with module cache updates. The only other
-- places are Pact Service startup and the
-- empty-module-cache-after-initial-rewind case caught in 'execTransactions'
-- which both hit the database.
--
applyUpgrades
  :: (Logger logger)
  => ChainwebVersion
  -> Chainweb.ChainId
  -> BlockHeight
  -> TransactionM logger p (Maybe (ModuleCache, CoreModuleCache))
applyUpgrades v cid height
     | Just upg <-
         v ^? versionUpgrades . onChain cid . at height . _Just = applyUpgrade upg
     | cleanModuleCache v cid height = filterModuleCache
     | otherwise = return Nothing
  where
    coinModuleName = ModuleName "coin" Nothing
    coinCoreModuleName = PCore.ModuleName "coin" Nothing
    installCoinModuleAdmin = set (evalCapabilities . capModuleAdmin) $ S.singleton coinModuleName
    installCoreCoinModuleAdmin = set (PCore.esCaps . PCore.csModuleAdmin) $ S.singleton coinCoreModuleName

    filterModuleCache = do
      mc <- use txCache
      pure $ Just $
        ( filterModuleCacheByKey (== coinModuleName) mc
        , mempty
        )

    applyUpgrade upg = do
      infoLog "Applying upgrade!"
      let payloads = map (fmap payloadObj) $ _upgradeTransactions upg

      --
      -- In order to prime the module cache with all new modules for subsequent
      -- blocks, the caches from each tx are collected and the union of all
      -- those caches is returned. The calling code adds this new cache to the
      -- init cache in the pact service state (_psInitCache).
      --

      let flags = flagsFor v cid (if _legacyUpgradeIsPrecocious upg then height + 1 else height)
      caches <- local
        (txExecutionConfig .~ ExecutionConfig flags)
        (mapM applyTx payloads)
      return $ Just $ bimap mconcat mconcat $ unzip $ reverse caches

    coreInitState = installCoreCoinModuleAdmin $ initCoreCapabilities [mkMagicCoreCapSlot "REMEDIATE"]

    applyTx tx = do
      infoLog $ "Running upgrade tx " <> sshow (_cmdHash tx)
      tryAllSynchronous (runGenesisCore tx permissiveNamespacePolicy coreInitState) >>= \case
        Right _ -> do
          c <- use txCache
          pure (c, undefined)
        Left e -> do
          logError $ "Upgrade transaction failed! " <> sshow e
          throwM e

failTxWith
    :: (Logger logger)
    => PactError
    -> Text
    -> TransactionM logger p (CommandResult [TxLogJson])
failTxWith err msg = do
    logs <- use txLogs
    gas <- view txGasLimit -- error means all gas was charged
    rk <- view txRequestKey
    l <- view txLogger

    liftIO $ logFunction l L.Info
      (Pact4TxFailureLog rk err msg)

    return $! CommandResult rk Nothing (PactResult (Left err))
      gas (Just logs) Nothing Nothing []

runPayload
    :: (Logger logger)
    => Command (Payload PublicMeta ParsedCode)
    -> NamespacePolicy
    -> TransactionM logger p (CommandResult [TxLogJson])
runPayload cmd nsp = do
    g0 <- use txGasUsed
    interp <- gasInterpreter g0
    let coreState = def

    -- Note [Throw out verifier proofs eagerly]
    let !verifiersWithNoProof =
            (fmap . fmap) (\_ -> ()) verifiers
            `using` (traverse . traverse) rseq

    case payload of
      Exec pm -> applyExec g0 coreState pm signers chash nsp
      Continuation ym -> applyContinuation g0 coreState ym signers chash nsp

  where
    verifiers = fromMaybe [] $ _pVerifiers $ _cmdPayload cmd
    signers = _pSigners $ _cmdPayload cmd
    chash = toUntypedHash $ _cmdHash cmd
    payload = _pPayload $ _cmdPayload cmd

runGenesisCore
    :: (Logger logger)
    => Command (Payload PublicMeta ParsedCode)
    -> NamespacePolicy
    -> PCore.EvalState PCore.CoreBuiltin PCore.SpanInfo
    -> TransactionM logger p ()
runGenesisCore cmd nsp coreState = case payload of
    Exec pm -> void $ applyExec' 0 coreState pm signers chash nsp
    Continuation _ -> error "runGenesisCore Continuation not supported"
  where
    signers = _pSigners $ _cmdPayload cmd
    chash = toUntypedHash $ _cmdHash cmd
    payload = _pPayload $ _cmdPayload cmd

mkCommandResultFromCoreResult
    :: (Logger logger)
    => PCore.EvalResult a
    -> TransactionM logger p (CommandResult [TxLogJson])
mkCommandResultFromCoreResult PCore.EvalResult{..} = do
    !logs <- use txLogs
    !rk <- view txRequestKey

    let convertPactValue pv = J.decodeStrict $ PCore.encodeStable pv

    let !lastResult = last _erOutput

    let
      toModuleName m =
        ModuleName
                { _mnName = PCore._mnName m
                , _mnNamespace = coerce <$> PCore._mnNamespace m
                }
      toQualifiedName qn =
        QualifiedName
            { _qnQual = toModuleName $ PCore._qnModName qn
            , _qnName = PCore._qnName qn
            , _qnInfo = Info Nothing
            }
      toPactContinuation dpc =
        PactContinuation
            { _pcDef = QName $ toQualifiedName $ PCore._pcName dpc
            , _pcArgs = catMaybes $ convertPactValue <$> PCore._pcArgs dpc
            }
      toYield y =
        Yield
            { _yData = ObjectMap $ Map.fromList $ map (\(f,s) -> (coerce f, fromJust $ convertPactValue s)) $ Map.toList $ PCore._yData y
            , _yProvenance = PCore._yProvenance y <&> \p -> Provenance
                { _pTargetChainId = coerce $ PCore._pTargetChainId p
                , _pModuleHash = coerce $ PCore._pModuleHash p
                }
            , _ySourceChain = coerce <$> PCore._ySourceChain y
            }
      toNested dpe =
        NestedPactExec
            { _npeStepCount = PCore._peStepCount dpe
            , _npeYield = toYield <$> PCore._peYield dpe
            , _npeExecuted = Nothing
            , _npeStep = PCore._peStep dpe
            , _npePactId = coerce $ PCore._peDefPactId dpe
            , _npeContinuation = toPactContinuation $ PCore._peContinuation dpe
            , _npeNested = Map.fromList $ map (\(f, s) -> (coerce f, toNested s)) $ Map.toList $ PCore._peNestedDefPactExec dpe
            }
      toPactExec dpe =
        PactExec
            { _peStepCount = PCore._peStepCount dpe
            , _peYield = toYield <$> PCore._peYield dpe
            , _peExecuted = Nothing
            , _peStep = PCore._peStep dpe
            , _pePactId = coerce $ PCore._peDefPactId dpe
            , _peContinuation = toPactContinuation $ PCore._peContinuation dpe
            , _peStepHasRollback = PCore._peStepHasRollback dpe
            , _peNested = Map.fromList $ map (\(f, s) -> (coerce f, toNested s)) $ Map.toList $ PCore._peNestedDefPactExec dpe
            }
      toValResult = \case
          PCore.InterpretValue v _ -> fromJust $ convertPactValue v
          _ -> undefined
      toPactEvent e =
          PactEvent
            { _eventName = PCore._peName e
            , _eventParams = catMaybes $ convertPactValue <$> PCore._peArgs e
            , _eventModule = toModuleName $ PCore._peModule e
            , _eventModuleHash = coerce $ PCore._peModuleHash e
            }

    return $ CommandResult rk (coerce _erTxId) (PactResult (Right $ toValResult lastResult))
      (case _erGas of { PCore.Gas g -> Gas $ fromIntegral g })
      (Just logs) (toPactExec <$> _erExec) Nothing (map toPactEvent _erEvents)

applyExec
    :: (Logger logger)
    => Gas
    -> PCore.EvalState PCore.CoreBuiltin PCore.SpanInfo
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p (CommandResult [TxLogJson])
applyExec initialGas coreState em senderSigs hsh nsp = do
    er <- applyExec' initialGas coreState em senderSigs hsh nsp

    mkCommandResultFromCoreResult er

applyExec'
    :: (Logger logger)
    => Gas
    -> PCore.EvalState PCore.CoreBuiltin PCore.SpanInfo
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p (PCore.EvalResult [PCore.TopLevel PCore.SpanInfo])
applyExec' (Gas initialGas) coreState (ExecMsg parsedCode execData) senderSigs hsh nsp
    | null (_pcExps parsedCode) = throwCmdEx "No expressions found"
    | otherwise = do

      evalEnv <- mkCoreEvalEnv nsp (MsgData execData Nothing hsh senderSigs [])

      setEnvGasCore (PCore.Gas $ fromIntegral initialGas) evalEnv

      er <- liftIO $! PCore.evalExec evalEnv coreState (PCore.RawCode $ _pcCode parsedCode)
      case er of
        Right er' -> do
          -- liftIO $ print ("PCORE._erOutput", PCore._erOutput er')

          -- if we specified this transaction's gas fee manually as a "quirk",
          -- here we set the result's gas fee to agree with that
          quirkGasFee <- view txQuirkGasFee
          let quirkedEvalResult = case quirkGasFee of
                Nothing -> er'
                Just (Gas fee) -> er' { PCore._erGas = PCore.Gas $ fromIntegral fee }

          return quirkedEvalResult
        Left err -> do
          -- TRACE.traceShowM ("CORE.applyExec' modulecache" :: String, show $ _getCoreModuleCache ccache)

          TRACE.traceShowM ("CORE.applyExec'!!!!" :: String, show err, show $ PCore.RawCode $ _pcCode parsedCode)
          fatal $ "Pact Tng execution failed: " <> (T.pack $ show $ PCore.pretty err)

enablePactEvents' :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePactEvents' v cid bh = [FlagDisablePactEvents | not (enablePactEvents v cid bh)]

enforceKeysetFormats' :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enforceKeysetFormats' v cid bh = [FlagEnforceKeyFormats | enforceKeysetFormats v cid bh]

enablePact40 :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePact40 v cid bh = [FlagDisablePact40 | not (pact4Coin3 v cid bh)]

enablePact42 :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePact42 v cid bh = [FlagDisablePact42 | not (pact42 v cid bh)]

enablePactModuleMemcheck :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePactModuleMemcheck v cid bh = [FlagDisableInlineMemCheck | not (chainweb213Pact v cid bh)]

enablePact43 :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePact43 v cid bh = [FlagDisablePact43 | not (chainweb214Pact v cid bh)]

enablePact431 :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePact431 v cid bh = [FlagDisablePact431 | not (chainweb215Pact v cid bh)]

enablePact44 :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePact44 v cid bh = [FlagDisablePact44 | not (chainweb216Pact v cid bh)]

enablePact45 :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePact45 v cid bh = [FlagDisablePact45 | not (chainweb217Pact v cid bh)]

enableNewTrans :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enableNewTrans v cid bh = [FlagDisableNewTrans | not (pact44NewTrans v cid bh)]

enablePact46 :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePact46 v cid bh = [FlagDisablePact46 | not (chainweb218Pact v cid bh)]

enablePact47 :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePact47 v cid bh = [FlagDisablePact47 | not (chainweb219Pact v cid bh)]

enablePact48 :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePact48 v cid bh = [FlagDisablePact48 | not (chainweb220Pact v cid bh)]

enablePact49 :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePact49 v cid bh = [FlagDisablePact49 | not (chainweb221Pact v cid bh)]

enablePact410 :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePact410 v cid bh = [FlagDisablePact410 | not (chainweb222Pact v cid bh)]

enablePact411 :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePact411 v cid bh = [FlagDisablePact411 | not (chainweb223Pact v cid bh)]

enablePact412 :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
enablePact412 v cid bh = [FlagDisablePact412 | not (chainweb224Pact v cid bh)]

-- | Even though this is not forking, abstracting for future shutoffs
disableReturnRTC :: ChainwebVersion -> V.ChainId -> BlockHeight -> [ExecutionFlag]
disableReturnRTC _v _cid _bh = [FlagDisableRuntimeReturnTypeChecking]

applyContinuation
    :: (Logger logger)
    => Gas
    -> PCore.EvalState PCore.CoreBuiltin PCore.SpanInfo
    -> ContMsg
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p (CommandResult [TxLogJson])
applyContinuation initialGas coreState cm senderSigs hsh nsp = do
    er <- applyContinuation' initialGas coreState cm senderSigs hsh nsp
    cr <- mkCommandResultFromCoreResult er
    -- for_ _erLogGas $ \gl -> gasLog $ "gas logs: " <> sshow gl

    -- TODO: set tx warnings to eval warnings
    -- txWarnings <>= _erWarnings

    return cr

setEnvGas :: Gas -> EvalEnv e -> TransactionM logger p ()
setEnvGas initialGas = liftIO . views eeGas (`writeIORef` gasToMilliGas initialGas)

setEnvGasCore :: PCore.Gas -> PCore.EvalEnv PCore.CoreBuiltin PCore.SpanInfo -> TransactionM logger p ()
setEnvGasCore initialGas = liftIO . views PCore.eeGasRef (`writeIORef` PCore.gasToMilliGas initialGas)

applyContinuation'
    :: (Logger logger)
    => Gas
    -> PCore.EvalState PCore.CoreBuiltin PCore.SpanInfo
    -> ContMsg
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p (PCore.EvalResult [PCore.TopLevel PCore.SpanInfo])
applyContinuation' initialGas coreState (ContMsg pid s rb d proof) senderSigs hsh nsp = do

    evalEnv <- mkCoreEvalEnv nsp (MsgData d pactStep hsh senderSigs [])

    setEnvGasCore (PCore.Gas $ fromIntegral initialGas) evalEnv

    let
      convertPactValue pv = PactConversion.fromLegacyPactValue $
          aeson (error "applyContinuation': failed to parseJSON pact value") id $ A.fromJSON $ _getLegacyValue pv

      coreCm = PCore.ContMsg
          { PCore._cmPactId = coerce pid
          , PCore._cmStep = s
          , PCore._cmRollback = rb
          , PCore._cmData = either (error "applyContinuation': failed to convert pact value") id $ convertPactValue d
          , PCore._cmProof = coerce proof
          }

    er <- liftIO $! PCore.evalContinuation evalEnv coreState coreCm
    case er of
      Right er' -> do
        -- if we specified this transaction's gas fee manually as a "quirk",
        -- here we set the result's gas fee to agree with that
        quirkGasFee <- view txQuirkGasFee
        let quirkedEvalResult = case quirkGasFee of
              Nothing -> er'
              Just (Gas fee) -> er' { PCore._erGas = PCore.Gas $ fromIntegral fee }

        return quirkedEvalResult
      Left err -> do
        fatal $ "Pact Tng execution failed: " <> (T.pack $ show $ PCore.pretty err)

  where
    pactStep = Just $ PactStep s rb pid Nothing

-- | Build and execute 'coin.buygas' command from miner info and user command
-- info (see 'TransactionExec.applyCmd')
--
-- see: 'pact/coin-contract/coin.pact#fund-tx'
--
buyGas :: (Logger logger) => TxContext -> Command (Payload PublicMeta ParsedCode) -> Miner -> TransactionM logger p ()
buyGas txCtx cmd (Miner mid mks) = go
  where
    isChainweb224Pact = guardCtx chainweb224Pact txCtx
    sender = view (cmdPayload . pMeta . pmSender) cmd

    initState mc logGas =
      set evalLogGas (guard logGas >> Just [("GBuyGas",0)]) $ setModuleCache mc $ initCapabilities [magic_GAS]

    run input = do
      (findPayer txCtx cmd) >>= \r -> case r of
        Nothing -> input
        Just withPayerCap -> withPayerCap input

    (Hash chash) = toUntypedHash (_cmdHash cmd)
    bgHash = Hash (chash <> "-buygas")

    go = do
      mcache <- use txCache
      supply <- gasSupplyOf <$> view txGasLimit <*> view txGasPrice
      logGas <- isJust <$> view txGasLogger

      let (buyGasTerm, buyGasCmd@(ExecMsg _ execData)) =
            -- post-chainweb 2.24, we call buy-gas directly rather than
            -- going through fund-tx which is a defpact.
            if isChainweb224Pact
            then mkBuyGasTerm sender supply
            else mkFundTxTerm mid mks sender supply
          -- I don't recall why exactly, but we set up an interpreter
          -- that ignores its argument and instead executes a term
          -- of our choice. we do the same to redeem gas.
          interp mc = Interpreter $ \_input ->
            put (initState mc logGas) >> run (pure <$> eval buyGasTerm)
          coreState = initCoreCapabilities [core_magic_GAS]

      let
        gasCapName = QualifiedName (ModuleName "coin" Nothing) "GAS" def
        signedForGas signer =
          any (\sc -> _scName sc == gasCapName) (_siCapList signer)
        addDebit signer
          | signedForGas signer =
            signer & siCapList %~ (debitCap sender:)
          | otherwise = signer
        addDebitToSigners =
          fmap addDebit
        signersWithDebit = addDebitToSigners $ _pSigners $ _cmdPayload cmd


      evalEnv <- mkCoreEvalEnv managedNamespacePolicy (MsgData execData Nothing bgHash signersWithDebit [])

      let
        (t, _) = if isChainweb224Pact
              then Pact5.mkBuyGasTerm sender supply
              else Pact5.mkFundTxTerm mid mks sender supply

      -- no verifiers are allowed in buy gas
      -- quirked gas is not used either
      er <- locally txQuirkGasFee (const Nothing) $ liftIO $ PCore.evalTermExec evalEnv coreState t
      case er of
        Right er' -> do
          case PCore._erExec er' of
            Nothing
              | isChainweb224Pact ->
                return ()
              | otherwise ->
                -- should never occur pre-chainweb 2.24:
                -- would mean coin.fund-tx is not a pact
                fatal "buyGas: Internal error - empty continuation before 2.24 fork"
            Just pe
              | isChainweb224Pact ->
                fatal "buyGas: Internal error - continuation found after 2.24 fork"
              | otherwise -> do
                void $! txGasId .= (Just $! GasId (coerce $ PCore._peDefPactId pe))
        Left err -> do
          TRACE.traceM $ "CORE.buyGas failed!!" <> sshow err <> "\n" <> sshow t
          fatal $ "buyGas: Internal error - " <> sshow err

findPayer
  :: TxContext
  -> Command (Payload PublicMeta ParsedCode)
  -> Eval e (Maybe (Eval e [Term Name] -> Eval e [Term Name]))
findPayer txCtx cmd = runMaybeT $ do
    (!m,!qn,!as) <- MaybeT findPayerCap
    pMod <- MaybeT $ lookupModule qn m
    capRef <- MaybeT $ return $ lookupIfaceModRef qn pMod
    return $ runCap (getInfo qn) capRef as
  where
    setEnvMsgBody v e = set eeMsgBody v e

    findPayerCap :: Eval e (Maybe (ModuleName,QualifiedName,[PactValue]))
    findPayerCap = preview $ eeMsgSigs . folded . folded . to sigPayerCap . _Just

    sigPayerCap (SigCapability q@(QualifiedName m n _) as)
      | n == "GAS_PAYER" = Just (m,q,as)
    sigPayerCap _ = Nothing

    gasPayerIface = ModuleName "gas-payer-v1" Nothing

    lookupIfaceModRef (QualifiedName _ n _) (ModuleData (MDModule Module{..}) refs _)
      | gasPayerIface `elem` _mInterfaces = HM.lookup n refs
    lookupIfaceModRef _ _ = Nothing

    mkApp i r as = App (TVar r i) (map (liftTerm . fromPactValue) as) i

    runCap i capRef as input = do
      let msgBody = enrichedMsgBody cmd
          enrichMsgBody | guardCtx pactBackCompat_v16 txCtx = id
                        | otherwise = setEnvMsgBody (toLegacyJson msgBody)
      ar <- local enrichMsgBody $ do
        (cap, capDef, args) <- appToCap $ mkApp i capRef as
        evalCap i CapCallStack False (cap, capDef, args, i)

      case ar of
        NewlyAcquired -> do
          r <- input
          popCapStack (const (return ()))
          return r
        _ -> evalError' i "Internal error, GAS_PAYER already acquired"

enrichedMsgBody :: Command (Payload PublicMeta ParsedCode) -> Value
enrichedMsgBody cmd = case (_pPayload $ _cmdPayload cmd) of
  Exec (ExecMsg (ParsedCode _ exps) userData) ->
    object [ "tx-type" A..= ( "exec" :: Text)
           , "exec-code" A..= map renderCompactText exps
           , "exec-user-data" A..= pactFriendlyUserData (_getLegacyValue userData) ]
  Continuation (ContMsg pid step isRollback userData proof) ->
    object [ "tx-type" A..= ("cont" :: Text)
           , "cont-pact-id" A..= toJsonViaEncode pid
           , "cont-step" A..= toJsonViaEncode (LInteger $ toInteger step)
           , "cont-is-rollback" A..= toJsonViaEncode (LBool isRollback)
           , "cont-user-data" A..= pactFriendlyUserData (_getLegacyValue userData)
           , "cont-has-proof" A..= toJsonViaEncode (isJust proof)
           ]
  where
    pactFriendlyUserData Null = object []
    pactFriendlyUserData v = v

-- | Build and execute 'coin.redeem-gas' command from miner info and previous
-- command results (see 'TransactionExec.applyCmd')
--
-- see: 'pact/coin-contract/coin.pact#fund-tx'
--
redeemGas :: (Logger logger) => TxContext -> Command (Payload PublicMeta ParsedCode) -> Miner -> TransactionM logger p [PactEvent]
redeemGas txCtx cmd (Miner mid mks) = do
    mcache <- use txCache
    let sender = view (cmdPayload . pMeta . pmSender) cmd
    fee <- gasSupplyOf <$> use txGasUsed <*> view txGasPrice
    -- if we're past chainweb 2.24, we don't use defpacts for gas

    let
      coreState = initCoreCapabilities [core_magic_GAS]

    if guardCtx chainweb224Pact txCtx
    then do
      total <- gasSupplyOf <$> view txGasLimit <*> view txGasPrice
      let (redeemGasTerm, redeemGasCmd@(ExecMsg _ execData)) =
            mkRedeemGasTerm mid mks sender total fee
          -- I don't recall why exactly, but we set up an interpreter
          -- that ignores its argument and instead executes a term
          -- of our choice. we do the same to buy gas.
          interp = Interpreter $ \_input -> do
            -- we don't log gas when redeeming, because nobody can pay for it
            put (initCapabilities [magic_GAS] & setModuleCache mcache)
            fmap List.singleton (eval redeemGasTerm)
          (Hash chash) = toUntypedHash (_cmdHash cmd)
          rgHash = Hash (chash <> "-redeemgas")
          (redeemGasTermCore, _) = Pact5.mkRedeemGasTerm mid mks sender total fee
      locally txQuirkGasFee (const Nothing) $ do
        evalEnv <- mkCoreEvalEnv managedNamespacePolicy (MsgData execData Nothing rgHash (_pSigners $ _cmdPayload cmd) [])

        er <- liftIO $ PCore.evalTermExec evalEnv coreState redeemGasTermCore
        case er of
          Right er' -> do
            let
              convertPactValue pv = J.decodeStrict $ PCore.encodeStable pv
              toModuleName m =
                ModuleName
                        { _mnName = PCore._mnName m
                        , _mnNamespace = coerce <$> PCore._mnNamespace m
                        }
              toPactEvent e =
                  PactEvent
                    { _eventName = PCore._peName e
                    , _eventParams = catMaybes $ convertPactValue <$> PCore._peArgs e
                    , _eventModule = toModuleName $ PCore._peModule e
                    , _eventModuleHash = coerce $ PCore._peModuleHash e
                    }
            return $ map toPactEvent $ PCore._erEvents er'

          Left err -> do
            TRACE.traceM $ "CORE.redeemGas failed!!" <> sshow err
            fatal $ "redeemGas: Internal error - " <> sshow err
    else do
      GasId gid <- use txGasId >>= \case
        Nothing -> fatal $! "redeemGas: no gas id in scope for gas refunds"
        Just g -> return g
      let redeemGasCmd =
            ContMsg gid 1 False (toLegacyJson $ object [ "fee" A..= toJsonViaEncode fee ]) Nothing

      fmap _crEvents $ locally txQuirkGasFee (const Nothing) $
        applyContinuation 0 coreState redeemGasCmd
          (_pSigners $ _cmdPayload cmd) (toUntypedHash $ _cmdHash cmd)
          managedNamespacePolicy

  where
    initState mc = initStateInterpreter
      $ setModuleCache mc
      $ initCapabilities [magic_GAS]


-- ---------------------------------------------------------------------------- --
-- Utilities

-- | Initialize a fresh eval state with magic capabilities.
-- This is the way we inject the correct guards into the environment
-- during Pact code execution
--
initCapabilities :: [CapSlot SigCapability] -> EvalState
initCapabilities cs = set (evalCapabilities . capStack) cs def
{-# INLINABLE initCapabilities #-}

initCoreCapabilities :: [PCore.CapSlot PCore.QualifiedName PCore.PactValue] -> PCore.EvalState PCore.CoreBuiltin PCore.SpanInfo
initCoreCapabilities cs = set (PCore.esCaps . PCore.csSlots) cs def
{-# INLINABLE initCoreCapabilities #-}

initStateInterpreter :: EvalState -> Interpreter e
initStateInterpreter s = Interpreter (put s >>)

-- | Check whether the cost of running a tx is more than the allowed
-- gas limit and do some action depending on the outcome
--
checkTooBigTx
    :: (Logger logger)
    => Gas
    -> GasLimit
    -> TransactionM logger p (CommandResult [TxLogJson])
    -> (CommandResult [TxLogJson] -> TransactionM logger p (CommandResult [TxLogJson]))
    -> TransactionM logger p (CommandResult [TxLogJson])
checkTooBigTx initialGas gasLimit next onFail
  | initialGas >= fromIntegral gasLimit = do

      let !pe = PactError GasError def []
            $ "Tx too big (" <> pretty initialGas <> "), limit "
            <> pretty gasLimit

      r <- failTxWith pe "Tx too big"
      onFail r
  | otherwise = next

gasInterpreter :: Gas -> TransactionM logger db (Interpreter p)
gasInterpreter g = do
    mc <- use txCache
    logGas <- isJust <$> view txGasLogger
    return $ initStateInterpreter
        $ set evalLogGas (guard logGas >> Just [("GTxSize",g)]) -- enables gas logging
        $ setModuleCache mc def


-- | Initial gas charged for transaction size
--   ignoring the size of a continuation proof, if present
--
initialGasOf :: PayloadWithText -> Gas
initialGasOf payload = gasFee
  where
    feePerByte :: Rational = 0.01

    contProofSize =
      case _pPayload (payloadObj payload) of
        Continuation (ContMsg _ _ _ _ (Just (ContProof p))) -> B.length p
        _ -> 0
    txSize = SB.length (payloadBytes payload) - contProofSize

    costPerByte = fromIntegral txSize * feePerByte
    sizePenalty = txSizeAccelerationFee costPerByte
    gasFee = ceiling (costPerByte + sizePenalty)
{-# INLINE initialGasOf #-}

txSizeAccelerationFee :: Rational -> Rational
txSizeAccelerationFee costPerByte = total
  where
    total = (costPerByte / bytePenalty) ^ power
    bytePenalty = 512
    power :: Integer = 7
{-# INLINE txSizeAccelerationFee #-}

-- | Set the module cache of a pact 'coreState
--
setModuleCache
  :: ModuleCache
  -> EvalState
  -> EvalState
setModuleCache mcache es =
  let allDeps = foldMap (allModuleExports . fst) $ _getModuleCache mcache
  in set (evalRefs . rsQualifiedDeps) allDeps $ set (evalRefs . rsLoadedModules) c es
 where
  c = moduleCacheToHashMap mcache
{-# INLINE setModuleCache #-}

setCoreModuleCache
  :: CoreModuleCache
  -> PCore.EvalState PCore.CoreBuiltin PCore.SpanInfo
  -> PCore.EvalState PCore.CoreBuiltin PCore.SpanInfo
setCoreModuleCache mcache es =
  let allDeps = foldMap PCore.allModuleExports $ _getCoreModuleCache mcache
  in set (PCore.esLoaded . PCore.loAllLoaded) allDeps $ set (PCore.esLoaded . PCore.loModules) c es
 where
  c = _getCoreModuleCache mcache
{-# INLINE setCoreModuleCache #-}

-- | Set tx result state
--
setTxResultState :: EvalResult -> TransactionM logger db ()
setTxResultState er = do
    txLogs <>= _erLogs er
    txCache .= moduleCacheFromHashMap (_erLoadedModules er)
    txGasUsed .= _erGas er
{-# INLINE setTxResultState #-}

unsafeModuleHashFromB64Text :: Text -> ModuleHash
unsafeModuleHashFromB64Text =
  either error ModuleHash . PU.fromText'

mkCoreEvalEnv
    :: NamespacePolicy
    -> MsgData
    -> TransactionM logger db (PCore.EvalEnv PCore.CoreBuiltin PCore.SpanInfo)
mkCoreEvalEnv nsp MsgData{..} = do
    tenv <- ask

    -- TODO: create a module to convert old pactvalues to new ones in chainweb
    let
      convertPactValue pv = aeson (\s -> error $ "mkCoreEvalEnv: failed to parse legacyValue " ++ s) id $ A.fromJSON $ _getLegacyValue pv
      convertQualName QualifiedName{..} = PCore.QualifiedName
        { PCore._qnName = _qnName
        , PCore._qnModName = _qnQual & \ModuleName{..} ->
            PCore.ModuleName
              { PCore._mnName = _mnName
              , PCore._mnNamespace = fmap coerce _mnNamespace
              }
        }
      convertCapability SigCapability{..} =
          PCore.CapToken (convertQualName _scName) (mapMaybe (either (const Nothing) Just . PactConversion.fromLegacyPactValue) _scArgs)

      convertVerifier Verifier{..} = PCore.Verifier
        { PCore._verifierName = coerce _verifierName
        , PCore._verifierProof = _verifierProof
        , PCore._verifierCaps = map convertCapability _verifierCaps
        }

    let
      txMode' = case _txMode tenv of
        Transactional -> PCore.Transactional
        Local -> PCore.Local

    let
      coreMsg = PCore.MsgData
        { PCore.mdData = either (const $ PCore.PObject mempty) id $ PactConversion.fromLegacyPactValue $ convertPactValue mdData
        , PCore.mdStep = mdStep <&> \PactStep{..} ->
            PCore.DefPactStep
              { PCore._psStep = _psStep
              , PCore._psRollback = _psRollback
              , PCore._psDefPactId = coerce _psPactId
              , PCore._psResume = _psResume <&> \Yield{..} ->
                  PCore.Yield
                    { PCore._yData = M.fromList $ mapMaybe (\(k, v) -> fmap (coerce k,) $ either (const Nothing) Just $ PactConversion.fromLegacyPactValue v) $ M.toList $ _objectMap _yData
                    , PCore._yProvenance = _yProvenance <&> \Provenance{..} ->
                        PCore.Provenance
                          { PCore._pTargetChainId = coerce _pTargetChainId
                          , PCore._pModuleHash = let (ModuleHash h) = _pModuleHash in PCore.ModuleHash $ coerce h
                          }
                    , PCore._ySourceChain = coerce _ySourceChain
                    }
              }
        , PCore.mdHash = coerce $ mdHash
        , PCore.mdSigners = mdSigners <&> \Signer{..} ->
            PCore.Signer
              { PCore._siScheme = _siScheme <&> \case
                  ED25519 -> PCore.ED25519
                  WebAuthn -> PCore.WebAuthn
              , PCore._siPubKey = _siPubKey
              , PCore._siAddress = _siAddress
              , PCore._siCapList = map convertCapability _siCapList
              }
        , PCore.mdVerifiers = map convertVerifier mdVerifiers
        }

    let
      coreNsp = case nsp of
        SimpleNamespacePolicy _ -> PCore.SimpleNamespacePolicy
        SmartNamespacePolicy rootUsage name -> PCore.SmartNamespacePolicy rootUsage (convertQualName name)

    let
      pd = _txPublicData tenv
      convertPublicMeta pm = PCore.PublicMeta
        { PCore._pmChainId = coerce $ _pmChainId pm
        , PCore._pmSender = _pmSender pm
        , PCore._pmGasLimit =
            let (GasLimit (ParsedInteger g)) = _pmGasLimit pm in PCore.Gas $ fromIntegral g
        , PCore._pmGasPrice = coerce $ _pmGasPrice pm
        , PCore._pmTTL =
            let (TTLSeconds (ParsedInteger s)) = _pmTTL pm in PCore.TTLSeconds s
        , PCore._pmCreationTime = coerce $ _pmCreationTime pm
        }
      cpd = PCore.PublicData
        { PCore._pdPublicMeta = convertPublicMeta $ _pdPublicMeta pd
        , PCore._pdBlockHeight = _pdBlockHeight pd
        , PCore._pdBlockTime = _pdBlockTime pd
        , PCore._pdPrevBlockHash = _pdPrevBlockHash pd
        }

    gasModel <- use txGasModel
    let
      toCoreExFlag = \case
        FlagDisableModuleInstall -> Just PCore.FlagDisableModuleInstall
        FlagDisableHistoryInTransactionalMode -> Just PCore.FlagDisableHistoryInTransactionalMode
        FlagAllowReadInLocal -> Just PCore.FlagAllowReadInLocal
        FlagDisablePactEvents -> Just PCore.FlagDisablePactEvents
        FlagEnforceKeyFormats -> Just PCore.FlagEnforceKeyFormats
        _ -> Nothing
      executionFlags = mapMaybe toCoreExFlag $ S.toList $ _ecFlags $ _txExecutionConfig tenv
    liftIO $ PCore.setupEvalEnv (either (error "impossible") id $ _txDbEnv tenv) txMode' coreMsg (either (error "impossible") id gasModel) coreNsp PCore.noSPVSupport cpd (S.fromList executionFlags)

-- | Managed namespace policy CAF
--
managedNamespacePolicy :: NamespacePolicy
managedNamespacePolicy = SmartNamespacePolicy False
  (QualifiedName (ModuleName "ns" Nothing) "validate" def)
{-# NOINLINE managedNamespacePolicy #-}

-- | Builder for "magic" capabilities given a magic cap name
--
mkMagicCapSlot :: Text -> CapSlot SigCapability
mkMagicCapSlot c = CapSlot CapCallStack (mkCoinCap c []) []
{-# INLINE mkMagicCapSlot #-}

mkCoinCap :: Text -> [PactValue] -> SigCapability
mkCoinCap c as = SigCapability fqn as
  where
    mn = ModuleName "coin" Nothing
    fqn = QualifiedName mn c def
{-# INLINE mkCoinCap #-}

mkMagicCoreCapSlot :: Text -> PCore.CapSlot PCore.QualifiedName PCore.PactValue
mkMagicCoreCapSlot c = PCore.CapSlot cap []
  where
    mn = PCore.ModuleName "coin" Nothing
    fqn = PCore.QualifiedName c mn
    cap = PCore.CapToken fqn []
{-# INLINE mkMagicCoreCapSlot #-}

-- | Build the 'ExecMsg' for some pact code fed to the function. The 'value'
-- parameter is for any possible environmental data that needs to go into
-- the 'ExecMsg'.
--
buildExecParsedCode
    :: PactParserVersion
    -> Maybe Value
    -> Text
    -> IO (ExecMsg ParsedCode)
buildExecParsedCode ppv value code = maybe (go Null) go value
  where
    go val = case parsePact ppv code of
      Right !t -> pure $! ExecMsg t (toLegacyJson val)
      -- if we can't construct coin contract calls, this should
      -- fail fast
      Left err -> internalError $ "buildExecParsedCode: parse failed: " <> T.pack err

-- | Retrieve public metadata from a command
--
publicMetaOf :: Command (Payload PublicMeta ParsedCode) -> PublicMeta
publicMetaOf = _pMeta . _cmdPayload
{-# INLINE publicMetaOf #-}

-- | Retrieve the optional Network identifier from a command
--
networkIdOf :: Command (Payload PublicMeta ParsedCode) -> Maybe NetworkId
networkIdOf = _pNetworkId . _cmdPayload
{-# INLINE networkIdOf #-}

-- | Calculate the gas fee (pact-generate gas cost * user-specified gas price),
-- rounding to the nearest stu.
--
gasSupplyOf :: Gas -> GasPrice -> GasSupply
gasSupplyOf gas (GasPrice (ParsedDecimal gp)) = GasSupply (ParsedDecimal gs)
  where
    gs = toCoinUnit ((fromIntegral gas) * gp)
{-# INLINE gasSupplyOf #-}

-- | Round to the nearest Stu
--
toCoinUnit :: Decimal -> Decimal
toCoinUnit = roundTo 12
{-# INLINE toCoinUnit #-}

gasLog :: (Logger logger) => Text -> TransactionM logger db ()
gasLog m = do
  l <- view txGasLogger
  rk <- view txRequestKey
  for_ l $ \logger ->
    logInfo_ logger $ m <> ": " <> sshow rk

-- | Log request keys at DEBUG when successful
--
debug :: (Logger logger) => Text -> TransactionM logger db ()
debug s = do
    l <- view txLogger
    rk <- view txRequestKey
    logDebug_ l $ s <> ": " <> sshow rk

-- | Denotes fatal failure points in the tx exec process
--
fatal :: (Logger logger) => Text -> TransactionM logger db a
fatal e = do
    l <- view txLogger
    rk <- view txRequestKey

    logError_ l
      $ "critical transaction failure: "
      <> sshow rk <> ": " <> e

    throwM $ PactTransactionExecError (fromUntypedHash $ unRequestKey rk) e

logError :: (Logger logger) => Text -> TransactionM logger db ()
logError msg = view txLogger >>= \l -> logError_ l msg

infoLog :: (Logger logger) => Text -> TransactionM logger db ()
infoLog msg = view txLogger >>= \l -> logInfo_ l msg
