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
module Chainweb.Pact.TransactionExec
( -- * Transaction Execution
  applyCmd
, applyGenesisCmd
, applyLocal
, applyExec
, applyExec'
, applyExecTng
, applyExecTng'
, applyContinuation
, applyContinuation'
, applyContinuationTng
, applyContinuationTng'
, runPayload
, readInitModules
, readInitModulesCore
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
import qualified Pact.Core.Serialise.LegacyPact as PCore
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
import Chainweb.Pact.Templates
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
        coreCache = _txCoreCache st
        warns = _txWarnings st

    pure $ T4 cr cache coreCache warns
  where
    stGasModel
      | chainweb217Pact' = gasModel
      | otherwise = _geGasModel freeGasEnv

    stGasModelCore
      | chainweb217Pact' = gasModelCore
      | otherwise = PCore.freeGasModel
    txst = TransactionState mcache0 mccache0 mempty 0 Nothing stGasModel stGasModelCore mempty
    quirkGasFee = v ^? versionQuirks . quirkGasFees . ix requestKey

    executionConfigNoHistory = ExecutionConfig
      $ S.singleton FlagDisableHistoryInTransactionalMode
      <> S.fromList
        ([ FlagOldReadOnlyBehavior | isPactBackCompatV16 ]
        ++ [ FlagPreserveModuleNameBug | not isModuleNameFix ]
        ++ [ FlagPreserveNsModuleInstallBug | not isModuleNameFix2 ])
      <> flagsFor v (ctxChainId txCtx) (ctxCurrentBlockHeight txCtx)

    cenv = TransactionEnv Transactional pdbenv coreDb logger gasLogger (ctxToPublicData txCtx) spv nid gasPrice
      requestKey (fromIntegral gasLimit) executionConfigNoHistory quirkGasFee usePactTng

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
    usePactTng = chainweb222Pact v cid currHeight
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
      txGasModel .= gasModel
      txGasModelCore .= gasModelCore
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
      txGasModel .= _geGasModel freeGasEnv
      txGasModelCore .= PCore.freeGasModel

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

applyGenesisCmd
    :: (Logger logger)
    => logger
      -- ^ Pact logger
    -> (PactDbEnv p, CoreDb)
      -- ^ Pact db environment
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> TxContext
      -- ^ tx metadata
    -> Command (Payload PublicMeta ParsedCode)
      -- ^ command with payload to execute
    -> IO (T2 (CommandResult [TxLogJson]) (ModuleCache, CoreModuleCache))
applyGenesisCmd logger (dbEnv, coreDb) spv txCtx cmd =
    second (\s -> (_txCache s, _txCoreCache s)) <$!> runTransactionM tenv txst go
  where
    nid = networkIdOf cmd
    rk = cmdToRequestKey cmd
    tenv = TransactionEnv
        { _txMode = Transactional
        , _txDbEnv = dbEnv
        , _txCoreDb = coreDb
        , _txLogger = logger
        , _txGasLogger = Nothing
        , _txPublicData = def
        , _txSpvSupport = spv
        , _txNetworkId = nid
        , _txGasPrice = 0.0
        , _txRequestKey = rk
        , _txGasLimit = 0
        , _txExecutionConfig = ExecutionConfig
          $ flagsFor (ctxVersion txCtx) (ctxChainId txCtx) (_blockHeight $ ctxBlockHeader txCtx)
          -- TODO this is very ugly. Genesis blocks need to install keysets
          -- outside of namespaces so we need to disable Pact 4.4. It would be
          -- preferable to have a flag specifically for the namespaced keyset
          -- stuff so that we retain this power in genesis and upgrade txs even
          -- after the block height where pact4.4 is on.
          <> S.fromList [ FlagDisableInlineMemCheck, FlagDisablePact44 ]
        , _txQuirkGasFee = Nothing
        , _txUsePactTng = False
        }
    txst = TransactionState
        { _txCache = mempty
        , _txCoreCache = mempty
        , _txLogs = mempty
        , _txGasUsed = 0
        , _txGasId = Nothing
        , _txGasModel = _geGasModel freeGasEnv
        , _txGasModelCore = PCore.freeGasModel
        , _txWarnings = mempty
        }

    interp = initStateInterpreter
      $ initCapabilities [magic_GENESIS, magic_COINBASE]
    -- coreState = initCoreCapabilities [core_magic_GENESIS, core_magic_COINBASE]

    go = do
      -- TODO: fix with version recordification so that this matches the flags at genesis heights.
      cr <- catchesPactError logger (onChainErrorPrintingFor txCtx) $! runGenesis cmd permissiveNamespacePolicy interp
      case cr of
        Left e -> fatal $ "Genesis command failed: " <> sshow e
        Right r -> r <$ debug "successful genesis tx for request key"

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
        coinbaseTerm = mkCoinbaseCoreTerm mid
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
    usePactTng = chainweb222Pact v (ctxChainId txCtx) bh
    tenv = TransactionEnv Transactional dbEnv coreDb logger Nothing (ctxToPublicData txCtx) noSPVSupport
           Nothing 0.0 rk 0 ec Nothing usePactTng
    txst = TransactionState mc cmc mempty 0 Nothing (_geGasModel freeGasEnv) (PCore.freeGasModel) mempty
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
        Just coinbaseTerm | usePactTng -> do
          -- coreState <-
          --   if (not $ (PCore.ModuleName "core" Nothing) `S.member` (M.keysSet $ _getCoreModuleCache cmc)) then do
          --     cmc' <- liftIO (readInitModulesCore logger (dbEnv, coreDb) txCtx)
          --     pure $ setCoreModuleCache cmc' evState
          --   else pure evState

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

              txCoreCache .= (CoreModuleCache (PCore._erLoadedModules er))

              coreCr <- mkCommandResultFromCoreResult er

              return $! T2
                coreCr
                upgradedModuleCache

            Left e
              | throwCritical -> throwM $ CoinbaseFailure $ sshow e
              | otherwise -> (`T2` Nothing) <$> failTxWith (PactError EvalError (Info Nothing) [] mempty) "coinbase tx failure"
        _ -> do
          cr <- catchesPactError logger (onChainErrorPrintingFor txCtx) $
            applyExec' 0 interp cexec [] [] chash managedNamespacePolicy

          -- liftIO $ print cr
          case cr of
            Left e
              | throwCritical -> throwM $ CoinbaseFailure $ sshow e
              | otherwise -> (`T2` Nothing) <$> failTxWith e "coinbase tx failure"
            Right er -> do
              debug
                $! "successful coinbase of "
                <> T.take 18 (sshow d)
                <> " to "
                <> sshow mid

              upgradedModuleCache <- applyUpgrades v cid bh

              logs <- use txLogs

              return $! T2
                CommandResult
                  { _crReqKey = rk
                  , _crTxId = _erTxId er
                  , _crResult = PactResult (Right (last (_erOutput er)))
                  , _crGas = _erGas er
                  , _crLogs = Just logs
                  , _crContinuation = _erExec er
                  , _crMetaData = Nothing
                  , _crEvents = _erEvents er
                  }
                upgradedModuleCache

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
    tenv = TransactionEnv Local dbEnv coreDb logger gasLogger (ctxToPublicData txCtx) spv nid gasPrice
           rk (fromIntegral gasLimit) execConfig Nothing usePactTng
    txst = TransactionState mc cmc mempty 0 Nothing gasModel gasModelCore mempty
    gas0 = initialGasOf (_cmdPayload cmdIn)
    currHeight = ctxCurrentBlockHeight txCtx
    cid = V._chainId txCtx
    v = _chainwebVersion txCtx
    allVerifiers = verifiersAt v cid currHeight
    usePactTng = chainweb222Pact (ctxVersion txCtx) (ctxChainId txCtx) currHeight
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
      coreState <- do
          cmc' <- use txCoreCache
          pure $ setCoreModuleCache cmc' def
      cr <- catchesPactError logger PrintsUnexpectedError $! case m of
        Exec em -> do
          if usePactTng then applyExecTng gas1 coreState em signers chash managedNamespacePolicy
          else applyExec gas1 interp em signers verifiersWithNoProof chash managedNamespacePolicy
        Continuation cm ->
          if usePactTng then applyContinuationTng gas1 coreState cm signers chash managedNamespacePolicy
          else applyContinuation gas1 interp cm signers chash managedNamespacePolicy


      case cr of
        Left e -> failTxWith e "applyLocal"
        Right r -> return $! r { _crMetaData = Just (J.toJsonViaEncode $ ctxToPublicData' txCtx) }

    go = checkTooBigTx gas0 gasLimit (applyVerifiers $ _pPayload $ _cmdPayload cmd) return

readInitModulesCore
    -- :: forall logger tbl. (Logger logger)
    -- => PactBlockM logger tbl CoreModuleCache
   :: forall logger p. (Logger logger)
    => logger
      -- ^ Pact logger
    -> (PactDbEnv p, CoreDb)
      -- ^ Pact db environment
    -> TxContext
      -- ^ tx metadata and parent header
    -> IO CoreModuleCache
readInitModulesCore logger (dbEnv, coreDb) txCtx = do
  -- logger <- view (psServiceEnv . psLogger)
  -- dbEnv <- _cpPactDbEnv <$> view psBlockDbEnv
  -- coreDb <- _cpPactCoreDbEnv <$> view psBlockDbEnv
  -- txCtx <- getTxContext def

  -- let chainweb217Pact' = guardCtx chainweb217Pact txCtx
  -- let chainweb224Pact' = guardCtx chainweb224Pact txCtx

  -- let usePactTng = True
  -- let emptyTxEnv =
  --       TransactionEnv
  --         { _txMode = Local
  --         , _txDbEnv = dbEnv
  --         , _txCoreDb = coreDb
  --         , _txLogger = logger
  --         , _txGasLogger = Nothing
  --         , _txPublicData = ctxToPublicData txCtx
  --         , _txSpvSupport = noSPVSupport
  --         , _txNetworkId = Nothing
  --         , _txGasPrice = 0.0
  --         , _txRequestKey = RequestKey pactInitialHash
  --         , _txGasLimit = 0
  --         , _txExecutionConfig = def
  --         , _txQuirkGasFee = Nothing
  --         , _txUsePactTng = usePactTng
  --         }
  -- let emptyTxState =
  --       TransactionState
  --         { _txCache = mempty
  --         , _txCoreCache = mempty
  --         , _txLogs = []
  --         , _txGasUsed = 0
  --         , _txGasId = Nothing
  --         , _txGasModel = _geGasModel freeGasEnv
  --         , _txGasModelCore = PCore.freeGasModel
  --         , _txWarnings = mempty
  --         }
  -- let die msg = throwM $ PactInternalError $ "readInitModules: " <> msg
  -- let mkCmd = buildExecParsedCode (pactParserVersion (ctxVersion txCtx) (ctxChainId txCtx) (_blockHeight (_parentHeader (_tcParentHeader txCtx)) + 1)) Nothing
  -- let run msg cmd = do
  --       er <- catchesPactError logger (onChainErrorPrintingFor txCtx) $! do
  --         applyExec' 0 defaultInterpreter cmd [] [] pactInitialHash permissiveNamespacePolicy
  --       case er of
  --         Left e -> die $ msg <> ": failed: " <> sshow e
  --         Right r -> case _erOutput r of
  --           [] -> die $ msg <> ": empty result"
  --           (o:_) -> return o
  let
    chash = pactInitialHash
    usePactTng = True
    tenv = TransactionEnv Local dbEnv coreDb logger Nothing (ctxToPublicData txCtx) noSPVSupport Nothing 0.0
           (RequestKey chash) 0 def Nothing usePactTng
    txst = TransactionState mempty mempty mempty 0 Nothing (_geGasModel freeGasEnv) PCore.freeGasModel mempty
    coinCoreModuleName = PCore.ModuleName "coin" Nothing
    installCoreCoinModuleAdmin = set (PCore.esCaps . PCore.csModuleAdmin) $ S.singleton coinCoreModuleName
    coreState = installCoreCoinModuleAdmin $ initCoreCapabilities [mkMagicCoreCapSlot "REMEDIATE"]
    applyTx tx = do
      coreCache <- use txCoreCache
      let evState = setCoreModuleCache coreCache coreState
      infoLog $ "readInitModulesCore. Running upgrade tx " <> sshow (_cmdHash tx)
      tryAllSynchronous (runGenesisCore tx permissiveNamespacePolicy evState) >>= \case
        Right _ -> pure ()
        Left e -> do
          logError $ "readInitModulesCore. Upgrade transaction failed! " <> sshow e
          throwM e

  evalTransactionM tenv txst $ do
    let payloads = map (fmap payloadObj) (CoinCoreV4.transactions)
    er <- catchesPactError logger (onChainErrorPrintingFor txCtx) $!
      mapM applyTx payloads
    case er of
      Left e -> throwM $ PactInternalError $ "readInitModulesCore: load modules: failed: " <> sshow e
      Right _ -> use txCoreCache

  --   -- Only load coin and its dependencies for chainweb >=2.17
  --   -- Note: no need to check if things are there, because this
  --   -- requires a block height that witnesses the invariant.
  --   --
  --   -- if this changes, we must change the filter in 'updateInitCache'
  -- let goCw217 :: TransactionM logger p CoreModuleCache
  --     goCw217 = do
  --       coinDepCmd <- liftIO $ mkCmd "coin.MINIMUM_PRECISION"
  --       void $ run "load modules" coinDepCmd
  --       use txCoreCache

  -- if | chainweb224Pact' -> pure mempty
  --    | chainweb217Pact' -> liftIO $ evalTransactionM emptyTxEnv emptyTxState goCw217
  --    | otherwise -> throwM $ PactInternalError $ "readInitModulesCore call prior Chainweb 2.17"

readInitModules
    :: forall logger tbl. (Logger logger)
    => PactBlockM logger tbl (ModuleCache, CoreModuleCache)
readInitModules = do
  logger <- view (psServiceEnv . psLogger)
  dbEnv <- _cpPactDbEnv <$> view psBlockDbEnv
  coreDb <- _cpPactCoreDbEnv <$> view psBlockDbEnv
  txCtx <- getTxContext def

  -- guarding chainweb 2.17 here to allow for
  -- cache purging everything but coin and its
  -- dependencies.
  let
    chainweb217Pact' = guardCtx chainweb217Pact txCtx
    chainweb224Pact' = guardCtx chainweb224Pact txCtx

    parent = _tcParentHeader txCtx
    v = ctxVersion txCtx
    cid = ctxChainId txCtx
    h = _blockHeight (_parentHeader parent) + 1
    rk = RequestKey chash
    nid = Nothing
    chash = pactInitialHash
    usePactTng = True
    tenv = TransactionEnv Local dbEnv coreDb logger Nothing (ctxToPublicData txCtx) noSPVSupport nid 0.0
           rk 0 def Nothing usePactTng
    txst = TransactionState mempty mempty mempty 0 Nothing (_geGasModel freeGasEnv) PCore.freeGasModel mempty
    interp = defaultInterpreter
    die msg = throwM $ PactInternalError $ "readInitModules: " <> msg
    mkCmd = buildExecParsedCode (pactParserVersion v cid h) Nothing
    run msg cmd = do
      er <- catchesPactError logger (onChainErrorPrintingFor txCtx) $! do
        applyExec' 0 interp cmd [] [] chash permissiveNamespacePolicy
      case er of
        Left e -> die $ msg <> ": failed: " <> sshow e
        Right r -> case _erOutput r of
          [] -> die $ msg <> ": empty result"
          (o:_) -> return o

    go :: TransactionM logger p (ModuleCache, CoreModuleCache)
    go = do
      -- see if fungible-v2 is there
      checkCmd <- liftIO $ mkCmd "(contains \"fungible-v2\" (list-modules))"
      checkFv2 <- run "check fungible-v2" checkCmd
      hasFv2 <- case checkFv2 of
        (PLiteral (LBool b)) -> return b
        t -> die $ "got non-bool result from module read: " <> T.pack (showPretty t)

      -- see if fungible-xchain-v1 is there
      checkCmdx <- liftIO $ mkCmd "(contains \"fungible-xchain-v1\" (list-modules))"
      checkFx <- run "check fungible-xchain-v1" checkCmdx
      hasFx <- case checkFx of
        (PLiteral (LBool b)) -> return b
        t -> die $ "got non-bool result from module read: " <> T.pack (showPretty t)

      -- load modules by referencing members
      refModsCmd <- liftIO $ mkCmd $ T.intercalate " " $
        [ "coin.MINIMUM_PRECISION"
        , "ns.GUARD_SUCCESS"
        , "(use gas-payer-v1)"
        , "fungible-v1.account-details"] ++
        [ "fungible-v2.account-details" | hasFv2 ] ++
        [ "(let ((m:module{fungible-xchain-v1} coin)) 1)" | hasFx ]
      void $ run "load modules" refModsCmd

      -- return loaded cache
      c <- use txCache
      cc <- use txCoreCache
      pure (c, cc)

    -- Only load coin and its dependencies for chainweb >=2.17
    -- Note: no need to check if things are there, because this
    -- requires a block height that witnesses the invariant.
    --
    -- if this changes, we must change the filter in 'updateInitCache'
    goCw217 :: TransactionM logger p (ModuleCache, CoreModuleCache)
    goCw217 = do
      coinDepCmd <- liftIO $ mkCmd "coin.MINIMUM_PRECISION"
      void $ run "load modules" coinDepCmd
      c <- use txCache
      cc <- use txCoreCache
      pure (c, cc)

  if | chainweb224Pact' -> pure mempty
     | chainweb217Pact' -> liftIO $ evalTransactionM tenv txst goCw217
     | otherwise -> liftIO $ evalTransactionM tenv txst go

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
    -- installCoreCoinModuleAdmin = set (PCore.esCaps . PCore.csModuleAdmin) $ S.singleton coinCoreModuleName

    filterModuleCache = do
      mc <- use txCache
      cmc <- use txCoreCache
      pure $ Just $
        ( filterModuleCacheByKey (== coinModuleName) mc
        , filterCoreModuleCacheByKey (== coinCoreModuleName) cmc
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

    interp = initStateInterpreter
        $ installCoinModuleAdmin
        $ initCapabilities [mkMagicCapSlot "REMEDIATE"]
    -- coreInitState = installCoreCoinModuleAdmin $ initCoreCapabilities [mkMagicCoreCapSlot "REMEDIATE"]

    applyTx tx = do
      infoLog $ "Running upgrade tx " <> sshow (_cmdHash tx)
      tryAllSynchronous (runGenesis tx permissiveNamespacePolicy interp) >>= \case
        Right _ -> do
          c <- use txCache
          cc <- use txCoreCache
          pure (c, cc)
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
      (TxFailureLog rk err msg)

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
    coreState <- do
        cmc <- use txCoreCache
        pure $ setCoreModuleCache cmc def

    -- Note [Throw out verifier proofs eagerly]
    let !verifiersWithNoProof =
            (fmap . fmap) (\_ -> ()) verifiers
            `using` (traverse . traverse) rseq

    usePactTng <- view txUsePactTng
    case payload of
      Exec pm -> do
        if usePactTng then applyExecTng g0 coreState pm signers chash nsp
        else applyExec g0 interp pm signers verifiersWithNoProof chash nsp
      Continuation ym ->
        if usePactTng then applyContinuationTng g0 coreState ym signers chash nsp
        else applyContinuation g0 interp ym signers chash nsp


  where
    verifiers = fromMaybe [] $ _pVerifiers $ _cmdPayload cmd
    signers = _pSigners $ _cmdPayload cmd
    chash = toUntypedHash $ _cmdHash cmd
    payload = _pPayload $ _cmdPayload cmd

-- | Run genesis transaction payloads with custom interpreter
--
runGenesis
    :: (Logger logger)
    => Command (Payload PublicMeta ParsedCode)
    -> NamespacePolicy
    -> Interpreter p
    -> TransactionM logger p (CommandResult [TxLogJson])
runGenesis cmd nsp interp = case payload of
    Exec pm ->
      applyExec 0 interp pm signers verifiersWithNoProof chash nsp
    Continuation ym ->
      applyContinuation 0 interp ym signers chash nsp
  where
    signers = _pSigners $ _cmdPayload cmd
    verifiers = fromMaybe [] $ _pVerifiers $ _cmdPayload cmd
    -- Note [Throw out verifier proofs eagerly]
    !verifiersWithNoProof =
        (fmap . fmap) (\_ -> ()) verifiers
        `using` (traverse . traverse) rseq
    chash = toUntypedHash $ _cmdHash cmd
    payload = _pPayload $ _cmdPayload cmd

runGenesisCore
    :: (Logger logger)
    => Command (Payload PublicMeta ParsedCode)
    -> NamespacePolicy
    -> PCore.EvalState PCore.CoreBuiltin PCore.SpanInfo
    -> TransactionM logger p ()
runGenesisCore cmd nsp coreState = case payload of
    Exec pm -> void $ applyExecTng' 0 coreState pm signers chash nsp
    Continuation _ -> error "runGenesisCore Continuation not supported"
  where
    signers = _pSigners $ _cmdPayload cmd
    chash = toUntypedHash $ _cmdHash cmd
    payload = _pPayload $ _cmdPayload cmd

-- | Execute an 'ExecMsg' and Return the result with module cache
--
applyExec
    :: (Logger logger)
    => Gas
    -> Interpreter p
    -> ExecMsg ParsedCode
    -> [Signer]
    -> [Verifier ()]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p (CommandResult [TxLogJson])
applyExec initialGas interp em senderSigs verifiers hsh nsp = do
    EvalResult{..} <- applyExec' initialGas interp em senderSigs verifiers hsh nsp
    for_ _erLogGas $ \gl -> gasLog $ "gas logs: " <> sshow gl
    !logs <- use txLogs
    !rk <- view txRequestKey

    -- concat tx warnings with eval warnings
    modify' $ txWarnings <>~ _erWarnings

    -- applyExec enforces non-empty expression set so `last` ok
    -- forcing it here for lazy errors. TODO NFData the Pacts
    let !lastResult = force $ last _erOutput
    return $ CommandResult rk _erTxId (PactResult (Right lastResult))
      _erGas (Just logs) _erExec Nothing _erEvents

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

applyExecTng
    :: (Logger logger)
    => Gas
    -> PCore.EvalState PCore.CoreBuiltin PCore.SpanInfo
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p (CommandResult [TxLogJson])
applyExecTng initialGas coreState em senderSigs hsh nsp = do
    er <- applyExecTng' initialGas coreState em senderSigs hsh nsp

    mkCommandResultFromCoreResult er

-- | Variation on 'applyExec' that returns 'EvalResult' as opposed to
-- wrapping it up in a JSON result.
--
applyExec'
    :: (Logger logger)
    => Gas
    -> Interpreter p
    -> ExecMsg ParsedCode
    -> [Signer]
    -> [Verifier ()]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p EvalResult
applyExec' initialGas interp (ExecMsg parsedCode execData) senderSigs verifiersWithNoProof hsh nsp
    | null (_pcExps parsedCode) = throwCmdEx "No expressions found"
    | otherwise = do

      eenv <- mkEvalEnv nsp (MsgData execData Nothing hsh senderSigs verifiersWithNoProof)

      setEnvGas initialGas eenv

      evalResult <- liftIO $! evalExec interp eenv parsedCode
      -- if we specified this transaction's gas fee manually as a "quirk",
      -- here we set the result's gas fee to agree with that
      quirkGasFee <- view txQuirkGasFee
      let quirkedEvalResult = case quirkGasFee of
            Nothing -> evalResult
            Just fee -> evalResult { _erGas = fee }

      for_ (_erExec quirkedEvalResult) $ \pe -> debug
        $ "applyExec: new pact added: "
        <> sshow (_pePactId pe, _peStep pe, _peYield pe, _peExecuted pe)

      -- set log + cache updates + used gas
      setTxResultState quirkedEvalResult

      return quirkedEvalResult

applyExecTng'
    :: (Logger logger)
    => Gas
    -> PCore.EvalState PCore.CoreBuiltin PCore.SpanInfo
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p (PCore.EvalResult [PCore.TopLevel PCore.SpanInfo])
applyExecTng' (Gas initialGas) coreState (ExecMsg parsedCode execData) senderSigs hsh nsp
    | null (_pcExps parsedCode) = throwCmdEx "No expressions found"
    | otherwise = do

      evalEnv <- mkCoreEvalEnv nsp (MsgData execData Nothing hsh senderSigs [])

      setEnvGasCore (PCore.Gas $ fromIntegral initialGas) evalEnv

      ccache <- use txCoreCache


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

          txCoreCache .= (CoreModuleCache $ PCore._erLoadedModules er')
          return quirkedEvalResult
        Left err -> do
          TRACE.traceShowM ("CORE.applyExec' modulecache" :: String, show $ _getCoreModuleCache ccache)

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

-- | Execute a 'ContMsg' and return the command result and module cache
--
applyContinuation
    :: (Logger logger)
    => Gas
    -> Interpreter p
    -> ContMsg
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p (CommandResult [TxLogJson])
applyContinuation initialGas interp cm senderSigs hsh nsp = do
    EvalResult{..} <- applyContinuation' initialGas interp cm senderSigs hsh nsp
    for_ _erLogGas $ \gl -> gasLog $ "gas logs: " <> sshow gl
    logs <- use txLogs
    rk <- view txRequestKey

    -- set tx warnings to eval warnings
    txWarnings <>= _erWarnings

    -- last safe here because cont msg is guaranteed one exp
    return $! CommandResult rk _erTxId (PactResult (Right (last _erOutput)))
      _erGas (Just logs) _erExec Nothing _erEvents

applyContinuationTng
    :: (Logger logger)
    => Gas
    -> PCore.EvalState PCore.CoreBuiltin PCore.SpanInfo
    -> ContMsg
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p (CommandResult [TxLogJson])
applyContinuationTng initialGas coreState cm senderSigs hsh nsp = do
    er <- applyContinuationTng' initialGas coreState cm senderSigs hsh nsp
    cr <- mkCommandResultFromCoreResult er
    -- for_ _erLogGas $ \gl -> gasLog $ "gas logs: " <> sshow gl

    -- TODO: set tx warnings to eval warnings
    -- txWarnings <>= _erWarnings

    return cr

setEnvGas :: Gas -> EvalEnv e -> TransactionM logger p ()
setEnvGas initialGas = liftIO . views eeGas (`writeIORef` gasToMilliGas initialGas)

setEnvGasCore :: PCore.Gas -> PCore.EvalEnv PCore.CoreBuiltin PCore.SpanInfo -> TransactionM logger p ()
setEnvGasCore initialGas = liftIO . views PCore.eeGasRef (`writeIORef` PCore.gasToMilliGas initialGas)

-- | Execute a 'ContMsg' and return just eval result, not wrapped in a
-- 'CommandResult' wrapper
--
applyContinuation'
    :: Gas
    -> Interpreter p
    -> ContMsg
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p EvalResult
applyContinuation' initialGas interp cm@(ContMsg pid s rb d _) senderSigs hsh nsp = do

    eenv <- mkEvalEnv nsp (MsgData d pactStep hsh senderSigs [])

    setEnvGas initialGas eenv

    evalResult <- liftIO $! evalContinuation interp eenv cm
    -- if we specified this transaction's gas fee manually as a "quirk",
    -- here we set the result's gas fee to agree with that
    quirkGasFee <- view txQuirkGasFee
    let quirkedEvalResult = case quirkGasFee of
          Nothing -> evalResult
          Just fee -> evalResult { _erGas = fee }

    setTxResultState quirkedEvalResult

    return quirkedEvalResult
  where
    pactStep = Just $ PactStep s rb pid Nothing

applyContinuationTng'
    :: (Logger logger)
    => Gas
    -> PCore.EvalState PCore.CoreBuiltin PCore.SpanInfo
    -> ContMsg
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p (PCore.EvalResult [PCore.TopLevel PCore.SpanInfo])
applyContinuationTng' initialGas coreState (ContMsg pid s rb d proof) senderSigs hsh nsp = do

    evalEnv <- mkCoreEvalEnv nsp (MsgData d pactStep hsh senderSigs [])

    setEnvGasCore (PCore.Gas $ fromIntegral initialGas) evalEnv

--     Pact4.PactValue -> PCore.PactValue
    let
      convertPactValue :: LegacyValue -> Either String PCore.PactValue
      convertPactValue pv = PCore.fromLegacyPactValue $
          maybe (error "applyContinuationTng': failed to parseJSON pact value") id $ J.decode $ J.encode pv

      coreCm = PCore.ContMsg
          { PCore._cmPactId = coerce pid
          , PCore._cmStep = s
          , PCore._cmRollback = rb
          , PCore._cmData = either (error "applyContinuationTng': failed to convert pact value") id $ convertPactValue  d
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

        txCoreCache .= (CoreModuleCache $ PCore._erLoadedModules er')
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
      cmcache <- use txCoreCache
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
          coreState = setCoreModuleCache cmcache $ initCoreCapabilities [core_magic_GAS]

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

      -- no verifiers are allowed in buy gas
      -- quirked gas is not used either
      result <- locally txQuirkGasFee (const Nothing) $
        applyExec' 0 (interp mcache) buyGasCmd signersWithDebit [] bgHash managedNamespacePolicy

      usePactTng <- view txUsePactTng
      if usePactTng then do
        evalEnv <- mkCoreEvalEnv managedNamespacePolicy (MsgData execData Nothing bgHash signersWithDebit [])

        let
          t = if isChainweb224Pact
                then mkBuyGasCoreTerm sender
                else mkFundTxCoreTerm mid sender

        er <- liftIO $ PCore.evalTermExec evalEnv coreState t
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
                  txCoreCache .= (CoreModuleCache $ PCore._erLoadedModules er')
          Left err -> do
            TRACE.traceM $ "CORE.buyGas failed!!" <> sshow err <> "\n" <> sshow t
            fatal $ "buyGas: Internal error - " <> sshow err
      else do
        -- no verifiers are allowed in buy gas
        -- quirked gas is not used either
        result <- locally txQuirkGasFee (const Nothing) $
          applyExec' 0 (interp mcache) buyGasCmd
            (_pSigners $ _cmdPayload cmd) [] bgHash managedNamespacePolicy

        case _erExec result of
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
            | otherwise ->
              void $! txGasId .= (Just $! GasId (_pePactId pe))

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
    cmcache <- use txCoreCache
    let sender = view (cmdPayload . pMeta . pmSender) cmd
    fee <- gasSupplyOf <$> use txGasUsed <*> view txGasPrice
    -- if we're past chainweb 2.24, we don't use defpacts for gas
    usePactTng <- view txUsePactTng

    let
      coreState = setCoreModuleCache cmcache $ initCoreCapabilities [core_magic_GAS]

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
      locally txQuirkGasFee (const Nothing) $
        if usePactTng then do
          evalEnv <- mkCoreEvalEnv managedNamespacePolicy (MsgData execData Nothing rgHash (_pSigners $ _cmdPayload cmd) [])

          er <- liftIO $ PCore.evalTermExec evalEnv coreState $ mkRedeemGasCoreTerm mid sender
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
              txCoreCache .= (CoreModuleCache $ PCore._erLoadedModules er')
              return $ map toPactEvent $ PCore._erEvents er'

            Left err -> do
              TRACE.traceM $ "CORE.redeemGas failed!!" <> sshow err
              fatal $ "redeemGas: Internal error - " <> sshow err
        else _erEvents <$>
            applyExec' 0 interp redeemGasCmd
              (_pSigners $ _cmdPayload cmd)
              []
              rgHash
              managedNamespacePolicy
    else do
      GasId gid <- use txGasId >>= \case
        Nothing -> fatal $! "redeemGas: no gas id in scope for gas refunds"
        Just g -> return g
      let redeemGasCmd =
            ContMsg gid 1 False (toLegacyJson $ object [ "fee" A..= toJsonViaEncode fee ]) Nothing
      -- evalEnv <- mkCoreEvalEnv managedNamespacePolicy (MsgData execData Nothing rgHash (_pSigners $ _cmdPayload cmd) [])

      fmap _crEvents $ locally txQuirkGasFee (const Nothing) $
        if usePactTng then
          applyContinuationTng 0 coreState redeemGasCmd
            (_pSigners $ _cmdPayload cmd) (toUntypedHash $ _cmdHash cmd)
            managedNamespacePolicy
        else
          applyContinuation 0 (initState mcache) redeemGasCmd
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

-- | Make an 'EvalEnv' given a tx env + state
--
mkEvalEnv
    :: NamespacePolicy
    -> MsgData
    -> TransactionM logger db (EvalEnv db)
mkEvalEnv nsp msg = do
    tenv <- ask
    genv <- GasEnv
      <$> view (txGasLimit . to (MilliGasLimit . gasToMilliGas))
      <*> view txGasPrice
      <*> use txGasModel
    fmap (set eeSigCapBypass txCapBypass)
      $ liftIO $ setupEvalEnv (_txDbEnv tenv) Nothing (_txMode tenv)
      msg (versionedNativesRefStore (_txExecutionConfig tenv)) genv
      nsp (_txSpvSupport tenv) (_txPublicData tenv) (_txExecutionConfig tenv)
  where
  txCapBypass =
    M.fromList
    [ (wizaDebit, (wizaBypass, wizaMH))
    , (skdxDebit, (kdxBypass, skdxMH))
    , (collectGallinasMarket, (collectGallinasBypass, collectGallinasMH))
    , (marmaladeGuardPolicyMint, (marmaladeBypass, marmaladeGuardPolicyMH))
    ]
    where
    -- wiza code
    wizaDebit = QualifiedName "free.wiza" "DEBIT" def
    wizaMH = unsafeModuleHashFromB64Text "8b4USA1ZNVoLYRT1LBear4YKt3GB2_bl0AghZU8QxjI"
    wizEquipmentOwner = QualifiedName "free.wiz-equipment" "OWNER" def
    wizEquipmentAcctGuard = QualifiedName "free.wiz-equipment" "ACCOUNT_GUARD" def
    wizArenaAcctGuard = QualifiedName "free.wiz-arena" "ACCOUNT_GUARD" def
    wizArenaOwner = QualifiedName "free.wiz-arena" "OWNER" def
    wizaTransfer = QualifiedName "free.wiza" "TRANSFER" def

    wizaBypass granted sigCaps =
      let debits = filter ((== wizaDebit) . _scName) $ S.toList granted
      in all (\c -> any (match c) sigCaps) debits
      where
      match prov sigCap = fromMaybe False $ do
        guard $ _scName sigCap `elem` wizaBypassList
        sender <- preview _head (_scArgs prov)
        (== sender) <$> preview _head (_scArgs sigCap)
      wizaBypassList =
        [ wizArenaOwner
        , wizEquipmentOwner
        , wizaTransfer
        , wizEquipmentAcctGuard
        , wizArenaAcctGuard]
    -- kaddex code
    skdxDebit = QualifiedName "kaddex.skdx" "DEBIT" def
    skdxMH = unsafeModuleHashFromB64Text "g90VWmbKj87GkMkGs8uW947kh_Wg8JdQowa8rO_vZ1M"
    kdxUnstake = QualifiedName "kaddex.staking" "UNSTAKE" def

    kdxBypass granted sigCaps =
      let debits = filter ((== skdxDebit) . _scName) $ S.toList granted
      in all (\c -> S.member (SigCapability kdxUnstake (_scArgs c)) sigCaps) debits
    -- Collect-gallinas code
    collectGallinasMH = unsafeModuleHashFromB64Text "x3BLGdidqSjUQy5q3MorGco9mBDpoVTh_Yoagzu0hls"
    collectGallinasMarket = QualifiedName "free.collect-gallinas" "MARKET" def
    collectGallinasAcctGuard = QualifiedName "free.collect-gallinas" "ACCOUNT_GUARD" def

    collectGallinasBypass granted sigCaps = fromMaybe False $ do
      let mkt = filter ((== collectGallinasMarket) . _scName) $ S.toList granted
      let matchingGuard provided toMatch = _scName toMatch == collectGallinasAcctGuard && (_scArgs provided == _scArgs toMatch)
      pure $ all (\c -> any (matchingGuard c) sigCaps) mkt
    -- marmalade code
    marmaladeGuardPolicyMH = unsafeModuleHashFromB64Text "LB5sRKx8jN3FP9ZK-rxDK7Bqh0gyznprzS8L4jYlT5o"
    marmaladeGuardPolicyMint = QualifiedName "marmalade-v2.guard-policy-v1" "MINT" def
    marmaladeLedgerMint = QualifiedName "marmalade-v2.ledger" "MINT-CALL" def

    marmaladeBypass granted sigCaps = fromMaybe False $ do
      let mkt = filter ((== marmaladeGuardPolicyMint) . _scName) $ S.toList granted
      let matchingGuard provided toMatch = _scName toMatch == marmaladeLedgerMint && (_scArgs provided == _scArgs toMatch)
      pure $ all (\c -> any (matchingGuard c) sigCaps) mkt

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
      convertPactValue pv = J.decode $ J.encode pv
      convertQualName QualifiedName{..} = PCore.QualifiedName
        { PCore._qnName = _qnName
        , PCore._qnModName = _qnQual & \ModuleName{..} ->
            PCore.ModuleName
              { PCore._mnName = _mnName
              , PCore._mnNamespace = fmap coerce _mnNamespace
              }
        }
      convertCapability SigCapability{..} =
          PCore.CapToken (convertQualName _scName) (mapMaybe (either (const $ error "FAILEDDDD111") Just . PCore.fromLegacyPactValue . maybe (error "mkCoreEvalEnv: failed to parseJSON pact value") id . convertPactValue) _scArgs)

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
        { PCore.mdData = either (\e -> error $ "FAILEDDDD22: " ++ show e ++ " for " ++ show mdData) id $ PCore.fromLegacyPactValue $ maybe (error "mkCoreEvalEnv: failed to parseJSON pact value") id $ convertPactValue $ _getLegacyValue mdData
        , PCore.mdStep = mdStep <&> \PactStep{..} ->
            PCore.DefPactStep
              { PCore._psStep = _psStep
              , PCore._psRollback = _psRollback
              , PCore._psDefPactId = coerce _psPactId
              , PCore._psResume = _psResume <&> \Yield{..} ->
                  PCore.Yield
                    { PCore._yData = M.fromList $ mapMaybe (\(k, v) -> fmap (coerce k,) $ either (const $ error "FAILEDDDD3") Just $ PCore.fromLegacyPactValue $ maybe (error "mkCoreEvalEnv: failed to parseJSON pact value") id $ convertPactValue v) $ M.toList $ _objectMap _yData
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

    gasModel <- use txGasModelCore
    let
      toCoreExFlag = \case
        FlagDisableModuleInstall -> Just PCore.FlagDisableModuleInstall
        FlagDisableHistoryInTransactionalMode -> Just PCore.FlagDisableHistoryInTransactionalMode
        FlagAllowReadInLocal -> Just PCore.FlagAllowReadInLocal
        FlagDisablePactEvents -> Just PCore.FlagDisablePactEvents
        FlagEnforceKeyFormats -> Just PCore.FlagEnforceKeyFormats
        _ -> Nothing
      executionFlags = mapMaybe toCoreExFlag $ S.toList $ _ecFlags $ _txExecutionConfig tenv
    liftIO $ PCore.setupEvalEnv (_txCoreDb tenv) txMode' coreMsg gasModel coreNsp PCore.noSPVSupport cpd (S.fromList executionFlags)

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
