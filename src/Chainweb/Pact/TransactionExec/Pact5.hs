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
, applyExec
, applyExec'
, applyContinuation
, applyContinuation'
, applyContinuation
, applyContinuation'
, runPayload

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
, buildExecRawCode
, mkMagicCapSlot
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

import Pact.Core.Serialise.LegacyPact ()
import Pact.Core.Compile
import Pact.Core.Evaluate
import Pact.Core.Command
import Pact.Core.Capabilities
import Pact.Core.Errors
import Pact.Core.Names
import Pact.Core.Namespace
import Pact.Core.Persistence
import Pact.Core.Pretty
import Pact.Core.Gas
import Pact.Core.Hash
import Pact.Core.PactValue
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Syntax.ParseTree
import Pact.Core.DefPacts.Types
import Pact.Core.Scheme
import Pact.Core.StableEncoding
import Pact.Core.SPV
import Pact.Core.Verifiers
import Pact.Core.Info

import qualified Pact.Parse as Pact4
import qualified Pact.Types.Command as Pact4
import qualified Pact.Types.RPC as Pact4
import qualified Pact.Types.Persistence as Pact4
import qualified Pact.Types.ChainId as Pact4
import qualified Pact.Types.Hash as Pact4
import qualified Pact.Types.SPV as Pact4

-- internal Chainweb modules
import qualified Chainweb.Pact.Transactions.CoinCoreV4Transactions as CoinCoreV4
import qualified Chainweb.Pact.Templates.Pact4 as Pact4

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import qualified Chainweb.ChainId as Chainweb
import Chainweb.Mempool.Mempool (requestKeyToTransactionHash)
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Templates.Pact5
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
magic_COINBASE :: CapSlot QualifiedName PactValue
magic_COINBASE = mkMagicCapSlot "COINBASE"

-- | "Magic" capability 'GAS' used in the coin contract to
-- constrain gas buy/redeem calls.
--
magic_GAS :: CapSlot QualifiedName PactValue
magic_GAS = mkMagicCapSlot "GAS"


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
    -> CoreDb
      -- ^ Pact db environment
    -> Miner
      -- ^ The miner chosen to mine the block
    -> GasModel CoreBuiltin
      -- ^ Gas model (pact Service config)
    -> TxContext
      -- ^ tx metadata and parent header
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command (Payload PublicMeta RawCode)
      -- ^ command with payload to execute
    -> Gas
      -- ^ initial gas used
    -> ApplyCmdExecutionContext
      -- ^ is this a local or send execution context?
    -> IO CommandResult
applyCmd v logger gasLogger coreDb miner gasModel txCtx spv cmd initialGas callCtx = do
    T2 cr st <- runTransactionM cenv txst applyBuyGas

    pure cr
  where
    stGasModelCore
      | chainweb217Pact' = gasModel
      | otherwise = freeGasModel
    txst = TransactionState mempty mempty 0 Nothing (Right stGasModelCore) mempty
    quirkGasFee = v ^? versionQuirks . quirkGasFees . ix requestKey

    cenv = TransactionEnv (Pact4.Transactional) (Right coreDb) logger gasLogger (ctxToPublicData txCtx) (undefined spv) nid gasPrice
      requestKey (fromIntegral gasLimit) def quirkGasFee

    !requestKey = Pact4.RequestKey $ Pact4.Hash $ unHash $ unRequestKey $ cmdToRequestKey cmd
    !gasPrice = undefined -- view cmdGasPrice cmd
    !gasLimit = undefined -- view cmdGasLimit cmd
    !nid = undefined -- networkIdOf cmd
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

    redeemAllGas r = do
      txGasUsed .= fromIntegral gasLimit
      applyRedeem r

    applyBuyGas =
      buyGas txCtx cmd miner >>= \case
        Left e -> undefined
         -- view txRequestKey >>= \rk ->
         --  throwM $ BuyGasFailure $ GasPurchaseFailure (requestKeyToTransactionHash rk) e
        Right _ -> checkTooBigTx initialGas gasLimit applyVerifiers redeemAllGas

    displayPactError e = do
      r <- failTxWith e "tx failure for request key when running cmd"
      redeemAllGas r

    applyVerifiers = do
      if chainweb223Pact'
      then do
        let gasUsed = undefined -- use txGasUsed
        let initGasRemaining = fromIntegral gasLimit - gasUsed
        verifierResult <- liftIO $ runVerifierPlugins (ctxVersion txCtx, cid, currHeight) logger allVerifiers initGasRemaining undefined
        case verifierResult of
          Left err -> do
            let errMsg = "Tx verifier error: " <> getVerifierError err
            -- cmdResult <- -- failTxWith
            --   undefined -- (PEExecutionError TxFailure def [] (pretty errMsg))
            --   errMsg
            redeemAllGas undefined
          Right verifierGasRemaining -> do
            txGasUsed += initGasRemaining - verifierGasRemaining
            applyPayload
      else applyPayload

    applyPayload = do
      txGasModel .= (Right gasModel)
      txGasUsed .= undefined -- initialGas

      -- cr <- runPayload cmd managedNamespacePolicy
      return undefined

    applyRedeem cr = undefined
    -- applyRedeem cr = do
    --   txGasModel .= _geGasModel freeGasEnv
    --   txGasModelCore .= freeGasModel

    --   r <- catchesPactError logger (onChainErrorPrintingFor txCtx) $! redeemGas txCtx cmd miner
    --   case r of
    --     Left e ->
    --       -- redeem gas failure is fatal (block-failing) so miner doesn't lose coins
    --       fatal $ "tx failure for request key while redeeming gas: " <> sshow e
    --     Right es -> do
    --       logs <- use txLogs

    --       -- /local requires enriched results with metadata, while /send strips them.
    --       -- when ?preflight=true is set, make sure that metadata occurs in result.

    --       let !cr' = case callCtx of
    --             ApplySend -> set crLogs (Just logs) $ over crEvents (es ++) cr
    --             ApplyLocal -> set crMetaData (Just $ J.toJsonViaEncode $ ctxToPublicData' txCtx)
    --               $ set crLogs (Just logs)
    --               $ over crEvents (es ++) cr

    --       return $ cr'

applyCoinbase
    :: (Logger logger)
    => ChainwebVersion
    -> logger
      -- ^ Pact logger
    -> CoreDb
      -- ^ Pact db environment
    -> Miner
      -- ^ The miner chosen to mine the block
    -> Pact4.ParsedDecimal
      -- ^ Miner reward
    -> TxContext
      -- ^ tx metadata and parent header
    -> EnforceCoinbaseFailure
      -- ^ enforce coinbase failure or not
    -> CoinbaseUsePrecompiled
      -- ^ always enable precompilation
    -> IO CommandResult
applyCoinbase v logger coreDb (Miner mid mks@(MinerKeys mk)) reward@(Pact4.ParsedDecimal d) txCtx
  (EnforceCoinbaseFailure enfCBFailure) (CoinbaseUsePrecompiled enablePC)
  | enablePC = do
    let state = initCapabilities [magic_COINBASE]
        (coinbaseTerm, cexec) = mkCoinbaseTerm mid mks (GasSupply reward)
    go state cexec coinbaseTerm
  | otherwise = do
    cexec <- Pact4.mkCoinbaseCmd mid mks reward
    let state = initCapabilities [magic_COINBASE]
    go state cexec undefined
  where
    tenv = TransactionEnv Pact4.Transactional (Right coreDb) logger Nothing (ctxToPublicData txCtx) Pact4.noSPVSupport
           Nothing 0.0 rk 0 def Nothing
    txst = TransactionState mempty mempty 0 Nothing (Right freeGasModel) mempty
    rk = Pact4.RequestKey $ Pact4.Hash $ unHash chash
    parent = _tcParentHeader txCtx

    bh = ctxCurrentBlockHeight txCtx
    cid = Chainweb._chainId parent
    chash = Hash $ SB.toShort $ encodeToByteString $ _blockHash $ _parentHeader parent
        -- NOTE: it holds that @ _pdPrevBlockHash pd == encode _blockHash@
        -- NOTE: chash includes the /quoted/ text of the parent header.

    go evState cexec@(Pact4.ExecMsg _ execData) coinbaseTerm = evalTransactionM tenv txst $! do
      evalEnv <- mkEvalEnv managedNamespacePolicy (MsgData (undefined execData) Nothing chash mempty [])
      cr <- liftIO $ evalTermExec evalEnv evState coinbaseTerm

      case cr of
        Right er -> do
          debug
            $! "successful coinbase of "
            <> T.take 18 (sshow d)
            <> " to "
            <> sshow mid

          -- upgradedModuleCache <- applyUpgrades v cid bh

          return $! CommandResult $ PactResult $ Right $ PERExpr er

        Left e
          | enfCBFailure -> throwM $ CoinbaseFailure $ sshow e
          | otherwise -> undefined -- (`T2` Nothing) <$> (Left <$> failTxWith (PactError EvalError (Info Nothing) [] mempty) "coinbase tx failure")


applyLocal
    :: (Logger logger)
    => logger
      -- ^ Pact logger
    -> Maybe logger
      -- ^ Pact gas logger
    -> CoreDb
      -- ^ Pact db environment
    -> GasModel CoreBuiltin
      -- ^ Gas model (pact Service config)
    -> TxContext
      -- ^ tx metadata and parent header
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command PayloadWithText
      -- ^ command with payload to execute
    -> ExecutionConfig
    -> IO CommandResult
applyLocal logger gasLogger coreDb gasModel txCtx spv cmdIn execConfig =
    evalTransactionM tenv txst go
  where
    !cmd = payloadObj <$> cmdIn `using` traverse rseq
    !rk = Pact4.RequestKey $ Pact4.Hash $ unHash $ unRequestKey $ cmdToRequestKey cmd
    !nid = undefined -- networkIdOf cmd
    !chash = _cmdHash cmd
    !signers = undefined -- _pSigners $ _cmdPayload cmd
    !verifiers = [] -- fromMaybe [] $ _pVerifiers $ _cmdPayload cmd
    !gasPrice = undefined -- view cmdGasPrice cmd
    !gasLimit = undefined -- view cmdGasLimit cmd
    tenv = TransactionEnv Pact4.Local (Right coreDb) logger gasLogger (ctxToPublicData txCtx) undefined nid gasPrice
           rk (fromIntegral gasLimit) undefined Nothing
    txst = TransactionState mempty mempty 0 Nothing (Right gasModel) mempty
    gas0 = initialGasOf (_cmdPayload cmdIn)
    currHeight = ctxCurrentBlockHeight txCtx
    cid = V._chainId txCtx
    v = _chainwebVersion txCtx
    allVerifiers = verifiersAt v cid currHeight
    usePact5 = pact5 (ctxVersion txCtx) (ctxChainId txCtx) currHeight
    -- Note [Throw out verifier proofs eagerly]
    !verifiersWithNoProof = undefined
        -- (fmap . fmap) (\_ -> ()) verifiers
        -- `using` (traverse . traverse) rseq

    applyVerifiers m = do
      let initGasRemaining = undefined -- fromIntegral gasLimit - gas0
      verifierResult <- liftIO $ runVerifierPlugins (v, cid, currHeight) logger allVerifiers (undefined initGasRemaining) (undefined cmd)
      case verifierResult of
        Left err -> do
          let errMsg = "Tx verifier error: " <> getVerifierError err
          undefined errMsg
          -- failTxWith
          --   (PactError TxFailure def [] (pretty errMsg))
          --   errMsg
        Right verifierGasRemaining -> do
          let gas1 = undefined -- (initGasRemaining - (undefined verifierGasRemaining)) + gas0
          applyPayload gas1 m

    applyPayload gas1 m = do
      let coreState = def
      cr <- case m of
        Pact4.Exec em -> applyExec gas1 coreState em signers chash managedNamespacePolicy
        Pact4.Continuation cm -> applyContinuation gas1 coreState undefined signers chash managedNamespacePolicy

      case _pactResult $ _crResult cr of
        Left e -> do
          failTx5With e "applyLocal"
          pure cr
        Right _ -> pure cr

    go = checkTooBigTx gas0 gasLimit (applyVerifiers undefined) return -- $ _pPayload $ _cmdPayload cmd) return

-- -- | Apply (forking) upgrade transactions and module cache updates
-- -- at a particular blockheight.
-- --
-- -- This is the place where we consistently /introduce/ new transactions
-- -- into the blockchain along with module cache updates. The only other
-- -- places are Pact Service startup and the
-- -- empty-module-cache-after-initial-rewind case caught in 'execTransactions'
-- -- which both hit the database.
-- --
-- applyUpgrades
--   :: (Logger logger)
--   => ChainwebVersion
--   -> Chainweb.ChainId
--   -> BlockHeight
--   -> TransactionM logger p (Maybe CoreModuleCache)
-- applyUpgrades v cid height
--      | Just upg <-
--          v ^? versionUpgrades . onChain cid . at height . _Just = applyUpgrade upg
--      | cleanModuleCache v cid height = filterModuleCache
--      | otherwise = return Nothing
--   where
--     coinModuleName = ModuleName "coin" Nothing
--     coinCoreModuleName = ModuleName "coin" Nothing
--     installCoinModuleAdmin = set (evalCapabilities . capModuleAdmin) $ S.singleton coinModuleName
--     -- installCoreCoinModuleAdmin = set (esCaps . csModuleAdmin) $ S.singleton coinCoreModuleName

--     filterModuleCache = do
--       mc <- use txCache
--       cmc <- use txCoreCache
--       pure $ Just $
--         ( filterModuleCacheByKey (== coinModuleName) mc
--         , filterCoreModuleCacheByKey (== coinCoreModuleName) cmc
--         )

--     applyUpgrade upg = do
--       infoLog "Applying upgrade!"
--       let payloads = map (fmap payloadObj) $ _upgradeTransactions upg

--       --
--       -- In order to prime the module cache with all new modules for subsequent
--       -- blocks, the caches from each tx are collected and the union of all
--       -- those caches is returned. The calling code adds this new cache to the
--       -- init cache in the pact service state (_psInitCache).
--       --

--       let flags = flagsFor v cid (if _legacyUpgradeIsPrecocious upg then height + 1 else height)
--       caches <- local
--         (txExecutionConfig .~ ExecutionConfig flags)
--         (mapM applyTx payloads)
--       return $ Just $ bimap mconcat mconcat $ unzip $ reverse caches

--     interp = initStateInterpreter
--         $ installCoinModuleAdmin
--         $ initCapabilities [mkMagicCapSlot "REMEDIATE"]
--     -- coreInitState = installCoreCoinModuleAdmin $ initCapabilities [mkMagicCapSlot "REMEDIATE"]

--     applyTx tx = do
--       infoLog $ "Running upgrade tx " <> sshow (_cmdHash tx)
--       tryAllSynchronous (runGenesis tx permissiveNamespacePolicy interp) >>= \case
--         Right _ -> do
--           c <- use txCache
--           cc <- use txCoreCache
--           pure (c, cc)
--         Left e -> do
--           logError $ "Upgrade transaction failed! " <> sshow e
--           throwM e

failTxWith
    :: (Logger logger)
    => PactError SpanInfo
    -> Text
    -> TransactionM logger p CommandResult
failTxWith err msg = do
    logs <- use txLogs
    gas <- view txGasLimit -- error means all gas was charged
    rk <- view txRequestKey
    l <- view txLogger

    -- liftIO $ logFunction l L.Info
    --   (Pact4TxFailureLog rk err msg)
    undefined
    -- return $! CommandResult rk Nothing (PactResult (Left err))
    --   gas (Just logs) Nothing Nothing []

failTx5With
    :: (Logger logger)
    => PactError SpanInfo
    -> Text
    -> TransactionM logger p ()
failTx5With err msg = do
    logs <- use txLogs
    gas <- view txGasLimit -- error means all gas was charged
    rk <- view txRequestKey
    l <- view txLogger

    liftIO $ logFunction l L.Info
      (Pact5TxFailureLog rk err msg)

runPayload
    :: (Logger logger)
    => Command (Payload PublicMeta RawCode)
    -> NamespacePolicy
    -> TransactionM logger p CommandResult
runPayload cmd nsp = do
    g0 <- use txGasUsed
    let coreState = def

    -- Note [Throw out verifier proofs eagerly]
    let !verifiersWithNoProof = undefined
            -- (fmap . fmap) (\_ -> ()) verifiers
            -- `using` (traverse . traverse) rseq

    case payload of
      Pact4.Exec pm -> applyExec (undefined g0) coreState pm signers chash nsp
      Pact4.Continuation ym -> applyContinuation (undefined g0) coreState (undefined ym) signers chash nsp

  where
    verifiers = undefined -- fromMaybe [] $ _pVerifiers $ _cmdPayload cmd
    signers = undefined -- _pSigners $ _cmdPayload cmd
    chash = undefined -- toUntypedHash $ _cmdHash cmd
    payload = undefined -- _pPayload $ _cmdPayload cmd

applyExec
    :: (Logger logger)
    => Gas
    -> EvalState CoreBuiltin SpanInfo
    -> Pact4.ExecMsg RawCode
    -> [Signer QualifiedName PactValue]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p CommandResult
applyExec initialGas coreState em senderSigs hsh nsp = do
    er <- applyExec' initialGas coreState em senderSigs hsh nsp

    -- return er
    pure $ CommandResult $ PactResult $ PERTopLevel <$> er
    -- either id mkCommandResultFromCoreResult er

applyExec'
    :: (Logger logger)
    => Gas
    -> EvalState CoreBuiltin SpanInfo
    -> Pact4.ExecMsg RawCode
    -> [Signer QualifiedName PactValue]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p (Either (PactError SpanInfo) (EvalResult [TopLevel SpanInfo]))
applyExec' (Gas initialGas) coreState (Pact4.ExecMsg rawCode execData) senderSigs hsh nsp = do
  evalEnv <- mkEvalEnv nsp (MsgData (undefined execData) Nothing hsh senderSigs [])

  setEnvGas (Gas $ fromIntegral initialGas) evalEnv

  er <- liftIO $! evalExec evalEnv coreState rawCode
  case er of
    Right er' -> do
      -- liftIO $ print ("_erOutput", _erOutput er')

      -- if we specified this transaction's gas fee manually as a "quirk",
      -- here we set the result's gas fee to agree with that
      quirkGasFee <- view txQuirkGasFee
      let quirkedEvalResult = undefined
       -- case quirkGasFee of
       --      Nothing -> er'
       --      Just (Gas fee) -> er' { _erGas = Gas $ fromIntegral fee }

      return $ Right quirkedEvalResult
    Left err -> do
      -- TODO: return either an error instead of throwing an exception here
      fatal $ "Pact Tng execution failed: " <> (T.pack $ show $ pretty err)
      return $ Left err

-- | Execute a 'ContMsg' and return the command result and module cache
--
applyContinuation
    :: (Logger logger)
    => Gas
    -> EvalState CoreBuiltin SpanInfo
    -> ContMsg
    -> [Signer QualifiedName PactValue]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p CommandResult -- (Either (PactError SpanInfo) (EvalResult [TopLevel SpanInfo]))
applyContinuation initialGas coreState cm senderSigs hsh nsp = do
    er <- applyContinuation' initialGas coreState cm senderSigs hsh nsp
    -- return er
    -- either id mkCommandResultFromCoreResult
    pure $ CommandResult $ PactResult $ PERTopLevel <$> er

    -- for_ _erLogGas $ \gl -> gasLog $ "gas logs: " <> sshow gl

    -- TODO: set tx warnings to eval warnings
    -- txWarnings <>= _erWarnings

setEnvGas :: Gas -> EvalEnv CoreBuiltin SpanInfo -> TransactionM logger p ()
setEnvGas initialGas = liftIO . views eeGasRef (`writeIORef` gasToMilliGas initialGas)

-- | Execute a 'ContMsg' and return just eval result, not wrapped in a
-- 'CommandResult' wrapper
--
applyContinuation'
    :: (Logger logger)
    => Gas
    -> EvalState CoreBuiltin SpanInfo
    -> ContMsg
    -> [Signer QualifiedName PactValue]
    -> Hash
    -> NamespacePolicy
    -> TransactionM logger p (Either (PactError SpanInfo) (EvalResult [TopLevel SpanInfo]))
applyContinuation' initialGas coreState (ContMsg pid s rb d proof) senderSigs hsh nsp = do

    evalEnv <- mkEvalEnv nsp (MsgData d pactStep hsh senderSigs [])

    setEnvGas (Gas $ fromIntegral $ undefined initialGas) evalEnv

    -- let
    --   convertPactValue pv = PactConversion.fromLegacyPactValue $
    --       aeson (error "applyContinuation': failed to parseJSON pact value") id $ A.fromJSON $ _getLegacyValue pv

    --   coreCm = ContMsg
    --       { _cmPactId = coerce pid
    --       , _cmStep = s
    --       , _cmRollback = rb
    --       , _cmData = either (error "applyContinuation': failed to convert pact value") id $ convertPactValue d
    --       , _cmProof = coerce proof
    --       }

    er <- liftIO $! evalContinuation evalEnv coreState (undefined)
    case er of
      Right er' -> do
        -- if we specified this transaction's gas fee manually as a "quirk",
        -- here we set the result's gas fee to agree with that
        quirkGasFee <- view txQuirkGasFee
        let quirkedEvalResult = undefined
         -- case quirkGasFee of
         --      Nothing -> er'
         --      Just (Gas fee) -> er' { _erGas = Gas $ fromIntegral fee }

        return $ Right $ quirkedEvalResult
      err -> pure err

  where
    pactStep = Just $ DefPactStep s rb pid Nothing

-- | Build and execute 'coin.buygas' command from miner info and user command
-- info (see 'TransactionExec.applyCmd')
--
-- see: 'pact/coin-contract/coin.pact#fund-tx'
--
buyGas :: (Logger logger) => TxContext -> Command (Payload PublicMeta RawCode) -> Miner -> TransactionM logger p (Either (PactError SpanInfo) [PactEvent PactValue])
buyGas txCtx cmd (Miner mid mks) = go
  where
    isChainweb224Pact = guardCtx chainweb224Pact txCtx
    sender = undefined -- view (cmdPayload . pMeta . pmSender) cmd

    run input = undefined
      -- (findPayer txCtx cmd) >>= \r -> case r of
      --   Nothing -> input
      --   Just withPayerCap -> withPayerCap input

    (Hash chash) = (_cmdHash cmd)
    bgHash = Hash (chash <> "-buygas")

    go = do
      supply <- undefined -- gasSupplyOf <$> view txGasLimit <*> view txGasPrice
      logGas <- isJust <$> view txGasLogger

      let (buyGasTerm, buyGasCmd@(Pact4.ExecMsg _ execData)) =
            -- post-chainweb 2.24, we call buy-gas directly rather than
            -- going through fund-tx which is a defpact.
            if isChainweb224Pact
            then mkBuyGasTerm sender supply
            else mkFundTxTerm mid mks sender supply
          coreState = initCapabilities [magic_GAS]

      let
        gasCapName = QualifiedName "GAS" (ModuleName "coin" Nothing)
        signedForGas signer =
          any (\sc -> _ctName sc == gasCapName) (_siCapList signer)
        addDebit signer
          | signedForGas signer =
            signer & siCapList %~ (undefined) --debitCap sender:)
          | otherwise = signer
        addDebitToSigners =
          fmap addDebit
        signersWithDebit = addDebitToSigners $ undefined -- _pSigners $ _cmdPayload cmd

      -- no verifiers are allowed in buy gas
      -- quirked gas is not used either
      -- result <-
      --   applyExec' 0 (interp mcache) buyGasCmd signersWithDebit [] bgHash managedNamespacePolicy

      evalEnv <- mkEvalEnv managedNamespacePolicy (MsgData (undefined execData) Nothing bgHash signersWithDebit [])

      er <- locally txQuirkGasFee (const Nothing) $ liftIO $ evalTermExec evalEnv coreState buyGasTerm
      case er of
        Right er' -> do
          case _erExec er' of
            Nothing
              | isChainweb224Pact ->
                return $ Right []
              | otherwise ->
                -- should never occur pre-chainweb 2.24:
                -- would mean coin.fund-tx is not a pact
                fatal "buyGas: Internal error - empty continuation before 2.24 fork"
            Just pe
              | isChainweb224Pact ->
                fatal "buyGas: Internal error - continuation found after 2.24 fork"
              | otherwise -> do
                void $! txGasId .= (Just $! GasId ( undefined $ _peDefPactId pe))
                pure $ Right []
        Left err -> do
          fatal $ "buyGas: Internal error - " <> sshow err

-- TODO: rewrite to core
-- findPayer
--   :: TxContext
--   -> Command (Payload PublicMeta RawCode)
--   -> Eval e (Maybe (Eval e [Term Name] -> Eval e [Term Name]))
-- findPayer txCtx cmd = runMaybeT $ do
--     (!m,!qn,!as) <- MaybeT findPayerCap
--     pMod <- MaybeT $ lookupModule qn m
--     capRef <- MaybeT $ return $ lookupIfaceModRef qn pMod
--     return $ runCap (getInfo qn) capRef as
--   where
--     setEnvMsgBody v e = set eeMsgBody v e

--     findPayerCap :: Eval e (Maybe (ModuleName,QualifiedName,[PactValue]))
--     findPayerCap = preview $ eeMsgSigs . folded . folded . to sigPayerCap . _Just

--     sigPayerCap (SigCapability q@(QualifiedName m n _) as)
--       | n == "GAS_PAYER" = Just (m,q,as)
--     sigPayerCap _ = Nothing

--     gasPayerIface = ModuleName "gas-payer-v1" Nothing

--     lookupIfaceModRef (QualifiedName _ n _) (ModuleData (MDModule Module{..}) refs _)
--       | gasPayerIface `elem` _mInterfaces = HM.lookup n refs
--     lookupIfaceModRef _ _ = Nothing

--     mkApp i r as = App (TVar r i) (map (liftTerm . fromPactValue) as) i

--     runCap i capRef as input = do
--       let msgBody = enrichedMsgBody cmd
--           enrichMsgBody | guardCtx pactBackCompat_v16 txCtx = id
--                         | otherwise = setEnvMsgBody (toLegacyJson msgBody)
--       ar <- local enrichMsgBody $ do
--         (cap, capDef, args) <- appToCap $ mkApp i capRef as
--         evalCap i CapCallStack False (cap, capDef, args, i)

--       case ar of
--         NewlyAcquired -> do
--           r <- input
--           popCapStack (const (return ()))
--           return r
--         _ -> evalError' i "Internal error, GAS_PAYER already acquired"


-- | Build and execute 'coin.redeem-gas' command from miner info and previous
-- command results (see 'TransactionExec.applyCmd')
--
-- see: 'pact/coin-contract/coin.pact#fund-tx'
--
redeemGas :: (Logger logger) => TxContext -> Command (Payload PublicMeta RawCode) -> Miner -> TransactionM logger p [PactEvent PactValue]
redeemGas txCtx cmd (Miner mid mks) = do
    let sender = undefined -- cmdPayload . pMeta . pmSender) cmd
    fee <- undefined -- gasSupplyOf <$> use txGasUsed <*> view txGasPrice
    -- if we're past chainweb 2.24, we don't use defpacts for gas

    let
      coreState = initCapabilities [magic_GAS]

    if guardCtx chainweb224Pact txCtx
    then do
      total <- undefined -- gasSupplyOf <$> view txGasLimit <*> view txGasPrice
      let (redeemGasTerm, redeemGasCmd@(Pact4.ExecMsg _ execData)) =
            mkRedeemGasTerm mid mks (undefined sender) total fee
          -- I don't recall why exactly, but we set up an interpreter
          -- that ignores its argument and instead executes a term
          -- of our choice. we do the same to buy gas.
          (Hash chash) = (_cmdHash cmd)
          rgHash = Hash (chash <> "-redeemgas")

      locally txQuirkGasFee (const Nothing) $ do
        evalEnv <- mkEvalEnv managedNamespacePolicy (MsgData (undefined execData) Nothing rgHash (undefined $ _cmdPayload cmd) [])

        er <- liftIO $ evalTermExec evalEnv coreState redeemGasTerm
        case er of
          Right er' -> return $ _erEvents er'

          Left err -> do
            fatal $ "redeemGas: Internal error - " <> sshow err
    else do
      GasId gid <- use txGasId >>= \case
        Nothing -> fatal $! "redeemGas: no gas id in scope for gas refunds"
        Just g -> return g
      let redeemGasCmd =
            ContMsg (undefined gid) 1 False (undefined $ object [ "fee" A..= toJsonViaEncode fee ]) Nothing

      locally txQuirkGasFee (const Nothing) $ do
        r <- applyContinuation (Gas 0) coreState redeemGasCmd
          (undefined) -- _pSigners $ _cmdPayload cmd)
          (_cmdHash cmd)
          managedNamespacePolicy
        return []

-- ---------------------------------------------------------------------------- --
-- Utilities

-- | Initialize a fresh eval state with magic capabilities.
-- This is the way we inject the correct guards into the environment
-- during Pact code execution
--
initCapabilities :: [CapSlot QualifiedName PactValue] -> EvalState CoreBuiltin SpanInfo
initCapabilities cs = set (esCaps . csSlots) cs def
{-# INLINABLE initCapabilities #-}

-- | Check whether the cost of running a tx is more than the allowed
-- gas limit and do some action depending on the outcome
--
checkTooBigTx
    :: (Logger logger)
    => Gas
    -> GasLimit
    -> TransactionM logger p CommandResult
    -> (CommandResult -> TransactionM logger p CommandResult)
    -> TransactionM logger p CommandResult
checkTooBigTx initialGas gasLimit next onFail
  | initialGas >= gasLimit = do

      let !pe = undefined
       -- PactError GasError def []
       --      $ "Tx too big (" <> pretty initialGas <> "), limit "
       --      <> pretty gasLimit

      r <- failTxWith pe "Tx too big"
      onFail r
  | otherwise = next

-- | Initial gas charged for transaction size
--   ignoring the size of a continuation proof, if present
--
initialGasOf :: PayloadWithText -> Gas
initialGasOf payload = gasFee
  where
    feePerByte :: Rational = 0.01

    contProofSize = undefined
      -- case _pPayload (payloadObj payload) of
      --   Pact4.Continuation (ContMsg _ _ _ _ (Just (ContProof p))) -> B.length p
      --   _ -> 0
    txSize = SB.length (payloadBytes payload) - contProofSize

    costPerByte = fromIntegral txSize * feePerByte
    sizePenalty = txSizeAccelerationFee costPerByte
    gasFee = undefined -- ceiling $  (costPerByte + sizePenalty)
{-# INLINE initialGasOf #-}

txSizeAccelerationFee :: Rational -> Rational
txSizeAccelerationFee costPerByte = total
  where
    total = (costPerByte / bytePenalty) ^ power
    bytePenalty = 512
    power :: Integer = 7
{-# INLINE txSizeAccelerationFee #-}

-- -- | Set tx result state
-- --
-- setTxResultState :: EvalResult -> TransactionM logger db ()
-- setTxResultState er = do
--     txLogs <>= _erLogs er
--     txGasUsed .= _erGas er
-- {-# INLINE setTxResultState #-}

-- | Make an 'EvalEnv' given a tx env + state
--
-- mkEvalEnv
--     :: NamespacePolicy
--     -> MsgData
--     -> TransactionM logger db (EvalEnv db)
-- mkEvalEnv nsp msg = do
--     tenv <- ask
--     genv <- GasEnv
--       <$> view (txGasLimit . to (MilliGasLimit . gasToMilliGas))
--       <*> view txGasPrice
--       <*> use txGasModel
--     liftIO $ setupEvalEnv (_txDbEnv tenv) Nothing (_txMode tenv)
--       msg (versionedNativesRefStore (_txExecutionConfig tenv)) genv
--       nsp (_txSpvSupport tenv) (_txPublicData tenv) (_txExecutionConfig tenv)

mkEvalEnv
    :: NamespacePolicy
    -> MsgData
    -> TransactionM logger db (EvalEnv CoreBuiltin SpanInfo)
mkEvalEnv nsp MsgData{..} = do
  undefined
    -- tenv <- ask

    -- let
    --   convertPactValue pv = aeson (\s -> error $ "mkEvalEnv: failed to parse legacyValue " ++ s) id $ A.fromJSON $ _getLegacyValue pv
    --   convertQualName QualifiedName{..} = QualifiedName
    --     { _qnName = _qnName
    --     , _qnModName = _qnQual & \ModuleName{..} ->
    --         ModuleName
    --           { _mnName = _mnName
    --           , _mnNamespace = fmap coerce _mnNamespace
    --           }
    --     }
    --   convertCapability SigCapability{..} =
    --       CapToken (convertQualName _scName) (mapMaybe (either (const Nothing) Just . PactConversion.fromLegacyPactValue) _scArgs)

    --   convertVerifier Verifier{..} = Verifier
    --     { _verifierName = coerce _verifierName
    --     , _verifierProof = _verifierProof
    --     , _verifierCaps = map convertCapability _verifierCaps
    --     }

    -- let
    --   txMode' = case _txMode tenv of
    --     Transactional -> Transactional
    --     Local -> Local

    -- let
    --   coreMsg = MsgData
    --     { mdData = either (const $ PObject mempty) id $ PactConversion.fromLegacyPactValue $ convertPactValue mdData
    --     , mdStep = mdStep <&> \PactStep{..} ->
    --         DefPactStep
    --           { _psStep = _psStep
    --           , _psRollback = _psRollback
    --           , _psDefPactId = coerce _psPactId
    --           , _psResume = _psResume <&> \Yield{..} ->
    --               Yield
    --                 { _yData = M.fromList $ mapMaybe (\(k, v) -> fmap (coerce k,) $ either (const Nothing) Just $ PactConversion.fromLegacyPactValue v) $ M.toList $ _objectMap _yData
    --                 , _yProvenance = _yProvenance <&> \Provenance{..} ->
    --                     Provenance
    --                       { _pTargetChainId = coerce _pTargetChainId
    --                       , _pModuleHash = let (ModuleHash h) = _pModuleHash in ModuleHash $ coerce h
    --                       }
    --                 , _ySourceChain = coerce _ySourceChain
    --                 }
    --           }
    --     , mdHash = coerce $ mdHash
    --     , mdSigners = mdSigners <&> \Signer{..} ->
    --         Signer
    --           { _siScheme = _siScheme <&> \case
    --               ED25519 -> ED25519
    --               WebAuthn -> WebAuthn
    --           , _siPubKey = _siPubKey
    --           , _siAddress = _siAddress
    --           , _siCapList = map convertCapability _siCapList
    --           }
    --     , mdVerifiers = map convertVerifier mdVerifiers
    --     }

    -- let
    --   coreNsp = case nsp of
    --     SimpleNamespacePolicy _ -> SimpleNamespacePolicy
    --     SmartNamespacePolicy rootUsage name -> SmartNamespacePolicy rootUsage (convertQualName name)

    -- let
    --   pd = _txPublicData tenv
    --   convertPublicMeta pm = PublicMeta
    --     { _pmChainId = coerce $ _pmChainId pm
    --     , _pmSender = _pmSender pm
    --     , _pmGasLimit =
    --         let (GasLimit (ParsedInteger g)) = _pmGasLimit pm in Gas $ fromIntegral g
    --     , _pmGasPrice = coerce $ _pmGasPrice pm
    --     , _pmTTL =
    --         let (TTLSeconds (ParsedInteger s)) = _pmTTL pm in TTLSeconds s
    --     , _pmCreationTime = coerce $ _pmCreationTime pm
    --     }
    --   cpd = PublicData
    --     { _pdPublicMeta = convertPublicMeta $ _pdPublicMeta pd
    --     , _pdBlockHeight = _pdBlockHeight pd
    --     , _pdBlockTime = _pdBlockTime pd
    --     , _pdPrevBlockHash = _pdPrevBlockHash pd
    --     }

    -- gasModel <- use txGasModelCore
    -- let
    --   toCoreExFlag = \case
    --     FlagDisableModuleInstall -> Just FlagDisableModuleInstall
    --     FlagDisableHistoryInTransactionalMode -> Just FlagDisableHistoryInTransactionalMode
    --     FlagAllowReadInLocal -> Just FlagAllowReadInLocal
    --     FlagDisablePactEvents -> Just FlagDisablePactEvents
    --     FlagEnforceKeyFormats -> Just FlagEnforceKeyFormats
    --     _ -> Nothing
    --   executionFlags = mapMaybe toCoreExFlag $ S.toList $ _ecFlags $ _txExecutionConfig tenv
    -- liftIO $ setupEvalEnv (_txCoreDb tenv) txMode' coreMsg gasModel coreNsp noSPVSupport cpd (S.fromList executionFlags)

-- | Managed namespace policy CAF
--
managedNamespacePolicy :: NamespacePolicy
managedNamespacePolicy = SmartNamespacePolicy False
  (QualifiedName "validate" (ModuleName "ns" Nothing))
{-# NOINLINE managedNamespacePolicy #-}

-- | Builder for "magic" capabilities given a magic cap name
--
mkMagicCapSlot :: Text -> CapSlot QualifiedName PactValue
mkMagicCapSlot c = CapSlot cap []
  where
    mn = ModuleName "coin" Nothing
    fqn = QualifiedName c mn
    cap = CapToken fqn []
{-# INLINE mkMagicCapSlot #-}

-- | Build the 'Pact4.ExecMsg' for some pact code fed to the function. The 'value'
-- parameter is for any possible environmental data that needs to go into
-- the 'Pact4.ExecMsg'.
--
buildExecRawCode
    :: PactParserVersion
    -> Maybe Value
    -> Text
    -> IO (Pact4.ExecMsg RawCode)
buildExecRawCode ppv value code = maybe (go Null) go value
  where
    go val = case parsePact ppv code of
      Right !t -> pure $! Pact4.ExecMsg (RawCode "") (undefined val)
      -- if we can't construct coin contract calls, this should
      -- fail fast
      Left err -> internalError $ "buildExecRawCode: parse failed: " <> T.pack err

-- | Retrieve public metadata from a command
--
publicMetaOf :: Command (Payload PublicMeta RawCode) -> PublicMeta
publicMetaOf = undefined -- _pMeta . _cmdPayload
{-# INLINE publicMetaOf #-}

-- | Retrieve the optional Network identifier from a command
--
networkIdOf :: Command (Payload PublicMeta RawCode) -> Maybe Pact4.NetworkId
networkIdOf = undefined -- _pNetworkId . _cmdPayload
{-# INLINE networkIdOf #-}

-- | Calculate the gas fee (pact-generate gas cost * user-specified gas price),
-- rounding to the nearest stu.
--
gasSupplyOf :: Gas -> GasPrice -> GasSupply
gasSupplyOf (Gas gas) gp = GasSupply (Pact4.ParsedDecimal gs)
  where
    gs = toCoinUnit (fromIntegral gas * gp)
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

    throwM $ PactTransactionExecError (Pact4.fromUntypedHash $ Pact4.unRequestKey rk) e

logError :: (Logger logger) => Text -> TransactionM logger db ()
logError msg = view txLogger >>= \l -> logError_ l msg

infoLog :: (Logger logger) => Text -> TransactionM logger db ()
infoLog msg = view txLogger >>= \l -> logInfo_ l msg
