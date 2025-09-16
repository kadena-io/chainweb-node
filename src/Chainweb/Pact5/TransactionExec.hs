{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      :  Chainweb.Pact.TransactionExec
-- Copyright   :  Copyright © 2018 Kadena LLC.
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mark Nichols <mark@kadena.io>, Emily Pillmore <emily@kadena.io>
-- Stability   :  experimental
--
-- Pact command execution and coin-contract transaction logic for Chainweb
--
module Chainweb.Pact5.TransactionExec
  ( -- * Public API
    TransactionM(..)
  , TransactionEnv(..)
  , applyCoinbase
  , applyLocal
  , applyCmd

  -- * Semipublic API, only used by the mempool
  , buyGas

  -- * Private API, exposed only for tests
  , runVerifiers
  , runPayload
  , runGenesisPayload
  , redeemGas
  , applyUpgrades
  , managedNamespacePolicy

  -- * Utility
  , initialGasOf
  , ctxToPublicData

) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Parallel.Strategies(using, rseq)

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SB
import Data.Coerce (coerce)
import Data.Decimal (Decimal, roundTo)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified System.LogLevel as L

-- internal Pact modules
import qualified Pact.JSON.Decode as J
import qualified Pact.JSON.Encode as J


import Pact.Core.Builtin
import Pact.Core.Capabilities
import Pact.Core.Compile
import Pact.Core.DefPacts.Types
import Pact.Core.Environment hiding (_chainId)
import Pact.Core.Evaluate
import Pact.Core.Gas
import Pact.Core.Hash
import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.Namespace
import Pact.Core.PactValue
import Pact.Core.Persistence.Types hiding (GasM(..))
import Pact.Core.SPV
import Pact.Core.Serialise.LegacyPact ()
import Pact.Core.Signer
import Pact.Core.StableEncoding
import Pact.Core.Verifiers
import Pact.Core.Syntax.ParseTree qualified as Lisp
import Pact.Core.Gas.Utils qualified as Pact5

-- internal Chainweb modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Types
import Chainweb.Pact5.Templates
import Chainweb.Pact5.Types

import Chainweb.Time
import Chainweb.Pact5.Transaction
import Chainweb.VerifierPlugin hiding (chargeGas)
import Chainweb.Utils
import Chainweb.Version as V
import Chainweb.Version.Guards as V
import Chainweb.Version.Utils as V

import Pact.Core.Command.Types
import Data.ByteString (ByteString)
import Pact.Core.Command.RPC
import qualified Pact.Types.Gas as Pact4
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Data.Set (Set)
import Data.Void
import Control.Monad.Except
import Data.Int
import qualified Pact.Types.Verifier as Pact4
import qualified Pact.Types.Capability as Pact4
import qualified Pact.Types.Names as Pact4
import qualified Pact.Types.Runtime as Pact4
import qualified Pact.Core.Errors as Pact5

-- Note [Throw out verifier proofs eagerly]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We try to discard verifier proofs eagerly so that we don't hang onto them in
-- the liveset. This implies that we also try to discard `Command`s for the same
-- reason, because they contain the verifier proofs and other data we probably
-- don't need.

-- -------------------------------------------------------------------------- --

coinCap :: Text -> [PactValue] -> CapToken QualifiedName PactValue
coinCap n vs = CapToken (QualifiedName n (ModuleName "coin" Nothing)) vs

data TransactionEnv logger = TransactionEnv
  { _txEnvLogger :: logger
  , _txEnvGasEnv :: GasEnv CoreBuiltin Info
  }

makeLenses ''TransactionEnv

-- | TODO: document how TransactionM is just for the "paid-for" fragment of the transaction flow
newtype TransactionM logger a
  = TransactionM { runTransactionM :: ReaderT (TransactionEnv logger) (ExceptT (Pact5.PactError Info) IO) a }
  deriving newtype
  ( Functor
  , Applicative
  , Monad
  , MonadReader (TransactionEnv logger)
  , MonadError (Pact5.PactError Info)
  , MonadThrow
  , MonadIO
  )

chargeGas :: Info -> GasArgs CoreBuiltin -> TransactionM logger ()
chargeGas info gasArgs = do
  gasEnv <- view txEnvGasEnv
  either throwError return =<<
    liftIO (chargeGasArgsM gasEnv info [] gasArgs)

-- run verifiers
-- nasty... perhaps later convert verifier plugins to use GasM instead of tracking "gas remaining"
-- TODO: Verifiers are also tied to Pact enough that this is going to be an annoying migration
runVerifiers :: Logger logger => TxContext -> Command (Payload PublicMeta ParsedCode) -> TransactionM logger ()
runVerifiers txCtx cmd = do
      logger <- view txEnvLogger
      let v = _chainwebVersion txCtx
      let gasLimit = cmd ^. cmdPayload . pMeta . pmGasLimit
      gasUsed <- liftIO . readIORef . _geGasRef . _txEnvGasEnv =<< ask
      let initGasRemaining = MilliGas $ case (gasToMilliGas (gasLimit ^. _GasLimit), gasUsed) of
            (MilliGas gasLimitMilliGasWord, MilliGas gasUsedMilliGasWord) -> gasLimitMilliGasWord - gasUsedMilliGasWord
      let allVerifiers = verifiersAt v (_chainId txCtx) (ctxCurrentBlockHeight txCtx)
      let toModuleName m =
            Pact4.ModuleName
                    { Pact4._mnName = _mnName m
                    , Pact4._mnNamespace = coerce <$> _mnNamespace m
                    }
      let toQualifiedName qn =
            Pact4.QualifiedName
                { Pact4._qnQual = toModuleName $ _qnModName qn
                , Pact4._qnName = _qnName qn
                , Pact4._qnInfo = Pact4.Info Nothing
                }
      -- TODO: correct error handling here? we should probably charge the user
      let convertPactValue pv = fromJuste $ J.decodeStrict $ encodeStable pv
      let pact4TxVerifiers =
            [ Pact4.Verifier
              { Pact4._verifierName = case _verifierName pact5Verifier of
                VerifierName n -> Pact4.VerifierName n
              , Pact4._verifierProof =
                  -- TODO: correct error handling here? we should probably charge the user
                  Pact4.ParsedVerifierProof $ fromJuste $
                    convertPactValue $ coerce @ParsedVerifierProof @PactValue $ _verifierProof pact5Verifier
              , Pact4._verifierCaps =
                [ Pact4.SigCapability (toQualifiedName n) (convertPactValue <$> args)
                | SigCapability (CapToken n args) <- _verifierCaps pact5Verifier
                ]
              }
              | pact5Verifier <- fromMaybe [] $ cmd ^. cmdPayload . pVerifiers
              ]
      verifierResult <- liftIO $ runVerifierPlugins
          (_chainwebVersion txCtx, _chainId txCtx, ctxCurrentBlockHeight txCtx) logger
          allVerifiers
          (Pact4.Gas $ fromIntegral @SatWord @Int64 $ _gas $ milliGasToGas $ initGasRemaining)
          pact4TxVerifiers
      case verifierResult of
        Left err -> do
          throwError (Pact5.PEVerifierError err noInfo)
        Right (Pact4.Gas pact4VerifierGasRemaining) -> do
          -- TODO: crash properly on negative?
          let verifierGasRemaining = fromIntegral @Int64 @SatWord pact4VerifierGasRemaining
          -- NB: this is not nice.
          -- TODO: better gas info here
          -- Explanation by cases:
          -- Case 1:
          -- gasToMilliGas verifierGasRemaining is less than initGasRemaining,
          -- in which case the verifier charges gas.
          -- In that case we can subtract it from initGasRemaining and charge that safely.
          -- Case 2:
          -- gasToMilliGas verifierGasRemaining is greater than or equal to initGasRemaining,
          -- in which case the verifier has not charged gas, or has charged less than
          -- rounding error.
          -- In that case we do not charge gas at all.
          --
          when (gasToMilliGas (Gas verifierGasRemaining) < initGasRemaining) $
            chargeGas noInfo $ GAConstant $ MilliGas $ coerce initGasRemaining - coerce (gasToMilliGas (Gas verifierGasRemaining))

applyLocal
    :: (Logger logger)
    => logger
      -- ^ Pact logger
    -> Maybe logger
      -- ^ Pact gas logger
    -> PactDb CoreBuiltin Info
      -- ^ Pact db environment
    -> TxContext
      -- ^ tx metadata and parent header
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command (Payload PublicMeta ParsedCode)
      -- ^ command with payload to execute
    -> IO (CommandResult [TxLog ByteString] (Pact5.PactError Info))
applyLocal logger maybeGasLogger coreDb txCtx spvSupport cmd = do
  let gasLogsEnabled = maybe GasLogsDisabled (const GasLogsEnabled) maybeGasLogger
  let gasLimitGas :: Gas = cmd ^. cmdPayload . pMeta . pmGasLimit . _GasLimit
  gasEnv <- mkTableGasEnv (MilliGasLimit (gasToMilliGas gasLimitGas)) gasLogsEnabled
  let runLocal = do
        runVerifiers txCtx cmd
        runPayload Local localFlags coreDb spvSupport [] managedNamespacePolicy gasEnv txCtx (TxBlockIdx 0) cmd
  let txEnv = TransactionEnv
        { _txEnvGasEnv = gasEnv
        , _txEnvLogger = logger
        }
  runExceptT (runReaderT (runTransactionM runLocal) txEnv) >>= \case
    Left err -> do
      return CommandResult
        { _crReqKey = RequestKey $ _cmdHash cmd
        , _crTxId = Nothing
        , _crResult = PactResultErr err
        -- all gas is used when a command fails
        , _crGas = cmd ^. cmdPayload . pMeta . pmGasLimit . _GasLimit
        , _crLogs = Nothing
        , _crContinuation = Nothing
        , _crEvents = []
        , _crMetaData = Nothing
        }
    Right payloadResult -> do
      gasUsed <- milliGasToGas <$> readIORef (_geGasRef gasEnv)
      let result = case reverse (_erOutput payloadResult) of
            x:_ -> x
            _ -> InterpretValue PUnit noInfo
      return CommandResult
        { _crReqKey = RequestKey $ _cmdHash cmd
        , _crTxId = _erTxId payloadResult
        , _crResult =
          PactResultOk $ compileValueToPactValue $ result
        , _crGas = gasUsed
        , _crLogs = Just $ _erLogs payloadResult
        , _crContinuation = _erExec payloadResult
        , _crEvents =  _erEvents payloadResult
        , _crMetaData = Just (J.toJsonViaEncode $ StableEncoding $ ctxToPublicData (cmd ^. cmdPayload . pMeta) txCtx)
        }

  where
  defaultFlags = S.fromList
    [ FlagDisableRuntimeRTC
    , FlagEnforceKeyFormats
    -- Note: this is currently not conditional
    -- in pact-5 exec. This may change if it breaks
    -- anyone's workflow
    , FlagAllowReadInLocal
    , FlagRequireKeysetNs
    ]
  localFlags = Set.unions
    [ defaultFlags
    , guardDisablePact51Flags txCtx
    , guardDisablePact52And53Flags txCtx
    , guardDisablePact54Flags txCtx]

-- | The main entry point to executing transactions. From here,
-- 'applyCmd' assembles the command environment for a command,
-- purchases gas for the command, executes the command, and
-- redeems leftover gas.
-- Note that crMetaData is intentionally left unset in this path;
-- it's populated by `/poll`, when using `applyLocal`, or by the preflight
-- codepath later.
--
applyCmd
    :: forall logger. (Logger logger)
    => logger
      -- ^ Pact logger
    -> Maybe logger
      -- ^ Pact gas logger
    -> PactDb CoreBuiltin Info
      -- ^ Pact db environment
    -> TxContext
      -- ^ tx metadata
    -> TxIdxInBlock
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Gas
      -- ^ initial gas cost
    -> Command (Payload PublicMeta ParsedCode)
      -- ^ command with payload to execute
    -> IO (Either Pact5GasPurchaseFailure (CommandResult [TxLog ByteString] (Pact5.PactError Info)))
applyCmd logger maybeGasLogger db txCtx txIdxInBlock spv initialGas cmd = do
  logDebug_ logger $ "applyCmd: " <> sshow (_cmdHash cmd)
  let defaultFlags = Set.fromList
        [ FlagDisableRuntimeRTC
        , FlagDisableHistoryInTransactionalMode
        , FlagEnforceKeyFormats
        , FlagRequireKeysetNs
        ]
  let flags = Set.unions
              [ defaultFlags
              , guardDisablePact51Flags txCtx
              , guardDisablePact52And53Flags txCtx
              , guardDisablePact54Flags txCtx]

  let gasLogsEnabled = maybe GasLogsDisabled (const GasLogsEnabled) maybeGasLogger
  gasEnv <- mkTableGasEnv (MilliGasLimit $ gasToMilliGas $ gasLimit ^. _GasLimit) gasLogsEnabled
  let !requestKey = cmdToRequestKey cmd
  -- this process is "paid for", i.e. it's powered by a supply of gas that was
  -- purchased by a user already. any errors here will result in the entire gas
  -- supply being paid to the miner and the transaction failing, but still going
  -- on-chain.
  -- Todo: Do we even need this assuming we are passing in the same gas env?
  let paidFor :: TransactionM logger EvalResult
      paidFor = do
        -- TODO: better "info" for this, for gas logs
        chargeGas noInfo (GAConstant $ gasToMilliGas initialGas)

        runVerifiers txCtx cmd

        liftIO $ dumpGasLogs "applyCmd.paidFor.beforeRunPayload" (_cmdHash cmd) maybeGasLogger gasEnv
        evalResult <- runPayload Transactional flags db spv [] managedNamespacePolicy gasEnv txCtx txIdxInBlock cmd
        liftIO $ dumpGasLogs "applyCmd.paidFor.afterRunPayload" (_cmdHash cmd) maybeGasLogger gasEnv
        return evalResult

  eBuyGasResult <- do
    if GasLimit initialGas > gasLimit
    then do
      pure $ Left (PurchaseGasTxTooBigForGasLimit requestKey)
    else do
      buyGas logger gasEnv db txCtx cmd >>= \case
        Left buyGasError -> do
          pure $ Left (BuyGasError requestKey buyGasError)
        Right buyGasResult -> do
          pure $ Right buyGasResult

  case eBuyGasResult of
    Left err -> do
      pure (Left err)
    Right buyGasResult -> do
      let txEnv = TransactionEnv
            { _txEnvGasEnv = gasEnv
            , _txEnvLogger = logger
            }
      runExceptT (runReaderT (runTransactionM paidFor) txEnv) >>= \case
        Left err -> do
          -- if any error occurs during the transaction or verifiers, the output of the command is that error
          -- and all of the gas is sent to the miner. only buying gas and sending it to the miner are recorded.
          -- the Pact transaction is cancelled, and events and logs from the command are not recorded.
          eRedeemGasResult <- redeemGas
            logger db txCtx
            (gasLimit ^. _GasLimit)
            (_peDefPactId <$> _erExec buyGasResult)
            cmd
          case eRedeemGasResult of
            Left redeemGasError -> do
              pure (Left (RedeemGasError requestKey redeemGasError))
            Right redeemGasResult -> do
              return $ Right $ CommandResult
                { _crReqKey = RequestKey $ _cmdHash cmd
                , _crTxId = Nothing
                , _crResult = PactResultErr err
                -- all gas is used when a command fails
                , _crGas = cmd ^. cmdPayload . pMeta . pmGasLimit . _GasLimit
                , _crLogs = Just $ _erLogs buyGasResult <> _erLogs redeemGasResult
                , _crContinuation = Nothing
                , _crEvents = _erEvents buyGasResult <> _erEvents redeemGasResult
                , _crMetaData = Nothing
                }
        Right payloadResult -> do
          gasUsed <- milliGasToGas <$> readIORef (_geGasRef gasEnv)
          -- return all unused gas to the user and send all used
          -- gas to the miner.
          eRedeemGasResult <- redeemGas
            logger db txCtx
            gasUsed
            (_peDefPactId <$> _erExec buyGasResult)
            cmd

          case eRedeemGasResult of
            Left redeemGasError -> do
              pure (Left (RedeemGasError requestKey redeemGasError))
            Right redeemGasResult -> do
              -- ensure we include the events and logs from buyGas and redeemGas in the result
              return $ Right $ CommandResult
                { _crReqKey = RequestKey $ _cmdHash cmd
                , _crTxId = _erTxId payloadResult
                , _crResult =
                    -- TODO: don't use `last` here for GHC 9.10 compat
                    PactResultOk $ compileValueToPactValue $ last $ _erOutput payloadResult
                , _crGas = gasUsed
                , _crLogs = Just $ _erLogs buyGasResult <> _erLogs payloadResult <> _erLogs redeemGasResult
                , _crContinuation = _erExec payloadResult
                , _crEvents = _erEvents buyGasResult <> _erEvents payloadResult <> _erEvents redeemGasResult
                , _crMetaData = Nothing
                }
  where
    !gasLimit = view (cmdPayload . pMeta . pmGasLimit) cmd

-- | Convert context to datatype for Pact environment using the
-- current blockheight, referencing the parent header (not grandparent!)
-- hash and blocktime data.
--
ctxToPublicData :: PublicMeta -> TxContext -> PublicData
ctxToPublicData pm (TxContext ph _) = PublicData
    { _pdPublicMeta = pm
    , _pdBlockHeight = bh
    , _pdBlockTime = bt
    , _pdPrevBlockHash = toText h
    }
  where
    bheader = _parentHeader ph
    BlockHeight !bh = succ $ view blockHeight bheader
    BlockCreationTime (Time (TimeSpan (Micros !bt))) =
      view blockCreationTime bheader
    BlockHash h = view blockHash bheader

-- | 'applyCoinbase' performs upgrade transactions and constructs and executes
-- a transaction which pays miners their block reward.
applyCoinbase
    :: (Logger logger)
    => logger
      -- ^ Pact logger
    -> PactDb CoreBuiltin Info
      -- ^ Pact db environment
    -> Decimal
      -- ^ Miner reward
    -> TxContext
      -- ^ tx metadata and parent header
    -> IO (Either Pact5CoinbaseError (CommandResult [TxLog ByteString] Void))
applyCoinbase logger db reward txCtx = do
  -- for some reason this is the base64-encoded hash, rather than the binary hash
  let coinbaseHash = Hash $ SB.toShort $ T.encodeUtf8 $ blockHashToText parentBlockHash
  -- applyCoinbase is when upgrades happen, so we call applyUpgrades first
  applyUpgrades logger db txCtx
  -- we construct the coinbase term and evaluate it
  freeGasEnv <- mkFreeGasEnv GasLogsDisabled
  let
    (coinbaseTerm, coinbaseData) = mkCoinbaseTerm mid mks reward
    defaultFlags = Set.singleton FlagDisableRuntimeRTC
    flags = Set.unions [defaultFlags, guardDisablePact52And53Flags txCtx]
  eCoinbaseTxResult <-
    evalExecTerm Transactional
      db noSPVSupport freeGasEnv flags SimpleNamespacePolicy
      (ctxToPublicData noPublicMeta txCtx)
      MsgData
        { mdHash = coinbaseHash
        , mdData = coinbaseData
        , mdSigners = []
        , mdVerifiers = []
        }
      -- applyCoinbase injects a magic capability to get permission to mint tokens
      (emptyCapState & csSlots .~ [CapSlot (coinCap "COINBASE" []) []])
      -- TODO: better info here might be very valuable for debugging
      (noSpanInfo <$ coinbaseTerm)
  case eCoinbaseTxResult of
    Left err -> do
      pure $ Left $ CoinbasePactError err
    Right coinbaseTxResult -> do
      return $! Right $! CommandResult
        { _crReqKey = RequestKey coinbaseHash
        , _crTxId = _erTxId coinbaseTxResult
        , _crResult =
          -- TODO: don't use `last` for GHC 9.10 compat
          PactResultOk $ compileValueToPactValue $ last $ _erOutput coinbaseTxResult
        , _crGas = _erGas coinbaseTxResult
        , _crLogs = Just $ _erLogs coinbaseTxResult
        , _crContinuation = _erExec coinbaseTxResult
        , _crMetaData = Nothing
        , _crEvents = _erEvents coinbaseTxResult
        }

  where
  parentBlockHash = view blockHash $ _parentHeader $ _tcParentHeader txCtx
  Miner mid mks = _tcMiner txCtx

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
  => logger
  -> PactDb CoreBuiltin Info
  -> TxContext
  -> IO ()
applyUpgrades logger db txCtx
    | Just Pact4Upgrade{} <-
        v ^? versionUpgrades . atChain cid . ix currentHeight = error "Expected Pact 4 upgrade, got Pact 5"
    | Just Pact5Upgrade{_pact5UpgradeTransactions = upgradeTxs} <-
        v ^? versionUpgrades . atChain cid . ix currentHeight = applyUpgrade upgradeTxs
     | otherwise = return ()
  where
    v = _chainwebVersion txCtx
    currentHeight = ctxCurrentBlockHeight txCtx
    cid = _chainId txCtx
    applyUpgrade :: [Transaction] -> IO ()
    applyUpgrade upgradeTxs = do
      forM_ upgradeTxs $ \tx ->
        tryAllSynchronous (runUpgrade logger db txCtx (view payloadObj <$> tx)) >>= \case
          Right _ -> pure ()
          Left e -> do
            logError_ logger $ "Upgrade transaction failed! " <> sshow e
            throwM e

-- | Run a genesis transaction. This differs from an ordinary transaction:
--   * Special capabilities allow making token allocations
--   * Coinbase is allowed, which allows minting as well
--   * Gas is free
--   * Any failures are fatal to PactService
runGenesisPayload
  :: Logger logger
  => logger
  -> PactDb CoreBuiltin Info
  -> SPVSupport
  -> TxContext
  -> Command (Payload PublicMeta ParsedCode)
  -> IO (Either (Pact5.PactError Info) (CommandResult [TxLog ByteString] Void))
runGenesisPayload logger db spv ctx cmd = do
  gasRef <- newIORef (MilliGas 0)
  let gasEnv = GasEnv gasRef Nothing freeGasModel
  let txEnv = TransactionEnv logger gasEnv
  -- Todo gas logs
  freeGasEnv <- mkFreeGasEnv GasLogsDisabled
  let defaultFlags = Set.fromList [FlagDisableRuntimeRTC]
      flags = Set.unions [defaultFlags, guardDisablePact52And53Flags ctx, guardDisablePact54Flags ctx]
  runExceptT
    (runReaderT
      (runTransactionM
        (runPayload
          Transactional
          flags
          db
          spv
          [ CapToken (QualifiedName "GENESIS" (ModuleName "coin" Nothing)) []
          , CapToken (QualifiedName "COINBASE" (ModuleName "coin" Nothing)) []
          ]
          -- allow installing to root namespace
          SimpleNamespacePolicy
          freeGasEnv
          ctx
          (TxBlockIdx 0)
          cmd <&> \evalResult ->
            CommandResult
              { _crReqKey = RequestKey (_cmdHash cmd)
              , _crTxId = _erTxId evalResult
              , _crResult = PactResultOk (compileValueToPactValue $ last $ _erOutput evalResult)
              , _crGas = _erGas evalResult
              , _crLogs = Just $ _erLogs evalResult
              , _crContinuation = _erExec evalResult
              , _crMetaData = Nothing
              , _crEvents = _erEvents evalResult
              }
        )
      ) txEnv
    )

runPayload
    :: (Logger logger)
    => ExecutionMode
    -> Set ExecutionFlag
    -> PactDb CoreBuiltin Info
    -> SPVSupport
    -> [CapToken QualifiedName PactValue]
    -> NamespacePolicy
    -> GasEnv CoreBuiltin Info
    -> TxContext
    -> TxIdxInBlock
    -> Command (Payload PublicMeta ParsedCode)
    -> TransactionM logger EvalResult
runPayload execMode execFlags db spv specialCaps namespacePolicy gasEnv txCtx txIdxInBlock cmd = do
    -- Note [Throw out verifier proofs eagerly]
  let !verifiersWithNoProof =
          (fmap . fmap) (\_ -> ()) verifiers
          `using` (traverse . traverse) rseq

  result <- (either throwError return =<<) $ liftIO $
    case payload ^. pPayload of
      Exec ExecMsg {..} ->
        evalExec (RawCode (_pcCode _pmCode)) execMode
          db spv gasEnv execFlags namespacePolicy
          (ctxToPublicData publicMeta txCtx)
          MsgData
            { mdHash = _cmdHash cmd
            , mdData = _pmData
            , mdVerifiers = verifiersWithNoProof
            , mdSigners = signers
            }
          (emptyCapState
            & csSlots .~ [CapSlot cap [] | cap <- specialCaps])
          -- Note: we delete the information here from spans for
          -- smaller db footprints
          (_pcExps _pmCode)
      Continuation ContMsg {..} ->
        evalContinuation execMode
          db spv gasEnv execFlags namespacePolicy
          (ctxToPublicData publicMeta txCtx)
          MsgData
            { mdHash = _cmdHash cmd
            , mdData = _cmData
            , mdSigners = signers
            , mdVerifiers = verifiersWithNoProof
            }
          (emptyCapState
            & csSlots .~ [CapSlot cap [] | cap <- specialCaps])
          Cont
              { _cPactId = _cmPactId
              , _cStep = _cmStep
              , _cRollback = _cmRollback
              , _cProof = _cmProof
              }

  case maybeQuirkGasFee of
    Nothing -> return result
    Just quirkGasFee -> do
      let convertedQuirkGasFee = Gas $ fromIntegral quirkGasFee
      liftIO $ writeIORef (_geGasRef gasEnv) $ gasToMilliGas convertedQuirkGasFee
      return result { _erGas = convertedQuirkGasFee }

  where
    payload = cmd ^. cmdPayload
    verifiers = payload ^. pVerifiers . _Just
    signers = payload ^. pSigners
    publicMeta = cmd ^. cmdPayload . pMeta
    v = _chainwebVersion txCtx
    cid = _chainId txCtx
    maybeQuirkGasFee = v ^? versionQuirks . quirkGasFees . ixg cid . ix (ctxCurrentBlockHeight txCtx, txIdxInBlock)

runUpgrade
    :: (Logger logger)
    => logger
    -> PactDb CoreBuiltin Info
    -> TxContext
    -> Command (Payload PublicMeta ParsedCode)
    -> IO ()
runUpgrade _logger db txContext cmd = case payload ^. pPayload of
    Exec pm -> do
      freeGasEnv <- mkFreeGasEnv GasLogsDisabled
      let flags = Set.unions
            [ Set.singleton FlagDisableRuntimeRTC
            , guardDisablePact52And53Flags txContext
            ]
      evalExec (RawCode (_pcCode (_pmCode pm))) Transactional
        db noSPVSupport freeGasEnv flags
        -- allow installing to root namespace
        SimpleNamespacePolicy
        (ctxToPublicData publicMeta txContext)
        MsgData
          { mdHash = chash
          , mdData = PObject mempty
          , mdSigners = []
          , mdVerifiers = []
          }
        (emptyCapState
          & csSlots .~ [CapSlot (CapToken (QualifiedName "REMEDIATE" (ModuleName "coin" Nothing)) []) []]
          & csModuleAdmin .~ S.singleton (ModuleName "coin" Nothing))
        (fmap (noSpanInfo <$) $ _pcExps $ _pmCode pm) >>= \case
        Left err -> internalError $ "Pact5.runGenesis: internal error " <> sshow err
        -- TODO: we should probably put these events somewhere!
        Right _r -> return ()
    Continuation _ -> error "runGenesisCore Continuation not supported"
  where
    payload = cmd ^. cmdPayload
    publicMeta = payload ^. pMeta
    chash = _cmdHash cmd

enrichedMsgBodyForGasPayer :: Map.Map Field PactValue -> Command (Payload PublicMeta ParsedCode) -> PactValue
enrichedMsgBodyForGasPayer dat cmd = case (_pPayload $ _cmdPayload cmd) of
  Exec exec ->
    PObject $ Map.fromList
      [ ("tx-type", PString "exec")
      -- As part of legacy compat, the "exec-code" field passes chunks of source code as a list
      -- in Pact 4, we used the pretty instance (O_O) for this.
      -- In Pact 5, what we instead do, is we use the lexer/parser generated `SpanInfo` to
      -- then slice the section of code each `TopLevel` uses.
      , ("exec-code", PList (Vector.fromList (fmap (PString . sliceFromSourceLines codeLines) expInfos)))
      , ("exec-user-data", _pmData exec)
      ] `Map.union` dat
    where
    expInfos = fmap (view Lisp.topLevelInfo) $ _pcExps $ _pmCode exec
    codeLines = T.lines (_pcCode (_pmCode exec))
  Continuation cont ->
    PObject $ Map.fromList
      [ ("tx-type", PString "cont")
      , ("cont-pact-id", PString (_defPactId (_cmPactId cont)))
      , ("cont-step", PInteger (fromIntegral (_cmStep cont)))
      , ("cont-is-rollback", PBool (_cmRollback cont))
      , ("cont-user-data", _cmData cont)
      , ("cont-has-proof", PBool (isJust (_cmProof cont)))
      ] `Map.union` dat

-- | Build and execute 'coin.buygas' command from miner info and user command
-- info (see 'TransactionExec.applyCmd' for more information).
--
-- see: 'pact/coin-contract/coin.pact#fund-tx' and 'pact/coin-contract/coin.pact#buy-gas'
--
buyGas
  :: (Logger logger)
  => logger
  -> GasEnv CoreBuiltin Info
  -> PactDb CoreBuiltin Info
  -> TxContext
  -> Command (Payload PublicMeta ParsedCode)
  -> IO (Either Pact5BuyGasError EvalResult)
buyGas logger origGasEnv db txCtx cmd = do
  let gasEnv = origGasEnv & geGasModel . gmGasLimit .~ Just (MilliGasLimit (MilliGas 1_500_000))
  logFunctionText logger L.Debug $
    "buying gas for " <> sshow (_cmdHash cmd)
  -- TODO: use quirked gas?
  let gasPayerCaps =
        [ cap
        | signer <- signers
        , SigCapability cap <- _siCapList signer
        , _qnName (_ctName cap) == "GAS_PAYER"
        ]
  let gasLimit = publicMeta ^. pmGasLimit
  let supply = gasSupplyOf (gasLimit ^. _GasLimit) gasPrice
  let (buyGasTerm, buyGasData) =
        if isChainweb224Pact
        then mkBuyGasTerm sender supply
        else mkFundTxTerm mid mks sender supply
      flags = Set.unions [Set.singleton FlagDisableRuntimeRTC, guardDisablePact52And53Flags txCtx]

  runExceptT $ do
    gasPayerCap <- case gasPayerCaps of
      [gasPayerCap] -> pure (Just gasPayerCap)
      [] -> pure Nothing
      _ -> do
        throwError BuyGasMultipleGasPayerCaps

    -- Todo: fix once pact-5 fixes the signature of evalGasPayercap.
    -- It's taking an `Expr Info` instead of `Expr SpanInfo` which is making this code not compile
    e <- liftIO $ case gasPayerCap of
      Just cap ->
        evalGasPayerCap cap db noSPVSupport gasEnv flags SimpleNamespacePolicy
          (ctxToPublicData publicMeta txCtx)
          MsgData
          -- Note: in the case of gaspayer, buyGas is given extra metadata that comes from
          -- the Command
          { mdData = maybe (PObject buyGasData) (const $ enrichedMsgBodyForGasPayer buyGasData cmd) gasPayerCap
          , mdHash = bgHash
          , mdSigners = signersWithDebit
          -- no verifiers are allowed in buy gas
          , mdVerifiers = []
          }
          (emptyCapState & csSlots .~ [CapSlot (coinCap "GAS" []) []])
          (noSpanInfo <$ buyGasTerm)
      Nothing ->
        evalExecTerm Transactional
        db
        noSPVSupport
        gasEnv
        flags
        SimpleNamespacePolicy
        (ctxToPublicData publicMeta txCtx)
        MsgData
          -- Note: in the case of gaspayer, buyGas is given extra metadata that comes from
          -- the Command
          { mdData = maybe (PObject buyGasData) (const $ enrichedMsgBodyForGasPayer buyGasData cmd) gasPayerCap
          , mdHash = bgHash
          , mdSigners = signersWithDebit
          -- no verifiers are allowed in buy gas
          , mdVerifiers = []
          }
        (emptyCapState & csSlots .~ [CapSlot (coinCap "GAS" []) []])
        (noSpanInfo <$ buyGasTerm)

    case e of
      Right er' -> do
        case _erExec er' of
          Nothing
            | isChainweb224Pact ->
              return er'
            | otherwise ->
              -- should never occur pre-chainweb 2.24:
              -- would mean coin.fund-tx is not a pact
              internalError "buyGas: Internal error - empty continuation before 2.24 fork"
          Just _pe
            | isChainweb224Pact ->
              internalError "buyGas: Internal error - continuation found after 2.24 fork"
            | otherwise ->
              return er'
      Left err -> do
        throwError $ BuyGasPactError err

  where
    isChainweb224Pact = guardCtx chainweb224Pact txCtx
    publicMeta = cmd ^. cmdPayload . pMeta
    sender = publicMeta ^. pmSender
    gasPrice = publicMeta ^. pmGasPrice
    signers = cmd ^. cmdPayload . pSigners
    signedForGas signer =
      any (\(SigCapability sc) -> sc == coinCap "GAS" []) (_siCapList signer)
    addDebit signer
      | signedForGas signer =
        signer & siCapList %~ (SigCapability (coinCap "DEBIT" [PString sender]):)
      | otherwise = signer
    signersWithDebit =
      fmap addDebit signers

    Miner mid mks = _tcMiner txCtx

    Hash chash = _cmdHash cmd
    bgHash = Hash (chash <> "-buygas")

-- | Build and execute 'coin.redeem-gas' command from miner info and previous
-- command results (see 'TransactionExec.applyCmd')
--
redeemGas :: (Logger logger)
  => logger
  -> PactDb CoreBuiltin Info -> TxContext
  -> Gas
  -> Maybe DefPactId
  -> Command (Payload PublicMeta ParsedCode)
  -> IO (Either Pact5RedeemGasError EvalResult)
redeemGas logger db txCtx gasUsed maybeFundTxPactId cmd
    | isChainweb224Pact, Nothing <- maybeFundTxPactId = do
      logFunctionText logger L.Debug $
        "redeeming gas (post-2.24) for " <> sshow (_cmdHash cmd)
      -- if we're past chainweb 2.24, we don't use defpacts for gas; see 'pact/coin-contract/coin.pact#redeem-gas'
      let (redeemGasTerm, redeemGasData) = mkRedeemGasTerm mid mks sender gasTotal gasFee
          flags = Set.unions [Set.singleton FlagDisableRuntimeRTC, guardDisablePact52And53Flags txCtx]
      -- todo: gas logs
      freeGasEnv <- mkFreeGasEnv GasLogsDisabled
      evalExecTerm
        Transactional
        -- TODO: more execution flags?
        db noSPVSupport freeGasEnv flags SimpleNamespacePolicy
        (ctxToPublicData publicMeta txCtx)
        MsgData
          { mdData = redeemGasData
          , mdHash = rgHash
          , mdSigners = signers
          , mdVerifiers = []
          }
        (emptyCapState & csSlots .~ [CapSlot (coinCap "GAS" []) []])
        (noSpanInfo <$ redeemGasTerm) >>= \case
          Left unknownPactError -> do
            pure $ Left (RedeemGasPactError unknownPactError)
          Right evalResult -> do
            pure $ Right evalResult

    | not isChainweb224Pact, Just fundTxPactId <- maybeFundTxPactId = do
      freeGasEnv <- mkFreeGasEnv GasLogsDisabled
      logFunctionText logger L.Debug $
        "redeeming gas (pre-2.24) for " <> sshow (_cmdHash cmd)
      -- before chainweb 2.24, we use defpacts for gas; see: 'pact/coin-contract/coin.pact#fund-tx'
      let redeemGasData = PObject $ Map.singleton "fee" (PDecimal $ _pact5GasSupply gasFee)
          flags = Set.unions [Set.singleton FlagDisableRuntimeRTC, guardDisablePact52And53Flags txCtx]

      evalContinuation Transactional
        db noSPVSupport freeGasEnv flags SimpleNamespacePolicy
        (ctxToPublicData publicMeta txCtx)
        MsgData
          { mdData = redeemGasData
          , mdHash = rgHash
          , mdSigners = signers
          , mdVerifiers = []
          }
        (emptyCapState & csSlots .~ [CapSlot (coinCap "GAS" []) []])
        Cont
        { _cPactId = fundTxPactId
        , _cStep = 1
        , _cRollback = False
        , _cProof = Nothing
        } >>= \case
          Left err -> do
            pure $ Left (RedeemGasPactError err)
          Right evalResult -> do
            return $ Right evalResult

    | otherwise =
      internalError "redeemGas: Internal error - defpact ID does not match chainweb224Pact flag"

  where
    Hash chash = _cmdHash cmd
    rgHash = Hash (chash <> "-redeemgas")
    Miner mid mks = _tcMiner txCtx
    isChainweb224Pact = guardCtx chainweb224Pact txCtx
    publicMeta = cmd ^. cmdPayload . pMeta
    signers = cmd ^. cmdPayload . pSigners
    sender = publicMeta ^. pmSender
    gasFee = gasSupplyOf
      gasUsed
      (publicMeta ^. pmGasPrice)
    gasTotal = gasSupplyOf
      (publicMeta ^. pmGasLimit . _GasLimit)
      (publicMeta ^. pmGasPrice)


-- -- ---------------------------------------------------------------------------- --
-- -- Utilities

-- | Initial gas charged for transaction size
--   ignoring the size of a continuation proof, if present
--
initialGasOf :: PayloadWithText meta ParsedCode -> Gas
initialGasOf payload = Gas gasFee
  where
    feePerByte :: Rational = 0.01

    contProofSize =
      case payload ^. payloadObj . pPayload of
        Continuation (ContMsg _ _ _ _ (Just (ContProof p))) -> B.length p
        _ -> 0
    txSize = SB.length (payload ^. payloadBytes) - contProofSize

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

-- | Chainweb's namespace policy for ordinary transactions.
-- Doesn't allow installing modules in the root namespace.
managedNamespacePolicy :: NamespacePolicy
managedNamespacePolicy = SmartNamespacePolicy
  False
  (QualifiedName "validate" (ModuleName "ns" Nothing))


-- | Calculate the gas fee (pact-generate gas cost * user-specified gas price),
-- rounding to the nearest stu.
--
gasSupplyOf :: Gas -> GasPrice -> GasSupply
gasSupplyOf (Gas gas) (GasPrice gp) = GasSupply gs
  where
    gs = toCoinUnit (fromIntegral gas * gp)
{-# INLINE gasSupplyOf #-}

-- | Round to the nearest Stu
--
toCoinUnit :: Decimal -> Decimal
toCoinUnit = roundTo 12
{-# INLINE toCoinUnit #-}

dumpGasLogs :: (Logger logger)
  => Text -- ^ context
  -> Hash -- ^ Command Hash
  -> Maybe logger -- ^ Gas logger
  -> GasEnv CoreBuiltin Info
  -> IO ()
dumpGasLogs ctx txHash maybeGasLogger gasEnv = do
  forM_ ((,) <$> _geGasLog gasEnv <*> maybeGasLogger) $ \(gasLogRef, gasLogger) -> do
    gasLogs <- reverse <$> readIORef gasLogRef
    let prettyLogs = Pact5.prettyGasLogs (_geGasModel gasEnv) (_gleArgs <$> gasLogs)
    let logger = addLabel ("transactionExecContext", ctx) $ addLabel ("cmdHash", hashToText txHash) $ gasLogger
    logFunctionText logger L.Debug $ "gas logs: " <> prettyLogs

    -- After every dump, we clear the gas logs, so that each context only writes the gas logs it induced.
    writeIORef gasLogRef mempty

guardDisablePact51Flags :: TxContext -> Set ExecutionFlag
guardDisablePact51Flags txCtx
  | guardCtx chainweb228Pact txCtx = Set.empty
  | otherwise = Set.singleton FlagDisablePact51

guardDisablePact52And53Flags :: TxContext -> Set ExecutionFlag
guardDisablePact52And53Flags txCtx
  | guardCtx chainweb230Pact txCtx = Set.empty
  | otherwise = Set.fromList [FlagDisablePact52, FlagDisablePact53, FlagDisableReentrancyCheck]

guardDisablePact54Flags :: TxContext -> Set ExecutionFlag
guardDisablePact54Flags txCtx
  | guardCtx chainweb231Pact txCtx = Set.empty
  | otherwise = Set.fromList [FlagDisablePact54]
