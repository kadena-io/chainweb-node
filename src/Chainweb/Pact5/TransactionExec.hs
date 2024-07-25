{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
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
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
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
( -- * Transaction Execution
  TransactionM(..)
, TransactionEnv(..)
, TxFailedError(..)
, BuyGasError(..)

, applyCoinbase
, applyCmd
, buyGas
, runVerifiers
, runPayload
, redeemGas
, applyLocal

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
import qualified Data.Text.Encoding as T
import qualified System.LogLevel as L

-- internal Pact modules

-- import Pact.Eval (eval, liftTerm)
-- import Pact.Gas (freeGasEnv)
-- import Pact.Interpreter
import qualified Pact.JSON.Decode as J
import qualified Pact.JSON.Encode as J
import Pact.JSON.Legacy.Value
-- import Pact.Native.Capabilities (evalCap)
-- import Pact.Native.Internal (appToCap)
import Pact.Parse (ParsedDecimal(..), ParsedInteger(..))
-- import Pact.Runtime.Capabilities (popCapStack)
-- import Pact.Runtime.Utils (lookupModule)
-- import Pact.Types.Capability
-- import Pact.Types.Command
-- import Pact.Types.Hash as Pact
-- import Pact.Types.KeySet
-- import Pact.Types.PactValue
-- import Pact.Types.Pretty
-- import Pact.Types.RPC
-- import Pact.Types.Runtime hiding (catchesPactError)
-- import Pact.Types.Server
-- import Pact.Types.SPV
-- import Pact.Types.Verifier

-- import Pact.Types.Util as PU

import Pact.Core.Serialise.LegacyPact ()
import Pact.Core.Legacy.LegacyPactValue
import Pact.Core.Compile
import Pact.Core.Evaluate
import Pact.Core.Capabilities
import Pact.Core.Names
import Pact.Core.Namespace
import Pact.Core.Persistence.Types hiding (GasM(..))
import Pact.Core.Pretty
import Pact.Core.Gas
import Pact.Core.Hash
import Pact.Core.PactValue
import Pact.Core.Environment hiding (_chainId)
import Pact.Core.Builtin
import Pact.Core.Syntax.ParseTree
import Pact.Core.DefPacts.Types
import Pact.Core.Scheme
import Pact.Core.StableEncoding
import Pact.Core.SPV
import Pact.Core.Verifiers
import Pact.Core.Info

-- internal Chainweb modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Logger
import qualified Chainweb.ChainId as Chainweb
import Chainweb.Mempool.Mempool (requestKeyToTransactionHash, TransactionConfig (txCodec))
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.Types
import Chainweb.Pact5.Templates
import Chainweb.Pact.Utils
import qualified Chainweb.Pact.Conversion as PactConversion
import Chainweb.Pact.Types hiding (ctxCurrentBlockHeight, ctxToPublicData, TxContext(..), guardCtx)
import Chainweb.Pact5.Types
import Chainweb.Pact.Backend.Types
import Chainweb.Time
import Chainweb.Pact5.Transaction
import Chainweb.VerifierPlugin hiding (chargeGas)
import Chainweb.Utils
import Chainweb.Version as V
import Chainweb.Version.Guards as V
import Chainweb.Version.Utils as V
import Pact.JSON.Encode (toJsonViaEncode)

import qualified Debug.Trace as TRACE
import Pact.Core.Command.Types
import Data.ByteString (ByteString)
import Pact.Core.Gas.TableGasModel (tableGasModel)
import Pact.Core.Command.RPC
import Chainweb.Pact.Backend.ChainwebPactCoreDb (chainwebPactCoreDb)
import qualified Pact.Types.ChainId as Pact4
import qualified Pact.Types.Gas as Pact4
import qualified Pact.Parse as Pact4
import qualified Pact.Types.ChainMeta as Pact4
import Pact.Core.Errors
import qualified Data.Set as Set
import Debug.Trace
import Data.Set (Set)
import Control.Monad.Except (MonadError(..), liftEither)
import qualified Pact.Core.Syntax.ParseTree as Lisp
import Pact.Core.Command.Types (Payload(_pSigners))
import Pact.Core.StableEncoding
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.Pact.SPV (pact5SPV)
import Data.Void
import Control.Error (ExceptT, runExceptT)
import Chainweb.Utils.Serialization (runPutS)
import Data.Word
import Data.Int
import qualified Pact.Types.Verifier as Pact4
import qualified Pact.Types.Capability as Pact4
import qualified Pact.Types.Names as Pact4
import qualified Pact.Types.Runtime as Pact4

-- Note [Throw out verifier proofs eagerly]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We try to discard verifier proofs eagerly so that we don't hang onto them in
-- the liveset. This implies that we also try to discard `Command`s for the same
-- reason, because they contain the verifier proofs and other data we probably
-- don't need.

-- -------------------------------------------------------------------------- --

coinCap :: Text -> [PactValue] -> CapToken QualifiedName PactValue
coinCap n vs = CapToken (QualifiedName n (ModuleName "coin" Nothing)) vs

data TxFailedError = TxPactError (PactError Info) | TxVerifierError VerifierError
  deriving stock (Eq, Show)

data TransactionEnv logger = TransactionEnv
  { _txEnvLogger :: logger
  , _txEnvGasEnv :: GasEnv CoreBuiltin Info
  }

makeLenses ''TransactionEnv

-- | TODO: document how TransactionM is just for the "paid-for" fragment of the transaction flow
newtype TransactionM logger a
  = TransactionM { runTransactionM :: ReaderT (TransactionEnv logger) (ExceptT TxFailedError IO) a }
  deriving newtype
  ( Functor
  , Applicative
  , Monad
  , MonadReader (TransactionEnv logger)
  , MonadError TxFailedError
  , MonadThrow
  , MonadIO
  )

chargeGas :: Info -> GasArgs _ -> TransactionM _ ()
chargeGas info gasArgs = do
  gasEnv <- view txEnvGasEnv
  either (throwError . TxPactError) return =<<
    liftIO (chargeGasArgsM gasEnv info [] gasArgs)

-- run verifiers
-- nasty... perhaps later convert verifier plugins to use GasM instead of tracking "gas remaining"
-- TODO: Verifiers are also tied to Pact enough that this is going to be an annoying migration
runVerifiers :: Logger logger => TxContext -> Command (Payload PublicMeta c) -> TransactionM logger ()
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
                | CapToken n args <- _verifierCaps pact5Verifier
                ]
              }
              | pact5Verifier <- fromMaybe [] $ cmd ^. cmdPayload . pVerifiers
              ]
      verifierResult <- liftIO $ runVerifierPlugins
          (_chainwebVersion txCtx, _chainId txCtx, ctxCurrentBlockHeight txCtx) logger
          allVerifiers
          (Pact4.Gas $ fromIntegral @Word64 @Int64 $ _gas $ milliGasToGas $ initGasRemaining)
          pact4TxVerifiers
      case verifierResult of
        Left err -> do
          let errMsg = "Tx verifier error: " <> getVerifierError err
          throwError (TxVerifierError err)
        Right (Pact4.Gas pact4VerifierGasRemaining) -> do
          -- TODO: crash properly on negative?
          let verifierGasRemaining = fromIntegral @Int64 @Word64 pact4VerifierGasRemaining
          -- NB: this is not nice.
          -- TODO: better gas info here
          chargeGas def $ GAConstant $ gasToMilliGas $ Gas $
            verifierGasRemaining - min (_gas (milliGasToGas initGasRemaining)) verifierGasRemaining

applyLocal
    :: (Logger logger)
    => logger
      -- ^ Pact logger
    -> Maybe logger
      -- ^ Pact gas logger
    -> Pact5Db
      -- ^ Pact db environment
    -> TxContext
      -- ^ tx metadata and parent header
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command (Payload PublicMeta ParsedCode)
      -- ^ command with payload to execute
    -> IO (CommandResult [TxLog ByteString] TxFailedError)
applyLocal logger maybeGasLogger coreDb txCtx spvSupport cmd = do
  let !gasLimit = view (cmdPayload . pMeta . pmGasLimit) cmd
  gasRef <- newIORef mempty
  gasLogRef <- forM maybeGasLogger $ \_ -> newIORef []
  let runLocal = runVerifiers txCtx cmd *> runPayload Local coreDb spvSupport txCtx cmd
  let gasEnv = GasEnv
        { _geGasRef = gasRef
        , _geGasLog = gasLogRef
        , _geGasModel = tableGasModel (MilliGasLimit $ gasToMilliGas $ gasLimit ^. _GasLimit)
        }
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
      gasUsed <- milliGasToGas <$> readIORef gasRef
      let result = case reverse (_erOutput payloadResult) of
            x:_ -> x
            _ -> InterpretValue PUnit (def :: Info)
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
  localFlags = S.fromList
    [ FlagDisableRuntimeRTC
    , FlagEnforceKeyFormats
    -- Note: this is currently not conditional
    -- in pact-5 exec. This may change if it breaks
    -- anyone's workflow
    , FlagAllowReadInLocal
    , FlagRequireKeysetNs]

-- | The main entry point to executing transactions. From here,
-- 'applyCmd' assembles the command environment for a command,
-- purchases gas for the command, executes the command, and
-- redeems leftover gas.
--
applyCmd
    :: (Logger logger)
    => logger
      -- ^ Pact logger
    -> Maybe logger
      -- ^ Pact gas logger
    -> Pact5Db
      -- ^ Pact db environment
    -> TxContext
      -- ^ tx metadata
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command (Payload PublicMeta ParsedCode)
      -- ^ command with payload to execute
    -> Gas
      -- ^ initial gas used
    -> IO (CommandResult [TxLog ByteString] TxFailedError)
applyCmd logger maybeGasLogger pact5Db txCtx spv cmd initialGas = do
  let !requestKey = cmdToRequestKey cmd
  -- this process is "paid for", i.e. it's powered by a supply of gas that was
  -- purchased by a user already. any errors here will result in the entire gas
  -- supply being paid to the miner.
  let paidFor buyGasResult = do
        -- TODO: better "info" for this, for gas logs
        chargeGas def (GAConstant $ gasToMilliGas initialGas)
        -- the gas consumed by buying gas is itself paid for by the user
        -- TODO: better "info" for this, for gas logs
        chargeGas def (GAConstant $ gasToMilliGas $ _erGas buyGasResult)

        runVerifiers txCtx cmd

        -- run payload
        runPayload Transactional pact5Db spv txCtx cmd

  when (GasLimit initialGas > gasLimit) $
    throwM $ BuyGasFailure $ Pact5GasPurchaseFailure requestKey "tx too big for gas limit"

  catchesPact5Error logger (buyGas logger pact5Db txCtx cmd) >>= \case
    Left uknownPactError -> do
      throwM $ BuyGasFailure $ Pact5GasPurchaseFailure requestKey (T.pack $ displayException uknownPactError)
    Right (Left (BuyGasPactError e)) -> do
      throwM $ BuyGasFailure $ Pact5GasPurchaseFailure requestKey (T.pack $ displayException e)
    Right (Left BuyGasMultipleGasPayerCaps) -> do
      throwM $ BuyGasFailure $ Pact5GasPurchaseFailure requestKey "multiple GAS_PAYER capabilities"
    Right (Right buyGasResult) -> do
      gasRef <- newIORef (MilliGas 0)
      gasLogRef <- forM maybeGasLogger $ \_ ->
        newIORef []
      let gasEnv = GasEnv
            { _geGasRef = gasRef
            , _geGasLog = gasLogRef
            , _geGasModel = tableGasModel (MilliGasLimit $ gasToMilliGas $ gasLimit ^. _GasLimit)
            }
      let txEnv = TransactionEnv
            { _txEnvGasEnv = gasEnv
            , _txEnvLogger = logger
            }
      runExceptT (runReaderT (runTransactionM (paidFor buyGasResult)) txEnv) >>= \case
        Left err -> do
          -- if any error occurs after buying gas, the output of the command is that error
          -- and all of the gas is sent to the miner.
          -- only buying gas and sending it to the miner are recorded.
          redeemGasResult <- redeemGas
            logger pact5Db txCtx
            (gasLimit ^. _GasLimit)
            (_peDefPactId <$> _erExec buyGasResult)
            cmd

          return CommandResult
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
          gasUsed <- milliGasToGas <$> readIORef gasRef
          -- immediately return all unused gas to the user and send all used
          -- gas to the miner.
          redeemGasResult <- redeemGas
            logger pact5Db txCtx
            gasUsed
            (_peDefPactId <$> _erExec buyGasResult)
            cmd
          -- ensure we include the events and logs from buyGas and redeemGas
          -- in the result
          return CommandResult
            { _crReqKey = RequestKey $ _cmdHash cmd
            , _crTxId = _erTxId payloadResult
            , _crResult =
              PactResultOk $ compileValueToPactValue $ last (_erOutput payloadResult)
            , _crGas = gasUsed
            , _crLogs = Just $ _erLogs buyGasResult <> _erLogs payloadResult <> _erLogs redeemGasResult
            , _crContinuation = _erExec payloadResult
            , _crEvents = _erEvents buyGasResult <> _erEvents payloadResult <> _erEvents redeemGasResult
            , _crMetaData = Nothing
            }
  where
    v = _chainwebVersion txCtx
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
    BlockHeight !bh = succ $ _blockHeight bheader
    BlockCreationTime (Time (TimeSpan (Micros !bt))) =
      _blockCreationTime bheader
    BlockHash h = _blockHash bheader

-- | 'applyCoinbase' performs upgrade transactions and constructs and executes
-- a transaction which pays miners their block reward.
applyCoinbase
    :: (Logger logger)
    => logger
      -- ^ Pact logger
    -> Pact5Db
      -- ^ Pact db environment
    -> Decimal
      -- ^ Miner reward
    -> TxContext
      -- ^ tx metadata and parent header
    -> IO (CommandResult [TxLog ByteString] Void)
applyCoinbase logger pact5Db reward txCtx = do
  -- for some reason this is the base64-encoded hash, rather than the binary hash
  let coinbaseHash = Hash $ SB.toShort $ T.encodeUtf8 $ blockHashToText parentBlockHash
  -- applyCoinbase is when upgrades happen, so we call applyUpgrades first
  applyUpgrades logger pact5Db txCtx
  -- we construct the coinbase term and evaluate it
  let
    (coinbaseTerm, coinbaseData) = mkCoinbaseTerm mid mks reward
  coinbaseTxResult <-
    either (throwM . CoinbaseFailure . sshow) return . join =<< catchesPact5Error logger
    (evalExec Transactional
      pact5Db noSPVSupport freeGasModel (Set.fromList [FlagDisableRuntimeRTC]) managedNamespacePolicy
      (ctxToPublicData def txCtx)
      MsgData
        { mdHash = coinbaseHash
        , mdData = coinbaseData
        , mdSigners = []
        , mdVerifiers = []
        }
      -- applyCoinbase injects a magic capability to get permission to mint tokens
      (def & csSlots .~ [CapSlot (coinCap "COINBASE" []) []])
      -- TODO: better info here might be very valuable for debugging
      [ def <$ TLTerm coinbaseTerm ]
    )

  return CommandResult
    { _crReqKey = RequestKey coinbaseHash
    , _crTxId = _erTxId coinbaseTxResult
    , _crResult =
      PactResultOk $ compileValueToPactValue $ last $ _erOutput coinbaseTxResult
    , _crGas = _erGas coinbaseTxResult
    , _crLogs = Just $ _erLogs coinbaseTxResult
    , _crContinuation = _erExec coinbaseTxResult
    , _crMetaData = Nothing
    , _crEvents = _erEvents coinbaseTxResult
    }

  where
  parentBlockHash = view blockHash $ _parentHeader $ _tcParentHeader txCtx
  Miner mid mks@(MinerKeys mk) = _tcMiner txCtx

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
  -> Pact5Db
  -> TxContext
  -> IO ()
applyUpgrades logger db txCtx
     | Just (ForPact5 upg) <- _chainwebVersion txCtx
          ^? versionUpgrades
          . onChain (_chainId txCtx)
          . ix (ctxCurrentBlockHeight txCtx)
         = applyUpgrade upg
     | otherwise = return ()
  where
    applyUpgrade :: PactUpgrade Pact5 -> IO ()
    applyUpgrade upg = do
      let payloads = map (fmap _payloadObj) $ _pact5UpgradeTransactions upg
      forM_ (_pact5UpgradeTransactions upg) $ \tx ->
        tryAllSynchronous (runUpgrade logger db txCtx (_payloadObj <$> tx)) >>= \case
          Right _ -> pure ()
          Left e -> do
            logError_ logger $ "Upgrade transaction failed! " <> sshow e
            throwM e

compileValueToPactValue :: CompileValue i -> PactValue
compileValueToPactValue = \case
  -- NOTE: this goes into outputs
  LoadedModule _modName modHash ->
    PString $
      "Loaded module " <> encodeB64UrlNoPaddingText (SB.fromShort $ unHash (_mhHash modHash))
  LoadedInterface _ifaceName ifaceHash ->
    PString $
      "Loaded interface " <> encodeB64UrlNoPaddingText (SB.fromShort $ unHash (_mhHash ifaceHash))
  LoadedImports imp ->
    PString $ "Import successful"
  InterpretValue value _info ->
    value

runPayload
    :: forall logger err
    . (Logger logger)
    => ExecutionMode
    -> Pact5Db
    -> SPVSupport
    -> TxContext
    -> Command (Payload PublicMeta ParsedCode)
    -> TransactionM logger EvalResult
runPayload execMode pact5Db spv txCtx cmd = do

    -- Note [Throw out verifier proofs eagerly]
  let !verifiersWithNoProof =
          (fmap . fmap) (\_ -> ()) verifiers
          `using` (traverse . traverse) rseq

  res <- case _pPayload (_cmdPayload cmd) of
    Exec ExecMsg {..} -> do
      either (throwError . TxPactError) return =<< catchUnknownExceptions
        (evalExec execMode
          pact5Db spv gm (Set.fromList [FlagDisableRuntimeRTC]) managedNamespacePolicy
          (ctxToPublicData publicMeta txCtx)
          MsgData
            { mdHash = _cmdHash cmd
            , mdData = _pmData
            , mdVerifiers = verifiersWithNoProof
            , mdSigners = signers
            }
          (def :: CapState _ _)
          -- TODO: better info here might be very valuable for debugging
          [ def <$ exp | exp <- _pcExps _pmCode ]
        )
    Continuation ContMsg {..} -> do
      either (throwError . TxPactError) return =<< catchUnknownExceptions
        (evalContinuation execMode
          pact5Db spv gm (Set.fromList [FlagDisableRuntimeRTC]) managedNamespacePolicy
          (ctxToPublicData publicMeta txCtx)
          MsgData
            { mdHash = _cmdHash cmd
            , mdData = _cmData
            , mdSigners = signers
            , mdVerifiers = verifiersWithNoProof
            }
          (def :: CapState _ _)
          Cont
            { _cPactId = _cmPactId
            , _cStep = _cmStep
            , _cRollback = _cmRollback
            , _cProof = _cmProof
            }
        )

  chargeGas def (GAConstant (gasToMilliGas $ _erGas res))
  return res

  where
    catchUnknownExceptions :: IO a -> TransactionM logger a
    catchUnknownExceptions act = do
      logger <- view txEnvLogger
      either (throwError . TxPactError) return =<<
        liftIO (catchesPact5Error @IO logger act)
    verifiers = fromMaybe [] $ _pVerifiers $ _cmdPayload cmd
    signers = _pSigners $ _cmdPayload cmd
    -- chash = toUntypedHash $ _cmdHash cmd
    payload = _pPayload $ _cmdPayload cmd
    publicMeta = cmd ^. cmdPayload . pMeta
    gasLimit :: Gas = publicMeta ^. pmGasLimit . _GasLimit
    gm = tableGasModel (MilliGasLimit (gasToMilliGas gasLimit))

runUpgrade
    :: (Logger logger)
    => logger
    -> Pact5Db
    -> TxContext
    -> Command (Payload PublicMeta ParsedCode)
    -> IO ()
runUpgrade logger pact5Db txContext cmd = case payload of
    Exec pm ->
      evalExec Transactional
        pact5Db noSPVSupport freeGasModel (Set.fromList [FlagDisableRuntimeRTC]) SimpleNamespacePolicy
        (ctxToPublicData publicMeta txContext)
        MsgData
          { mdHash = chash
          , mdData = PObject mempty
          , mdSigners = []
          , mdVerifiers = []
          }
        (def
          & csSlots .~ [CapSlot (CapToken (QualifiedName "REMEDIATE" (ModuleName "coin" Nothing)) []) []]
          & csModuleAdmin .~ S.singleton (ModuleName "coin" Nothing))
        (fmap (def <$) $ _pcExps $ _pmCode pm) >>= \case
        Left err -> internalError $ "Pact5.runGenesis: internal error " <> sshow err
        Right _r -> return ()
    Continuation _ -> error "runGenesisCore Continuation not supported"
  where
    publicMeta = cmd ^. cmdPayload . pMeta
    signers = _pSigners $ _cmdPayload cmd
    chash = _cmdHash cmd
    payload = _pPayload $ _cmdPayload cmd

data BuyGasError
  = BuyGasPactError (PactError Info)
  | BuyGasMultipleGasPayerCaps
  deriving stock (Eq, Show)

-- | Build and execute 'coin.buygas' command from miner info and user command
-- info (see 'TransactionExec.applyCmd' for more information).
--
-- see: 'pact/coin-contract/coin.pact#fund-tx' and 'pact/coin-contract/coin.pact#buy-gas'
--
buyGas
  :: (Logger logger)
  => logger
  -> PactDb CoreBuiltin Info
  -> TxContext
  -> Command (Payload PublicMeta a)
  -> IO (Either BuyGasError EvalResult)
buyGas logger db txCtx cmd = do
  -- TODO: use quirked gas?
  let gasPayerCaps =
        [ cap
        | signer <- signers
        , cap <- _siCapList signer
        , _qnName (_ctName cap) == "GAS_PAYER"
        ]
  let gasLimit = publicMeta ^. pmGasLimit
  let supply = gasSupplyOf (gasLimit ^. _GasLimit) gasPrice
  let (buyGasTerm, buyGasData) =
        if isChainweb224Pact
        then mkBuyGasTerm sender supply
        else mkFundTxTerm mid mks sender supply

  runExceptT $ do
    eval <- case gasPayerCaps of
      [gasPayerCap] -> do
        pure $ evalGasPayerCap gasPayerCap
      [] -> do
        pure $ evalExecTerm Transactional
      _ -> do
        throwError BuyGasMultipleGasPayerCaps

    e <- liftIO $ eval
      -- TODO: magic constant, 1500 max gas limit for buyGas?
      db
      noSPVSupport
      (tableGasModel (MilliGasLimit $ gasToMilliGas $ min (Gas 3000) (gasLimit ^. _GasLimit)))
      (Set.fromList [FlagDisableRuntimeRTC]) SimpleNamespacePolicy
      (ctxToPublicData publicMeta txCtx)
      -- no verifiers are allowed in buy gas
      MsgData
        { mdData = buyGasData
        , mdHash = bgHash
        , mdSigners = signersWithDebit
        , mdVerifiers = []
        }
      (def & csSlots .~ [CapSlot (coinCap "GAS" []) []])
      (def <$ buyGasTerm)

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
          Just pe
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
      any (\sc -> sc == coinCap "GAS" []) (_siCapList signer)
    addDebit signer
      | signedForGas signer =
        signer & siCapList %~ (coinCap "DEBIT" [PString sender]:)
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
  -> Command (Payload PublicMeta ParsedCode) -> IO EvalResult
redeemGas logger pactDb txCtx gasUsed maybeFundTxPactId cmd
    | isChainweb224Pact, Nothing <- maybeFundTxPactId = do
      -- if we're past chainweb 2.24, we don't use defpacts for gas; see 'pact/coin-contract/coin.pact#redeem-gas'
      let (redeemGasTerm, redeemGasData) = mkRedeemGasTerm mid mks sender gasTotal gasFee
      evalExec Transactional
        -- TODO: more execution flags?
        pactDb noSPVSupport freeGasModel (Set.fromList [FlagDisableRuntimeRTC]) SimpleNamespacePolicy
        (ctxToPublicData publicMeta txCtx)
        MsgData
          { mdData = redeemGasData
          , mdHash = rgHash
          , mdSigners = cmd ^. cmdPayload . pSigners
          , mdVerifiers = []
          }
        (def & csSlots .~ [CapSlot (coinCap "GAS" []) []])
        [TLTerm (def <$ redeemGasTerm)] >>= \case
        Right evalResult ->
          return evalResult
        Left err ->
          internalError $ "redeemGas: Internal error - " <> sshow err

    | not isChainweb224Pact, Just fundTxPactId <- maybeFundTxPactId = do
      -- before chainweb 2.24, we use defpacts for gas; see: 'pact/coin-contract/coin.pact#fund-tx'
      let redeemGasData = PObject $ Map.singleton "fee" (PDecimal $ _pact5GasSupply gasFee)
      evalContinuation Transactional
        pactDb noSPVSupport freeGasModel (Set.fromList [FlagDisableRuntimeRTC]) SimpleNamespacePolicy
        (ctxToPublicData publicMeta txCtx)
        MsgData
          { mdData = redeemGasData
          , mdHash = rgHash
          , mdSigners = cmd ^. cmdPayload . pSigners
          , mdVerifiers = []
          }
        (def & csSlots .~ [CapSlot (coinCap "GAS" []) []])
        Cont
        { _cPactId = fundTxPactId
        , _cStep = 1
        , _cRollback = False
        , _cProof = Nothing
        } >>= \case
        Left err ->
          internalError $ "redeemGas: Internal error - " <> sshow err
        Right evalResult ->
          return evalResult

    | otherwise =
      internalError "redeemGas: Internal error - defpact ID does not match chainweb224Pact flag"

  where
    Hash chash = _cmdHash cmd
    rgHash = Hash (chash <> "-redeemgas")
    Miner mid mks = _tcMiner txCtx
    isChainweb224Pact = guardCtx chainweb224Pact txCtx
    publicMeta = cmd ^. cmdPayload . pMeta
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
initialGasOf :: PayloadWithText -> Gas
initialGasOf payload = Gas gasFee
  where
    feePerByte :: Rational = 0.01

    contProofSize =
      case _pPayload (_payloadObj payload) of
        Continuation (ContMsg _ _ _ _ (Just (ContProof p))) -> B.length p
        _ -> 0
    txSize = SB.length (_payloadBytes payload) - contProofSize

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
gasSupplyOf :: Gas -> GasPrice -> Pact5GasSupply
gasSupplyOf (Gas gas) (GasPrice gp) = Pact5GasSupply gs
  where
    gs = toCoinUnit (fromIntegral gas * gp)
{-# INLINE gasSupplyOf #-}

-- | Round to the nearest Stu
--
toCoinUnit :: Decimal -> Decimal
toCoinUnit = roundTo 12
{-# INLINE toCoinUnit #-}
