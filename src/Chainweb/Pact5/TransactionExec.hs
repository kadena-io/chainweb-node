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

, applyCmd
, buyGas
, runPayload
, redeemGas


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
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.Pact.SPV (pact5SPV)
import Data.Void
import Control.Error (ExceptT, runExceptT)

-- Note [Throw out verifier proofs eagerly]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We try to discard verifier proofs eagerly so that we don't hang onto them in
-- the liveset. This implies that we also try to discard `Command`s for the same
-- reason, because they contain the verifier proofs and other data we probably
-- don't need.

-- -------------------------------------------------------------------------- --

-- newtype TransactionM b = TransactionM
--   { runTransactionM :: GasM b Int Int

--   }

-- -- | "Magic" capability 'COINBASE' used in the coin contract to
-- -- constrain coinbase calls.
-- --
-- magic_COINBASE :: CapSlot SigCapability
-- magic_COINBASE = mkMagicCapSlot "COINBASE"

coinCap :: Text -> [PactValue] -> CapToken QualifiedName PactValue
coinCap n vs = CapToken (QualifiedName n (ModuleName "coin" Nothing)) vs

data TxFailedError = TxPactError (PactError Info) | TxVerifierError VerifierError
  deriving stock (Eq, Show)

data TransactionEnv logger = TransactionEnv
  { _txEnvLogger :: logger
  , _txEnvGasEnv :: GasEnv CoreBuiltin Info
  }

makeLenses ''TransactionEnv

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

-- -- | Check whether the cost of running a tx is more than the allowed
-- -- gas limit and do some action depending on the outcome
-- --
-- checkTooBigTx
--     :: (Logger logger)
--     => Gas
--     -> GasLimit
--     -> TransactionM logger p (CommandResult [TxLogJson])
--     -> (CommandResult [TxLogJson] -> TransactionM logger p (CommandResult [TxLogJson]))
--     -> TransactionM logger p (CommandResult [TxLogJson])
-- checkTooBigTx initialGas gasLimit next onFail
--   | initialGas >= fromIntegral gasLimit = do

--       let !pe = PactError GasError def []
--             $ "Tx too big (" <> pretty initialGas <> "), limit "
--             <> pretty gasLimit

--       r <- failTxWith pe "Tx too big"
--       onFail r
--   | otherwise = next


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
    -> TxContext
      -- ^ tx metadata
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command (Payload PublicMeta ParsedCode)
      -- ^ command with payload to execute
    -> Gas
      -- ^ initial gas used
    -> IO (CommandResult [TxLog ByteString] TxFailedError)
applyCmd v logger maybeGasLogger coreDb txCtx spv cmd initialGas = do
  let !requestKey = cmdToRequestKey cmd
  -- this process is "paid for", i.e. it's powered by a supply of gas that was
  -- purchased by a user already.
  let paidFor buyGasResult = do
        -- TODO: better "info" for this, for gas logs
        chargeGas def (GAConstant $ gasToMilliGas initialGas)
        -- the gas consumed by buying gas is itself paid for by the user
        chargeGas def (GAConstant $ gasToMilliGas $ _erGas buyGasResult)

        -- run verifiers
        -- do
          -- nasty... perhaps later convert verifier plugins to use GasM instead of tracking "gas remaining"
          -- TODO: Verifiers are also tied to Pact enough that this is going to be an annoying migration
          -- gasUsed <- liftIO . readIORef . _geGasRef =<< ask
          -- let initGasRemaining = fromIntegral gasLimit - gasUsed
          -- verifierResult <- liftIO $ runVerifierPlugins (ctxVersion txCtx, cid, currHeight) logger allVerifiers initGasRemaining cmd
          -- case verifierResult of
          --   Left err -> do
          --     let errMsg = "Tx verifier error: " <> getVerifierError err
          --     throwError
          --       (TxVerifierError err)
          --   Right verifierGasRemaining -> do
          --     chargeMilliGasM $ gasToMilliGas $ initGasRemaining - verifierGasRemaining

        -- run payload
        runPayload coreDb spv txCtx cmd

  catchesPact5Error logger (buyGas logger coreDb txCtx cmd) >>= \case
    Left e ->
      throwM $ BuyGasFailure $ Pact5GasPurchaseFailure requestKey e
    Right buyGasResult -> do
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
          redeemGasResult <- redeemGas logger coreDb txCtx (gasLimit ^. _GasLimit) (_peDefPactId <$> _erExec buyGasResult) cmd

          return CommandResult
            { _crReqKey = RequestKey $ _cmdHash cmd
            , _crTxId = Nothing
            , _crResult = PactResultErr err
            -- all gas is used when a command fails
            , _crGas = cmd ^. cmdPayload . pMeta . pmGasLimit . _GasLimit
            , _crLogs = Nothing
            , _crContinuation = Nothing
            , _crEvents = _erEvents buyGasResult <> _erEvents redeemGasResult
            , _crMetaData = Nothing
            }
        Right payloadResult -> do
          gasUsed <- milliGasToGas <$> readIORef gasRef
          redeemGasResult <- redeemGas logger coreDb txCtx gasUsed (_peDefPactId <$> _erExec buyGasResult) cmd
          return CommandResult
            { _crReqKey = RequestKey $ _cmdHash cmd
            , _crTxId = _erTxId payloadResult
            , _crResult = case last (_erOutput payloadResult) of
              -- NOTE that this goes into outputs
              LoadedModule _modName modHash ->
                PactResultOk $ PString $
                  "Loaded module " <> encodeB64UrlNoPaddingText (SB.fromShort $ unHash (_mhHash modHash))
              LoadedInterface _ifaceName ifaceHash ->
                PactResultOk $ PString $
                  "Loaded interface " <> encodeB64UrlNoPaddingText (SB.fromShort $ unHash (_mhHash ifaceHash))
              LoadedImports imp ->
                PactResultOk $ PString $ "Import successful"
              InterpretValue value _info ->
                PactResultOk value
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
    BlockHeight !bh = succ $ _blockHeight bheader
    BlockCreationTime (Time (TimeSpan (Micros !bt))) =
      _blockCreationTime bheader
    BlockHash h = _blockHash bheader

applyCoinbase
    :: (Logger logger)
    => ChainwebVersion
    -> logger
      -- ^ Pact logger
    -> CoreDb
      -- ^ Pact db environment
    -> Miner
      -- ^ The miner chosen to mine the block
    -> ParsedDecimal
      -- ^ Miner reward
    -> TxContext
      -- ^ tx metadata and parent header
    -> IO (CommandResult [TxLog ByteString] (PactError Info))
applyCoinbase v logger coreDb (Miner mid mks@(MinerKeys mk)) reward@(ParsedDecimal d) txCtx = do
  undefined

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
  -> CoreDb
  -> TxContext
  -> IO ()
applyUpgrades logger db txCtx
     | Just upg <- _chainwebVersion txCtx
          ^? versionPact5Upgrades
          . onChain (_chainId txCtx)
          . at (ctxCurrentBlockHeight txCtx)
          . _Just
         = applyUpgrade upg
     | otherwise = return ()
  where
    applyUpgrade :: Pact5Upgrade -> IO ()
    applyUpgrade upg = do
      let payloads = map (fmap _payloadObj) $ _pact5UpgradeTransactions upg
      forM_ (_pact5UpgradeTransactions upg) $ \tx ->
        tryAllSynchronous (runUpgrade logger db txCtx (_payloadObj <$> tx)) >>= \case
          Right _ -> pure ()
          Left e -> do
            logError_ logger $ "Upgrade transaction failed! " <> sshow e
            throwM e

runPayload
    :: forall logger err
    . (Logger logger)
    => CoreDb
    -> SPVSupport
    -> TxContext
    -> Command (Payload PublicMeta ParsedCode)
    -> TransactionM logger EvalResult
runPayload coreDb spv txCtx cmd = do

    -- Note [Throw out verifier proofs eagerly]
  let !verifiersWithNoProof =
          (fmap . fmap) (\_ -> ()) verifiers
          `using` (traverse . traverse) rseq

  res <- case _pPayload (_cmdPayload cmd) of
    Exec ExecMsg {..} -> do
      either (throwError . TxPactError) return =<< catchUnknownExceptions
        (evalExec
          coreDb spv gm (Set.fromList [FlagDisableRuntimeRTC]) managedNamespacePolicy
          (ctxToPublicData publicMeta txCtx)
          (initMsgData (_cmdHash cmd))
            { mdData = _pmData, mdVerifiers = verifiersWithNoProof }
          (def :: CapState _ _)
          -- TODO: better info here might be very valuable for debugging
          [ def <$ exp | exp <- _pcExps _pmCode ]
        )
    Continuation ContMsg {..} -> do
      either (throwError . TxPactError) return =<< catchUnknownExceptions
        (evalContinuation
          coreDb spv gm (Set.fromList [FlagDisableRuntimeRTC]) managedNamespacePolicy
          (ctxToPublicData publicMeta txCtx)
          (initMsgData (_cmdHash cmd))
            { mdData = _cmData, mdVerifiers = verifiersWithNoProof }
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
    -> CoreDb
    -> TxContext
    -> Command (Payload PublicMeta ParsedCode)
    -> IO ()
runUpgrade logger coreDb txContext cmd = case payload of
    Exec pm ->
      evalExec
        coreDb noSPVSupport freeGasModel (Set.fromList [FlagDisableRuntimeRTC]) SimpleNamespacePolicy
        (ctxToPublicData publicMeta txContext) (initMsgData chash)
        (def
          & csSlots .~ [CapSlot (CapToken (QualifiedName "REMEDIATE" (ModuleName "coin" Nothing)) []) []]
          & csModuleAdmin .~ S.singleton coinCoreModuleName)
        (fmap (def <$) $ _pcExps $ _pmCode pm) >>= \case
        Left err -> internalError $ "Pact5.runGenesis: internal error " <> sshow err
        Right _r -> return ()
    Continuation _ -> error "runGenesisCore Continuation not supported"
  where
    coinCoreModuleName = ModuleName "coin" Nothing
    publicMeta = cmd ^. cmdPayload . pMeta
    signers = _pSigners $ _cmdPayload cmd
    chash = _cmdHash cmd
    payload = _pPayload $ _cmdPayload cmd

-- | Build and execute 'coin.buygas' command from miner info and user command
-- info (see 'TransactionExec.applyCmd')
--
-- see: 'pact/coin-contract/coin.pact#fund-tx' and 'pact/coin-contract/coin.pact#buy-gas'
--
buyGas
  :: (Logger logger)
  => logger
  -> PactDb CoreBuiltin Info -> TxContext
  -> Command (Payload PublicMeta a) -> IO EvalResult
buyGas logger db txCtx cmd = do
  -- TODO: use quirked gas?
  eval <- case gasPayerCaps of
    [gasPayerCap] -> return $ evalGasPayerCap gasPayerCap
    [] -> return evalExecTerm
    _ -> internalError "buyGas: error - multiple gas payer caps"
  eval
    -- TODO: magic constant, 1500 max gas limit for buyGas?
    db noSPVSupport (tableGasModel (MilliGasLimit $ gasToMilliGas $ min (Gas 1500) (gasLimit ^. _GasLimit))) (Set.fromList [FlagDisableRuntimeRTC]) SimpleNamespacePolicy
    (ctxToPublicData publicMeta txCtx)
    -- no verifiers are allowed in buy gas
    (initMsgData bgHash) { mdData = buyGasData, mdSigners = signersWithDebit }
    (def & csSlots .~ [CapSlot (coinCap "GAS" []) []])
    (def <$ buyGasTerm) >>= \case
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
      internalError $ "buyGas: Internal error - " <> sshow err
  where
    Miner mid mks = _tcMiner txCtx
    isChainweb224Pact = guardCtx chainweb224Pact txCtx
    publicMeta = cmd ^. cmdPayload . pMeta
    sender = publicMeta ^. pmSender
    gasLimit = publicMeta ^. pmGasLimit
    gasPrice = publicMeta ^. pmGasPrice
    signers = cmd ^. cmdPayload . pSigners
    gasPayerCaps =
      [ cap
      | signer <- signers
      , cap <- _siCapList signer
      , _qnName (_ctName cap) == "GAS_PAYER"
      ]
    supply = gasSupplyOf (gasLimit ^. _GasLimit) gasPrice
    signedForGas signer =
      any (\sc -> sc == coinCap "GAS" []) (_siCapList signer)
    addDebit signer
      | signedForGas signer =
        signer & siCapList %~ (coinCap "DEBIT" [PString sender]:)
      | otherwise = signer
    signersWithDebit =
      fmap addDebit signers

    (buyGasTerm, buyGasData) =
      if isChainweb224Pact
      then mkBuyGasTerm sender supply
      else mkFundTxTerm mid mks sender supply

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
      evalExec
        -- TODO: more execution flags?
        pactDb noSPVSupport freeGasModel (Set.fromList [FlagDisableRuntimeRTC]) SimpleNamespacePolicy
        (ctxToPublicData publicMeta txCtx)
        (initMsgData rgHash) { mdData = redeemGasData }
        (def & csSlots .~ [CapSlot (coinCap "GAS" []) []])
        [TLTerm (def <$ redeemGasTerm)] >>= \case
        Right evalResult ->
          return evalResult
        Left err ->
          internalError $ "redeemGas: Internal error - " <> sshow err

    | not isChainweb224Pact, Just fundTxPactId <- maybeFundTxPactId = do
      -- before chainweb 2.24, we use defpacts for gas; see: 'pact/coin-contract/coin.pact#fund-tx'
      evalContinuation
        pactDb noSPVSupport freeGasModel (Set.fromList [FlagDisableRuntimeRTC]) SimpleNamespacePolicy
        (ctxToPublicData publicMeta txCtx)
        (initMsgData rgHash) { mdData = PObject $ Map.singleton "fee" (PDecimal $ _pact5GasSupply gasFee) }
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

-- -- | Initial gas charged for transaction size
-- --   ignoring the size of a continuation proof, if present
-- --
-- initialGasOf :: PayloadWithText -> Gas
-- initialGasOf payload = gasFee
--   where
--     feePerByte :: Rational = 0.01

--     contProofSize =
--       case _pPayload (Pact4.payloadObj payload) of
--         Continuation (ContMsg _ _ _ _ (Just (ContProof p))) -> B.length p
--         _ -> 0
--     txSize = SB.length (pact4PayloadBytes payload) - contProofSize

--     costPerByte = fromIntegral txSize * feePerByte
--     sizePenalty = txSizeAccelerationFee costPerByte
--     gasFee = ceiling (costPerByte + sizePenalty)
-- {-# INLINE initialGasOf #-}

-- txSizeAccelerationFee :: Rational -> Rational
-- txSizeAccelerationFee costPerByte = total
--   where
--     total = (costPerByte / bytePenalty) ^ power
--     bytePenalty = 512
--     power :: Integer = 7
-- {-# INLINE txSizeAccelerationFee #-}

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
