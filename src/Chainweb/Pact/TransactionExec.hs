{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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
, applyContinuation
, applyContinuation'
, runPayload

  -- * Gas Execution
, buyGas
, mkBuyGasCmd

  -- * Coinbase Execution
, applyCoinbase
, mkCoinbaseCmd
, EnforceCoinbaseFailure(..)

  -- * Command Helpers
, publicMetaOf
, networkIdOf
, gasFeeOf
, gasPriceOf
, mkGasEnvOf

  -- * Utilities
, buildExecParsedCode
, jsonErrorResult
, logDebugRequestKey
, logErrorRequestKey
, mkMagicCapSlot
) where

import Control.Lens
import Control.Monad.Catch (Exception(..))
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe


import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SB
import Data.Decimal (Decimal, roundTo)
import Data.Default (def)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))

-- internal Pact modules

import Pact.Eval (liftTerm, lookupModule)
import Pact.Gas (freeGasEnv)
import Pact.Interpreter
import Pact.Native.Capabilities (evalCap)
import Pact.Parse (parseExprs)
import Pact.Parse (ParsedDecimal(..))
import Pact.Runtime.Capabilities (popCapStack)
import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Hash as Pact
import Pact.Types.Logger
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.Server
import Pact.Types.SPV

-- internal Chainweb modules

import Chainweb.BlockHash
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.Types (internalError)
import Chainweb.Pact.Types
import Chainweb.Transaction
import Chainweb.Utils (sshow)


-- | "Magic" capability 'COINBASE' used in the coin contract to
-- constrain coinbase calls.
--
magic_COINBASE :: CapSlot UserCapability
magic_COINBASE = mkMagicCapSlot "COINBASE"

-- | "Magic" capability 'GAS' used in the coin contract to
-- constrain gas buy/redeem calls.
--
magic_GAS :: CapSlot UserCapability
magic_GAS = mkMagicCapSlot "GAS"

-- | "Magic" capability 'GENESIS' used in the coin contract to
-- constrain genesis-only allocations
--
magic_GENESIS :: CapSlot UserCapability
magic_GENESIS = mkMagicCapSlot "GENESIS"

-- | The main entry point to executing transactions. From here,
-- 'applyCmd' assembles the command environment for a command and
-- orchestrates gas buys/redemption, and executing payloads.
--
applyCmd
    :: Logger
      -- ^ Pact logger
    -> PactDbEnv p
      -- ^ Pact db environment
    -> Miner
      -- ^ The miner chosen to mine the block
    -> GasModel
      -- ^ Gas model (pact Service config)
    -> PublicData
      -- ^ Contains block height, time, prev hash + metadata
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command PayloadWithText
      -- ^ command with payload to execute
    -> ModuleCache
      -- ^ cached module state
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
applyCmd logger pdbenv miner gasModel pd spv cmdIn mcache0 =
    second _txCache <$>
      runTransactionM cenv txst applyBuyGas
  where
    txst = set txCache mcache0
      $ TransactionState mempty mempty 0 Nothing (_geGasModel freeGasEnv)
    cenv = TransactionEnv Transactional pdbenv logger pd' spv nid gasPrice
      requestKey gasLimit

    cmd = payloadObj <$> cmdIn
    requestKey = cmdToRequestKey cmd
    pd' = set pdPublicMeta (publicMetaOf cmd) pd
    gasPrice = gasPriceOf cmd
    gasLimit = fromIntegral $ gasLimitOf cmd
    initialGas = initialGasOf (_cmdPayload cmdIn)
    nid = networkIdOf cmd

    applyBuyGas =
      catchesPactError (buyGas cmd miner) >>= \case
        Left e -> fatal $ "tx failure for requestKey when buying gas: " <> sshow e
        Right _ -> checkTooBigTx applyPayload

    checkTooBigTx next
      | initialGas >= gasLimit = do
          txGasUsed .= gasLimit -- all gas is consumed
          --
          let !pe = PactError GasError def []
                $ "Tx too big (" <> pretty initialGas <> "), limit "
                <> pretty gasLimit

          jsonErrorResult pe "Tx too big"
      | otherwise = next

    applyPayload = do
      txGasModel .= gasModel
      txGasUsed .= initialGas

      cr <- catchesPactError $! runPayload cmd managedNamespacePolicy
      case cr of
        Left e -> jsonErrorResult e "tx failure for request key when running cmd"
        Right r -> applyRedeem r

    applyRedeem cr = do
      txGasModel .= (_geGasModel freeGasEnv)

      r <- catchesPactError $! redeemGas cmd initialGas
      case r of
        Left e -> jsonErrorResult e "tx failure for request key while redeeming gas"
        Right _ -> do
          logs <- use txLogs
          return $! set crLogs (Just logs) cr

applyGenesisCmd
    :: Logger
      -- ^ Pact logger
    -> PactDbEnv p
      -- ^ Pact db environment
    -> PublicData
      -- ^ Contains block height, time, prev hash + metadata
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command (Payload PublicMeta ParsedCode)
      -- ^ command with payload to execute
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
applyGenesisCmd logger dbEnv pd spv cmd =
    evalTransactionM tenv txst go
  where
    pd' = set pdPublicMeta (publicMetaOf cmd) pd
    nid = networkIdOf cmd
    rk = cmdToRequestKey cmd
    tenv = TransactionEnv Transactional dbEnv logger pd' spv nid 0.0 rk 0
    txst = TransactionState mempty mempty 0 Nothing (_geGasModel freeGasEnv)

    -- when calling genesis commands, we bring all magic capabilities in scope
    interp = initStateInterpreter $ initCapabilities [magic_GENESIS, magic_COINBASE]

    go = do
      cr <- catchesPactError $! runGenesis cmd permissiveNamespacePolicy interp
      case cr of
        Left e -> internalError
          $ "Genesis command failed: "
          <> sshow e
        Right r -> do
          logDebugRequestKey "successful genesis tx for request key"
          return $ T2 r mempty

applyCoinbase
    :: Logger
      -- ^ Pact logger
    -> PactDbEnv p
      -- ^ Pact db environment
    -> Miner
      -- ^ The miner chosen to mine the block
    -> ParsedDecimal
      -- ^ Miner reward
    -> PublicData
      -- ^ Contains block height, time, prev hash + metadata
    -> BlockHash
      -- ^ hash of the mined block
    -> EnforceCoinbaseFailure
      -- ^ treat
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
applyCoinbase logger dbEnv (Miner mid mks) reward@(ParsedDecimal d) pd ph (EnforceCoinbaseFailure throwCritical) =
    second _txCache <$> runTransactionM tenv txst go
  where
    tenv = TransactionEnv Transactional dbEnv logger pd noSPVSupport Nothing 0.0 rk 0
    txst =TransactionState mempty mempty 0 Nothing (_geGasModel freeGasEnv)
    interp = initStateInterpreter $ initCapabilities [magic_COINBASE]
    chash = Pact.Hash (sshow ph)
    rk = RequestKey chash

    go = do
      cexec <- liftIO $ mkCoinbaseCmd mid mks reward
      cr <- catchesPactError $!
        applyExec' interp cexec mempty chash managedNamespacePolicy

      case cr of
        Left e
          | throwCritical -> internalError $ "Coinbase tx failure: " <> sshow e
          | otherwise -> jsonErrorResult e "coinbase tx failure"
        Right er -> do
          logDebugRequestKey
            $! "successful coinbase of "
            <> (T.take 18 $ sshow d)
            <> " to "
            <> sshow mid

          return $! CommandResult rk (_erTxId er) (PactResult (Right (last $ _erOutput er)))
           (_erGas er) (Just $ _erLogs er) (_erExec er) Nothing

applyLocal
    :: Logger
      -- ^ Pact logger
    -> PactDbEnv p
      -- ^ Pact db environment
    -> PublicData
      -- ^ Contains block height, time, prev hash + metadata
    -> SPVSupport
      -- ^ SPV support (validates cont proofs)
    -> Command (Payload PublicMeta ParsedCode)
      -- ^ command with payload to execute
    -> IO (CommandResult [TxLog Value])
applyLocal logger dbEnv pd spv cmd =
    evalTransactionM tenv txst go
  where
    pd' = set pdPublicMeta (publicMetaOf cmd) pd
    rk = cmdToRequestKey cmd
    nid = networkIdOf cmd
    chash = toUntypedHash $ _cmdHash cmd
    signers = _pSigners $ _cmdPayload cmd
    tenv = TransactionEnv Local dbEnv logger pd' spv nid 0.0 rk 0
    txst = TransactionState mempty mempty 0 Nothing (_geGasModel freeGasEnv)

    go = do
      em <- case _pPayload $ _cmdPayload cmd of
        Exec !pm -> return pm
        _ -> throwCmdEx "local continuations not supported"

      cr <- catchesPactError $!
        applyExec defaultInterpreter em signers chash managedNamespacePolicy

      case cr of
        Left e -> jsonErrorResult e "applyLocal"
        Right r -> return $! r { _crMetaData = Just (toJSON pd') }

jsonErrorResult
    :: PactError
    -> Text
    -> TransactionM p (CommandResult [TxLog Value])
jsonErrorResult err msg = do
    logs <- use txLogs
    gas <- use txGasUsed
    rk <- view txRequestKey

    void $! logErrorRequestKey err msg

    return $! CommandResult rk Nothing (PactResult (Left err))
      gas (Just logs) Nothing Nothing

runPayload
    :: Command (Payload PublicMeta ParsedCode)
    -> NamespacePolicy
    -> TransactionM p (CommandResult [TxLog Value])
runPayload cmd nsp = do
    mcache <- use txCache
    g0 <- use txGasUsed

    let interp = initStateInterpreter
          $ set evalGas g0
          $ setModuleCache mcache def

    case payload of
      Exec pm ->
        applyExec interp pm signers chash nsp
      Continuation ym ->
        applyContinuation interp ym signers chash nsp

  where
    signers = _pSigners $ _cmdPayload cmd
    chash = toUntypedHash $ _cmdHash cmd
    payload = _pPayload $ _cmdPayload cmd

-- | Run genesis transaction payloads with custom interpreter
--
runGenesis
    :: Command (Payload PublicMeta ParsedCode)
    -> NamespacePolicy
    -> Interpreter p
    -> TransactionM p (CommandResult [TxLog Value])
runGenesis cmd nsp interp = case payload of
    Exec pm ->
      applyExec interp pm signers chash nsp
    Continuation ym ->
      applyContinuation interp ym signers chash nsp
  where
    signers = _pSigners $ _cmdPayload cmd
    chash = toUntypedHash $ _cmdHash cmd
    payload = _pPayload $ _cmdPayload cmd

-- | Execute an 'ExecMsg' and Return the result with module cache
--
applyExec
    :: Interpreter p
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM p (CommandResult [TxLog Value])
applyExec interp em senderSigs hsh nsp = do
    EvalResult{..} <- applyExec' interp em senderSigs hsh nsp
    logs <- use txLogs
    rk <- view txRequestKey
    -- applyExec enforces non-empty expression set so `last` ok
    return $! CommandResult rk _erTxId (PactResult (Right (last _erOutput)))
      _erGas (Just logs) _erExec Nothing

-- | Variation on 'applyExec' that returns 'EvalResult' as opposed to
-- wrapping it up in a JSON result.
--
applyExec'
    :: Interpreter p
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM p EvalResult
applyExec' interp (ExecMsg parsedCode execData) senderSigs hsh nsp
    | null (_pcExps parsedCode) = throwCmdEx "No expressions found"
    | otherwise = do

      tenv <- ask
      genv <- GasEnv
        <$> view (txGasLimit . to fromIntegral)
        <*> view txGasPrice
        <*> use txGasModel

      let eenv = evalEnv tenv genv
      er <- liftIO $! evalExec senderSigs interp eenv parsedCode

      liftIO $! for_ (_erExec er) $ \pe -> logLog (_txLogger tenv) "DEBUG"
        $ "applyExec: new pact added: "
        <> show (_pePactId pe, _peStep pe, _peYield pe, _peExecuted pe)

      -- set log + cache updates + used gas
      setTxResultState er

      return er
  where
    evalEnv c g = setupEvalEnv (_txDbEnv c) Nothing (_txMode c)
      (MsgData execData Nothing hsh) initRefStore g
      nsp (_txSpvSupport c) (_txPublicData c)

-- | Execute a 'ContMsg' and return the command result and module cache
--
applyContinuation
    :: Interpreter p
    -> ContMsg
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM p (CommandResult [TxLog Value])
applyContinuation interp cm senderSigs hsh nsp = do
    EvalResult{..} <- applyContinuation' interp cm senderSigs hsh nsp
    logs <- use txLogs
    rk <- view txRequestKey
    -- last safe here because cont msg is guaranteed one exp
    return $! (CommandResult rk _erTxId (PactResult (Right (last _erOutput)))
      _erGas (Just logs) _erExec Nothing)

-- | Execute a 'ContMsg' and return just eval result, not wrapped in a
-- 'CommandResult' wrapper
--
applyContinuation'
    :: Interpreter p
    -> ContMsg
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM p EvalResult
applyContinuation' interp cm senderSigs hsh nsp = do
    tenv <- ask
    genv <- GasEnv
      <$> view (txGasLimit . to fromIntegral)
      <*> view txGasPrice
      <*> use txGasModel

    let eenv = evalEnv tenv genv
    er <- liftIO $! evalContinuation senderSigs interp eenv cm

    -- set log + cache updates
    setTxResultState er

    return er
  where
    step = _cmStep cm
    rollback = _cmRollback cm
    pid = _cmPactId cm
    pactStep = Just $ PactStep step rollback pid Nothing
    evalEnv c g = setupEvalEnv (_txDbEnv c) Nothing (_txMode c)
      (MsgData (_cmData cm) pactStep hsh) initRefStore
      g nsp (_txSpvSupport c) (_txPublicData c)

-- | Build and execute 'coin.buygas' command from miner info and user command
-- info (see 'TransactionExec.applyCmd')
--
-- see: 'pact/coin-contract/coin.pact#fund-tx'
--
buyGas :: Command (Payload PublicMeta ParsedCode) -> Miner -> TransactionM p ()
buyGas cmd (Miner mid mks) = go
  where
    sender = view (cmdPayload . pMeta . pmSender) cmd
    initState mc = setModuleCache mc $ initCapabilities [magic_GAS]
    interp mc = Interpreter $ \start end withRollback input ->
      withRollback (put (initState mc) >> start (run input) >>= end)
    run input = do
      findPayer >>= \r -> case r of
        Nothing -> input
        Just withPayerCap -> withPayerCap input

    (Hash chash) = toUntypedHash (_cmdHash cmd)
    bgHash = Hash (chash <> "-buygas")

    go = do
      mcache <- use txCache
      supply <- gasFeeOf <$> view txGasLimit <*> view txGasPrice

      buyGasCmd <- liftIO $! mkBuyGasCmd mid mks sender supply

      result <- applyExec' (interp mcache) buyGasCmd
        (_pSigners $ _cmdPayload cmd) bgHash managedNamespacePolicy

      case _erExec result of
        Nothing -> fatal "buyGas: Internal error - empty continuation"
        Just pe -> void $! txGasId .= (Just $ GasId (_pePactId pe))

findPayer :: Eval e (Maybe (RunEval e -> RunEval e))
findPayer = runMaybeT $ do
    (!m,!qn,!as) <- MaybeT findPayerCap
    pMod <- MaybeT $ lookupModule qn m
    capRef <- MaybeT $ return $ lookupIfaceModRef qn pMod
    return $ runCap (getInfo qn) capRef as
  where
    findPayerCap :: Eval e (Maybe (ModuleName,QualifiedName,[PactValue]))
    findPayerCap = preview $ eeMsgSigs . folded . folded . to sigPayerCap . _Just

    sigPayerCap (SigCapability q@(QualifiedName m n _) as)
      | n == "GAS_PAYER" = Just (m,q,as)
    sigPayerCap _ = Nothing

    gasPayerIface = ModuleName "gas-payer-v1" Nothing

    lookupIfaceModRef (QualifiedName _ n _) (ModuleData (MDModule (Module {..})) refs)
      | gasPayerIface `elem` _mInterfaces = HM.lookup n refs
    lookupIfaceModRef _ _ = Nothing

    mkApp i r as = App (TVar r i) (map (liftTerm . fromPactValue) as) i

    runCap i capRef as input = do
      ar <- evalCap i CapCallStack False $ mkApp i capRef as
      case ar of
        NewlyAcquired -> do
          r <- input
          popCapStack (const (return ()))
          return r
        _ -> evalError' i "Internal error, GAS_PAYER already acquired"


-- | Build and execute 'coin.redeem-gas' command from miner info and previous
-- command results (see 'TransactionExec.applyCmd')
--
-- see: 'pact/coin-contract/coin.pact#fund-tx'
--
redeemGas
    :: Command (Payload PublicMeta ParsedCode)
    -> Gas
      -- ^ initial gas of the cmd payload
    -> TransactionM p ()
redeemGas cmd initialGas = do
    mcache <- use txCache

    gid <- use txGasId >>= \case
      Nothing -> fatal $! "redeemGas: no gas id in scope for gas refunds"
      Just g -> return g

    fee <- feeOf initialGas

    void $! applyContinuation (initState mcache) (redeemGasCmd fee gid)
      (_pSigners $ _cmdPayload cmd) (toUntypedHash $ _cmdHash cmd)
      managedNamespacePolicy

  where
    feeOf g0 = do
      g <- use txGasUsed
      p <- view txGasPrice
      return $! gasFeeOf (g + g0) p

    initState mc = initStateInterpreter
      $ setModuleCache mc
      $ initCapabilities [magic_GAS]

    redeemGasCmd fee' (GasId pid) =
      ContMsg pid 1 False (object [ "fee" A..= fee' ]) Nothing

-- | Build the 'coin-contract.buygas' command
--
mkBuyGasCmd
    :: MinerId   -- ^ Id of the miner to fund
    -> MinerKeys -- ^ Miner keyset
    -> Text      -- ^ Address of the sender from the command
    -> GasSupply -- ^ The gas limit total * price
    -> IO (ExecMsg ParsedCode)
mkBuyGasCmd (MinerId mid) (MinerKeys ks) sender total =
    buildExecParsedCode buyGasData $ mconcat
      [ "(coin.fund-tx"
      , " \"" <> sender <> "\""
      , " \"" <> mid <> "\""
      , " (read-keyset \"miner-keyset\")"
      , " (read-decimal \"total\"))"
      ]
  where
    buyGasData = Just $ object
      [ "miner-keyset" A..= ks
      , "total" A..= total
      ]
{-# INLINABLE mkBuyGasCmd #-}

mkCoinbaseCmd :: MinerId -> MinerKeys -> ParsedDecimal -> IO (ExecMsg ParsedCode)
mkCoinbaseCmd (MinerId mid) (MinerKeys ks) reward =
    buildExecParsedCode coinbaseData $ mconcat
      [ "(coin.coinbase"
      , " \"" <> mid <> "\""
      , " (read-keyset \"miner-keyset\")"
      , " (read-decimal \"reward\"))"
      ]
  where
    coinbaseData = Just $ object
      [ "miner-keyset" A..= ks
      , "reward" A..= reward
      ]
{-# INLINABLE mkCoinbaseCmd #-}

-- | Set tx result state
--
setTxResultState :: EvalResult -> TransactionM db ()
setTxResultState er = do
    txLogs <>= (_erLogs er)
    txCache .= (_erLoadedModules er)
    txGasUsed .= (_erGas er)
{-# INLINE setTxResultState #-}

-- | Managed namespace policy CAF
--
managedNamespacePolicy :: NamespacePolicy
managedNamespacePolicy = SmartNamespacePolicy False
  (QualifiedName (ModuleName "ns" Nothing) "validate" def)
{-# NOINLINE managedNamespacePolicy #-}

-- ---------------------------------------------------------------------------- --
-- Utilities

-- | Initialize a fresh eval state with magic capabilities.
-- This is the way we inject the correct guards into the environment
-- during Pact code execution
--
initCapabilities :: [CapSlot UserCapability] -> EvalState
initCapabilities cs = set (evalCapabilities . capStack) cs def
{-# INLINABLE initCapabilities #-}

initStateInterpreter :: EvalState -> Interpreter p
initStateInterpreter s = Interpreter $ \start end withRollback runInput ->
    withRollback (put s >> start runInput >>= end)

-- | Initial gas charged for transaction size
--   ignoring the size of a continuation proof, if present
--
initialGasOf :: PayloadWithText -> Gas
initialGasOf cmd = gasFee
  where
    feePerByte :: Decimal = 0.01

    contProofSize =
      case _pPayload (payloadObj cmd) of
        Continuation (ContMsg _ _ _ _ (Just (ContProof p))) -> B.length p
        _ -> 0
    txSize = SB.length (payloadBytes cmd) - contProofSize
    gasFee = round $ fromIntegral txSize * feePerByte
{-# INLINE initialGasOf #-}

-- | Set the module cache of a pact 'EvalState'
--
setModuleCache
  :: ModuleCache
  -> EvalState
  -> EvalState
setModuleCache = set (evalRefs . rsLoadedModules)
{-# INLINE setModuleCache #-}

-- | Builder for "magic" capabilities given a magic cap name
--
mkMagicCapSlot :: Text -> CapSlot UserCapability
mkMagicCapSlot c = CapSlot CapCallStack cap []
  where
    mn = ModuleName "coin" Nothing
    fqn = QualifiedName mn c def
    cap = SigCapability fqn []
{-# INLINE mkMagicCapSlot #-}

-- | Build the 'ExecMsg' for some pact code fed to the function. The 'value'
-- parameter is for any possible environmental data that needs to go into
-- the 'ExecMsg'.
--
buildExecParsedCode :: Maybe Value -> Text -> IO (ExecMsg ParsedCode)
buildExecParsedCode value code = maybe (go Null) go value
  where
    go v = case ParsedCode code <$> parseExprs code of
      Right !t -> pure $! ExecMsg t v
      -- if we can't construct coin contract calls, this should
      -- fail fast
      Left err -> internalError $ "buildExecParsedCode: parse failed: " <> T.pack err


-- | Create a gas environment from a verified command
--
mkGasEnvOf :: Command (Payload PublicMeta c) -> GasModel -> GasEnv
mkGasEnvOf cmd gasModel = GasEnv (gasLimitOf cmd) (gasPriceOf cmd) gasModel
{-# INLINE mkGasEnvOf #-}

-- | Retrieve public metadata from a command
--
publicMetaOf :: Command (Payload PublicMeta c) -> PublicMeta
publicMetaOf = _pMeta . _cmdPayload
{-# INLINE publicMetaOf #-}

-- | Retrieve the optional Network identifier from a command
--
networkIdOf :: Command (Payload a b) -> Maybe NetworkId
networkIdOf = _pNetworkId . _cmdPayload
{-# INLINE networkIdOf #-}

-- | Calculate the gas fee (pact-generate gas cost * user-specified gas price),
-- rounding to the nearest stu.
--
gasFeeOf :: Gas -> GasPrice -> GasSupply
gasFeeOf gas (GasPrice (ParsedDecimal gp)) = GasSupply (ParsedDecimal gs)
  where
    gs = toCoinUnit ((fromIntegral gas) * gp)
{-# INLINE gasFeeOf #-}

toCoinUnit :: Decimal -> Decimal
toCoinUnit = roundTo 12

-- | Log request keys at DEBUG when successful
--
logDebugRequestKey :: forall db. Text -> TransactionM db ()
logDebugRequestKey s = do
    l <- view txLogger
    rk <- view txRequestKey
    liftIO $! logLog l "DEBUG" $! T.unpack s <> ": " <> show rk

-- | Log request keys and error message at ERROR when failed
--
logErrorRequestKey
    :: forall db e
    . Exception e
    => e
    -> Text
    -> TransactionM db ()
logErrorRequestKey e reason = do
    l <- view txLogger
    rk <- view txRequestKey

    liftIO
      $! logLog l "ERROR"
      $! T.unpack reason
      <> ": " <> show rk
      <> ": " <> show e

-- | Denotes fatal failure points in the tx exec process
--
fatal :: forall db a. Text -> TransactionM db a
fatal e = do
    l <- view txLogger
    rk <- view txRequestKey

    liftIO
      $! logLog l "ERROR"
      $! "critical transaction failure: "
      <> sshow rk <> ": " <> T.unpack e

    internalError e
