{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SB
import Data.Decimal (Decimal, roundTo)
import Data.Default (def)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text, pack, unpack)
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
    evalTransactionM cenv0 txst0 applyBuyGas
  where
    txst0 = set transactionCache mcache0 def
    cmd = payloadObj <$> cmdIn
    requestKey = cmdToRequestKey cmd
    pd' = set pdPublicMeta (publicMetaOf cmd) pd
    gasPrice = gasPriceOf cmd
    gasLimit = fromIntegral $ gasLimitOf cmd
    supply = gasFeeOf gasLimit gasPrice
    initialGas = initialGasOf (_cmdPayload cmdIn)
    nid = networkIdOf cmd
    cenv0 = CommandEnv Nothing Transactional pdbenv logger freeGasEnv pd' spv nid

    -- Discount the initial gas charge from the Pact execution gas limit
    userGasEnv = mkGasEnvOf cmd gasModel
      & over geGasLimit (\l -> l - fromIntegral initialGas)

    fatal e = do
      liftIO
        $ logLog logger "ERROR"
        $ "critical transaction failure: "
        <> show requestKey <> ": "
        <> unpack e
      internalError e

    applyBuyGas = do
      buyGasResultE <- catchesPactError $! buyGas cmd miner supply
      case buyGasResultE of
        Left e1 -> fatal $ "tx failure for requestKey when buying gas: " <> tShow e1
        Right (Left e) -> fatal e
        Right (Right pid) -> checkTooBigTx $ applyPayload pid

    checkTooBigTx next
      | initialGas >= gasLimit =
        let
          !pe = PactError GasError def []
            $ "Tx too big (" <> pretty initialGas <> "), limit "
            <> pretty gasLimit
        in jsonErrorResult requestKey pe gasLimit "Tx too big"
      | otherwise = next

    applyPayload pid = do
      transactionGasEnv .= userGasEnv
      mcache <- use transactionCache

      let interp = initStateInterpreter $
            setModuleCache mcache def

      cr <- catchesPactError $!
        runPayload interp cmd managedNamespacePolicy

      case cr of
        Left e ->
          jsonErrorResult requestKey e gasLimit
          "tx failure for request key when running cmd"
        Right r -> applyRedeem pid r

    applyRedeem pid pr = do
      transactionGasEnv .= freeGasEnv

      rr <- catchesPactError $! redeemGas cmd initialGas pr pid
      case rr of
        Left e ->
          jsonErrorResult requestKey e (_crGas pr)
          "tx failure for request key while redeeming gas"
        Right r -> do
          mcache <- use transactionCache
          let !redeemLogs = fromMaybe [] $ _crLogs r
              !fr = crLogs . _Just <>~ redeemLogs $ pr

          return $! T2 fr mcache

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
    evalTransactionM cenv def go
  where
    pd' = set pdPublicMeta (publicMetaOf cmd) pd
    nid = networkIdOf cmd
    cenv = CommandEnv Nothing Transactional dbEnv logger freeGasEnv pd' spv nid
    rk = cmdToRequestKey cmd

    -- when calling genesis commands, we bring all magic capabilities in scope
    interp = initStateInterpreter $ initCapabilities [magic_GENESIS, magic_COINBASE]

    go = do
      cr <- catchesPactError $!
        runPayload interp cmd permissiveNamespacePolicy
      case cr of
        Left e -> internalError
          $ "Genesis command failed: "
          <> sshow e
        Right r -> do
          liftIO $ logDebugRequestKey logger rk "successful genesis tx for request key"
          return $ T2 (r { _crGas = 0 }) mempty

-- | Whether to ignore coinbase failures, or "enforce" (fail block)
-- Backward-compat fix is to enforce in new block, but ignore in validate.
newtype EnforceCoinbaseFailure = EnforceCoinbaseFailure Bool

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
    evalTransactionM cenv def go
  where
    cenv = CommandEnv Nothing Transactional dbEnv logger freeGasEnv pd noSPVSupport Nothing
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
          | otherwise -> flip T2 mempty <$>
            jsonErrorResult' rk e 0 "coinbase tx failure"
        Right er -> do
          liftIO
            $ logDebugRequestKey logger rk
            $ "successful coinbase of "
            ++ (take 18 $ show d)
            ++ " to "
            ++ show mid

          return $! T2
           (CommandResult rk (_erTxId er) (PactResult (Right (last $ _erOutput er)))
           (_erGas er) (Just $ _erLogs er) (_erExec er) Nothing)
           (_erLoadedModules er)

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
    evalTransactionM cenv def go
  where
    pd' = set pdPublicMeta (publicMetaOf cmd) pd
    rk = cmdToRequestKey cmd
    nid = networkIdOf cmd
    chash = toUntypedHash $ _cmdHash cmd
    signers = _pSigners $ _cmdPayload cmd
    cenv = CommandEnv Nothing Local dbEnv logger freeGasEnv pd' spv nid

    go = do
      em <- case _pPayload $ _cmdPayload cmd of
        Exec !pm -> return pm
        _ -> throwCmdEx "local continuations not supported"

      cr <- catchesPactError $!
        applyExec defaultInterpreter rk em signers chash managedNamespacePolicy

      case cr of
        Left e -> jsonErrorResult' rk e 0 "applyLocal"
        Right r -> return $! r { _crMetaData = Just (toJSON pd') }

-- | Present a failure as a pair of json result of Command Error and associated logs
jsonErrorResult
    :: RequestKey
    -> PactError
    -> Gas
    -> String
    -> TransactionM p (T2 (CommandResult [TxLog Value]) ModuleCache)
jsonErrorResult rk err gas msg = do
    l <- view ceLogger
    logs <- use transactionLogs
    mcache <- use transactionCache

    liftIO $! logErrorRequestKey l rk err msg

    return $! T2 (CommandResult rk Nothing (PactResult (Left err))
      gas (Just logs) Nothing Nothing) mcache

jsonErrorResult'
    :: RequestKey
    -> PactError
    -> Gas
    -> String
    -> TransactionM p (CommandResult [TxLog Value])
jsonErrorResult' rk err gas msg = do
    l <- view ceLogger
    logs <- use transactionLogs

    liftIO $! logErrorRequestKey l rk err msg
    return $! CommandResult rk Nothing (PactResult (Left err))
      gas (Just logs) Nothing Nothing

runPayload
    :: Interpreter p
    -> Command (Payload PublicMeta ParsedCode)
    -> NamespacePolicy
    -> TransactionM p (CommandResult [TxLog Value])
runPayload interp cmd nsp = case payload of
    Exec pm ->
      applyExec interp rk pm signers chash nsp
    Continuation ym ->
      applyContinuation interp rk ym signers chash nsp
  where
    rk = cmdToRequestKey cmd
    signers = _pSigners $ _cmdPayload cmd
    chash = toUntypedHash $ _cmdHash cmd
    payload = _pPayload $ _cmdPayload cmd

-- | Execute an 'ExecMsg' and Return the result with module cache
--
applyExec
    :: Interpreter p
    -> RequestKey
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM p (CommandResult [TxLog Value])
applyExec interp rk em senderSigs hsh nsp = do
    EvalResult{..} <- applyExec' interp em senderSigs hsh nsp
    logs <- use transactionLogs
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

      cenv <- ask
      genv <- use transactionGasEnv

      let eenv = evalEnv cenv genv
      er <- liftIO $! evalExec senderSigs interp eenv parsedCode

      liftIO $! for_ (_erExec er) $ \pe -> logLog (_ceLogger cenv) "DEBUG"
        $ "applyExec: new pact added: "
        <> show (_pePactId pe, _peStep pe, _peYield pe, _peExecuted pe)

      -- set log + cache updates
      transactionLogs <>= (_erLogs er)
      transactionCache .= (_erLoadedModules er)

      return er
  where
    evalEnv c g = setupEvalEnv (_ceDbEnv c) (_ceEntity c) (_ceMode c)
      (MsgData execData Nothing hsh) initRefStore g
      nsp (_ceSPVSupport c) (_cePublicData c)

managedNamespacePolicy :: NamespacePolicy
managedNamespacePolicy = SmartNamespacePolicy False
  (QualifiedName (ModuleName "ns" Nothing) "validate" def)

-- | Execute a 'ContMsg' and return the command result and module cache
--
applyContinuation
    :: Interpreter p
    -> RequestKey
    -> ContMsg
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> TransactionM p (CommandResult [TxLog Value])
applyContinuation interp rk cm senderSigs hsh nsp = do
    EvalResult{..} <- applyContinuation' interp cm senderSigs hsh nsp
    logs <- use transactionLogs
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
    cenv <- ask
    genv <- use transactionGasEnv

    let eenv = evalEnv cenv genv
    er <- liftIO $! evalContinuation senderSigs interp eenv cm

    -- set log + cache updates
    transactionLogs <>= (_erLogs er)
    transactionCache .= (_erLoadedModules er)

    return er
  where
    step = _cmStep cm
    rollback = _cmRollback cm
    pid = _cmPactId cm
    pactStep = Just $ PactStep step rollback pid Nothing
    evalEnv c g = setupEvalEnv (_ceDbEnv c) (_ceEntity c) (_ceMode c)
      (MsgData (_cmData cm) pactStep hsh) initRefStore
      g nsp (_ceSPVSupport c) (_cePublicData c)

-- | Build and execute 'coin.buygas' command from miner info and user command
-- info (see 'TransactionExec.applyCmd')
--
-- see: 'pact/coin-contract/coin.pact#fund-tx'
--
buyGas
    :: Command (Payload PublicMeta ParsedCode)
    -> Miner
    -> GasSupply
    -> TransactionM p (Either Text GasId)
buyGas cmd (Miner mid mks) supply = go
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
      mcache <- use transactionCache
      buyGasCmd <- liftIO $! mkBuyGasCmd mid mks sender supply

      result <- applyExec' (interp mcache) buyGasCmd
        (_pSigners $ _cmdPayload cmd) bgHash managedNamespacePolicy

      case _erExec result of
        Nothing -> return $!
          Left "buyGas: Internal error - empty continuation"
        Just pe -> do
          return $! Right $ GasId (_pePactId pe)

    findPayer :: Eval e (Maybe (RunEval e -> RunEval e))
    findPayer = runMaybeT $ do
      (!m,!qn,!as) <- MaybeT findPayerCap
      pMod <- MaybeT $ lookupModule qn m
      capRef <- MaybeT $ return $ lookupIfaceModRef qn pMod
      return $ runCap (getInfo qn) capRef as

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
    -> Gas             -- ^ total gas usage from command payload
    -> CommandResult a -- ^ result from the user command payload
    -> GasId           -- ^ result of the buy-gas continuation
    -> TransactionM p (CommandResult [TxLog Value])
redeemGas cmd initialGas cmdResult gid = do
    mcache <- use transactionCache

    applyContinuation (initState mcache) rk (redeemGasCmd fee gid)
      (_pSigners $ _cmdPayload cmd) (toUntypedHash $ _cmdHash cmd)
      managedNamespacePolicy
  where
    totalGas = initialGas + _crGas cmdResult
    fee = gasFeeOf totalGas (gasPriceOf cmd)
    rk = cmdToRequestKey cmd
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
      Left err -> internalError $ "buildExecParsedCode: parse failed: " <> pack err


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
logDebugRequestKey :: Logger -> RequestKey -> String -> IO ()
logDebugRequestKey l k reason = logLog l "DEBUG" $ reason <> ": " <> show k

-- | Log request keys and error message at ERROR when failed
--
logErrorRequestKey
    :: Exception e
    => Logger
    -> RequestKey
    -> e
    -> String
    -> IO ()
logErrorRequestKey l k e reason = logLog l "ERROR" $ reason
    <> ": " <> show k
    <> ": " <> show e
