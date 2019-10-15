{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Lens hiding ((.=))
import Control.Monad (when)
import Control.Monad.Catch (Exception(..))

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SB
import Data.Decimal (roundTo)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Maybe
import Data.Text (Text, pack)
import Data.Tuple.Strict (T2(..), T3(..))

-- internal Pact modules

import Pact.Gas (freeGasEnv)
import Pact.Interpreter
import Pact.Parse (parseExprs)
import Pact.Parse (ParsedDecimal(..), ParsedInteger(..))
import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Hash as Pact
import Pact.Types.Logger
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.Server
import Pact.Types.SPV

-- internal Chainweb modules

import Chainweb.BlockHash
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.Types (internalError)
import Chainweb.Pact.Types (GasId(..), GasSupply(..), ModuleCache)
import Chainweb.Transaction
import Chainweb.Utils (sshow)


-- | "Magic" capability 'COINBASE' used in the coin contract to
-- constrain coinbase calls.
--
magic_COINBASE :: CapSlot Capability
magic_COINBASE = mkMagicCapSlot "COINBASE"

-- | "Magic" capability 'FUND_TX' used in the coin contract to
-- constrain gas buy/redeem calls.
--
magic_FUND_TX :: CapSlot Capability
magic_FUND_TX = mkMagicCapSlot "FUND_TX"

-- | "Magic" capability 'GENESIS' used in the coin contract to
-- constrain genesis-only allocations
--
magic_GENESIS :: CapSlot Capability
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
applyCmd logger pactDbEnv miner gasModel pd spv cmdIn mcache
  | initialGasFee > supply =
    -- no gas actually charged
    jsonErrorResult buyGasEnv requestKey
    (PactError GasError def [] "tx too big") [] 0 mcache
    ("tx failure for requestKey: gas limit of "
      <> show (view geGasLimit naiveGasEnv) <> " but expected " <> show initialGas)
  | otherwise = apply

  where
    cmd = _payloadObj <$> cmdIn
    requestKey = cmdToRequestKey cmd
    pd' = set pdPublicMeta (publicMetaOf cmd) pd
    supply = gasSupplyOf cmd
    nid = networkIdOf cmd

    gasPrice = fromRational $ toRational $ gasPriceOf cmd
    initialGas = initialGasOf (_cmdPayload cmdIn)
    initialGasFee = gasFeeOf initialGas gasPrice

    naiveGasEnv = mkGasEnvOf cmd gasModel
    -- Discount the initial gas charge from the Pact execution gas limit
    userGasEnv = over geGasLimit (\l -> l - fromIntegral initialGas) naiveGasEnv

    buyGasEnv = CommandEnv Nothing Transactional pactDbEnv logger
      freeGasEnv pd' spv nid

    apply = do

      buyGasResultE <- catchesPactError $! buyGas buyGasEnv cmd miner supply mcache

      case buyGasResultE of

        Left e1 ->
          jsonErrorResult buyGasEnv requestKey e1 [] (Gas 0) mcache
          "tx failure for requestKey when buying gas"
        Right (Left e) ->
          -- this call needs to fail hard if Left. It means the continuation did not process
          -- correctly, and we should fail the transaction
          internalError e
        Right (Right (T3 pactId buyGasLogs mcache')) -> do
          logDebugRequestKey logger requestKey "successful gas buy for request key"

          let !payloadEnv = set ceGasEnv userGasEnv
                $ set cePublicData pd' buyGasEnv

          -- initialize refstate with cached module definitions
          let st0 = set (evalRefs . rsLoadedModules) mcache' def

          cmdResultE <- catchesPactError $!
            runPayload payloadEnv st0 cmd buyGasLogs managedNamespacePolicy

          case cmdResultE of

            Left e2 ->
              -- we return the limit here as opposed to the supply (price * limit).
              -- Private chains have no notion of price, and the user knows what price
              -- they haggled for this tx, so this is justified.
              let
                (GasLimit (ParsedInteger !g)) = gasLimitOf cmd
              in jsonErrorResult payloadEnv requestKey e2 buyGasLogs (Gas $ fromIntegral g) mcache'
                 "tx failure for request key when running cmd"
            Right (T2 cmdResult !mcache'') -> do

              logDebugRequestKey logger requestKey "success for requestKey"

              let !redeemGasEnv = set ceGasEnv freeGasEnv payloadEnv
                  cmdLogs = fromMaybe [] $ _crLogs cmdResult
              redeemResultE <- catchesPactError $!
                redeemGas redeemGasEnv cmd initialGas cmdResult pactId cmdLogs mcache''

              case redeemResultE of

                Left e3 ->

                  jsonErrorResult redeemGasEnv requestKey e3 cmdLogs (_crGas cmdResult) mcache''
                  "tx failure for request key while redeeming gas"

                Right (T2 redeemResult !mcache''') -> do

                  let !redeemLogs = fromMaybe [] $ _crLogs redeemResult
                      !finalResult = over (crLogs . _Just) (<> redeemLogs) cmdResult

                  logDebugRequestKey logger requestKey "successful gas redemption for request key"
                  pure $! T2 finalResult $! mcache'''

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
applyGenesisCmd logger dbEnv pd spv cmd = do
    -- cmd env with permissive gas model

    let pd' = set pdPublicMeta (publicMetaOf cmd) pd
        nid = networkIdOf cmd

    let cmdEnv = CommandEnv Nothing Transactional dbEnv logger freeGasEnv pd' spv nid
        requestKey = cmdToRequestKey cmd
    -- when calling genesis commands, we bring all magic capabilities in scope
    let initState = initCapabilities [magic_GENESIS, magic_COINBASE]

    resultE <- catchesPactError $! runPayload cmdEnv initState cmd [] permissiveNamespacePolicy
    fmap (`T2` mempty) $! case resultE of
      Left e ->
        jsonErrorResult' cmdEnv requestKey e [] (Gas 0)
          "genesis tx failure for request key while running genesis"
      Right (T2 result _) -> do
        logDebugRequestKey logger requestKey "successful genesis tx for request key"
        return result


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
    -> IO (CommandResult [TxLog Value])
applyCoinbase logger dbEnv (Miner mid mks) mr@(ParsedDecimal d) pd ph = do
    -- cmd env with permissive gas model
    let cenv = CommandEnv Nothing Transactional dbEnv logger freeGasEnv pd noSPVSupport Nothing
        initState = initCapabilities [magic_COINBASE]
        ch = Pact.Hash (sshow ph)

    let rk = RequestKey ch

    cexec <- mkCoinbaseCmd mid mks mr
    cre <- catchesPactError $! applyExec' cenv initState cexec [] ch managedNamespacePolicy

    case cre of
      Left e -> jsonErrorResult' cenv rk e [] (Gas 0) "coinbase tx failure"
      Right er -> do
        logDebugRequestKey logger rk
          $ "successful coinbase of "
          ++ (take 18 $ show d)
          ++ " to "
          ++ show mid

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
applyLocal logger dbEnv pd spv cmd@Command{..} = do

  -- cmd env with permissive gas model
  let pd' = set pdPublicMeta (publicMetaOf cmd) pd
      requestKey = cmdToRequestKey cmd
      nid = networkIdOf cmd


  let cmdEnv = CommandEnv Nothing Local dbEnv logger freeGasEnv pd' spv nid

  exec <- case _pPayload _cmdPayload of
    Exec !pm -> return pm
    _ -> throwCmdEx "local continuations not supported"

  !r <- catchesPactError $!
    applyExec cmdEnv def requestKey exec (_pSigners _cmdPayload)
    (toUntypedHash _cmdHash) [] managedNamespacePolicy

  case r of
    Left e -> jsonErrorResult' cmdEnv requestKey e [] 0 "applyLocal"
    Right (T2 rr _) -> return $! rr { _crMetaData = Just (toJSON pd') }



-- | Present a failure as a pair of json result of Command Error and associated logs
jsonErrorResult
    :: CommandEnv a
    -> RequestKey
    -> PactError
    -> [TxLog Value]
    -> Gas
    -> ModuleCache
    -> String
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
jsonErrorResult cmdEnv reqKey err txLogs gas mcache msg = do
    logErrorRequestKey (_ceLogger cmdEnv) reqKey err msg
    return $! T2 (CommandResult reqKey Nothing (PactResult (Left err))
      gas (Just txLogs) Nothing Nothing) mcache

jsonErrorResult'
    :: CommandEnv a
    -> RequestKey
    -> PactError
    -> [TxLog Value]
    -> Gas
    -> String
    -> IO (CommandResult [TxLog Value])
jsonErrorResult' cmdEnv reqKey err txLogs gas msg = do
    logErrorRequestKey (_ceLogger cmdEnv) reqKey err msg
    return $! CommandResult reqKey Nothing (PactResult (Left err))
      gas (Just txLogs) Nothing Nothing

runPayload
    :: CommandEnv p
    -> EvalState
    -> Command (Payload PublicMeta ParsedCode)
    -> [TxLog Value] -- log state
    -> NamespacePolicy
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
runPayload env initState c@Command{..} txLogs nsp = case _pPayload _cmdPayload of
    Exec pm ->
      applyExec env initState (cmdToRequestKey c) pm (_pSigners _cmdPayload) (toUntypedHash _cmdHash) txLogs nsp
    Continuation ym ->
      applyContinuation env initState (cmdToRequestKey c) ym (_pSigners _cmdPayload) (toUntypedHash _cmdHash) txLogs nsp

-- | Execute an 'ExecMsg' and Return the result with module cache
--
applyExec
    :: CommandEnv p
    -> EvalState
    -> RequestKey
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> [TxLog Value]
    -> NamespacePolicy
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
applyExec env@CommandEnv{..} initState rk em senderSigs hsh prevLogs nsp = do
    EvalResult{..} <- applyExec' env initState em senderSigs hsh nsp
    -- applyExec enforces non-empty expression set so `last` ok
    return $! T2 (CommandResult rk _erTxId (PactResult (Right (last _erOutput)))
      _erGas (Just $ prevLogs <> _erLogs) _erExec Nothing) _erLoadedModules -- TODO add perf metadata

-- | Variation on 'applyExec' that returns 'EvalResult' as opposed to
-- wrapping it up in a JSON result.
--
applyExec'
    :: CommandEnv p
    -> EvalState
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> IO EvalResult
applyExec' CommandEnv{..} initState (ExecMsg parsedCode execData) senderSigs hsh nsp = do
    -- fail if parsed code contains no expressions
    --
    when (null $ _pcExps parsedCode) $
      throwCmdEx "No expressions found"

    er <- evalExec senderSigs initState evalEnv parsedCode

    for_ (_erExec er) $ \PactExec{..} -> logLog _ceLogger "DEBUG"
      $ "applyExec: new pact added: "
      <> show (_pePactId, _peStep, _peYield, _peExecuted)

    return er
  where
    evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
      (MsgData execData Nothing hsh) initRefStore _ceGasEnv
      nsp _ceSPVSupport _cePublicData

managedNamespacePolicy :: NamespacePolicy
managedNamespacePolicy = SmartNamespacePolicy False
  (QualifiedName (ModuleName "ns" Nothing) "validate" def)

-- | Execute a 'ContMsg' and return the command result and module cache
--
applyContinuation
    :: CommandEnv p
    -> EvalState
    -> RequestKey
    -> ContMsg
    -> [Signer]
    -> Hash
    -> [TxLog Value]
    -> NamespacePolicy
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
applyContinuation env@CommandEnv{..} initState rk cm senderSigs hsh prevLogs nsp = do
    EvalResult{..} <- applyContinuation' env initState cm senderSigs hsh nsp
    -- last safe here because cont msg is guaranteed one exp

    return $! T2 (CommandResult rk _erTxId (PactResult (Right (last _erOutput)))
      _erGas (Just $ prevLogs <> _erLogs) _erExec Nothing) _erLoadedModules -- TODO add perf metadata

-- | Execute a 'ContMsg' and return just eval result, not wrapped in a
-- 'CommandResult' wrapper
--
applyContinuation'
    :: CommandEnv p
    -> EvalState
    -> ContMsg
    -> [Signer]
    -> Hash
    -> NamespacePolicy
    -> IO EvalResult
applyContinuation' CommandEnv{..} initState cm senderSigs hsh nsp =
    evalContinuation senderSigs initState evalEnv cm
  where
    step = _cmStep cm
    rollback = _cmRollback cm
    pid = _cmPactId cm
    pactStep = Just $ PactStep step rollback pid Nothing
    evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
      (MsgData (_cmData cm) pactStep hsh) initRefStore
      _ceGasEnv nsp _ceSPVSupport _cePublicData

-- | Initial gas charged for transaction size
--   ignoring the size of a continuation proof, if present
--
initialGasOf :: PayloadWithText -> Gas
initialGasOf cmd = Gas (fromIntegral gasFee)
  where
    feePerByte :: Float = 0.01

    contProofSize :: Float = realToFrac $
      case _pPayload (_payloadObj cmd) of
        Continuation (ContMsg _ _ _ _ (Just (ContProof p))) -> B.length p
        _ -> 0
    txSize :: Float = realToFrac (SB.length (_payloadBytes cmd))
    actualSize = txSize - contProofSize
    gasFee :: Int = ceiling (actualSize * feePerByte)

-- | Build and execute 'coin.buygas' command from miner info and user command
-- info (see 'TransactionExec.applyCmd')
--
-- see: 'pact/coin-contract/coin.pact#fund-tx'
--
buyGas
    :: CommandEnv p
    -> Command (Payload PublicMeta ParsedCode)
    -> Miner
    -> GasSupply
    -> ModuleCache
    -> IO (Either Text (T3 GasId [TxLog Value] ModuleCache))
buyGas env cmd (Miner mid mks) supply mcache = do
    let sender    = view (cmdPayload . pMeta . pmSender) cmd
        initState = set (evalRefs . rsLoadedModules) mcache
          $ initCapabilities [magic_FUND_TX]

        chash = toUntypedHash (_cmdHash cmd)

    let bgHash = case chash of Hash h -> Hash (h <> "-buygas")

    buyGasCmd <- mkBuyGasCmd mid mks sender supply
    result <- applyExec' env initState buyGasCmd
      (_pSigners $ _cmdPayload cmd) bgHash managedNamespacePolicy

    let !mcache' = _erLoadedModules result

    case _erExec result of
      Nothing -> return $!
        Left "buyGas: Internal error - empty continuation"
      Just pe -> return $! Right $ T3 (GasId $ _pePactId pe) (_erLogs result) mcache'

-- | Build and execute 'coin.redeem-gas' command from miner info and previous
-- command results (see 'TransactionExec.applyCmd')
--
-- see: 'pact/coin-contract/coin.pact#fund-tx'
--
redeemGas
    :: CommandEnv p
    -> Command (Payload PublicMeta ParsedCode)
    -> Gas
    -> CommandResult a -- ^ result from the user command payload
    -> GasId           -- ^ result of the buy-gas continuation
    -> [TxLog Value]   -- ^ previous txlogs
    -> ModuleCache
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
redeemGas env cmd initialGas cmdResult gid prevLogs mcache = do
    let totalGas   = initialGas + _crGas cmdResult
        fee        = gasFeeOf totalGas (gasPriceOf cmd)
        rk         = cmdToRequestKey cmd
        initState  = set (evalRefs . rsLoadedModules) mcache
          $ initCapabilities [magic_FUND_TX]

    applyContinuation env initState rk (redeemGasCmd fee gid)
      (_pSigners $ _cmdPayload cmd) (toUntypedHash $ _cmdHash cmd) prevLogs
      managedNamespacePolicy

  where
    redeemGasCmd fee (GasId pid) =
      ContMsg pid 1 False (object [ "fee" .= fee ]) Nothing

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
      [ "miner-keyset" .= ks
      , "total" .= total
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
      [ "miner-keyset" .= ks
      , "reward" .= reward
      ]
{-# INLINABLE mkCoinbaseCmd #-}

-- | Initialize a fresh eval state with magic capabilities.
-- This is the way we inject the correct guards into the environment
-- during Pact code execution
--
initCapabilities :: [CapSlot Capability] -> EvalState
initCapabilities cs = set (evalCapabilities . capStack) cs def
{-# INLINABLE initCapabilities #-}

-- | Builder for "magic" capabilities given a magic cap name
--
mkMagicCapSlot :: Text -> CapSlot Capability
mkMagicCapSlot c = CapSlot CapCallStack cap []
  where
    mn = ModuleName "coin" Nothing
    fqn = QualifiedName mn c def
    cap = UserCapability fqn []
{-# INLINABLE mkMagicCapSlot #-}

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
{-# INLINABLE mkGasEnvOf #-}

-- | Retrieve public metadata from a command
--
publicMetaOf :: Command (Payload PublicMeta c) -> PublicMeta
publicMetaOf = _pMeta . _cmdPayload
{-# INLINABLE publicMetaOf #-}

-- | Retrieve the optional Network identifier from a command
--
networkIdOf :: Command (Payload a b) -> Maybe NetworkId
networkIdOf = _pNetworkId . _cmdPayload
{-# INLINEABLE networkIdOf #-}

-- | Calculate gas supply (user-specified limit * user-specified price) of
-- a command
--
gasSupplyOf :: Command (Payload PublicMeta c) -> GasSupply
gasSupplyOf cmd = l * p
  where
    l :: GasSupply = fromIntegral @GasLimit @GasSupply $ gasLimitOf cmd
    p :: GasSupply = fromRational $ toRational $ gasPriceOf cmd
{-# INLINABLE gasSupplyOf #-}

-- | Calculate the gas fee (pact-generate gas cost * user-specified gas price),
-- rounding to the nearest stu.
--
gasFeeOf :: Gas -> GasPrice -> GasSupply
gasFeeOf gas (GasPrice (ParsedDecimal gp)) = GasSupply (ParsedDecimal gs)
  where
    gs = roundTo 12 ((fromIntegral gas) * gp)
{-# INLINABLE gasFeeOf #-}

-- | Log request keys at DEBUG when successful
--
logDebugRequestKey :: Logger -> RequestKey -> String -> IO ()
logDebugRequestKey l k reason = logLog l "DEBUG" $ reason <> ": " <> show k

-- | Log request keys and error message at ERROR when failed
--
logErrorRequestKey
    :: Exception e
    => Logger -> RequestKey -> e -> String -> IO ()
logErrorRequestKey l k e reason = logLog l "ERROR" $ reason
    <> ": " <> show k
    <> ": " <> show e
