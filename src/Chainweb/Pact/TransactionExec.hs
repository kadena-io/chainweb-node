{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
( -- * pact service api
  applyCmd
, applyGenesisCmd
, applyCoinbase
, applyLocal
, applyExec
, applyExec'
, applyContinuation
, applyContinuation'
, runPayload
  -- * coin contract api
, buyGas
  -- * commands
, mkBuyGasCmd
, mkCoinbaseCmd
  -- * code parsing utils
, buildExecParsedCode
  -- * utilities
, jsonErrorResult
, logDebugRequestKey
, logErrorRequestKey
, publicMetaOf
  -- * capabilities
, magic_COINBASE
, magic_FUND_TX
, initCapabilities
, initModules
) where

import Control.Lens hiding ((.=))
import Control.Monad (when)
import Control.Monad.Catch (Exception(..))

import Data.Aeson
import Data.Default (def)
import Data.Foldable (for_)
import Data.Maybe
import Data.Text (Text, pack)
import Data.Tuple.Strict (T2(..), T3(..))

import NeatInterpolation (text)

-- internal Pact modules

import Pact.Gas (freeGasEnv)
import Pact.Interpreter
import Pact.Parse (parseExprs)
import Pact.Parse (ParsedDecimal)
import Pact.Types.Command
import Pact.Types.Gas (Gas(..), GasLimit(..), GasModel(..))
import Pact.Types.Hash as Pact
import Pact.Types.Logger
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.Server
import Pact.Types.SPV
import Pact.Types.Term (DefName(..), ModuleName(..))

-- internal Chainweb modules

import Chainweb.BlockHash
import Chainweb.Pact.Service.Types (internalError)
import Chainweb.Pact.Types (GasSupply(..), MinerInfo(..), GasId(..), ModuleCache)
import Chainweb.Transaction (gasLimitOf, gasPriceOf)
import Chainweb.Utils (sshow)



magic_COINBASE :: Capability
magic_COINBASE = mkMagicCap "COINBASE"
magic_FUND_TX :: Capability
magic_FUND_TX = mkMagicCap "FUND_TX"



applyCmd
    :: Logger
    -> PactDbEnv p
    -> MinerInfo
    -> GasModel
    -> PublicData
    -> SPVSupport
    -> Command (Payload PublicMeta ParsedCode)
    -> ModuleCache
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
applyCmd logger pactDbEnv minerInfo gasModel pd spv cmd mcache = do

    let userGasEnv = mkGasEnvOf cmd gasModel
        requestKey = cmdToRequestKey cmd
        pd' = set pdPublicMeta (publicMetaOf cmd) pd
        supply = gasSupplyOf cmd

    let buyGasEnv = CommandEnv Nothing Transactional pactDbEnv logger freeGasEnv pd' spv

    buyGasResultE <- catchesPactError $! buyGas buyGasEnv cmd minerInfo supply mcache

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
        let st0 = initModules mcache' def

        cmdResultE <- catchesPactError $! runPayload payloadEnv st0 cmd buyGasLogs

        case cmdResultE of

          Left e2 ->

            jsonErrorResult payloadEnv requestKey e2 buyGasLogs (Gas 0) mcache'
              "tx failure for request key when running cmd"

          Right (T2 cmdResult mcache'') -> do

            logDebugRequestKey logger requestKey "success for requestKey"

            let !redeemGasEnv = set ceGasEnv freeGasEnv payloadEnv
                cmdLogs = fromMaybe [] $ _crLogs cmdResult
            redeemResultE <- catchesPactError $!
              redeemGas redeemGasEnv cmd cmdResult pactId cmdLogs mcache''

            case redeemResultE of

              Left e3 ->

                jsonErrorResult redeemGasEnv requestKey e3 cmdLogs (_crGas cmdResult) mcache''
                  "tx failure for request key while redeeming gas"

              Right (T2 redeemResult mcache''') -> do

                let !redeemLogs = fromMaybe [] $ _crLogs redeemResult
                    !finalResult = over (crLogs . _Just) (<> redeemLogs) cmdResult

                logDebugRequestKey logger requestKey "successful gas redemption for request key"
                pure $! T2 finalResult mcache'''

applyGenesisCmd
    :: Logger
    -> PactDbEnv p
    -> PublicData
    -> SPVSupport
    -> Command (Payload PublicMeta ParsedCode)
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
applyGenesisCmd logger dbEnv pd spv cmd = do
    -- cmd env with permissive gas model

    let pd' = set pdPublicMeta (publicMetaOf cmd) pd
    let cmdEnv = CommandEnv Nothing Transactional dbEnv logger freeGasEnv pd' spv
        requestKey = cmdToRequestKey cmd
    -- when calling genesis commands, we bring all magic capabilities in scope
    let initState = initCapabilities [magic_FUND_TX, magic_COINBASE]

    resultE <- catchesPactError $! runPayload cmdEnv initState cmd []
    fmap (`T2` mempty) $! case resultE of
      Left e ->
        jsonErrorResult' cmdEnv requestKey e [] (Gas 0)
          "genesis tx failure for request key while running genesis"
      Right (T2 result _) -> do
        logDebugRequestKey logger requestKey "successful genesis tx for request key"
        return result


applyCoinbase
    :: Logger
    -> PactDbEnv p
    -> MinerInfo
    -> ParsedDecimal
    -> PublicData
    -> BlockHash
    -> IO (CommandResult [TxLog Value])
applyCoinbase logger dbEnv mi@(MinerInfo mid mks) reward pd ph = do
    -- cmd env with permissive gas model
    let cenv = CommandEnv Nothing Transactional dbEnv logger freeGasEnv pd noSPVSupport
        initState = initCapabilities [magic_COINBASE]
        ch = Pact.Hash (sshow ph)

    let rk = RequestKey ch

    cexec <- mkCoinbaseCmd mid mks reward
    cre <- catchesPactError $! applyExec' cenv initState cexec [] ch

    case cre of
      Left e -> jsonErrorResult' cenv rk e [] (Gas 0) "coinbase tx failure"
      Right !er -> do
        logDebugRequestKey logger rk
          $ "successful coinbase for miner "
          ++ show mi
          ++ ": "
          ++ show reward

        let r = PactResult (Right (last $ _erOutput er))

        return $! CommandResult rk (_erTxId er) r
          (_erGas er) (Just $ _erLogs er) (_erExec er) Nothing


applyLocal
    :: Logger
    -> PactDbEnv p
    -> PublicData
    -> SPVSupport
    -> Command (Payload PublicMeta ParsedCode)
    -> IO (CommandResult [TxLog Value])
applyLocal logger dbEnv pd spv cmd@Command{..} = do

  -- cmd env with permissive gas model
  let pd' = set pdPublicMeta (publicMetaOf cmd) pd
      cmdEnv = CommandEnv Nothing Local dbEnv logger freeGasEnv pd' spv
      requestKey = cmdToRequestKey cmd


  exec <- case _pPayload _cmdPayload of
    (Exec !pm) -> return pm
    _ -> throwCmdEx "local continuations not supported"

  !r <- catchesPactError $!
    applyExec cmdEnv def requestKey exec (_pSigners _cmdPayload) (toUntypedHash _cmdHash) []

  case r of
    Left e -> jsonErrorResult' cmdEnv requestKey e [] 0 "applyLocal"
    Right (T2 rr _) -> return rr


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
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
runPayload env initState c@Command{..} txLogs = case _pPayload _cmdPayload of
    Exec pm ->
      applyExec env initState (cmdToRequestKey c) pm (_pSigners _cmdPayload) (toUntypedHash _cmdHash) txLogs
    Continuation ym ->
      applyContinuation env initState (cmdToRequestKey c) ym (_pSigners _cmdPayload) (toUntypedHash _cmdHash) txLogs

applyExec
    :: CommandEnv p
    -> EvalState
    -> RequestKey
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> [TxLog Value]
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
applyExec env@CommandEnv{..} initState rk em senderSigs hsh prevLogs = do
    EvalResult{..} <- applyExec' env initState em senderSigs hsh
    -- applyExec enforces non-empty expression set so `last` ok
    return $! T2 (CommandResult rk _erTxId (PactResult (Right (last _erOutput)))
      _erGas (Just $ prevLogs <> _erLogs) _erExec Nothing) _erLoadedModules -- TODO add perf metadata

-- | variation on 'applyExec' that returns 'EvalResult' as opposed to
-- wrapping it up in a JSON result.
applyExec'
    :: CommandEnv p
    -> EvalState
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> IO EvalResult
applyExec' CommandEnv{..} initState (ExecMsg parsedCode execData) senderSigs hsh = do
    when (null (_pcExps parsedCode)) $ throwCmdEx "No expressions found"
    let sigs = userSigsToPactKeySet senderSigs
        evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
                  (MsgData sigs execData Nothing hsh) initRefStore _ceGasEnv
                  permissiveNamespacePolicy _ceSPVSupport _cePublicData
    er@EvalResult{..} <- evalExec initState evalEnv parsedCode
    for_ _erExec $ \PactExec{..} ->
      logLog _ceLogger "DEBUG" $ "applyExec: new pact added: " ++
      show (_pePactId, _peStep, _peYield, _peExecuted)
    return er


applyContinuation
    :: CommandEnv p
    -> EvalState
    -> RequestKey
    -> ContMsg
    -> [Signer]
    -> Hash
    -> [TxLog Value]
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
applyContinuation env@CommandEnv{..} initState rk cm senderSigs hsh prevLogs = do
    EvalResult{..} <- applyContinuation' env initState cm senderSigs hsh
    -- last safe here because cont msg is guaranteed one exp

    return $! T2 (CommandResult rk _erTxId (PactResult (Right (last _erOutput)))
      _erGas (Just $ prevLogs <> _erLogs) _erExec Nothing) _erLoadedModules -- TODO add perf metadata


applyContinuation'
    :: CommandEnv p
    -> EvalState
    -> ContMsg
    -> [Signer]
    -> Hash
    -> IO EvalResult
applyContinuation' CommandEnv{..} initState cm senderSigs hsh = do
    let sigs = userSigsToPactKeySet senderSigs
        pactStep = Just $ PactStep (_cmStep cm) (_cmRollback cm) (_cmPactId cm) Nothing

    let evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
          (MsgData sigs (_cmData cm) pactStep hsh) initRefStore
          _ceGasEnv permissiveNamespacePolicy _ceSPVSupport _cePublicData

    evalContinuation initState evalEnv cm


-- | Build and execute 'coin.buygas' command from miner info and user command
-- info (see 'TransactionExec.applyCmd')
--
-- see: 'pact/coin-contract/coin.pact#fund-tx'
--
buyGas
    :: CommandEnv p
    -> Command (Payload PublicMeta ParsedCode)
    -> MinerInfo
    -> GasSupply
    -> ModuleCache
    -> IO (Either Text (T3 GasId [TxLog Value] ModuleCache))
buyGas env cmd (MinerInfo minerId minerKeys) supply mcache = do
    let sender    = view (cmdPayload . pMeta . pmSender) cmd
        initState = initModules mcache
          $ initCapabilities [magic_FUND_TX]

        chash = toUntypedHash (_cmdHash cmd)

    let bgHash = case chash of Hash h -> Hash (h <> "-buygas")

    buyGasCmd <- mkBuyGasCmd minerId minerKeys sender supply
    result <- applyExec' env initState buyGasCmd
      (_pSigners $ _cmdPayload cmd) bgHash

    let mcache' = _erLoadedModules result

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
    -> CommandResult a -- ^ result from the user command payload
    -> GasId           -- ^ result of the buy-gas continuation
    -> [TxLog Value]   -- ^ previous txlogs
    -> ModuleCache
    -> IO (T2 (CommandResult [TxLog Value]) ModuleCache)
redeemGas env cmd cmdResult gid prevLogs mcache = do
    let fee       = gasFeeOf (_crGas cmdResult) (gasPriceOf cmd)
        rk        = cmdToRequestKey cmd
        initState = initModules mcache
          $ initCapabilities [magic_FUND_TX]

    applyContinuation env initState rk (redeemGasCmd fee gid)
      (_pSigners $ _cmdPayload cmd) (toUntypedHash $ _cmdHash cmd) prevLogs

  where
    redeemGasCmd fee (GasId pid) =
      ContMsg pid 1 False (object [ "fee" .= fee ]) Nothing

-- | Build the 'coin-contract.buygas' command
--
mkBuyGasCmd
    :: Text    -- ^ Id of the miner to fund
    -> KeySet  -- ^ Miner keyset
    -> Text    -- ^ Address of the sender from the command
    -> GasSupply -- ^ The gas limit total * price
    -> IO (ExecMsg ParsedCode)
mkBuyGasCmd minerId minerKeys sender total =
    buildExecParsedCode buyGasData
      [text|
        (coin.fund-tx '$sender '$minerId (read-keyset 'miner-keyset) (read-decimal 'total))
        |]
  where
    buyGasData = Just $ object
      [ "miner-keyset" .= minerKeys
      , "total" .= total
      ]
{-# INLINABLE mkBuyGasCmd #-}

mkCoinbaseCmd :: Text -> KeySet -> ParsedDecimal -> IO (ExecMsg ParsedCode)
mkCoinbaseCmd minerId minerKeys reward =
    buildExecParsedCode coinbaseData
      [text|
        (coin.coinbase '$minerId (read-keyset 'miner-keyset) (read-decimal 'reward))
        |]
  where
    coinbaseData = Just $ object
      [ "miner-keyset" .= minerKeys
      , "reward" .= reward
      ]
{-# INLINABLE mkCoinbaseCmd #-}

-- | Initialize a fresh eval state with magic capabilities.
-- This is the way we inject the correct guards into the environment
-- during Pact code execution
initCapabilities :: [Capability] -> EvalState
initCapabilities cs = set (evalCapabilities . capGranted) cs def
{-# INLINABLE initCapabilities #-}

initModules :: ModuleCache -> EvalState -> EvalState
initModules mcache = set (evalRefs . rsLoadedModules) mcache

mkMagicCap :: Text -> Capability
mkMagicCap c = UserCapability (ModuleName "coin" Nothing) (DefName c) []
{-# INLINABLE mkMagicCap #-}

-- | Build the 'ExecMsg' for some pact code fed to the function. The 'value'
-- parameter is for any possible environmental data that needs to go into
-- the 'ExecMsg'.
--
buildExecParsedCode :: Maybe Value -> Text -> IO (ExecMsg ParsedCode)
buildExecParsedCode value code = maybe (go Null) go value
  where
    go v = case ParsedCode code <$> parseExprs code of
      (Right !t) -> pure $! ExecMsg t v
      -- if we can't construct coin contract calls, this should
      -- fail fast
      Left err -> internalError $ "buildExecParsedCode: parse failed: " <> pack err

-- | Create a gas environment from a verified command
--
mkGasEnvOf :: Command (Payload PublicMeta c) -> GasModel -> GasEnv
mkGasEnvOf cmd gasModel = GasEnv (gasLimitOf cmd) (gasPriceOf cmd) gasModel
{-# INLINABLE mkGasEnvOf #-}

-- | Retrieve public metadata from a command
publicMetaOf :: Command (Payload PublicMeta c) -> PublicMeta
publicMetaOf = _pMeta . _cmdPayload
{-#  INLINABLE publicMetaOf #-}

gasSupplyOf :: Command (Payload PublicMeta c) -> GasSupply
gasSupplyOf cmd = l * p
  where
    l :: GasSupply = fromIntegral @GasLimit @GasSupply $ gasLimitOf cmd
    p :: GasSupply = fromRational $ toRational $ gasPriceOf cmd
{-# INLINABLE gasSupplyOf #-}

gasFeeOf :: Gas -> GasPrice -> GasSupply
gasFeeOf gas gp = s * p
  where
    p = fromRational $ toRational gp
    s = fromIntegral @Gas @GasSupply gas
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
