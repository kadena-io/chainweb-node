{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
, applyExec
, applyExec'
, applyContinuation
, applyContinuation'
  -- * coin contract api
, buyGas
, coinbase
, redeemGas
  -- * commands
, mkBuyGasCmd
, mkCoinbaseCmd
  -- * code parsing utils
, buildExecParsedCode
, initCapabilities
) where

import Control.Concurrent
import Control.Exception.Safe (SomeException(..), throwM, tryAny)
import Control.Lens hiding ((.=))
import Control.Monad (join, void, when)
import Control.Monad.Catch (Exception(..))

import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Decimal (Decimal)
import Data.Default (def)
import Data.Foldable (for_)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import Data.Text.Lazy (toStrict)

import NeatInterpolation (text)

-- internal Pact modules

import Pact.Gas (freeGasEnv)
import Pact.Interpreter
import Pact.Parse (parseExprs)
import Pact.Types.Command
import Pact.Types.Gas (Gas(..), GasLimit(..), GasModel(..))
import Pact.Types.Logger
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.Server
import Pact.Types.Term (DefName(..), ModuleName(..), Name(..), Term(..))
import Pact.Types.Util (Hash(..))

-- internal Chainweb modules

import Chainweb.Pact.Types (MinerId, MinerInfo(..), MinerKeys)
import Chainweb.Transaction (gasLimitOf, gasPriceOf)

------------------------------------------------------------------------------
-- Transaction logic
------------------------------------------------------------------------------

applyCmd
    :: Logger
    -> Maybe EntityName
    -> MinerInfo
    -> PactDbEnv p
    -> MVar CommandState
    -> GasModel
    -> ExecutionMode
    -> Command a
    -> ProcessedCommand PublicMeta ParsedCode
    -> IO (CommandResult, [TxLog Value])
applyCmd _ _ _ _ _ _ ex cmd (ProcFail s) = pure (jsonResult ex (cmdToRequestKey cmd) (Gas 0) s, [])
applyCmd logger entityM minerInfo pactDbEnv cmdState gasModel exMode _ (ProcSucc cmd) = do

    let gasEnv = mkGasEnvOf cmd gasModel
        cmdEnv = CommandEnv entityM exMode pactDbEnv cmdState logger gasEnv
        requestKey = cmdToRequestKey cmd
        modifiedEnv = set ceGasEnv freeGasEnv cmdEnv

    buyGasResultE <- tryAny $ buyGas modifiedEnv cmd minerInfo

    case buyGasResultE of
      Left e1 -> do
        logErrorRequestKey logger requestKey e1 "tx failure for requestKey when buying gas"
        jsonErrorResult exMode requestKey e1 [] (Gas 0)
      Right _buyGasResult -> do
        -- this call needs to fail hard if Left. It means the continuation did not process
        -- correctly, and we should fail the transaction
        -- pactId <- either fail pure buyGasResult

        logDebugRequestKey logger requestKey "successful gas buy for request key"
        -- Note the use of 'def' here: we run the payload normally without inserting
        -- initial state.
        cmdResultE <- tryAny $ runPayload cmdEnv def cmd []
        case cmdResultE of
          Left e2 -> do
            logErrorRequestKey logger requestKey e2 "tx failure for request key when running cmd"
            jsonErrorResult exMode requestKey e2 [] (Gas 0)
          Right (cmdResult, cmdLogs) -> do
            logDebugRequestKey logger requestKey "success for requestKey"
            redeemResultE <- tryAny $ redeemGas modifiedEnv cmd cmdResult {- pactId -} cmdLogs
            case redeemResultE of
              Left e3 -> do
                logErrorRequestKey logger requestKey e3 "tx failure for request key while redeeming gas"
                jsonErrorResult exMode requestKey e3 cmdLogs $ _crGas cmdResult
              Right (_, redeemLogs) -> do
                logDebugRequestKey logger requestKey "successful gas redemption for request key"
                pure (cmdResult, redeemLogs)

applyGenesisCmd
    :: Logger
    -> Maybe EntityName
    -> PactDbEnv p
    -> MVar CommandState
    -> ExecutionMode
    -> Command a
    -> ProcessedCommand PublicMeta ParsedCode
    -> IO (CommandResult, [TxLog Value])
applyGenesisCmd _ _ _ _ ex cmd (ProcFail pCmd) = pure (jsonResult ex (cmdToRequestKey cmd) (Gas 0) pCmd, [])
applyGenesisCmd logger entityM dbEnv cmdState execMode _ (ProcSucc cmd) = do
    -- cmd env with permissive gas model
    let cmdEnv = CommandEnv entityM execMode dbEnv cmdState logger freeGasEnv
        requestKey = cmdToRequestKey cmd

    resultE <- tryAny $ runPayload cmdEnv def cmd []
    case resultE of
      Left e -> do
        logErrorRequestKey logger requestKey e
          "genesis tx failure for request key while running genesis"
        jsonErrorResult execMode requestKey e [] (Gas 0)
      Right result -> do
        logDebugRequestKey logger requestKey "successful genesis tx for request key"
        pure $! result

-- | Present a failure as a pair of json result of Command Error and associated logs
jsonErrorResult
    :: ExecutionMode
    -> RequestKey
    -> SomeException
    -> [TxLog Value]
    -> Gas
    -> IO (CommandResult, [TxLog Value])
jsonErrorResult exMode reqKey err txLogs gas = pure $!
    ( jsonResult exMode reqKey gas $
        CommandError "Command execution failed" (Just . show $ err)
    , txLogs
    )

jsonResult :: ToJSON a => ExecutionMode -> RequestKey -> Gas -> a -> CommandResult
jsonResult ex cmd gas a = CommandResult cmd (exToTx ex) (toJSON a) gas

exToTx :: ExecutionMode -> Maybe TxId
exToTx (Transactional t) = Just t
exToTx Local = Nothing

runPayload
    :: CommandEnv p
    -> EvalState
    -> Command (Payload PublicMeta ParsedCode)
    -> [TxLog Value] -- log state
    -> IO (CommandResult, [TxLog Value])
runPayload env initState c@Command{..} txLogs = case _pPayload _cmdPayload of
    Exec pm ->
      applyExec env initState (cmdToRequestKey c) pm _cmdSigs _cmdHash txLogs
    Continuation ym ->
      applyContinuation env initState (cmdToRequestKey c) ym _cmdSigs _cmdHash txLogs

applyExec
    :: CommandEnv p
    -> EvalState
    -> RequestKey
    -> ExecMsg ParsedCode
    -> [UserSig]
    -> Hash
    -> [TxLog Value]
    -> IO (CommandResult, [TxLog Value])
applyExec env@CommandEnv{..} initState rk em senderSigs hash prevLogs = do
  EvalResult{..} <- applyExec' env initState em senderSigs hash
  return (jsonResult _ceMode rk _erGas $
          CommandSuccess (last _erOutput), prevLogs <> _erLogs)

-- | variation on 'applyExec' that returns 'EvalResult' as opposed to
-- wrapping it up in a JSON result.
applyExec'
    :: CommandEnv p
    -> EvalState
    -> ExecMsg ParsedCode
    -> [UserSig]
    -> Hash
    -> IO EvalResult
applyExec' env@CommandEnv{..} initState (ExecMsg parsedCode execData) senderSigs hash = do
    when (null (_pcExps parsedCode)) $ throwCmdEx "No expressions found"
    (CommandState refStore pacts) <- readMVar _ceState
    let sigs = userSigsToPactKeySet senderSigs
        evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
                  (MsgData sigs execData Nothing hash) refStore _ceGasEnv permissiveNamespacePolicy
    er@EvalResult{..} <- evalExecState initState evalEnv parsedCode
    newCmdPact <- join <$> mapM (handlePactExec env _erInput) _erExec
    let newPacts = case newCmdPact of
          Nothing -> pacts
          Just cmdPact -> Map.insert (_cpTxId cmdPact) cmdPact pacts
    void $! swapMVar _ceState $ CommandState _erRefStore newPacts
    for_ newCmdPact $ \p ->
      logLog _ceLogger "DEBUG" $ "applyExec: new pact added: " ++ show p
    return er

handlePactExec :: CommandEnv p -> [Term Name] -> PactExec -> IO (Maybe CommandPact)
handlePactExec CommandEnv{..} [t] PactExec{..} = case _ceMode of
    Local -> return Nothing
    Transactional tid -> return $ Just $ CommandPact tid t _peStepCount _peStep _peYield
handlePactExec _ em _ =  throwCmdEx $
    "handlePactExec: defpact execution must occur as a single command: " ++ show em

applyContinuation
    :: CommandEnv p
    -> EvalState
    -> RequestKey
    -> ContMsg
    -> [UserSig]
    -> Hash
    -> [TxLog Value]
    -> IO (CommandResult, [TxLog Value])
applyContinuation env@CommandEnv{..} initState rk msg@ContMsg{..} senderSigs hash prevLogs = do
  EvalResult{..} <- applyContinuation' env initState msg senderSigs hash
  pure $! (jsonResult _ceMode rk _erGas $
           CommandSuccess (last _erOutput), prevLogs <> _erLogs)

applyContinuation'
    :: CommandEnv p
    -> EvalState
    -> ContMsg
    -> [UserSig]
    -> Hash
    -> IO EvalResult
applyContinuation' env@CommandEnv{..} initState msg@ContMsg{..} senderSigs hash =
    case _ceMode of
        Local -> throwCmdEx "Local continuation exec not supported"
        Transactional _ -> do
            state@CommandState{..} <- readMVar _ceState
            case Map.lookup _cmTxId _csPacts of
                Nothing -> throwCmdEx $ "applyContinuation: txid not found: " ++ show _cmTxId
                Just pact@CommandPact{..}
          -- Verify valid ContMsg Step
                 -> do
                    when (_cmStep < 0 || _cmStep >= _cpStepCount) $
                        throwCmdEx $ "Invalid step value: " ++ show _cmStep
                    if _cmRollback
                        then when (_cmStep /= _cpStep) $
                             throwCmdEx
                                 ("Invalid rollback step value: Received " ++
                                  show _cmStep ++ " but expected " ++ show _cpStep)
                        else when
                                 (_cmStep /= (_cpStep + 1))
                                 (throwCmdEx $
                                  "Invalid continuation step value: Received " ++
                                  show _cmStep ++ " but expected " ++ show (_cpStep + 1))
          -- Setup environement and get result
                    let sigs = userSigsToPactKeySet senderSigs
                        pactStep =
                            Just $ PactStep _cmStep _cmRollback (PactId $ pack $ show _cmTxId) _cpYield
                        evalEnv =
                            setupEvalEnv
                                _ceDbEnv
                                _ceEntity
                                _ceMode
                                (MsgData sigs _cmData pactStep hash)
                                _csRefStore
                                _ceGasEnv
                                permissiveNamespacePolicy
                    res <- tryAny $ evalContinuationState initState evalEnv _cpContinuation
          -- Update pact's state
                    case res of
                        Left (SomeException ex) -> throwM ex
                        Right er@EvalResult{..} -> do
                            exec@PactExec{..} <-
                                maybe
                                    (throwCmdEx "No pact execution in continuation exec!")
                                    return
                                    _erExec
                            if _cmRollback
                                then rollbackUpdate env msg state
                                else continuationUpdate env msg state pact exec
                            pure er

rollbackUpdate :: CommandEnv p -> ContMsg -> CommandState -> IO ()
rollbackUpdate CommandEnv{..} ContMsg{..} CommandState{..} = do
    -- if step doesn't have a rollback function, no error thrown.
    -- Therefore, pact will be deleted from state.
    let newState = CommandState _csRefStore $ Map.delete _cmTxId _csPacts
    logLog _ceLogger "DEBUG" $
        "applyContinuation: rollbackUpdate: reaping pact " ++ show _cmTxId
    void $! swapMVar _ceState newState

continuationUpdate
    :: CommandEnv p
    -> ContMsg
    -> CommandState
    -> CommandPact
    -> PactExec
    -> IO ()
continuationUpdate CommandEnv{..} ContMsg{..} CommandState{..} CommandPact{..} PactExec{..} = do
    let nextStep = _cmStep + 1
        isLast = nextStep >= _cpStepCount
        updateState pacts = CommandState _csRefStore pacts -- never loading modules during continuations
    if isLast
        then do
          logLog _ceLogger "DEBUG" $
            "applyContinuation: continuationUpdate: reaping pact: " ++ show _cmTxId
          void $! swapMVar _ceState $ updateState $ Map.delete _cmTxId _csPacts
        else do
            let newPact = CommandPact _cpTxId _cpContinuation
                  _cpStepCount _cmStep _peYield
            logLog _ceLogger "DEBUG" $ "applyContinuation: updated state of pact " ++ show _cmTxId ++ ": " ++ show newPact
            void $! swapMVar _ceState $ updateState $
              Map.insert _cmTxId newPact _csPacts

------------------------------------------------------------------------------
-- Coin Contract
------------------------------------------------------------------------------

-- | Build and execute 'coin.buygas' command from miner info and user command
-- info (see 'TransactionExec.applyCmd')
buyGas
    :: CommandEnv p
    -> Command (Payload PublicMeta ParsedCode)
    -> MinerInfo
    -> IO (CommandResult, [TxLog Value])
--    -> IO (Either String PactId)
buyGas env cmd (MinerInfo _minerId _minerKeys) = do
    let _gasLimit  = gasLimitOf cmd
        _sender    = view (cmdPayload . pMeta . pmSender) cmd
        _initState  = initCapabilities ["TRANSFER", "FUND_TX"]
        requestKey = cmdToRequestKey cmd

    -- buyGasCmd <- mkBuyGasCmd minerId minerKeys sender gasLimit
    buyGasCmd <- buildExecParsedCode Nothing "(+ 1 1)"
    applyExec env def requestKey buyGasCmd (_cmdSigs cmd) (_cmdHash cmd) []
    -- applyExec' env initState buyGasCmd (_cmdSigs cmd) (_cmdHash cmd)
    -- pure $! case _erExec result of
    --   Nothing ->
    --     Left "buyGas: Internal error - continuation result is Nothing"
    --   Just pe -> Right $! _pePactId pe

-- | Build and execute 'coin.redeem-gas' command from miner info and previous
-- command results (see 'TransactionExec.applyCmd')
redeemGas
    :: CommandEnv p
    -> Command (Payload PublicMeta ParsedCode)
    -> CommandResult -- ^ result from the user command payload
--    -> PactId        -- ^ result of the buy-gas continuation
    -> [TxLog Value] -- ^ log state (to be passed along to 'applyExec')
    -> IO (CommandResult, [TxLog Value])
redeemGas env cmd cmdResult {- pactId -} txLogs = do
    let requestKey = cmdToRequestKey cmd
        _limit      = gasLimitOf cmd
        _initState  = initCapabilities ["TRANSFER", "FUND_TX"]
        _gas        = _crGas cmdResult
        -- pid        = toTxId pactId

    redeemGasCmd <- buildExecParsedCode Nothing "(+ 2 2)"
    applyExec env def requestKey redeemGasCmd (_cmdSigs cmd) (_cmdHash cmd) txLogs
  --   applyContinuation env initState requestKey (redeemGasCmd gas limit pid)
  --     (_cmdSigs cmd) (_cmdHash cmd) txLogs
  -- where
  --   redeemGasCmd (Gas g) (GasLimit l) pid = ContMsg pid 1 False $
  --     object [ "fee" .= (l - fromIntegral g) ]

  --   -- TODO: this needs to be revisited. 'PactId' contains 'Text', but the 'TxId'
  --   -- required by 'ContMsg' requires 'Word64'. The 'read' here is dirty but
  --   -- necessary until this is fixed in pact.
  --   toTxId (PactId pid) = TxId . read . unpack $ pid

-- | The miner reward function (i.e. 'coinbase'). Miners are rewarded
-- on a per-block, rather than a per-transaction basis.
--
-- See: 'pact/coin-contract/coin-contract.pact#coinbase'
--
coinbase
    :: CommandEnv p
    -> Command (Payload PublicMeta ParsedCode)
    -> MinerInfo -- ^ Account to associate reward
    -> Decimal   -- ^ Reward amount
    -> IO (CommandResult, [TxLog Value])
coinbase env cmd (MinerInfo minerId minerKeys) reward = do
    let requestKey = cmdToRequestKey cmd
        initState = initCapabilities ["COINBASE"]

    coinbaseCmd <- mkCoinbaseCmd minerId minerKeys reward
    applyExec env initState requestKey coinbaseCmd (_cmdSigs cmd) (_cmdHash cmd) []

------------------------------------------------------------------------------
-- Command Builders
------------------------------------------------------------------------------

-- | Build the 'coin-contract.buygas' command
mkBuyGasCmd
    :: MinerId   -- ^ Id of the miner to fund
    -> MinerKeys -- ^ Miner keyset
    -> Text      -- ^ Address of the sender from the command
    -> GasLimit
    -> IO (ExecMsg ParsedCode)
mkBuyGasCmd minerId minerKeys sender gasLimit =
    buildExecParsedCode buyGasData
      [text|
        (coin.fund-tx ('$sender '$minerId (read-keyset 'miner-keyset) $gl))
      |]
  where
    gl = gasLimitToText gasLimit
    buyGasData = Just $ object [ "miner-keyset" .= minerKeys ]
{-# INLINE mkBuyGasCmd #-}

mkCoinbaseCmd :: MinerId -> MinerKeys -> Decimal -> IO (ExecMsg ParsedCode)
mkCoinbaseCmd minerId minerKeys reward = buildExecParsedCode coinbaseData
    [text| (coin.coinbase '$minerId (read-keyset 'minerKeys) $rr) |]
  where
    rr  = pack . show $ reward
    coinbaseData = Just $ object [ "miner-keyset" .= minerKeys ]
{-# INLINE mkCoinbaseCmd #-}

-- | Build the 'ExecMsg' for some pact code fed to the function. The 'value'
-- parameter is for any possible environmental data that needs to go into
-- the 'ExecMsg'.
buildExecParsedCode :: Maybe Value -> Text -> IO (ExecMsg ParsedCode)
buildExecParsedCode value code = maybe (go Null) go value
  where
    go v = case ParsedCode code <$> parseExprs code of
      Right t -> pure $ ExecMsg t v
      -- if we can't construct coin contract calls, this should
      -- fail fast
      Left err -> fail $ "Coin contract call failed: " <> show err
{-# INLINE buildExecParsedCode #-}

-- | Initialize a fresh eval state with magic capabilities.
-- This is the way we inject the correct guards into the environment
-- during Pact code execution
initCapabilities :: [Text] -> EvalState
initCapabilities cs = set (evalCapabilities . capGranted) (toCap <$> cs) def
  where
    -- construct an empty capability for coin contract with name 'c'
    toCap c = UserCapability (ModuleName "coin" Nothing) (DefName c) []
{-# INLINE initCapabilities #-}

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

mkGasEnvOf :: Command (Payload PublicMeta c) -> GasModel -> GasEnv
mkGasEnvOf cmd gasModel = GasEnv (gasLimitOf cmd) (gasPriceOf cmd) gasModel
{-# INLINE mkGasEnvOf #-}

gasLimitToText :: GasLimit -> Text
gasLimitToText (GasLimit g) = toStrict . encodeToLazyText $ g
{-# INLINE gasLimitToText #-}

-- | Log request keys at DEBUG when successful
logDebugRequestKey :: Logger -> RequestKey -> String -> IO ()
logDebugRequestKey l k reason = logLog l "DEBUG" $ reason <> ": " <> show k

-- | Log request keys and error message at ERROR when failed
logErrorRequestKey :: Exception e => Logger -> RequestKey -> e -> String -> IO ()
logErrorRequestKey l k e reason = logLog l "ERROR" $ reason
    <> ": "
    <> show k
    <> ": "
    <> show e
