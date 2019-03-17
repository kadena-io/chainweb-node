{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
  -- * helpers
, bumpExecMode
) where

import Control.Concurrent
import Control.Exception.Safe (SomeException(..), throwM, tryAny)
import Control.Lens hiding ((.=))
import Control.Monad (join, void, when)
import Control.Monad.Catch (Exception(..))

import Data.Aeson
import Data.Decimal (Decimal)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Int (Int64)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import Data.Word (Word64)

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

import Chainweb.Pact.Types (MinerId, MinerInfo(..), MinerKeys, GasSupply(..))
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
    -> Command (Payload PublicMeta ParsedCode)
    -> IO ((CommandResult, [TxLog Value]), ExecutionMode)
applyCmd logger entityM minerInfo pactDbEnv cmdState gasModel startEM cmd = do

    let gasEnv = mkGasEnvOf cmd gasModel
        cmdEnv = CommandEnv entityM startEM pactDbEnv cmdState logger gasEnv
        requestKey = cmdToRequestKey cmd
        modifiedEnv = set ceGasEnv freeGasEnv cmdEnv
        supply = gasSupplyOf cmd

        -- bump the txId for the buyGas transaction
        buyGasEM = bumpExecMode startEM
        buyGasEnv = set ceMode buyGasEM modifiedEnv

    buyGasResultE <- tryAny $ buyGas buyGasEnv cmd minerInfo supply

    case buyGasResultE of
      Left e1 -> do
        logErrorRequestKey logger requestKey e1 "tx failure for requestKey when buying gas"
        r <- jsonErrorResult buyGasEM requestKey e1 [] (Gas 0)
        pure (r, buyGasEM)
      Right buyGasResult -> do
        -- this call needs to fail hard if Left. It means the continuation did not process
        -- correctly, and we should fail the transaction
        pactId <- either fail pure buyGasResult
        logDebugRequestKey logger requestKey "successful gas buy for request key"
        -- Note the use of 'def' here: we run the payload normally without inserting
        -- initial state.

        -- bump the txId for the payload transaction
        let payloadEM = bumpExecMode buyGasEM
        let payloadEnv = set ceMode payloadEM cmdEnv
        cmdResultE <- tryAny $ runPayload payloadEnv def cmd []

        case cmdResultE of
          Left e2 -> do
            logErrorRequestKey logger requestKey e2 "tx failure for request key when running cmd"
            r <- jsonErrorResult payloadEM requestKey e2 [] (Gas 0)
            pure (r, payloadEM)
          Right (cmdResult, cmdLogs) -> do
            logDebugRequestKey logger requestKey "success for requestKey"
            -- bump the txId for the redeem gas transaction
            let redeemGasEM = bumpExecMode payloadEM
            let redeemGasEnv = set ceMode redeemGasEM buyGasEnv
            redeemResultE <- tryAny $ redeemGas redeemGasEnv cmd cmdResult pactId minerInfo supply

            case redeemResultE of
              Left e3 -> do
                logErrorRequestKey logger requestKey e3 "tx failure for request key while redeeming gas"
                r <- jsonErrorResult redeemGasEM requestKey e3 cmdLogs $ _crGas cmdResult
                pure (r, redeemGasEM)
              Right (_, redeemLogs) -> do
                logDebugRequestKey logger requestKey "successful gas redemption for request key"
                pure ((cmdResult, cmdLogs <> redeemLogs), redeemGasEM)

applyGenesisCmd
    :: Logger
    -> Maybe EntityName
    -> PactDbEnv p
    -> MVar CommandState
    -> ExecutionMode
    -> Command (Payload PublicMeta ParsedCode)
    -> IO ((CommandResult, [TxLog Value]), ExecutionMode)
applyGenesisCmd logger entityM dbEnv cmdState execMode cmd = do
    -- cmd env with permissive gas model
    let cmdEnv = CommandEnv entityM execMode dbEnv cmdState logger freeGasEnv
        requestKey = cmdToRequestKey cmd
    -- when calling genesis commands, we bring all capabilities in scope
    let initState = initCapabilities ["TRANSFER", "FUND_TX", "COINBASE"]

        -- bump the txId for the payload transaction
        payloadEM = bumpExecMode execMode
        payloadEnv = set ceMode payloadEM cmdEnv

    resultE <- tryAny $ runPayload payloadEnv initState cmd []
    case resultE of
      Left e -> do
        logErrorRequestKey logger requestKey e
          "genesis tx failure for request key while running genesis"
        r <- jsonErrorResult payloadEM requestKey e [] (Gas 0)
        pure (r, payloadEM)
      Right result -> do
        logDebugRequestKey logger requestKey "successful genesis tx for request key"
        pure $! (result, payloadEM)

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
jsonResult execMode cmd gas a =
    let txId = case execMode of
          Transactional tx -> Just tx
          _otherMode       -> Nothing
    in CommandResult cmd txId (toJSON a) gas

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
    -> GasSupply
    -> IO (Either String PactId)
buyGas env cmd (MinerInfo minerId minerKeys) (GasSupply supply) = do
    let sender    = view (cmdPayload . pMeta . pmSender) cmd
        initState = initCapabilities ["FUND_TX"]

    buyGasCmd <- mkBuyGasCmd minerId minerKeys sender supply
    result <- applyExec' env initState buyGasCmd (_cmdSigs cmd) (_cmdHash cmd)
    pure $! case _erExec result of
      Nothing ->
        Left "buyGas: Internal error - empty continuation"
      Just pe -> Right $! _pePactId pe

-- | Build and execute 'coin.redeem-gas' command from miner info and previous
-- command results (see 'TransactionExec.applyCmd')
redeemGas
    :: CommandEnv p
    -> Command (Payload PublicMeta ParsedCode)
    -> CommandResult -- ^ result from the user command payload
    -> PactId        -- ^ result of the buy-gas continuation
    -> MinerInfo     -- ^ the block miner
    -> GasSupply     -- ^ the total calculated supply of gas (as Decimal)
    -> IO (CommandResult, [TxLog Value])
redeemGas env cmd cmdResult pactId (MinerInfo _minerId minerKeys) (GasSupply supply) = do
    let (Gas fee)  = _crGas cmdResult
        pid        = toTxId pactId
        rk         = cmdToRequestKey cmd
        initState  = initCapabilities ["FUND_TX"]

    applyContinuation env initState rk (redeemGasCmd fee supply pid)
      (_cmdSigs cmd) (_cmdHash cmd) []
  where
    redeemGasCmd fee total pid = ContMsg pid 1 False $ object
      [ "fee" .= (feeOf total fee)
      , "total" .= supply
      , "miner-keyset" .= minerKeys
      ]

    feeOf total fee = total - (fromIntegral @Int64 @Decimal fee)
    -- TODO: this needs to be revisited. 'PactId' contains 'Text', but the 'TxId'
    -- required by 'ContMsg' requires 'Word64'. The 'read' here is dirty but
    -- necessary until this is fixed in pact.
    toTxId (PactId pid) = TxId . read . unpack $ pid

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
        initState = initCapabilities ["COINBASE", "TRANSFER"]

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
    -> Decimal   -- ^ The gas limit total * price
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

mkCoinbaseCmd :: MinerId -> MinerKeys -> Decimal -> IO (ExecMsg ParsedCode)
mkCoinbaseCmd minerId minerKeys reward = buildExecParsedCode coinbaseData
    [text| (coin.coinbase '$minerId (read-keyset 'minerKeys) (read-decimal 'reward)) |]
  where
    coinbaseData = Just $ object [ "miner-keyset" .= minerKeys, "reward" .= reward ]
{-# INLINABLE mkCoinbaseCmd #-}

-- | Initialize a fresh eval state with magic capabilities.
-- This is the way we inject the correct guards into the environment
-- during Pact code execution
initCapabilities :: [Text] -> EvalState
initCapabilities cs = set (evalCapabilities . capGranted) (toCap <$> cs) def
  where
    -- construct an empty capability for coin contract with name 'c'
    toCap c = UserCapability (ModuleName "coin" Nothing) (DefName c) []
{-# INLINABLE initCapabilities #-}

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
      Left err -> fail $ "buildExecParsedCode: parse failed: " <> show err

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

mkGasEnvOf :: Command (Payload PublicMeta c) -> GasModel -> GasEnv
mkGasEnvOf cmd gasModel = GasEnv (gasLimitOf cmd) (gasPriceOf cmd) gasModel
{-# INLINABLE mkGasEnvOf #-}

gasSupplyOf :: Command (Payload PublicMeta c) -> GasSupply
gasSupplyOf cmd =
  let (GasLimit l) = gasLimitOf cmd
      (GasPrice p) = gasPriceOf cmd
  in GasSupply $ (fromIntegral @Word64 @Decimal l) * p
{-# INLINABLE gasSupplyOf #-}

-- | Log request keys at DEBUG when successful
logDebugRequestKey :: Logger -> RequestKey -> String -> IO ()
logDebugRequestKey l k reason = logLog l "DEBUG" $ reason <> ": " <> show k

-- | Log request keys and error message at ERROR when failed
logErrorRequestKey
    :: Exception e
    => Logger -> RequestKey -> e -> String -> IO ()
logErrorRequestKey l k e reason = logLog l "ERROR" $ reason
    <> ": " <> show k
    <> ": " <> show e


bumpExecMode :: ExecutionMode -> ExecutionMode
bumpExecMode (Transactional (TxId txId)) = Transactional (TxId (succ txId))
bumpExecMode otherMode = otherMode
