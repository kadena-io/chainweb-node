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
, locally
) where

import Control.Concurrent
import Control.Exception.Safe (SomeException(..), throwM, tryAny)
import Control.Lens hiding ((.=))
import Control.Monad (join, void, when, unless)
import Control.Monad.Catch (Exception(..))
import Control.Monad.Reader as Reader

import Data.Aeson
import Data.Decimal (Decimal)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Int (Int64)
import qualified Data.Map.Lazy as Map
import Data.Text (Text,pack)
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

import Chainweb.Pact.Types (MinerInfo(..), GasSupply(..))
import Chainweb.Pact.Service.Types (internalError)
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
    -> PublicData
    -> ExecutionMode
    -> SPVSupport
    -> Command (Payload PublicMeta ParsedCode)
    -> IO ((CommandResult, [TxLog Value]), ExecutionMode)
applyCmd logger entityM minerInfo pactDbEnv cmdState gasModel pubData startEM spv cmd = do

    let gasEnv = mkGasEnvOf cmd gasModel
        requestKey = cmdToRequestKey cmd
        supply = gasSupplyOf cmd

    let pd = pubData & pdPublicMeta .~ publicMetaOf cmd
    let cmdEnv = CommandEnv entityM startEM pactDbEnv cmdState logger gasEnv pd
        modifiedEnv = set ceGasEnv freeGasEnv cmdEnv

    -- bump the txId for the buyGas transaction
    let buyGasEM = bumpExecMode startEM
        buyGasEnv = set ceMode buyGasEM modifiedEnv

    buyGasResultE <- tryAny $! buyGas buyGasEnv cmd spv minerInfo supply

    case buyGasResultE of
      Left e1 -> do
        logErrorRequestKey logger requestKey e1 "tx failure for requestKey when buying gas"
        r <- jsonErrorResult buyGasEM requestKey e1 [] (Gas 0)
        pure (r, buyGasEM)
      Right buyGasResult -> do
        -- this call needs to fail hard if Left. It means the continuation did not process
        -- correctly, and we should fail the transaction
        pactId <- either internalError pure buyGasResult
        logDebugRequestKey logger requestKey "successful gas buy for request key"
        -- Note the use of 'def' here: we run the payload normally without inserting
        -- initial state.

        -- bump the txId for the payload transaction
        let payloadEM = bumpExecMode buyGasEM
        let payloadEnv = set ceMode payloadEM cmdEnv

        cmdResultE <- tryAny $! runPayload payloadEnv def cmd spv []

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
            redeemResultE <- tryAny $! redeemGas redeemGasEnv cmd spv cmdResult pactId supply

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
    -> PublicData
    -> SPVSupport
    -> Command (Payload PublicMeta ParsedCode)
    -> IO ((CommandResult, [TxLog Value]), ExecutionMode)
applyGenesisCmd logger entityM dbEnv cmdState execMode pubData spv cmd = do
    -- cmd env with permissive gas model
    let cmdEnv =
          CommandEnv entityM execMode dbEnv cmdState logger freeGasEnv pubData

        requestKey = cmdToRequestKey cmd
    -- when calling genesis commands, we bring all capabilities in scope
    let initState = initCapabilities ["TRANSFER", "FUND_TX", "COINBASE"]

        -- bump the txId for the payload transaction
        payloadEM = bumpExecMode execMode
        payloadEnv = set ceMode payloadEM cmdEnv

    resultE <- tryAny $! runPayload payloadEnv initState cmd spv []
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
    -> SPVSupport
    -> [TxLog Value] -- log state
    -> IO (CommandResult, [TxLog Value])
runPayload env initState c@Command{..} spv txLogs = case _pPayload _cmdPayload of
    Exec pm ->
      applyExec env initState (cmdToRequestKey c) pm _cmdSigs _cmdHash spv txLogs
    Continuation ym ->
      applyContinuation env initState (cmdToRequestKey c) ym _cmdSigs _cmdHash spv txLogs

applyExec
    :: CommandEnv p
    -> EvalState
    -> RequestKey
    -> ExecMsg ParsedCode
    -> [UserSig]
    -> Hash
    -> SPVSupport
    -> [TxLog Value]
    -> IO (CommandResult, [TxLog Value])
applyExec env@CommandEnv{..} initState rk em senderSigs hash spv prevLogs = do
    EvalResult{..} <- applyExec' env initState em senderSigs hash spv
    return (jsonResult _ceMode rk _erGas $
            CommandSuccess (last _erOutput), prevLogs <> _erLogs)

-- | variation on 'applyExec' that returns 'EvalResult' as opposed to
-- wrapping it up in a JSON result.
applyExec'
    ::CommandEnv p
    -> EvalState
    -> ExecMsg ParsedCode
    -> [UserSig]
    -> Hash
    -> SPVSupport
    -> IO EvalResult
applyExec' CommandEnv{..} initState (ExecMsg parsedCode execData) senderSigs hash spv = do
    when (null (_pcExps parsedCode)) $ throwCmdEx "No expressions found"
    (CommandState refStore pacts) <- readMVar _ceState
    let sigs = userSigsToPactKeySet senderSigs
        evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
                  (MsgData sigs execData Nothing hash) refStore _ceGasEnv
                  permissiveNamespacePolicy spv _cePublicData
    er@EvalResult{..} <- evalExecState initState evalEnv parsedCode
    newCmdPact <- join <$> mapM (handlePactExec _erInput) _erExec
    let newPacts = case newCmdPact of
          Nothing -> pacts
          Just cmdPact -> Map.insert (_pePactId cmdPact) cmdPact pacts
    void $! swapMVar _ceState $ CommandState _erRefStore newPacts
    for_ newCmdPact $ \PactExec{..} ->
      logLog _ceLogger "DEBUG" $ "applyExec: new pact added: " ++ show (_pePactId,_peStep,_peYield,_peExecuted)
    return er

handlePactExec :: Either PactContinuation [Term Name] -> PactExec -> IO (Maybe PactExec)
handlePactExec (Left pc) _ = throwCmdEx $ "handlePactExec: internal error, continuation input: " ++ show pc
handlePactExec (Right em) pe = do
  unless (length em == 1) $
    throwCmdEx $ "handlePactExec: defpact execution must occur as a single command: " ++ show em
  return $ Just pe


applyContinuation
    :: CommandEnv p
    -> EvalState
    -> RequestKey
    -> ContMsg
    -> [UserSig]
    -> Hash
    -> SPVSupport
    -> [TxLog Value]
    -> IO (CommandResult, [TxLog Value])
applyContinuation env@CommandEnv{..} initState rk msg@ContMsg{..} senderSigs hash spv prevLogs = do
  EvalResult{..} <- applyContinuation' env initState msg senderSigs hash spv
  pure $! (jsonResult _ceMode rk _erGas $
           CommandSuccess (last _erOutput), prevLogs <> _erLogs)

applyContinuation'
    :: CommandEnv p
    -> EvalState
    -> ContMsg
    -> [UserSig]
    -> Hash
    -> SPVSupport
    -> IO EvalResult
applyContinuation' env@CommandEnv{..} initState msg@ContMsg{..} senderSigs hash spv =
    case _ceMode of
        Local -> throwCmdEx "Local continuation exec not supported"
        Transactional _ -> do
            state@CommandState{..} <- readMVar _ceState
            case Map.lookup _cmPactId _csPacts of
                Nothing -> throwCmdEx $ "applyContinuation: txid not found: " ++ show _cmPactId
                Just PactExec{..}
          -- Verify valid ContMsg Step
                 -> do
                    when (_cmStep < 0 || _cmStep >= _peStepCount) $
                        throwCmdEx $ "Invalid step value: " ++ show _cmStep
                    if _cmRollback
                        then when (_cmStep /= _peStep) $
                             throwCmdEx
                                 ("Invalid rollback step value: Received " ++
                                  show _cmStep ++ " but expected " ++ show _peStep)
                        else when
                                 (_cmStep /= (_peStep + 1))
                                 (throwCmdEx $
                                  "Invalid continuation step value: Received " ++
                                  show _cmStep ++ " but expected " ++ show (_peStep + 1))
          -- Setup environement and get result
                    let sigs = userSigsToPactKeySet senderSigs
                        pactStep =
                            Just $ PactStep _cmStep _cmRollback _cmPactId _peYield
                        evalEnv =
                            setupEvalEnv
                                _ceDbEnv
                                _ceEntity
                                _ceMode
                                (MsgData sigs _cmData pactStep hash)
                                _csRefStore
                                _ceGasEnv
                                permissiveNamespacePolicy
                                spv
                                _cePublicData
                    res <- tryAny $ evalContinuationState initState evalEnv _peContinuation
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
                                else continuationUpdate env msg state exec
                            pure er

rollbackUpdate :: CommandEnv p -> ContMsg -> CommandState -> IO ()
rollbackUpdate CommandEnv{..} ContMsg{..} CommandState{..} = do
    -- if step doesn't have a rollback function, no error thrown.
    -- Therefore, pact will be deleted from state.
    let newState = CommandState _csRefStore $ Map.delete _cmPactId _csPacts
    void $! logLog _ceLogger "DEBUG" $
      "applyContinuation: rollbackUpdate: reaping pact " ++ show _cmPactId
    void $! swapMVar _ceState newState

continuationUpdate
    :: CommandEnv p
    -> ContMsg
    -> CommandState
    -> PactExec
    -> IO ()
continuationUpdate CommandEnv{..} ContMsg{..} CommandState{..} newPactExec@PactExec{..} = do
    let nextStep = _cmStep + 1
        isLast = nextStep >= _peStepCount
        updateState pacts = CommandState _csRefStore pacts -- never loading modules during continuations
    if isLast
        then do
          logLog _ceLogger "DEBUG" $
            "applyContinuation: continuationUpdate: reaping pact: " ++ show _pePactId
          void $! swapMVar _ceState $ updateState $ Map.delete _pePactId _csPacts
        else do
            logLog _ceLogger "DEBUG" $ "applyContinuation: updated state of pact "
              ++ show _pePactId ++ ": " ++ show newPactExec
            void $! swapMVar _ceState $ updateState $
              Map.insert _pePactId newPactExec _csPacts

------------------------------------------------------------------------------
-- Coin Contract
------------------------------------------------------------------------------

-- | Build and execute 'coin.buygas' command from miner info and user command
-- info (see 'TransactionExec.applyCmd')
--
-- see: pact/coin-contract/coin.pact#fund-tx
--
buyGas
    :: CommandEnv p
    -> Command (Payload PublicMeta ParsedCode)
    -> SPVSupport
    -> MinerInfo
    -> GasSupply
    -> IO (Either Text PactId)
buyGas env cmd spv (MinerInfo minerId minerKeys) (GasSupply supply) = do
    let sender    = view (cmdPayload . pMeta . pmSender) cmd
        initState = initCapabilities ["FUND_TX"]

    buyGasCmd <- mkBuyGasCmd minerId minerKeys sender supply
    result <- applyExec' env initState buyGasCmd (_cmdSigs cmd) (_cmdHash cmd) spv
    pure $! case _erExec result of
      Nothing ->
        Left "buyGas: Internal error - empty continuation"
      Just pe -> Right $! _pePactId pe

-- | Build and execute 'coin.redeem-gas' command from miner info and previous
-- command results (see 'TransactionExec.applyCmd')
--
-- see: pact/coin-contract/coin.pact#fund-tx
--
redeemGas
    :: CommandEnv p
    -> Command (Payload PublicMeta ParsedCode)
    -> SPVSupport
    -> CommandResult -- ^ result from the user command payload
    -> PactId        -- ^ result of the buy-gas continuation
    -> GasSupply     -- ^ the total calculated supply of gas (as Decimal)
    -> IO (CommandResult, [TxLog Value])
redeemGas env cmd spv cmdResult pactId (GasSupply supply) = do
    let (Gas fee)  = _crGas cmdResult
        rk         = cmdToRequestKey cmd
        initState  = initCapabilities ["FUND_TX"]

    applyContinuation env initState rk (redeemGasCmd fee supply pactId)
      (_cmdSigs cmd) (_cmdHash cmd) spv []
  where
    redeemGasCmd fee total pid = ContMsg pid 1 False $ object
      [ "fee" .= feeOf total fee
      ]

    feeOf total fee = total - fromIntegral @Int64 @Decimal fee

-- | The miner reward function (i.e. 'coinbase'). Miners are rewarded
-- on a per-block, rather than a per-transaction basis.
--
-- See: 'pact/coin-contract/coin.pact#coinbase'
--
coinbase
    :: CommandEnv p
    -> Command (Payload PublicMeta ParsedCode)
    -> SPVSupport
    -> MinerInfo -- ^ Account to associate reward
    -> Decimal   -- ^ Reward amount
    -> IO (CommandResult, [TxLog Value])
coinbase env cmd spv (MinerInfo minerId minerKeys) reward = do
    let requestKey = cmdToRequestKey cmd
        initState = initCapabilities ["COINBASE", "TRANSFER"]

    coinbaseCmd <- mkCoinbaseCmd minerId minerKeys reward
    applyExec env initState requestKey coinbaseCmd (_cmdSigs cmd) (_cmdHash cmd) spv []

------------------------------------------------------------------------------
-- Command Builders
------------------------------------------------------------------------------

-- | Build the 'coin-contract.buygas' command
--
mkBuyGasCmd
    :: Text    -- ^ Id of the miner to fund
    -> KeySet  -- ^ Miner keyset
    -> Text    -- ^ Address of the sender from the command
    -> Decimal -- ^ The gas limit total * price
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

mkCoinbaseCmd :: Text -> KeySet -> Decimal -> IO (ExecMsg ParsedCode)
mkCoinbaseCmd minerId minerKeys reward = buildExecParsedCode coinbaseData
    [text|
      (coin.coinbase '$minerId (read-keyset 'minerKeys) (read-decimal 'reward))
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
--
initCapabilities :: [Text] -> EvalState
initCapabilities cs = set (evalCapabilities . capGranted) (toCap <$> cs) def
  where
    -- construct an empty capability for coin contract with name 'c'
    toCap c = UserCapability (ModuleName "coin" Nothing) (DefName c) []
{-# INLINABLE initCapabilities #-}

-- | Build the 'ExecMsg' for some pact code fed to the function. The 'value'
-- parameter is for any possible environmental data that needs to go into
-- the 'ExecMsg'.
--
buildExecParsedCode :: Maybe Value -> Text -> IO (ExecMsg ParsedCode)
buildExecParsedCode value code = maybe (go Null) go value
  where
    go v = case ParsedCode code <$> parseExprs code of
      Right t -> pure $ ExecMsg t v
      -- if we can't construct coin contract calls, this should
      -- fail fast
      Left err -> internalError $ "buildExecParsedCode: parse failed: " <> pack err

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

-- | Create a gas environment from a verified command
--
mkGasEnvOf :: Command (Payload PublicMeta c) -> GasModel -> GasEnv
mkGasEnvOf cmd gasModel = GasEnv (gasLimitOf cmd) (gasPriceOf cmd) gasModel
{-# INLINABLE mkGasEnvOf #-}

publicMetaOf :: Command(Payload PublicMeta c) -> PublicMeta
publicMetaOf = _pMeta . _cmdPayload

gasSupplyOf :: Command (Payload PublicMeta c) -> GasSupply
gasSupplyOf cmd =
  let (GasLimit l) = gasLimitOf cmd
      (GasPrice p) = gasPriceOf cmd
  in GasSupply $ (fromIntegral @Word64 @Decimal l) * p
{-# INLINABLE gasSupplyOf #-}

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

-- | Bump the tx id for a given transaction
--
bumpExecMode :: ExecutionMode -> ExecutionMode
bumpExecMode (Transactional (TxId txId)) = Transactional (TxId (succ txId))
bumpExecMode otherMode = otherMode

-- | Like 'local' for reader environments, but modifies the
-- target of a lens possibly deep in the environment
--
locally :: MonadReader s m => ASetter s s a b -> (a -> b) -> m r -> m r
locally l f = Reader.local (over l f)
