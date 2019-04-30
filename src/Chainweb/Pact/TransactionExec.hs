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
, applyCoinbase
, applyLocal
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
  -- * helpers
, bumpExecMode
) where

import Control.Concurrent
import Control.Exception.Safe (SomeException(..), throwM, tryAny)
import Control.Lens hiding ((.=))
import Control.Monad (join, unless, void, when)
import Control.Monad.Catch (Exception(..))

import Data.Aeson
import Data.Decimal (Decimal)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Int (Int64)
import qualified Data.Map.Lazy as Map
import Data.Text (Text, pack)
import Data.Word (Word64)

import NeatInterpolation (text)

-- internal Pact modules

import Pact.Gas (freeGasEnv)
import Pact.Interpreter
import Pact.Parse (parseExprs)
import Pact.Types.Command
import Pact.Types.Gas (Gas(..), GasLimit(..), GasModel(..))
import qualified Pact.Types.Hash as H
import Pact.Types.Logger
import Pact.Types.PactValue
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.Server
import Pact.Types.Term (DefName(..), ModuleName(..), Name(..), Term(..))

-- internal Chainweb modules

import Chainweb.Pact.Service.Types (internalError)
import Chainweb.Pact.Types (GasSupply(..), MinerInfo(..))
import Chainweb.Transaction (gasLimitOf, gasPriceOf)

------------------------------------------------------------------------------
-- Transaction logic
------------------------------------------------------------------------------

magic_COINBASE :: Capability
magic_COINBASE = mkMagicCap "COINBASE"
magic_FUND_TX :: Capability
magic_FUND_TX = mkMagicCap "FUND_TX"


applyCmd
    :: Logger
    -> PactDbEnv p
    -> MVar CommandState
    -> ExecutionMode
    -> MinerInfo
    -> GasModel
    -> PublicData
    -> SPVSupport
    -> Command (Payload PublicMeta ParsedCode)
    -> IO ((CommandResult, [TxLog Value]), CommandEnv p)
applyCmd logger pactDbEnv cmdState startEM minerInfo gasModel pd spv cmd = do

    let userGasEnv = mkGasEnvOf cmd gasModel
        requestKey = cmdToRequestKey cmd
        pd' = set pdPublicMeta (publicMetaOf cmd) $ pd
        supply = gasSupplyOf cmd

    let buyGasEnv = CommandEnv Nothing (bumpExecMode startEM) pactDbEnv cmdState logger freeGasEnv pd'

    buyGasResultE <- tryAny $! buyGas buyGasEnv cmd spv minerInfo supply

    case buyGasResultE of

      Left e1 ->
        jsonErrorResult buyGasEnv requestKey e1 [] (Gas 0)
          "tx failure for requestKey when buying gas"

      Right buyGasResult -> do

        -- this call needs to fail hard if Left. It means the continuation did not process
        -- correctly, and we should fail the transaction
        (pactId,buyGasLogs) <- either internalError pure buyGasResult
        logDebugRequestKey logger requestKey "successful gas buy for request key"

        let payloadEnv = set ceGasEnv userGasEnv
              $ set cePublicData pd'
              $ incrementExecMode buyGasEnv

        cmdResultE <- tryAny $! runPayload payloadEnv def cmd spv buyGasLogs

        case cmdResultE of

          Left e2 ->

            jsonErrorResult payloadEnv requestKey e2 buyGasLogs (Gas 0)
              "tx failure for request key when running cmd"

          Right (cmdResult, cmdLogs) -> do

            logDebugRequestKey logger requestKey "success for requestKey"

            let redeemGasEnv = set ceGasEnv freeGasEnv $ incrementExecMode payloadEnv
            redeemResultE <- tryAny $! redeemGas redeemGasEnv cmd cmdResult pactId supply spv cmdLogs

            case redeemResultE of

              Left e3 ->

                jsonErrorResult redeemGasEnv requestKey e3 cmdLogs (_crGas cmdResult)
                  "tx failure for request key while redeeming gas"

              Right (_, redeemLogs) -> do

                logDebugRequestKey logger requestKey "successful gas redemption for request key"
                pure ((cmdResult, cmdLogs <> redeemLogs), redeemGasEnv)


applyGenesisCmd
    :: Logger
    -> PactDbEnv p
    -> MVar CommandState
    -> ExecutionMode
    -> PublicData
    -> SPVSupport
    -> Command (Payload PublicMeta ParsedCode)
    -> IO ((CommandResult, [TxLog Value]), CommandEnv p)
applyGenesisCmd logger dbEnv cmdState execMode pd spv cmd = do
    -- cmd env with permissive gas model

    let pd' = set pdPublicMeta (publicMetaOf cmd) pd
    let cmdEnv = CommandEnv Nothing (bumpExecMode execMode) dbEnv cmdState logger freeGasEnv pd'
        requestKey = cmdToRequestKey cmd
    -- when calling genesis commands, we bring all magic capabilities in scope
    let initState = initCapabilities [magic_FUND_TX, magic_COINBASE]

    resultE <- tryAny $! runPayload cmdEnv initState cmd spv []
    case resultE of

      Left e ->

        jsonErrorResult cmdEnv requestKey e [] (Gas 0)
          "genesis tx failure for request key while running genesis"

      Right result -> do
        logDebugRequestKey logger requestKey "successful genesis tx for request key"
        pure $! (result, cmdEnv)


applyCoinbase
    :: Logger
    -> PactDbEnv p
    -> MVar CommandState
    -> ExecutionMode
    -> MinerInfo
    -> Decimal
    -> PublicData
    -> IO ((CommandResult, [TxLog Value]), CommandEnv p)
applyCoinbase logger dbEnv cmdState execMode minerInfo reward pd = do

    -- cmd env with permissive gas model
    let cmdEnv = CommandEnv Nothing (bumpExecMode execMode) dbEnv cmdState logger freeGasEnv pd
        coinbaseReq = RequestKey $ H.toUntypedHash (H.hash "COINBASE" :: H.PactHash)

    resultE <- tryAny $ coinbase cmdEnv minerInfo reward coinbaseReq

    case resultE of

      Left e ->
        jsonErrorResult cmdEnv coinbaseReq e [] (Gas 0)
          "genesis tx failure for request key while running genesis"

      Right result -> do
        logDebugRequestKey logger coinbaseReq $
          "successful coinbase for miner " ++ show minerInfo ++ " of " ++ show reward
        pure $! (result, cmdEnv)


applyLocal
    :: Logger
    -> PactDbEnv p
    -> MVar CommandState
    -> PublicData
    -> SPVSupport
    -> Command (Payload PublicMeta ParsedCode)
    -> IO (Either SomeException (CommandSuccess (Term Name)))
applyLocal logger dbEnv cmdState pd spv cmd@Command{..} = do

  -- cmd env with permissive gas model
  let pd' = set pdPublicMeta (publicMetaOf cmd) $ pd
      cmdEnv = CommandEnv Nothing Local dbEnv cmdState logger freeGasEnv pd'

  exec <- case _pPayload _cmdPayload of
    Exec pm -> return pm
    _ -> throwCmdEx "local continuations not supported"

  r <- tryAny $! applyExec' cmdEnv def exec (_pSigners _cmdPayload) (toUntypedHash _cmdHash) spv

  traverse mkSuccess r

-- | Present a failure as a pair of json result of Command Error and associated logs
jsonErrorResult
    :: CommandEnv a
    -> RequestKey
    -> SomeException
    -> [TxLog Value]
    -> Gas
    -> String
    -> IO ((CommandResult, [TxLog Value]), CommandEnv a)
jsonErrorResult cmdEnv reqKey err txLogs gas msg = do
    logErrorRequestKey (_ceLogger cmdEnv) reqKey err msg
    return (( jsonResult (_ceMode cmdEnv) reqKey gas $
              CommandError "Command execution failed" (Just . show $ err)
            , txLogs
            ), cmdEnv)

jsonResult :: ToJSON a => ExecutionMode -> RequestKey -> Gas -> a -> CommandResult
jsonResult execMode cmd gas a =
    let txId = case execMode of
          Transactional tx -> Just tx
          _otherMode -> Nothing
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
      applyExec env initState (cmdToRequestKey c) pm (_pSigners _cmdPayload) (toUntypedHash _cmdHash) spv txLogs
    Continuation ym ->
      applyContinuation env initState (cmdToRequestKey c) ym (_pSigners _cmdPayload) (toUntypedHash _cmdHash) spv txLogs

mkSuccess :: EvalResult -> IO (CommandSuccess (Term Name))
mkSuccess er = case _erOutput er of
  [] -> throwCmdEx "unexpected empty results"
  outs -> return . CommandSuccess . fromPactValue $ last outs

applyExec
    :: CommandEnv p
    -> EvalState
    -> RequestKey
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> SPVSupport
    -> [TxLog Value]
    -> IO (CommandResult, [TxLog Value])
applyExec env@CommandEnv{..} initState rk em senderSigs hsh spv prevLogs = do
    er@EvalResult{..} <- applyExec' env initState em senderSigs hsh spv
    mkSuccess er >>= \s ->
      return (jsonResult _ceMode rk _erGas s, prevLogs <> _erLogs)

-- | variation on 'applyExec' that returns 'EvalResult' as opposed to
-- wrapping it up in a JSON result.
applyExec'
    ::CommandEnv p
    -> EvalState
    -> ExecMsg ParsedCode
    -> [Signer]
    -> Hash
    -> SPVSupport
    -> IO EvalResult
applyExec' CommandEnv{..} initState (ExecMsg parsedCode execData) senderSigs hsh spv = do
    when (null (_pcExps parsedCode)) $ throwCmdEx "No expressions found"
    (CommandState refStore pacts) <- readMVar _ceState
    let sigs = userSigsToPactKeySet senderSigs
        evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
                  (MsgData sigs execData Nothing hsh) refStore _ceGasEnv
                  permissiveNamespacePolicy spv _cePublicData
    er@EvalResult{..} <- evalExecState initState evalEnv parsedCode
    unless (_ceMode == Local) $ do
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
    -> [Signer]
    -> Hash
    -> SPVSupport
    -> [TxLog Value]
    -> IO (CommandResult, [TxLog Value])
applyContinuation env@CommandEnv{..} initState rk msg@ContMsg{..} senderSigs hsh spv prevLogs = do
  er@EvalResult{..} <- applyContinuation' env initState msg senderSigs hsh spv
  mkSuccess er >>= \s ->
    pure $! (jsonResult _ceMode rk _erGas s, prevLogs <> _erLogs)

applyContinuation'
    :: CommandEnv p
    -> EvalState
    -> ContMsg
    -> [Signer]
    -> Hash
    -> SPVSupport
    -> IO EvalResult
applyContinuation' env@CommandEnv{..} initState msg@ContMsg{..} senderSigs hsh spv =
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
                            Just $ PactStep _cmStep _cmRollback _cmPactId (fmap fromPactValue <$> _peYield)
                        evalEnv =
                            setupEvalEnv
                                _ceDbEnv
                                _ceEntity
                                _ceMode
                                (MsgData sigs _cmData pactStep hsh)
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
    logLog _ceLogger "DEBUG" $
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
    -> IO (Either Text (PactId, [TxLog Value]))
buyGas env cmd spv (MinerInfo minerId minerKeys) (GasSupply supply) = do
    let sender    = view (cmdPayload . pMeta . pmSender) cmd
        initState = initCapabilities [magic_FUND_TX]

    buyGasCmd <- mkBuyGasCmd minerId minerKeys sender supply
    result <- applyExec' env initState buyGasCmd (_pSigners $ _cmdPayload cmd) (toUntypedHash $ _cmdHash cmd) spv
    pure $! case _erExec result of
      Nothing ->
        Left "buyGas: Internal error - empty continuation"
      Just pe -> Right $! (_pePactId pe, _erLogs result)

-- | Build and execute 'coin.redeem-gas' command from miner info and previous
-- command results (see 'TransactionExec.applyCmd')
--
-- see: pact/coin-contract/coin.pact#fund-tx
--
redeemGas
    :: CommandEnv p
    -> Command (Payload PublicMeta ParsedCode)
    -> CommandResult -- ^ result from the user command payload
    -> PactId        -- ^ result of the buy-gas continuation
    -> GasSupply     -- ^ the total calculated supply of gas (as Decimal)
    -> SPVSupport
    -> [TxLog Value]       -- ^ previous txlogs
    -> IO (CommandResult, [TxLog Value])
redeemGas env cmd cmdResult pactId (GasSupply supply) spv prevLogs = do
    let (Gas fee)  = _crGas cmdResult
        rk         = cmdToRequestKey cmd
        initState  = initCapabilities [magic_FUND_TX]

    applyContinuation env initState rk (redeemGasCmd fee supply pactId)
      (_pSigners $ _cmdPayload cmd) (toUntypedHash $ _cmdHash cmd) spv prevLogs
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
    -> MinerInfo  -- ^ Account to associate reward
    -> Decimal    -- ^ Reward amount
    -> RequestKey -- ^ Hash for exec, request key
    -> IO (CommandResult, [TxLog Value])
coinbase env (MinerInfo minerId minerKeys) reward rk@(RequestKey h) = do
    let initState = initCapabilities [magic_COINBASE]

    coinbaseCmd <- mkCoinbaseCmd minerId minerKeys reward
    applyExec env initState rk coinbaseCmd [] h noSPVSupport []

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

incrementExecMode :: CommandEnv a -> CommandEnv a
incrementExecMode = over ceMode bumpExecMode
