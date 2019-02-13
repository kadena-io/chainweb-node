{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module      :  Chainweb.Pact.TransactionExec
-- Copyright   :  (C) 2018 Mark NIchols
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Nichols <mark@kadena.io>
--
-- Pact transaction code for Chainweb
--
module Chainweb.Pact.TransactionExec where

import Control.Concurrent
import Control.Exception.Safe
import Control.Lens
import Control.Monad.Catch (Exception(..))
import Control.Monad.Except

import Data.Aeson
import qualified Data.Map.Lazy as Map
import Data.Text (Text)

-- import NeatInterpolation (text)

-- internal Pact modules

import Pact.Interpreter
import Pact.Parse (ParsedInteger(..), ParsedDecimal(..), parseExprs)
import Pact.Types.Command
import Pact.Types.Gas (GasModel(..), Gas(..), GasLimit(..), GasPrice(..) )
import Pact.Types.Logger
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.Server
import Pact.Types.Term (Term(..), Name(..))
import Pact.Types.Util (Hash(..))

-- internal Chainweb modules

import Chainweb.Pact.Types (MinerInfo(..))

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
applyCmd _ _ _ _ _ _ ex cmd (ProcFail s) = return (jsonResult ex (cmdToRequestKey cmd) (Gas 0) s, [])
applyCmd logger entityM minerInfo pactDbEnv cmdState gasModel exMode _ (ProcSucc cmd) = do

    let gasEnv = mkGasEnvOf cmd gasModel
        cmdEnv = CommandEnv entityM exMode pactDbEnv cmdState logger gasEnv
        requestKey = cmdToRequestKey cmd
        modifiedEnv = set (ceGasEnv . geGasModel) permissiveGasModel cmdEnv

    buyGasResultE <- tryAny $ buyGas modifiedEnv minerInfo cmd

    case buyGasResultE of
      Left e1 -> do
        logErrorRequestKey logger requestKey e1 "tx failure for requestKey when buying gas"
        jsonErrorResult requestKey e1 [] (Gas 0)
      Right (buyGasResult, buyGasLogs) -> do
        logDebugRequestKey logger requestKey "successful gas buy for requestKey"
        cmdResultE <- tryAny $ runPayload cmdEnv cmd buyGasLogs
        case cmdResultE of
          Left e2 -> do
            logErrorRequestKey logger requestKey e2 "tx failure for requestKey when running cmd"
            jsonErrorResult requestKey e2 buyGasLogs $ _crGas buyGasResult
          Right (cmdResult, cmdLogs) -> do
            logDebugRequestKey logger requestKey "success for requestKey"
            redeemResultE <- tryAny $ redeemGas modifiedEnv minerInfo cmd cmdResult cmdLogs
            case redeemResultE of
              Left e3 -> do
                logErrorRequestKey logger requestKey e3 "tx failure for requestKey while redeeming gas"
                jsonErrorResult requestKey e3 cmdLogs $ _crGas cmdResult
              Right (_, redeemLogs) -> do
                logDebugRequestKey logger requestKey "successful gas redemption for requestkey"
                pure (cmdResult, redeemLogs)
  where
    jsonErrorResult k e txLogs gas = pure $
      (jsonResult exMode k gas $ CommandError "Command execution failed" (Just . show $ e)
      , txLogs
      )

jsonResult :: ToJSON a => ExecutionMode -> RequestKey -> Gas -> a -> CommandResult
jsonResult ex cmd gas a = CommandResult cmd (exToTx ex) (toJSON a) gas

exToTx :: ExecutionMode -> Maybe TxId
exToTx (Transactional t) = Just t
exToTx Local = Nothing

runPayload
    :: CommandEnv p
    -> Command (Payload PublicMeta ParsedCode)
    -> [TxLog Value] -- log state
    -> IO (CommandResult, [TxLog Value])
runPayload env c@Command{..} txLogs = case _pPayload _cmdPayload of
    Exec pm -> applyExec env (cmdToRequestKey c) pm _cmdSigs _cmdHash txLogs
    Continuation ym -> applyContinuation env (cmdToRequestKey c) ym c

applyExec
    :: CommandEnv p
    -> RequestKey
    -> ExecMsg ParsedCode
    -> [UserSig]
    -> Hash
    -> [TxLog Value]
    -> IO (CommandResult, [TxLog Value])
applyExec env@CommandEnv{..} rk (ExecMsg parsedCode edata) senderSigs hash previousLogs = do
  when (null (_pcExps parsedCode)) $ throwCmdEx "No expressions found"
  (CommandState refStore pacts) <- readMVar _ceState
  let sigs = userSigsToPactKeySet senderSigs
      evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
                (MsgData sigs edata Nothing hash) refStore _ceGasEnv permissiveNamespacePolicy
  EvalResult{..} <- evalExec evalEnv parsedCode
  let txLogs = _erLogs
  newCmdPact <- join <$> mapM (handlePactExec env _erInput) _erExec
  let newPacts = case newCmdPact of
        Nothing -> pacts
        Just cmdPact -> Map.insert (_cpTxId cmdPact) cmdPact pacts
  void $ swapMVar _ceState $ CommandState _erRefStore newPacts
  mapM_ (\p -> logLog _ceLogger "DEBUG" $ "applyExec: new pact added: " ++ show p) newCmdPact
  return (jsonResult _ceMode rk _erGas $ CommandSuccess (last _erOutput), previousLogs <> txLogs)

handlePactExec :: CommandEnv p -> [Term Name] -> PactExec -> IO (Maybe CommandPact)
handlePactExec CommandEnv{..} em PactExec{..} = do
    unless (length em == 1) $
        throwCmdEx $ "handlePactExec: defpact execution must occur as a single command: " ++ show em
    case _ceMode of
        Local -> return Nothing
        Transactional tid -> return $ Just $ CommandPact tid (head em) _peStepCount _peStep _peYield

applyContinuation
    :: CommandEnv p
    -> RequestKey
    -> ContMsg
    -> Command a
    -> IO (CommandResult, [TxLog Value])
applyContinuation env@CommandEnv{..} rk msg@ContMsg{..} Command{..} =
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
                    let sigs = userSigsToPactKeySet _cmdSigs
                        pactStep =
                            Just $
                            PactStep _cmStep _cmRollback (PactId $ pack $ show _cmTxId) _cpYield
                        evalEnv =
                            setupEvalEnv
                                _ceDbEnv
                                _ceEntity
                                _ceMode
                                (MsgData sigs _cmData pactStep _cmdHash)
                                _csRefStore
                                _ceGasEnv
                                permissiveNamespacePolicy
                    res <- tryAny $ evalContinuation evalEnv _cpContinuation
          -- Update pact's state
                    case res of
                        Left (SomeException ex) -> throwM ex
                        Right EvalResult{..} -> do
                            exec@PactExec{..} <-
                                maybe
                                    (throwCmdEx "No pact execution in continuation exec!")
                                    return
                                    _erExec
                            if _cmRollback
                                then rollbackUpdate env msg state
                                else continuationUpdate env msg state pact exec
                            return (jsonResult _ceMode rk _erGas $ CommandSuccess (last _erOutput), _erLogs)

rollbackUpdate :: CommandEnv p -> ContMsg -> CommandState -> IO ()
rollbackUpdate CommandEnv{..} ContMsg{..} CommandState{..}
  -- if step doesn't have a rollback function, no error thrown. Therefore, pact will be deleted
  -- from state.
 = do
    let newState = CommandState _csRefStore $ Map.delete _cmTxId _csPacts
    logLog _ceLogger "DEBUG" $
        "applyContinuation: rollbackUpdate: reaping pact " ++ show _cmTxId
    void $ swapMVar _ceState newState

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
          logLog _ceLogger "DEBUG" $ "applyContinuation: continuationUpdate: reaping pact: " ++ show _cmTxId
          void $ swapMVar _ceState $ updateState $ Map.delete _cmTxId _csPacts
        else do
            let newPact = CommandPact _cpTxId _cpContinuation _cpStepCount _cmStep _peYield
            logLog _ceLogger "DEBUG" $
                "applyContinuation: updated state of pact " ++ show _cmTxId ++ ": " ++ show newPact
            void $ swapMVar _ceState $ updateState $ Map.insert _cmTxId newPact _csPacts
------------------------------------------------------------------------------
-- Coin Contract
------------------------------------------------------------------------------

-- | Build and execute 'coin.buygas' command from miner info and user command
-- info (see 'TransactionExec.applyCmd')
buyGas
    :: CommandEnv p
    -> MinerInfo -- ^ miner account registered to mine block
    -> Command (Payload PublicMeta ParsedCode) -- ^ User-supplied command
    -> IO (CommandResult, [TxLog Value])
buyGas env MinerInfo{..} cmd@Command{..} = do
    let gasLimit = gasLimitOf cmd
        gasPrice = gasPriceOf cmd
        requestKey = cmdToRequestKey cmd
    buyGasCmd <- mkBuyGasCmd _minerKeys _cmdSigs gasLimit gasPrice
    -- tx logs are empty to start
    applyExec env requestKey buyGasCmd _cmdSigs _cmdHash []

-- | Build and execute 'coin.redeem-gas' command from miner info and previous
-- command results (see 'TransactionExec.applyCmd')
redeemGas
    :: CommandEnv p
    -> MinerInfo
    -> Command (Payload PublicMeta ParsedCode)
    -> CommandResult
    -> [TxLog Value] -- log state
    -> IO (CommandResult, [TxLog Value])
redeemGas env miner cmd@Command{..} result txLogs = do
    let requestKey = cmdToRequestKey cmd
        resultGas = _crGas result
        minerKeyset = _minerKeys miner
        gasLimit = gasLimitOf cmd
    redeemGasCmd <- mkRedeemGasCmd minerKeyset _cmdSigs gasLimit resultGas
    applyExec env requestKey redeemGasCmd _cmdSigs _cmdHash txLogs


-- | Build the 'coin.buygas' command
mkBuyGasCmd
    :: KeySet    -- ^ Keys of the miner account
    -> [UserSig] -- ^ Sigs of the sender contained in the command payload
    -> GasLimit  -- ^ The gas limit derived from the command payload
    -> GasPrice  -- ^ The gas price derived from the command payload
    -> IO (ExecMsg ParsedCode)
mkBuyGasCmd _minerKeys _senderSigs (GasLimit _l) (GasPrice _p) =
    buildExecParsedCode "(+ 1 1)" -- [text| (coin.buygas ($minerKeys $senderSigs $l $p)) |]
{-# INLINE mkBuyGasCmd #-}

-- | Build the 'coin.redeem-gas' command
mkRedeemGasCmd
    :: KeySet    -- ^ Keys from the miner account brought in by 'TransactionExec.applyExec'
    -> [UserSig] -- ^ Sigs from user contained in the command payload (see 'TransactionExec.applyExec')
    -> GasLimit  -- ^ The amount sender attempted to provision for gas
    -> Gas       -- ^ The uncharged gas from the executed user command
    -> IO (ExecMsg ParsedCode)
mkRedeemGasCmd _minerKeys _senderSigs (GasLimit _l) (Gas _g) =
    buildExecParsedCode "(+ 2 2)" -- [text| (coin.redeem-gas ($minerKeys senderSigs $l $g)) |]
{-# INLINE mkRedeemGasCmd #-}

------------------------------------------------------------------------------
-- Utilities and Optics
------------------------------------------------------------------------------

-- | This permissive gas model will be used for coin contracts when we execute the
-- buy portion of the execution model.
permissiveGasModel :: GasModel
permissiveGasModel = GasModel $ \_ _ -> Gas 0
{-# INLINE permissiveGasModel #-}

-- | Get the sender of a public chain command payload
senderOf :: forall c. Command (Payload PublicMeta c) -> Text
senderOf = _pmSender . _pMeta . _cmdPayload
{-# INLINE senderOf #-}

-- | Get the gas limit/supply of a public chain command payload
gasLimitOf :: forall c. Command (Payload PublicMeta c) -> GasLimit
gasLimitOf cmd = case _pmGasLimit . _pMeta . _cmdPayload $ cmd of
    ParsedInteger limit -> GasLimit . fromIntegral $ limit
{-# INLINE gasLimitOf #-}

-- | Get the gas price of a public chain command payload
gasPriceOf :: forall c. Command (Payload PublicMeta c) -> GasPrice
gasPriceOf cmd = case _pmGasPrice . _pMeta . _cmdPayload $ cmd of
    ParsedDecimal price -> GasPrice price
{-# INLINE gasPriceOf #-}

-- | Get the public meta information of a given command payload
publicMetaOf :: forall c. Command (Payload PublicMeta c) -> PublicMeta
publicMetaOf = _pMeta . _cmdPayload
{-# INLINE publicMetaOf #-}

mkGasEnvOf :: forall c. Command (Payload PublicMeta c) -> GasModel -> GasEnv
mkGasEnvOf cmd gasModel = GasEnv (gasLimitOf cmd) (gasPriceOf cmd) gasModel
{-# INLINE mkGasEnvOf #-}

-- | Build the 'ExecMsg' for some pact code fed to the function
buildExecParsedCode :: Text -> IO (ExecMsg ParsedCode)
buildExecParsedCode code =
   case ParsedCode code <$> parseExprs code of
     Right t  -> pure $ ExecMsg t Null
     -- DESNote(emily): I am fine with inlining this code because it *needs* to fail fast
     Left err -> throwIO . userError $ "Coin contract call failed: " <> show err
{-# INLINE buildExecParsedCode #-}

-- | Log request keys at DEBUG when successful
logDebugRequestKey :: Logger -> RequestKey -> String -> IO ()
logDebugRequestKey l k reason = logLog l "DEBUG" $ reason <> ": " <> show k

-- | Log request keys and error message at ERROR when failed
logErrorRequestKey :: forall e. Exception e => Logger -> RequestKey -> e -> String -> IO ()
logErrorRequestKey l k e reason = logLog l "ERROR" $ reason
    <> ": "
    <> show k
    <> ": "
    <> show e
