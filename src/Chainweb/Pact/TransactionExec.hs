{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad.Except
import Control.Monad.Reader

import Data.Aeson as A
import qualified Data.Map.Strict as M

import Pact.Interpreter
import Pact.Parse
import Pact.Persist.SQLite ()
import Pact.Types.Command
import Pact.Types.Logger
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Server

applyCmd
    :: Logger
    -> Maybe EntityName
    -> PactDbEnv p
    -> MVar CommandState
    -> GasModel
    -> ExecutionMode
    -> Command a
    -> ProcessedCommand PublicMeta ParsedCode
    -> IO (CommandResult, [TxLog Value])
applyCmd _ _ _ _ _ ex cmd (ProcFail s) = return (jsonResult ex (cmdToRequestKey cmd) s, [])
applyCmd logger conf dbv cv gasModel exMode _ (ProcSucc cmd) = do
    let pubMeta = _pMeta $ _cmdPayload cmd
        (ParsedDecimal gasPrice) = _pmGasPrice pubMeta
        gasEnv = GasEnv (fromIntegral $ _pmGasLimit pubMeta) (GasPrice gasPrice) gasModel
    r <- tryAny $ runReaderT (runPayload cmd) (CommandEnv conf exMode dbv cv logger gasEnv)
    case r of
        Right p -> do
          logLog logger "DEBUG" $ "success for requestKey: " ++ show (cmdToRequestKey cmd)
          return p
        Left e -> do
            logLog logger "ERROR" $ "tx failure for requestKey: " ++ show (cmdToRequestKey cmd)
                ++ ": " ++ show e
            return (jsonResult exMode (cmdToRequestKey cmd) $
                         CommandError "Command execution failed" (Just $ show e)
                   , [])

jsonResult :: ToJSON a => ExecutionMode -> RequestKey -> a -> CommandResult
jsonResult ex cmd a = CommandResult cmd (exToTx ex) (toJSON a)

exToTx :: ExecutionMode -> Maybe TxId
exToTx (Transactional t) = Just t
exToTx Local = Nothing

runPayload :: Command (Payload PublicMeta ParsedCode) -> CommandM p (CommandResult, [TxLog Value])
runPayload c@Command{..} = case _pPayload _cmdPayload of
  Exec pm -> applyExec (cmdToRequestKey c) pm c
  Continuation ym -> applyContinuation (cmdToRequestKey c) ym c

applyExec
    :: RequestKey
    -> ExecMsg ParsedCode
    -> Command a
    -> CommandM p (CommandResult, [TxLog Value])
applyExec rk (ExecMsg parsedCode edata) Command{..} = do
  CommandEnv {..} <- ask
  when (null (_pcExps parsedCode)) $ throwCmdEx "No expressions found"
  (CommandState refStore pacts) <- liftIO $ readMVar _ceState
  let sigs = userSigsToPactKeySet _cmdSigs
      evalEnv = setupEvalEnv _ceDbEnv _ceEntity _ceMode
                (MsgData sigs edata Nothing _cmdHash) refStore _ceGasEnv permissiveNamespacePolicy
  pr <- liftIO $ evalExec evalEnv parsedCode
  let txLogs = erLogs pr
  newCmdPact <- join <$> mapM (handlePactExec (erInput pr)) (erExec pr)
  let newPacts = case newCmdPact of
        Nothing -> pacts
        Just cmdPact -> M.insert (_cpTxId cmdPact) cmdPact pacts
  void $ liftIO $ swapMVar _ceState $ CommandState (erRefStore pr) newPacts
  mapM_ (\p -> liftIO $ logLog _ceLogger "DEBUG" $ "applyExec: new pact added: " ++ show p) newCmdPact
  return (jsonResult _ceMode rk $ CommandSuccess (last (erOutput pr)), txLogs)

handlePactExec :: [Term Name] -> PactExec -> CommandM p (Maybe CommandPact)
handlePactExec em PactExec {..} = do
    CommandEnv {..} <- ask
    unless (length em == 1) $
        throwCmdEx $ "handlePactExec: defpact execution must occur as a single command: " ++ show em
    case _ceMode of
        Local -> return Nothing
        Transactional tid -> return $ Just $ CommandPact tid (head em) _peStepCount _peStep _peYield

applyContinuation :: RequestKey -> ContMsg -> Command a -> CommandM p (CommandResult, [TxLog Value])
applyContinuation rk msg@ContMsg {..} Command {..} = do
    env@CommandEnv {..} <- ask
    case _ceMode of
        Local -> throwCmdEx "Local continuation exec not supported"
        Transactional _ -> do
            state@CommandState {..} <- liftIO $ readMVar _ceState
            case M.lookup _cmTxId _csPacts of
                Nothing -> throwCmdEx $ "applyContinuation: txid not found: " ++ show _cmTxId
                Just pact@CommandPact {..}
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
                    res <- tryAny (liftIO $ evalContinuation evalEnv _cpContinuation)
          -- Update pact's state
                    case res of
                        Left (SomeException ex) -> throwM ex
                        Right EvalResult {..} -> do
                            exec@PactExec {..} <-
                                maybe
                                    (throwCmdEx "No pact execution in continuation exec!")
                                    return
                                    erExec
                            if _cmRollback
                                then rollbackUpdate env msg state
                                else continuationUpdate env msg state pact exec
                            return (jsonResult _ceMode rk $ CommandSuccess (last erOutput), erLogs)

rollbackUpdate :: CommandEnv p -> ContMsg -> CommandState -> CommandM p ()
rollbackUpdate CommandEnv {..} ContMsg {..} CommandState {..}
  -- if step doesn't have a rollback function, no error thrown. Therefore, pact will be deleted
  -- from state.
 = do
    let newState = CommandState _csRefStore $ M.delete _cmTxId _csPacts
    liftIO $
        logLog _ceLogger "DEBUG" $
        "applyContinuation: rollbackUpdate: reaping pact " ++ show _cmTxId
    void $ liftIO $ swapMVar _ceState newState

continuationUpdate
  :: CommandEnv p
  -> ContMsg
  -> CommandState
  -> CommandPact
  -> PactExec
  -> CommandM p ()
continuationUpdate CommandEnv {..} ContMsg {..} CommandState {..} CommandPact {..} PactExec {..} = do
    let nextStep = _cmStep + 1
        isLast = nextStep >= _cpStepCount
        updateState pacts = CommandState _csRefStore pacts -- never loading modules during continuations
    if isLast
        then do
            liftIO $
                logLog _ceLogger "DEBUG" $
                "applyContinuation: continuationUpdate: reaping pact: " ++ show _cmTxId
            void $ liftIO $ swapMVar _ceState $ updateState $ M.delete _cmTxId _csPacts
        else do
            let newPact = CommandPact _cpTxId _cpContinuation _cpStepCount _cmStep _peYield
            liftIO $
                logLog _ceLogger "DEBUG" $
                "applyContinuation: updated state of pact " ++ show _cmTxId ++ ": " ++ show newPact
            void $ liftIO $ swapMVar _ceState $ updateState $ M.insert _cmTxId newPact _csPacts
