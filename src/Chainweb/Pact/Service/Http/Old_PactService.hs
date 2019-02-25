{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Chainweb.Pact.Http.PactService
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact service for Chainweb

module Chainweb.Pact.Service.Http.PactService
    ( execTransactions
    , initPactServiceHttp
    , mkPureState
    , mkSQLiteState
    , newBlock
    , serviceRequests
    , setupConfig
    , toCommandConfig
    , validateBlock
    ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe

import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.PersistPactDb as P ()
import qualified Pact.Types.Gas as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Server as P

-- internal modules
import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.Backend.MemoryDb
import Chainweb.Pact.Backend.SQLiteCheckpointer
import Chainweb.Pact.Backend.SqliteDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.Http.Types
import Chainweb.Pact.Service.Http.PactQueue
import Chainweb.Pact.Types

import Chainweb.Pact.Utils

initPactServiceHttp
  :: (TQueue RequestHttpMsg)
  -> IO ()
initPactServiceHttp reqQVar = do
    let loggers = P.neverLog
    let logger = P.newLogger loggers $ P.LogName "PactService"
    pactCfg <- setupConfig $ pactFilesDir ++ "pact.yaml"
    let cmdConfig = toCommandConfig pactCfg
    let gasLimit = fromMaybe 0 (P._ccGasLimit cmdConfig)
    let gasRate = fromMaybe 0 (P._ccGasRate cmdConfig)
    let gasEnv = P.GasEnv (fromIntegral gasLimit) 0.0 (P.constGasModel (fromIntegral gasRate))
    (checkpointEnv, theState) <-
        case P._ccSqlite cmdConfig of
            Nothing -> do
                env <- P.mkPureEnv loggers
                liftA2
                    (,)
                    (initInMemoryCheckpointEnv cmdConfig logger gasEnv)
                    (mkPureState env cmdConfig)
            Just sqlc -> do
                env <- P.mkSQLiteEnv logger False sqlc loggers
                liftA2
                    (,)
                    (initSQLiteCheckpointEnv cmdConfig logger gasEnv)
                    (mkSQLiteState env cmdConfig)
    estate <- saveInitial (_cpeCheckpointer checkpointEnv) theState
    case estate of
        Left s -> do -- TODO: fix - If this error message does not appear, the database has been closed.
            when (s == "SQLiteCheckpointer.save': Save key not found exception") (closePactDb theState)
            fail s
        Right _ -> return ()
    void $ evalStateT
           (runReaderT (serviceRequestsHttp memPoolAccess reqQVar respQVar) checkpointEnv)
           theState

serviceRequestsHttp
    :: MemPoolAccess
    -> (TQueue RequestHttpMsg)
    -> (TQueue ResponseHttpMsg)
    -> PactT ()
serviceRequestsHttp memPoolAccess reqQ respQ =
    forever run where
        run = do
            reqMsg <- liftIO $ getNextHttpRequest reqQ
            respMsg <- case _reqhRequestType reqMsg of
                NewBlock -> do
                    h <- newBlock memPoolAccess (_reqhBlockHeader reqMsg)
                    return $ ResponseHttpMsg
                        { _resphRequestType = NewBlock
                        , _resphRequestId = _reqhRequestId reqMsg
                        , _resphPayload = h }
                ValidateBlock -> do
                    h <- validateBlock memPoolAccess (_reqhBlockHeader reqMsg)
                    return $ ResponseHttpMsg
                        { _resphRequestType = ValidateBlock
                        , _resphRequestId = _reqhRequestId reqMsg
                        , _resphPayload = h }
            void . liftIO $ addResponse respQ respMsg
