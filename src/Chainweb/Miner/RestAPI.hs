{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Miner.RestAPI
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Edmund Noble <edmund@kadena.io>
-- Stability: experimental
--
module Chainweb.Miner.RestAPI
( miningApi
) where

import Control.Concurrent.STM.TVar
    (TVar, readTVar, readTVarIO, registerDelay)
import Control.Monad (when, unless)
import Control.Monad.Catch (bracket, catches)
import qualified Control.Monad.Catch as E
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)

import Data.Binary.Builder (fromByteString)
import Data.Foldable
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import Network.HTTP.Types
import qualified Network.Wai as Wai
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)

import System.Random

import Web.DeepRoute
import Web.DeepRoute.Wai


-- internal modules

import Chainweb.Cut (Cut)
import Chainweb.Cut.Create
import Chainweb.CutDB (awaitNewCutByChainIdStm, _cut)
import Chainweb.Logger (Logger)
import Chainweb.Miner.Config
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Pact
import Chainweb.Utils.Serialization
import Chainweb.Version

miningApi :: Logger l => MiningCoordination l cas -> Route Wai.Application
miningApi mc = seg "mining" $ fold
    [ seg "work" $ endpoint methodGet "application/octet-stream" (workHandler mc)
    , seg "solved" $ endpoint methodPost "application/json" (solvedHandler mc)
    , seg "updates" $ endpoint methodGet "application/octet-stream" (updatesHandler mc)
    ]

workHandler :: Logger l => MiningCoordination l cas -> Wai.Application
workHandler mr req respond = do
    mcid <- getParams req (queryParamMaybe "chain")
    m@(Miner (MinerId mid) _) <- requestFromJSON req
    MiningState ms <- readTVarIO $ _coordState mr
    when (M.size ms > _coordLimit mr) $ do
        atomicModifyIORef' (_coord503s mr) (\c -> (c + 1, ()))
        errorWithStatus status503 "Too many work requests"
    let conf = _coordConf mr
        primed = S.member m $ _coordinationMiners conf
    unless primed $ do
        liftIO $ atomicModifyIORef' (_coord403s mr) (\c -> (c + 1, ()))
        errorWithStatus status403 $ "Unauthorized Miner: " <> mid
    wh <- liftIO $ work mr mcid m
    respond $ Wai.responseLBS status200 [] $ runPutL $ encodeWorkHeader wh

solvedHandler :: Logger l => MiningCoordination l cas -> Wai.Application
solvedHandler mr req respond = do
    bytes <- Wai.lazyRequestBody req
    case runGetEitherL decodeSolvedWork bytes of
        Left e ->
            errorWithStatus status400 $ "Decoding error: " <> T.pack e

        Right !solved -> do
            result <- liftIO $ catches (Right () <$ solve mr solved)
                [ E.Handler $ \NoAssociatedPayload ->
                    errorWithStatus status404 "No associated Payload"
                , E.Handler $ \(InvalidSolvedHeader _ msg) ->
                    errorWithStatus status400 $ "Invalid solved work: " <> msg
                ]
            case result of
                Left e -> throwError e
                Right () -> respond $ Wai.responseLBS status200 [] ""

-- -------------------------------------------------------------------------- --
--  Updates Handler

updatesHandler :: Logger l => MiningCoordination l cas -> Wai.Application
updatesHandler mr req respond = withLimit $ do
    cbytes <- Wai.lazyRequestBody req
    !cid <- case runGetEitherL decodeChainId cbytes of
        Left e ->
            errorWithStatus status400 $ "Decoding error: " <> T.pack e
        Right cid -> return cid
    cv  <- _cut (_coordCutDb mr) >>= newIORef

    -- An update stream is closed after @timeout@ seconds. We add some jitter to
    -- availablility of streams is uniformily distributed over time and not
    -- predictable.
    --
    jitter <- randomRIO @Double (0.9, 1.1)
    timer <- registerDelay (round $ jitter * realToFrac timeout * 1_000_000)

    eventSourceAppIO (go timer cid cv) req respond
  where
    timeout = _coordinationUpdateStreamTimeout $ _coordConf mr

    -- | A nearly empty `ServerEvent` that signals the discovery of a new
    -- `Cut`. Currently there is no need to actually send any information over
    -- to the caller.
    --
    f :: ServerEvent
    f = ServerEvent (Just $ fromByteString "New Cut") Nothing []

    go :: TVar Bool -> ChainId -> IORef Cut -> IO ServerEvent
    go timer cid cv = do
        c <- readIORef cv

        -- await either a timeout or a new event
        maybeCut <- atomically $ do
            t <- readTVar timer
            if t
                then return Nothing
                else Just <$> awaitNewCutByChainIdStm (_coordCutDb mr) cid c

        case maybeCut of
            Nothing -> return CloseEvent
            Just c' -> do
                writeIORef cv $! c'
                return f

    count = _coordUpdateStreamCount mr

    withLimit inner = bracket
        (atomicModifyIORef' count $ \x -> (x - 1, x - 1))
        (const $ atomicModifyIORef' count $ \x -> (x + 1, ()))
        (\x ->
            if x <= 0
            then errorWithStatus status503 "No more update streams available currently. Retry later."
            else inner
        )
