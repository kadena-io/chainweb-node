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
-- Module: Chainweb.Miner.RestAPI.Server
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Miner.RestAPI.Server where

import Control.Concurrent.STM.TVar
    (TVar, modifyTVar', readTVar, readTVarIO, registerDelay)
import Control.Lens (over, view)
import Control.Monad (when)
import Control.Monad.Catch (bracket, finally, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)

import Data.Binary.Builder (fromByteString)
import Data.Bool (bool)
import Data.Generics.Wrapped (_Unwrapped)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy(..))
import qualified Data.Set as S
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Tuple.Strict (T2(..), T3(..))

import Network.HTTP.Types.Status
import Network.Wai (responseLBS)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)

import Servant.API
import Servant.Server

import System.Random

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.Chainweb.MinerResources (MiningCoordination(..))
import Chainweb.Cut (Cut)
import Chainweb.Cut.Create
import Chainweb.CutDB (CutDb, awaitNewCutByChainIdStm, cutDbPactService, cutDbWebBlockHeaderDb, _cut)
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Miner.Config
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Core
import Chainweb.Miner.Pact
import Chainweb.Miner.RestAPI (MiningApi)
import Chainweb.Payload
import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (getCurrentTimeIntegral)
import Chainweb.Utils (EncodingException(..), runGet, runPut)
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.LogMessage (LogFunction)

---

workHandler
    :: Logger l
    => MiningCoordination l cas
    -> Maybe ChainId
    -> Miner
    -> Handler WorkBytes
workHandler mr mcid m@(Miner (MinerId mid) _) = do
    MiningState ms <- liftIO . readTVarIO $ _coordState mr
    when (M.size ms > _coordLimit mr) $ do
        liftIO $ atomicModifyIORef' (_coord503s mr) (\c -> (c + 1, ()))
        throwError err503 { errBody = "Too many work requests" }
    let !conf = _coordConf mr
        !primed = S.member m $ _coordinationMiners conf
        !miner = bool (Plebian m) (Primed m) primed
    when (_coordinationMode conf == Private && not primed) $ do
        liftIO $ atomicModifyIORef' (_coord403s mr) (\c -> (c + 1, ()))
        let midb = TL.encodeUtf8 $ TL.fromStrict mid
        throwError err403 { errBody = "Unauthorized Miner: " <> midb }
    liftIO $ workHandler' mr mcid miner

workHandler'
    :: forall l cas
    .  Logger l
    => MiningCoordination l cas
    -> Maybe ChainId
    -> MinerStatus
    -> IO WorkBytes
workHandler' mr mcid m = do
    c <- _cut cdb
    T2 wh pd <- newWork logf choice m hdb pact (_coordPrimedWork mr) c
    now <- getCurrentTimeIntegral
    let key = _payloadDataPayloadHash pd
    atomically
        . modifyTVar' (_coordState mr)
        . over _Unwrapped
        . M.insert key
        $ T3 (minerStatus m) pd now
    return $ WorkBytes $ runPut $ encodeWorkHeader wh
  where
    logf :: LogFunction
    logf = logFunction $ _coordLogger mr

    hdb :: WebBlockHeaderDb
    hdb = view cutDbWebBlockHeaderDb cdb

    choice :: ChainChoice
    choice = maybe Anything Suggestion mcid

    cdb :: CutDb cas
    cdb = _coordCutDb mr

    pact :: PactExecutionService
    pact = _webPactExecutionService $ view cutDbPactService cdb

solvedHandler
    :: forall l cas
    . Logger l
    => MiningCoordination l cas
    -> HeaderBytes
    -> Handler NoContent
solvedHandler mr (HeaderBytes bytes) =
    liftIO (try $ runGet decodeSolvedWork bytes) >>= \case
        Left (DecodeException e) -> do
            throwError err400 { errBody = "Decoding error: " <> toErrText e }
        Left _ ->
            throwError err400 { errBody = "Unexpected encoding exception" }

        Right solved@(SolvedWork hdr) -> do
            -- Fail Early: If a `BlockHeader` comes in that isn't associated with any
            -- Payload we know about, reject it.
            --
            let key = _blockPayloadHash hdr
            MiningState ms <- liftIO $ readTVarIO tms
            case M.lookup key ms of
                Nothing -> throwError err404 { errBody = "No associated Payload" }
                Just x -> tryPublishWork solved x `finally` deleteKey key
                    -- There is a race here, but we don't care if the same cut
                    -- is published twice. There is also the risk that an item
                    -- doesn't get deleted. Items get GCed on a regular basis by
                    -- the coordinator.
  where
    toErrText = TL.encodeUtf8 . TL.fromStrict
    tms = _coordState mr

    lf :: LogFunction
    lf = logFunction $ _coordLogger mr

    deleteKey key = liftIO . atomically . modifyTVar' tms . over _Unwrapped $ M.delete key

    tryPublishWork solved (T3 m pd _) =
        liftIO (try $ publish lf (_coordCutDb mr) (view minerId m) pd solved) >>= \case
            Left (InvalidSolvedHeader _ msg) ->
                throwError err400 { errBody = "Invalid solved work: " <> toErrText msg}
            Right _ -> return NoContent

updatesHandler
    :: Logger l
    => MiningCoordination l cas
    -> ChainBytes
    -> Tagged Handler Application
updatesHandler mr (ChainBytes cbytes) = Tagged $ \req respond -> withLimit respond $ do
    cid <- runGet decodeChainId cbytes
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

    withLimit respond inner = bracket
        (atomicModifyIORef' count $ \x -> (x - 1, x - 1))
        (const $ atomicModifyIORef' count $ \x -> (x + 1, ()))
        (\x -> if x <= 0 then ret503 respond else inner)

    ret503 respond = do
        respond $ responseLBS status503 [] "No more update streams available currently. Retry later."

miningServer
    :: forall l cas (v :: ChainwebVersionT)
    .  Logger l
    => MiningCoordination l cas
    -> Server (MiningApi v)
miningServer mr = workHandler mr :<|> solvedHandler mr :<|> updatesHandler mr

someMiningServer :: Logger l => ChainwebVersion -> MiningCoordination l cas -> SomeServer
someMiningServer (FromSingChainwebVersion (SChainwebVersion :: Sing vT)) mr =
    SomeServer (Proxy @(MiningApi vT)) $ miningServer mr
