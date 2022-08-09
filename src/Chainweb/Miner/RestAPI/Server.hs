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
module Chainweb.Miner.RestAPI.Server
( miningServer
, someMiningServer
) where

import Control.Concurrent.STM.TVar
    (TVar, readTVar, readTVarIO, registerDelay)
import Control.Monad (when, unless)
import Control.Monad.Catch (bracket, try, catches)
import qualified Control.Monad.Catch as E
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)

import Data.Binary.Builder (fromByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy(..))
import qualified Data.Set as S
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Network.HTTP.Types.Status
import Network.Wai (responseLBS)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)

import Servant.API
import Servant.Server

import System.Random

-- internal modules

import Chainweb.Cut (Cut)
import Chainweb.Cut.Create
import Chainweb.CutDB (awaitNewCutByChainIdStm, _cut)
import Chainweb.Logger (Logger)
import Chainweb.Miner.Config
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Core
import Chainweb.Miner.Pact
import Chainweb.Miner.RestAPI (MiningApi)
import Chainweb.RestAPI.Utils (SomeServer(..))
import Chainweb.Utils (EncodingException(..))
import Chainweb.Utils.Serialization
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Work Handler

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
    unless primed $ do
        liftIO $ atomicModifyIORef' (_coord403s mr) (\c -> (c + 1, ()))
        let midb = TL.encodeUtf8 $ TL.fromStrict mid
        throwError err403 { errBody = "Unauthorized Miner: " <> midb }
    wh <- liftIO $ work mr mcid m
    return $ WorkBytes $ runPutS $ encodeWorkHeader wh

-- -------------------------------------------------------------------------- --
-- Solved Handler

solvedHandler
    :: forall l cas
    . Logger l
    => MiningCoordination l cas
    -> HeaderBytes
    -> Handler NoContent
solvedHandler mr (HeaderBytes bytes) = do
    liftIO (try $ runGetS decodeSolvedWork bytes) >>= \case
        Left (DecodeException e) ->
            throwError err400 { errBody = "Decoding error: " <> toErrText e }
        Left _ ->
            throwError err400 { errBody = "Unexpected encoding exception" }

        Right solved -> do
            result <- liftIO $ catches (Right () <$ solve mr solved)
                [ E.Handler $ \NoAsscociatedPayload ->
                    return $ Left err404 { errBody = "No associated Payload" }
                , E.Handler $ \(InvalidSolvedHeader _ msg) ->
                    return $ Left err400 { errBody = "Invalid solved work: " <> toErrText msg}
                ]
            case result of
                Left e -> throwError e
                Right () -> return NoContent
  where
    toErrText = TL.encodeUtf8 . TL.fromStrict

-- -------------------------------------------------------------------------- --
--  Updates Handler

updatesHandler
    :: Logger l
    => MiningCoordination l cas
    -> ChainBytes
    -> Tagged Handler Application
updatesHandler mr (ChainBytes cbytes) = Tagged $ \req resp -> withLimit resp $ do
    cid <- runGetS decodeChainId cbytes
    cv  <- _cut (_coordCutDb mr) >>= newIORef

    -- An update stream is closed after @timeout@ seconds. We add some jitter to
    -- availablility of streams is uniformily distributed over time and not
    -- predictable.
    --
    jitter <- randomRIO @Double (0.9, 1.1)
    timer <- registerDelay (round $ jitter * realToFrac timeout * 1_000_000)

    eventSourceAppIO (go timer cid cv) req resp
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

    withLimit resp inner = bracket
        (atomicModifyIORef' count $ \x -> (x - 1, x - 1))
        (const $ atomicModifyIORef' count $ \x -> (x + 1, ()))
        (\x -> if x <= 0 then ret503 resp else inner)

    ret503 resp = do
        resp $ responseLBS status503 [] "No more update streams available currently. Retry later."

-- -------------------------------------------------------------------------- --
-- Mining API Server

miningServer
    :: forall l cas (v :: ChainwebVersionT)
    .  Logger l
    => MiningCoordination l cas
    -> Server (MiningApi v)
miningServer mr = workHandler mr :<|> solvedHandler mr :<|> updatesHandler mr

someMiningServer :: Logger l => ChainwebVersion -> MiningCoordination l cas -> SomeServer
someMiningServer (FromSingChainwebVersion (SChainwebVersion :: Sing vT)) mr =
    SomeServer (Proxy @(MiningApi vT)) $ miningServer mr
