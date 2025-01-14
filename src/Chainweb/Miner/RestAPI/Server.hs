{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Control.Lens
import Control.Monad (when, unless)
import Control.Monad.Catch (bracket, try, catches)
import qualified Control.Monad.Catch as E
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM

import Data.Binary.Builder (fromByteString)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy(..))
import qualified Data.Set as S
import qualified Data.Vector as V

import Network.HTTP.Types.Status
import Network.Wai (responseLBS)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)

import Servant.API
import Servant.Server

import System.LogLevel
import System.Random

-- internal modules

import Chainweb.Cut.Create
import Chainweb.Logger
import Chainweb.Miner.Config
import Chainweb.Miner.Coordinator
import Chainweb.Miner.Core
import Chainweb.Miner.Pact
import Chainweb.Miner.RestAPI (MiningApi)
import Chainweb.Pact.Types(BlockInProgress(..), Transactions(..))
import Chainweb.Payload
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.WebPactExecutionService

-- -------------------------------------------------------------------------- --
-- Work Handler

workHandler
    :: Logger l
    => MiningCoordination l tbl
    -> Maybe ChainId
    -> Miner
    -> Handler WorkBytes
workHandler mr mcid m@(Miner (MinerId mid) _) = do
    MiningState ms <- liftIO . readTVarIO $ _coordState mr
    when (M.size ms > _coordLimit mr) $ do
        liftIO $ atomicModifyIORef' (_coord503s mr) (\c -> (c + 1, ()))
        throwError $ setErrText "Too many work requests" err503
    let !conf = _coordConf mr
        !primed = S.member m $ _coordinationMiners conf
    unless primed $ do
        liftIO $ atomicModifyIORef' (_coord403s mr) (\c -> (c + 1, ()))
        throwError $ setErrText ("Unauthorized Miner: " <> mid) err403
    wh <- liftIO $ work mr mcid m
    return $ WorkBytes $ runPutS $ encodeWorkHeader wh

-- -------------------------------------------------------------------------- --
-- Solved Handler

solvedHandler
    :: forall l tbl
    . Logger l
    => MiningCoordination l tbl
    -> HeaderBytes
    -> Handler NoContent
solvedHandler mr (HeaderBytes bytes) = do
    liftIO (try $ runGetS decodeSolvedWork bytes) >>= \case
        Left (DecodeException e) ->
            throwError $ setErrText ("Decoding error: " <> e) err400
        Left _ ->
            throwError $ setErrText "Unexpected encoding exception" err400
        Right solved -> do
            result <- liftIO $ catches (Right () <$ solve mr solved)
                [ E.Handler $ \NoAsscociatedPayload ->
                    return $ Left $ setErrText "No associated Payload" err404
                , E.Handler $ \(InvalidSolvedHeader _ msg) ->
                    return $ Left $ setErrText ("Invalid solved work: " <> msg) err400
                ]
            case result of
                Left e -> throwError e
                Right () -> return NoContent

-- -------------------------------------------------------------------------- --
--  Updates Handler

-- | Whether the work is outdated and should be thrown out immediately,
-- or there's just fresher work available and the old work is still valid.
data WorkChange = WorkOutdated | WorkRefreshed | WorkRegressed
    deriving stock (Show, Eq)

updatesHandler
    :: Logger l
    => MiningCoordination l tbl
    -> ChainBytes
    -> Tagged Handler Application
updatesHandler mr (ChainBytes cbytes) = Tagged $ \req resp -> withLimit resp $ do
    watchedChain <- runGetS decodeChainId cbytes
    (watchedMiner, blockOnChain) <- atomically $ do
        PrimedWork pw <- readTVar (_coordPrimedWork mr)
        -- we check if `watchedMiner` has new work. we ignore
        -- all other miner IDs, because we don't know which miner
        -- is asking for updates, and if we watched all of them at once
        -- we'd send too many messages.
        -- this is deliberately partial, primed work will always have
        -- at least one miner or that's an error
        let (watchedMiner, minerBlocks) = HM.toList pw ^?! _head
        -- and that miner will always have this chain(?)
        -- if this chain doesn't exist yet just wait
        blockOnChain <- do
          case minerBlocks ^? ix watchedChain of
            Just (WorkReady newBlock) -> return newBlock
            _ -> retry
        return (watchedMiner, blockOnChain)
    blockOnChainRef <- newIORef (WorkReady blockOnChain)

    -- An update stream is closed after @timeout@ seconds. We add some jitter to
    -- availablility of streams is uniformily distributed over time and not
    -- predictable.
    --
    jitter <- randomRIO @Double (0.9, 1.1)
    timer <- registerDelay (round $ jitter * realToFrac timeout * 1_000_000)

    eventSourceAppIO (go timer watchedChain watchedMiner blockOnChainRef) req resp
  where
    timeout = _coordinationUpdateStreamTimeout $ _coordConf mr

    -- | A nearly empty `ServerEvent` that signals the work on this chain has
    -- changed, and how.
    --
    eventForWorkChangeType :: WorkChange -> ServerEvent
    eventForWorkChangeType WorkOutdated = ServerEvent (Just $ fromByteString "New Cut") Nothing []
    eventForWorkChangeType WorkRefreshed = ServerEvent (Just $ fromByteString "Refreshed Block") Nothing []
    -- this is only different from WorkRefreshed for logging
    eventForWorkChangeType WorkRegressed = ServerEvent (Just $ fromByteString "Refreshed Block") Nothing []

    go :: TVar Bool -> ChainId -> MinerId -> IORef WorkState -> IO ServerEvent
    go timer watchedChain watchedMiner blockOnChainRef = do
        lastBlockOnChain <- readIORef blockOnChainRef

        -- await either a timeout or a new event
        maybeNewBlock <- atomically $ do
            t <- readTVar timer
            if t
                then return Nothing
                else Just <$> awaitNewPrimedWork watchedChain watchedMiner lastBlockOnChain

        case maybeNewBlock of
            Nothing -> return CloseEvent
            Just (workChange, currentBlockOnChain) -> do
                writeIORef blockOnChainRef currentBlockOnChain
                logFunctionText logger Debug $
                    "sent update to miner on chain " <> toText watchedChain <> ": " <> sshow workChange
                when (workChange == WorkRegressed) $
                    logFunctionText logger Warn $
                        "miner block regressed: " <> sshow currentBlockOnChain
                return (eventForWorkChangeType workChange)
        where
        logger = addLabel ("chain", toText watchedChain) (_coordLogger mr)

    count = _coordUpdateStreamCount mr

    awaitNewPrimedWork watchedChain watchedMiner lastBlockOnChain = do
        PrimedWork pw <- readTVar (_coordPrimedWork mr)
        let currentBlockOnChain = pw ^?! ix watchedMiner . ix watchedChain
        case (lastBlockOnChain, currentBlockOnChain) of

            -- there was no work, and that hasn't changed.
            (WorkStale, WorkStale) -> retry
            (WorkAlreadyMined _, WorkAlreadyMined _) -> retry

            (WorkReady (NewBlockInProgress (ForPact4 lastBip)), WorkReady (NewBlockInProgress (ForPact4 currentBip)))
                | lastPh <- _blockInProgressParentHeader lastBip
                , currentPh <- _blockInProgressParentHeader currentBip
                , lastPh /= currentPh ->
                -- we've got a new block on a new parent, we must've missed
                -- the update where the old block became outdated.
                -- miner should restart
                    return (WorkOutdated, currentBlockOnChain)

                | lastTlen <- V.length (_transactionPairs $ _blockInProgressTransactions lastBip)
                , currentTlen <- V.length (_transactionPairs $ _blockInProgressTransactions currentBip)
                , lastTlen /= currentTlen ->
                    if currentTlen < lastTlen
                    then
                        -- our refreshed block somehow has less transactions,
                        -- but the same parent header, log this as a bizarre case
                        return (WorkRegressed, currentBlockOnChain)
                    else
                        -- we've got a block that's been extended with new transactions
                        -- miner should restart
                        return (WorkRefreshed, currentBlockOnChain)

                -- no apparent change
                | otherwise -> retry

            (WorkReady (NewBlockInProgress (ForPact5 lastBip)), WorkReady (NewBlockInProgress (ForPact5 currentBip)))
                | lastPh <- _blockInProgressParentHeader lastBip
                , currentPh <- _blockInProgressParentHeader currentBip
                , lastPh /= currentPh ->
                -- we've got a new block on a new parent, we must've missed
                -- the update where the old block became outdated.
                -- miner should restart
                    return (WorkOutdated, currentBlockOnChain)

                | lastTlen <- V.length (_transactionPairs $ _blockInProgressTransactions lastBip)
                , currentTlen <- V.length (_transactionPairs $ _blockInProgressTransactions currentBip)
                , lastTlen /= currentTlen ->
                    if currentTlen < lastTlen
                    then
                        -- our refreshed block somehow has less transactions,
                        -- but the same parent header, log this as a bizarre case
                        return (WorkRegressed, currentBlockOnChain)
                    else
                        -- we've got a block that's been extended with new transactions
                        -- miner should restart
                        return (WorkRefreshed, currentBlockOnChain)

                -- no apparent change
                | otherwise -> retry
            (WorkReady (NewBlockPayload lastPh lastPwo), WorkReady (NewBlockPayload currentPh currentPwo))
                | lastPh /= currentPh ->
                    -- we've got a new block on a new parent, we must've missed
                    -- the update where the old block became outdated.
                    -- miner should restart.
                    return (WorkOutdated, currentBlockOnChain)

                | _payloadWithOutputsPayloadHash lastPwo /= _payloadWithOutputsPayloadHash currentPwo ->
                    -- this should be impossible because NewBlockPayload is for
                    -- when Pact is off so blocks can't be refreshed, but we've got
                    -- a different block with the same parent, so the miner should restart.
                    return (WorkRefreshed, currentBlockOnChain)

                -- no apparent change
                | otherwise -> retry
            (WorkReady _, WorkReady _) ->
                error "awaitNewPrimedWork: impossible: NewBlockInProgress replaced by a NewBlockPayload"

            _ -> return (WorkOutdated, currentBlockOnChain)

    withLimit resp inner = bracket
        (atomicModifyIORef' count $ \x -> (x - 1, x - 1))
        (const $ atomicModifyIORef' count $ \x -> (x + 1, ()))
        (\x -> if x <= 0 then ret503 resp else inner)

    ret503 resp = do
        resp $ responseLBS status503 [] "No more update streams available currently. Retry later."

-- -------------------------------------------------------------------------- --
-- Mining API Server

miningServer
    :: forall l tbl (v :: ChainwebVersionT)
    .  Logger l
    => MiningCoordination l tbl
    -> Server (MiningApi v)
miningServer mr = workHandler mr :<|> solvedHandler mr :<|> updatesHandler mr

someMiningServer :: Logger l => ChainwebVersion -> MiningCoordination l tbl -> SomeServer
someMiningServer (FromSingChainwebVersion (SChainwebVersion :: Sing vT)) mr =
    SomeServer (Proxy @(MiningApi vT)) $ miningServer mr
