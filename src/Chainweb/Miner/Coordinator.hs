{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Miner.Coordinator
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--

module Chainweb.Miner.Coordinator
( -- * Types
  MiningState(..)
, miningState
, MiningStats(..)
, PrevTime(..)
, ChainChoice(..)
, PrimedWork(..)
, MiningCoordination(..)
, NoAsscociatedPayload(..)

-- * Mining API Functions
, work
, solve

-- ** Internal Functions
, publish
) where

import Control.Concurrent
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar
import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Data.Aeson (ToJSON)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List(sort)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V

import GHC.Generics (Generic)
import GHC.Stack

import System.LogLevel (LogLevel(..))

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.Cut hiding (join)
import Chainweb.Cut.Create
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Logger (Logger, logFunction)
import Chainweb.Logging.Miner
import Chainweb.Miner.Config
import Chainweb.Miner.Pact (Miner(..), MinerId(..), minerId)
import Chainweb.Payload
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (Micros(..), Time(..), getCurrentTimeIntegral)
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.LogMessage (JsonLog(..), LogFunction)

-- -------------------------------------------------------------------------- --
-- Utils

-- | Lookup a 'BlockHeader' for a 'ChainId' in a cut and raise a meaningfull
-- error if the lookup fails.
--
-- Generally, failing lookup in a cut is a code invariant violation. In almost
-- all circumstances there should be a invariant in scope that guarantees that
-- the lookup succeeds. This function is useful when debugging corner cases of
-- new code logic, like graph changes.
--
lookupInCut :: HasCallStack => HasChainId cid => Cut -> cid -> BlockHeader
lookupInCut c cid
    | Just x <- lookupCutM cid c = x
    | otherwise = error $ T.unpack
        $ "Chainweb.Miner.Coordinator.lookupInCut: failed to lookup chain in cut."
        <> " Chain: " <> sshow (_chainId cid) <> "."
        <> " Cut Hashes: " <> encodeToText (cutToCutHashes Nothing c) <> "."

-- -------------------------------------------------------------------------- --
-- MiningCoordination

-- | For coordinating requests for work and mining solutions from remote Mining
-- Clients.
--
data MiningCoordination logger tbl = MiningCoordination
    { _coordLogger :: !logger
    , _coordCutDb :: !(CutDb tbl)
    , _coordState :: !(TVar MiningState)
    , _coordLimit :: !Int
    , _coord503s :: !(IORef Int)
    , _coord403s :: !(IORef Int)
    , _coordConf :: !CoordinationConfig
    , _coordUpdateStreamCount :: !(IORef Int)
    , _coordPrimedWork :: !(TVar PrimedWork)
    }

-- | Precached payloads for Private Miners. This allows new work requests to be
-- made as often as desired, without clogging the Pact queue.
--
newtype PrimedWork =
    PrimedWork (HM.HashMap MinerId (HM.HashMap ChainId (T2 ParentHeader (Maybe PayloadData))))
    deriving newtype (Semigroup, Monoid)
    deriving stock Generic
    deriving anyclass (Wrapped)

resetPrimed :: MinerId -> ChainId -> PrimedWork -> PrimedWork
resetPrimed mid cid (PrimedWork pw) = PrimedWork
    $! HM.adjust (HM.adjust (_2 .~ Nothing) cid) mid pw

-- | Data shared between the mining threads represented by `newWork` and
-- `publish`.
--
-- The key is hash of the current block's payload.
--
newtype MiningState = MiningState
    { _miningState :: M.Map BlockPayloadHash (T3 Miner PayloadData (Time Micros)) }
    deriving stock (Generic)
    deriving newtype (Semigroup, Monoid)

makeLenses ''MiningState

-- | For logging during `MiningState` manipulation.
--
data MiningStats = MiningStats
    { _statsCacheSize :: !Int
    , _stats503s :: !Int
    , _stats403s :: !Int
    , _statsAvgTxs :: !Int
    , _statsPrimedSize :: !Int }
    deriving stock (Generic)
    deriving anyclass (ToJSON, NFData)

-- | The `BlockCreationTime` of the parent of some current, "working"
-- `BlockHeader`.
--
newtype PrevTime = PrevTime BlockCreationTime

data ChainChoice = Anything | TriedLast !ChainId | Suggestion !ChainId

-- | Construct a new `BlockHeader` to mine on.
--
newWork
    :: LogFunction
    -> ChainChoice
    -> Miner
    -> WebBlockHeaderDb
        -- ^ this is used to lookup parent headers that are not in the cut
        -- itself.
    -> PactExecutionService
    -> TVar PrimedWork
    -> Cut
    -> IO (Maybe (T2 WorkHeader PayloadData))
newWork logFun choice eminer@(Miner mid _) hdb pact tpw c = do

    -- Randomly pick a chain to mine on. we no longer support the caller
    -- specifying any particular one.
    --
    cid <- case choice of
        Anything -> randomChainIdAt c (minChainHeight c)
        Suggestion cid' -> pure cid'
        TriedLast _ -> randomChainIdAt c (minChainHeight c)
    logFun @T.Text Debug $ "newWork: picked chain " <> sshow cid

    -- wait until at least one chain has primed work. we don't wait until *our*
    -- chain has primed work, because if other chains have primed work, we want
    -- to loop and select one of those chains. it is not a normal situation to
    -- have no chains with primed work if there are more than a couple chains.
    mpw <- atomically $ do
        PrimedWork pw <- readTVar tpw
        mpw <- maybe retry return (HM.lookup mid pw)
        guard (any (isJust . ssnd) mpw)
        return mpw
    let mr = T2
            <$> HM.lookup cid mpw
            <*> getCutExtension c cid

    case mr of
        Just (T2 (T2 _ Nothing) _) -> do
            logFun @T.Text Debug $ "newWork: chain " <> sshow cid <> " has stale work"
            newWork logFun Anything eminer hdb pact tpw c
        Nothing -> do
            logFun @T.Text Debug $ "newWork: chain " <> sshow cid <> " not mineable"
            newWork logFun Anything eminer hdb pact tpw c
        Just (T2 (T2 (ParentHeader primedParent) (Just payload)) extension)
            | _blockHash primedParent == _blockHash (_parentHeader (_cutExtensionParent extension)) -> do
                let !phash = _payloadDataPayloadHash payload
                !wh <- newWorkHeader hdb extension phash
                pure $ Just $ T2 wh payload
            | otherwise -> do
                -- The cut is too old or the primed work is outdated. Probably
                -- the former because it the mining coordination background job
                -- is updating the primed work cache regularly. We could try
                -- another chain, but it's safer to just return 'Nothing' here
                -- and retry with an updated cut.
                --
                let !extensionParent = _parentHeader (_cutExtensionParent extension)
                logFun @T.Text Info
                    $ "newWork: chain " <> sshow cid <> " not mineable because of parent header mismatch"
                    <> ". Primed parent hash: " <> toText (_blockHash primedParent)
                    <> ". Primed parent height: " <> sshow (_blockHeight primedParent)
                    <> ". Extension parent: " <> toText (_blockHash extensionParent)
                    <> ". Extension height: " <> sshow (_blockHeight extensionParent)

                return Nothing

-- | Accepts a "solved" `BlockHeader` from some external source (e.g. a remote
-- mining client), attempts to reassociate it with the current best `Cut`, and
-- publishes the result to the `Cut` network.
--
-- There are a number of "fail fast" conditions which will kill the candidate
-- `BlockHeader` before it enters the Cut pipeline.
--
publish
    :: LogFunction
    -> CutDb tbl
    -> TVar PrimedWork
    -> MinerId
    -> PayloadData
    -> SolvedWork
    -> IO ()
publish lf cdb pwVar miner pd s = do
    c <- _cut cdb
    now <- getCurrentTimeIntegral
    try (extend c pd s) >>= \case

        -- Publish CutHashes to CutDb and log success
        Right (bh, Just ch) -> do

            -- reset the primed payload for this cut extension
            atomically $ modifyTVar pwVar $ resetPrimed miner (_chainId bh)
            addCutHashes cdb ch

            let bytes = sum . fmap (BS.length . _transactionBytes) $
                        _payloadDataTransactions pd
            lf Info $ JsonLog $ NewMinedBlock
                { _minedBlockHeader = ObjectEncoded bh
                , _minedBlockTrans = int . V.length $ _payloadDataTransactions pd
                , _minedBlockSize = int bytes
                , _minedBlockMiner = _minerId miner
                , _minedBlockDiscoveredAt = now
                }

        -- Log Orphaned Block
        Right (bh, Nothing) -> do
            let !p = lookupInCut c bh
            lf Info $ orphandMsg now p bh "orphaned solution"

        -- Log failure and rethrow
        Left e@(InvalidSolvedHeader bh msg) -> do
            let !p = lookupInCut c bh
            lf Info $ orphandMsg now p bh msg
            throwM e
  where
    orphandMsg now p bh msg = JsonLog OrphanedBlock
        { _orphanedHeader = ObjectEncoded bh
        , _orphanedBestOnCut = ObjectEncoded p
        , _orphanedDiscoveredAt = now
        , _orphanedMiner = _minerId miner
        , _orphanedReason = msg
        }

-- -------------------------------------------------------------------------- --
-- Mining API

-- | Get new work
--
-- This function does not check if the miner is authorized. If the miner doesn't
-- yet exist in the primed work cache it is added.
--
work
    :: forall l tbl
    .  Logger l
    => MiningCoordination l tbl
    -> Maybe ChainId
    -> Miner
    -> IO WorkHeader
work mr mcid m = do
    T2 wh pd <-
        withAsync (logDelays 0) $ \_ -> newWorkForCut
    now <- getCurrentTimeIntegral
    atomically
        . modifyTVar' (_coordState mr)
        . over miningState
        . M.insert (_payloadDataPayloadHash pd)
        $ T3 m pd now
    return wh
  where
    -- here we log the case that the work loop has stalled.
    logDelays :: Int -> IO ()
    logDelays n = do
        threadDelay 10_000_000
        let !n' = n + 1
        PrimedWork primedWork <- readTVarIO (_coordPrimedWork mr)
        logf @T.Text Warn
          ("findWork: stalled for " <> sshow n' <> "s. " <>
            case HM.lookup (view minerId m) primedWork of
                Nothing ->
                     "no primed work for miner key" <> sshow m
                Just mpw
                    | HM.null mpw ->
                        "no chains have primed work"
                    | otherwise ->
                        "all chains with primed work may be stalled. chains with primed payloads: "
                        <> sshow (sort [cid | (cid, T2 _ (Just _)) <- HM.toList mpw])
          )

        logDelays n'

    -- There is no strict synchronization between the primed work cache and the
    -- new work selection. There is a chance that work selection picks a primed
    -- work that is out of sync with the current cut. In that case we just try
    -- again with a new cut. In case the cut was good but the primed work was
    -- outdated, chances are that in the next attempt we pick a different chain
    -- with update work or that the primed work cache caught up in the meantime.
    --
    newWorkForCut = do
        c' <- _cut cdb
        newWork logf choice m hdb pact (_coordPrimedWork mr) c' >>= \case
            Nothing -> newWorkForCut
            Just x -> return x

    logf :: LogFunction
    logf = logFunction $ _coordLogger mr

    hdb :: WebBlockHeaderDb
    hdb = view cutDbWebBlockHeaderDb cdb

    choice :: ChainChoice
    choice = maybe Anything Suggestion mcid

    cdb :: CutDb tbl
    cdb = _coordCutDb mr

    pact :: PactExecutionService
    pact = _webPactExecutionService $ view cutDbPactService cdb

data NoAsscociatedPayload = NoAsscociatedPayload
    deriving (Show, Eq)

instance Exception NoAsscociatedPayload

solve
    :: forall l tbl
    . Logger l
    => MiningCoordination l tbl
    -> SolvedWork
    -> IO ()
solve mr solved@(SolvedWork hdr) = do
    -- Fail Early: If a `BlockHeader` comes in that isn't associated with any
    -- Payload we know about, reject it.
    --
    MiningState ms <- readTVarIO tms
    case M.lookup key ms of
        Nothing -> throwM NoAsscociatedPayload
        Just x -> publishWork x `finally` deleteKey
            -- There is a race here, but we don't care if the same cut
            -- is published twice. There is also the risk that an item
            -- doesn't get deleted. Items get GCed on a regular basis by
            -- the coordinator.
  where
    key = _blockPayloadHash hdr
    tms = _coordState mr

    lf :: LogFunction
    lf = logFunction $ _coordLogger mr

    deleteKey = atomically . modifyTVar' tms . over miningState $ M.delete key
    publishWork (T3 m pd _) =
        publish lf (_coordCutDb mr) (_coordPrimedWork mr) (view minerId m) pd solved
