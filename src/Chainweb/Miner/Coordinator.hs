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
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

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
, WorkState(..)
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
import qualified Data.HashSet as HS
import Data.IORef
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Maybe(mapMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V

import GHC.Generics (Generic)
import GHC.Stack

import System.LogLevel (LogLevel(..))

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash (BlockHash)
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
    PrimedWork (HM.HashMap MinerId (HM.HashMap ChainId WorkState))
    deriving newtype (Semigroup, Monoid)
    deriving stock Generic
    deriving anyclass (Wrapped)

data WorkState
  = WorkReady NewBlock
    -- ^ We have work ready for the miner
  | WorkAlreadyMined BlockHash
    -- ^ A block with this parent has already been mined and submitted to the
    --   cut pipeline - we don't want to mine it again.
  | WorkStale
    -- ^ No work has been produced yet with the latest parent block on this
    --   chain.
  deriving stock (Show)

isWorkReady :: WorkState -> Bool
isWorkReady = \case
  WorkReady {} -> True
  _ -> False

-- | Data shared between the mining threads represented by `newWork` and
-- `publish`.
--
-- The key is hash of the current block's payload.
--
newtype MiningState = MiningState
    { _miningState :: M.Map BlockPayloadHash (T3 Miner PayloadWithOutputs (Time Micros)) }
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
    -> IO (Maybe (T2 WorkHeader PayloadWithOutputs))
newWork logFun choice eminer@(Miner mid _) hdb pact tpw c = do

    -- Randomly pick a chain to mine on. we no longer support the caller
    -- specifying any particular one.
    --
    cid <- case choice of
        Anything -> randomChainIdAt c (_cutMinHeight c)
        Suggestion cid' -> pure cid'
        TriedLast _ -> randomChainIdAt c (_cutMinHeight c)
    logFun @T.Text Debug $ "newWork: picked chain " <> toText cid

    -- wait until at least one chain has primed work. we don't wait until *our*
    -- chain has primed work, because if other chains have primed work, we want
    -- to loop and select one of those chains. it is not a normal situation to
    -- have no chains with primed work if there are more than a couple chains.
    mpw <- atomically $ do
        PrimedWork pw <- readTVar tpw
        mpw <- maybe retry return (HM.lookup mid pw)
        guard (any isWorkReady mpw)
        return mpw
    let mr = T2
            <$> HM.lookup cid mpw
            <*> getCutExtension c cid

    case mr of
        Just (T2 WorkStale _) -> do
            logFun @T.Text Debug $ "newWork: chain " <> toText cid <> " has stale work"
            newWork logFun Anything eminer hdb pact tpw c
        Just (T2 (WorkAlreadyMined _) _) -> do
            logFun @T.Text Debug $ "newWork: chain " <> sshow cid <> " has a payload that was already mined"
            newWork logFun Anything eminer hdb pact tpw c
        Nothing -> do
            logFun @T.Text Debug $ "newWork: chain " <> toText cid <> " not mineable"
            newWork logFun Anything eminer hdb pact tpw c
        Just (T2 (WorkReady newBlock) extension) -> do
            let (primedParentHash, primedParentHeight, _) = newBlockParent newBlock
            if primedParentHash == view blockHash (_parentHeader (_cutExtensionParent extension))
            then do
                let payload = newBlockToPayloadWithOutputs newBlock
                let !phash = _payloadWithOutputsPayloadHash payload
                !wh <- newWorkHeader hdb extension phash
                pure $ Just $ T2 wh payload
            else do
                -- The cut is too old or the primed work is outdated. Probably
                -- the former because it the mining coordination background job
                -- is updating the primed work cache regularly. We could try
                -- another chain, but it's safer to just return 'Nothing' here
                -- and retry with an updated cut.
                --
                let !extensionParent = _parentHeader (_cutExtensionParent extension)
                logFun @T.Text Info
                    $ "newWork: chain " <> toText cid <> " not mineable because of parent header mismatch"
                    <> ". Primed parent hash: " <> toText primedParentHash
                    <> ". Primed parent height: " <> sshow primedParentHeight
                    <> ". Extension parent: " <> toText (view blockHash extensionParent)
                    <> ". Extension height: " <> sshow (view blockHeight extensionParent)

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
    -> PayloadWithOutputs
    -> SolvedWork
    -> IO ()
publish lf cdb pwVar miner pwo s = do
    c <- _cut cdb
    now <- getCurrentTimeIntegral
    try (extend c pwo s) >>= \case

        -- Publish CutHashes to CutDb and log success
        Right (bh, Just ch) -> do

            -- reset the primed payload for this cut extension
            atomically $ modifyTVar pwVar $ \(PrimedWork pw) ->
              PrimedWork $! HM.adjust (HM.insert (_chainId bh) (WorkAlreadyMined (view blockParent bh))) miner pw

            addCutHashes cdb ch

            let bytes = sum . fmap (BS.length . _transactionBytes . fst) $
                        _payloadWithOutputsTransactions pwo
            lf Info $ JsonLog $ NewMinedBlock
                { _minedBlockHeader = ObjectEncoded bh
                , _minedBlockTrans = int . V.length $ _payloadWithOutputsTransactions pwo
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
    T2 wh pwo <-
        withAsync (logDelays False 0) $ \_ -> newWorkForCut
    now <- getCurrentTimeIntegral
    atomically
        . modifyTVar' (_coordState mr)
        . over miningState
        . M.insert (_payloadWithOutputsPayloadHash pwo)
        $ T3 m pwo now
    return wh
  where
    -- here we log the case that the work loop has stalled.
    logDelays :: Bool -> Int -> IO ()
    logDelays loggedOnce n = do
        if loggedOnce
        then threadDelay 60_000_000
        else threadDelay 10_000_000
        let !n' = n + 1
        PrimedWork primedWork <- readTVarIO (_coordPrimedWork mr)
        -- technically this is in a race with the newWorkForCut function,
        -- which is likely benign when the mining loop has stalled for 10 seconds.
        currentCut <- _cut cdb
        let primedWorkMsg =
                case HM.lookup (view minerId m) primedWork of
                    Nothing ->
                        "no primed work for miner key" <> sshow m
                    Just mpw ->
                        let chainsWithBlocks = HS.fromMap $ flip HM.mapMaybe mpw $ \case
                                WorkReady {} -> Just ()
                                _ -> Nothing
                        in if
                            | HS.null chainsWithBlocks ->
                                "no chains have primed blocks"
                            | cids == chainsWithBlocks ->
                                "all chains have primed blocks"
                            | otherwise ->
                                "chains with primed blocks may be stalled. chains with primed work: "
                                <> sshow (toText <$> List.sort (HS.toList chainsWithBlocks))
        let extensibleChains =
                HS.fromList $ mapMaybe (\cid -> cid <$ getCutExtension currentCut cid) $ HS.toList cids
        let extensibleChainsMsg =
                if HS.null extensibleChains
                then "no chains are extensible in the current cut! here it is: " <> sshow currentCut
                else "the following chains can be extended in the current cut: " <> sshow (toText <$> HS.toList extensibleChains)
        logf @T.Text Warn $
          "findWork: stalled for " <>
          (
          if loggedOnce
          then "10s"
          else sshow n' <> "m"
          ) <>
          ". " <> primedWorkMsg <> ". " <> extensibleChainsMsg

        logDelays True n'

    v  = _chainwebVersion hdb
    cids = chainIds v

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
    key = view blockPayloadHash hdr
    tms = _coordState mr

    lf :: LogFunction
    lf = logFunction $ _coordLogger mr

    deleteKey = atomically . modifyTVar' tms . over miningState $ M.delete key
    publishWork (T3 m pwo _) =
        publish lf (_coordCutDb mr) (_coordPrimedWork mr) (view minerId m) pwo solved
