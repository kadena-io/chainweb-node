{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
, MiningStats(..)
, PrevTime(..)
, ChainChoice(..)
, PrimedWork(..)
, MinerStatus(..), minerStatus
  -- * Functions
, newWork
, publish
) where

import Control.Concurrent.STM.TVar
import Control.DeepSeq (NFData)
import Control.Lens (view, (^?!))
import Control.Monad
import Control.Monad.Catch

import Data.Aeson (ToJSON)
import Data.Bool (bool)
import qualified Data.ByteString as BS
import Data.Generics.Wrapped (_Unwrapped)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..), T3(..))
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
import Chainweb.Logging.Miner
import Chainweb.Miner.Pact (Miner(..), MinerId(..))
import Chainweb.Payload
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (Micros(..), Time(..), getCurrentTimeIntegral)
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB

import Data.LogMessage (JsonLog(..), LogFunction)

import Utils.Logging.Trace (trace)

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
-- Miner

-- | Precached payloads for Private Miners. This allows new work requests to be
-- made as often as desired, without clogging the Pact queue.
--
newtype PrimedWork =
    PrimedWork (HM.HashMap MinerId (HM.HashMap ChainId (Maybe PayloadData)))
    deriving newtype (Semigroup, Monoid)

-- | Data shared between the mining threads represented by `newWork` and
-- `publish`.
--
-- The key is hash of the current block's payload.
--
newtype MiningState = MiningState
    (M.Map BlockPayloadHash (T3 Miner PayloadData (Time Micros)))
    deriving stock (Generic)
    deriving newtype (Semigroup, Monoid)

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

data ChainChoice = Anything | TriedLast ChainId | Suggestion ChainId

-- | A `Miner`'s status. Will be `Primed` if defined in the nodes `miners` list.
-- This affects whether to serve the Miner primed payloads.
--
data MinerStatus = Primed Miner | Plebian Miner

minerStatus :: MinerStatus -> Miner
minerStatus (Primed m) = m
minerStatus (Plebian m) = m

-- | Construct a new `BlockHeader` to mine on.
--
newWork
    :: LogFunction
    -> ChainChoice
    -> MinerStatus
    -> WebBlockHeaderDb
        -- ^ this is used to lookup parent headers that are not in the cut
        -- itself.
    -> PactExecutionService
    -> TVar PrimedWork
    -> Cut
    -> IO (T2 WorkHeader PayloadData)
newWork logFun choice eminer hdb pact tpw c = do

    -- Randomly pick a chain to mine on, unless the caller specified a specific
    -- one.
    --
    cid <- chainChoice c choice
    logFun @T.Text Debug $ "newWork: picked chain " <> sshow cid

    mr <- case eminer of
        Primed m -> primed m cid <$> readTVarIO tpw
        Plebian m -> public m cid
    case mr of
        Nothing -> do
            logFun @T.Text Debug $ "newWork: chain " <> sshow cid <> " not mineable"
            newWork logFun (TriedLast cid) eminer hdb pact tpw c
        Just (T2 payload extension) -> do
            let !phash = _payloadDataPayloadHash payload
            !wh <- newWorkHeader hdb extension phash
            pure $ T2 wh payload
  where
    primed
        :: Miner
        -> ChainId
        -> PrimedWork
        -> Maybe (T2 PayloadData CutExtension)
    primed (Miner mid _) cid (PrimedWork pw) = T2
        <$> join (HM.lookup mid pw >>= HM.lookup cid)
        <*> getCutExtension c cid

    public
        :: Miner
        -> ChainId
        -> IO (Maybe (T2 PayloadData CutExtension))
    public miner cid = case getCutExtension c cid of
        Nothing -> do
            logFun @T.Text Debug
                $ "newWork.public: failed to get adjacent parents."
                <> " Parent: " <> encodeToText (ObjectEncoded $ c ^?! ixg cid)
                <> " Cuthashes: " <> encodeToText (cutToCutHashes Nothing c)
            pure Nothing
        Just ext -> do
            -- This is an expensive call --
            payload <- trace logFun "Chainweb.Miner.Coordinator.newWork.newBlock" () 1
                (_pactNewBlock pact miner $ _cutExtensionParent ext)
            pure . Just $ T2 (payloadWithOutputsToPayloadData payload) ext

chainChoice :: Cut -> ChainChoice -> IO ChainId
chainChoice c choice = case choice of
    Anything -> randomChainIdAt c (minChainHeight c)
    Suggestion cid -> pure cid
    TriedLast cid -> loop cid
  where
    loop :: ChainId -> IO ChainId
    loop cid = do
        new <- randomChainIdAt c (minChainHeight c)
        bool (pure new) (loop cid) $ new == cid

-- | Accepts a "solved" `BlockHeader` from some external source (e.g. a remote
-- mining client), attempts to reassociate it with the current best `Cut`, and
-- publishes the result to the `Cut` network.
--
-- There are a number of "fail fast" conditions which will kill the candidate
-- `BlockHeader` before it enters the Cut pipeline.
--
publish
    :: LogFunction
    -> CutDb cas
    -> MinerId
    -> PayloadData
    -> SolvedWork
    -> IO ()
publish lf cdb miner pd s = do
    c <- _cut cdb
    now <- getCurrentTimeIntegral
    try (extend c pd s) >>= \case

        -- Publish CutHashes to CutDb and log success
        Right (bh, Just ch) -> do
            addCutHashes cdb ch
            let bytes = sum . fmap (BS.length . _transactionBytes) $
                        _payloadDataTransactions pd

            lf Info $ JsonLog $ NewMinedBlock
                { _minedBlockHeader = ObjectEncoded bh
                , _minedBlockTrans = int . V.length $ _payloadDataTransactions pd
                , _minedBlockSize = int bytes
                , _minedBlockMiner = view _Unwrapped miner
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
        , _orphanedMiner = view _Unwrapped miner
        , _orphanedReason = msg
        }

