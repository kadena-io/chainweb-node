{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Error.Util (hoistEither, (!?), (??))
import Control.Lens (imapM, set, to, (^.), (^?!))
import Control.Monad (join, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)

import Data.Aeson (ToJSON)
import Data.Bool (bool)
import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import Data.Generics.Wrapped (_Unwrapped)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..), T3(..))
import qualified Data.Vector as V

import GHC.Generics (Generic)
import GHC.Stack

import System.LogLevel (LogLevel(..))

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash (BlockHash, BlockHashRecord(..))
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation (prop_block_pow)
import Chainweb.Cut hiding (join)
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Logging.Miner
import Chainweb.Miner.Pact (Miner(..), MinerId(..), minerId)
import Chainweb.Payload
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (Micros(..), Time(..), getCurrentTimeIntegral)
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Version.Utils

import Data.LogMessage (JsonLog(..), LogFunction)

import Utils.Logging.Trace (trace)

-- -------------------------------------------------------------------------- --
-- Miner

-- | Precached payloads for Private Miners. This allows new work requests to be
-- made as often as desired, without clogging the Pact queue.
--
newtype PrimedWork =
    PrimedWork (HM.HashMap MinerId (HM.HashMap ChainId (Maybe PayloadWithOutputs)))
    deriving newtype (Semigroup, Monoid)

-- | Data shared between the mining threads represented by `newWork` and
-- `publish`.
--
-- The key is a unique pair of `BlockHash` of the parent block with the hash of
-- the current block's payload.
--
newtype MiningState = MiningState
    (M.Map (T2 BlockHash BlockPayloadHash) (T3 Miner PayloadWithOutputs (Time Micros)))
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
    -> PactExecutionService
    -> TVar PrimedWork
    -> Cut
    -> IO (T2 BlockHeader PayloadWithOutputs)
newWork logFun choice eminer pact tpw c = do
    -- Randomly pick a chain to mine on, unless the caller specified a specific
    -- one.
    --
    cid <- chainChoice c choice
    logFun @T.Text Debug $ "newWork: picked chain " <> sshow cid

    -- The parent block to mine on. Any given chain will always
    -- contain at least a genesis block, so this otherwise naughty
    -- `^?!` will always succeed.
    --
    let !p = ParentHeader (c ^?! ixg cid)

    mr <- case eminer of
        Primed m -> primed m cid p <$> readTVarIO tpw
        Plebian m -> public p m
    case mr of
        -- The proposed Chain wasn't mineable, either because the adjacent
        -- parents weren't available, or because the chain is mid-update.
        Nothing -> do
            logFun @T.Text Debug $ "newWork: chain " <> sshow cid <> " not mineable"
            newWork logFun (TriedLast cid) eminer pact tpw c
        Just (T2 payload adjParents) -> do
            -- Assemble a candidate `BlockHeader` without a specific `Nonce`
            -- value. `Nonce` manipulation is assumed to occur within the
            -- core Mining logic.
            --
            creationTime <- BlockCreationTime <$> getCurrentTimeIntegral
            let !phash = _payloadWithOutputsPayloadHash payload
                !header = newBlockHeader adjParents phash (Nonce 0) creationTime p
            logFun @T.Text Debug $ "newWork: got work for header " <> encodeToText (ObjectEncoded header)
            pure $ T2 header payload
  where
    primed
        :: Miner
        -> ChainId
        -> ParentHeader
        -> PrimedWork
        -> Maybe (T2 PayloadWithOutputs BlockHashRecord)
    primed (Miner mid _) cid p (PrimedWork pw) = T2
        <$> join (HM.lookup mid pw >>= HM.lookup cid)
        <*> getAdjacentParents c p

    public
        :: ParentHeader
        -> Miner
        -> IO (Maybe (T2 PayloadWithOutputs BlockHashRecord))
    public p miner = case getAdjacentParents c p of
        Nothing -> do
            logFun @T.Text Debug
                $ "newWork.public: failed to get adjacent parents."
                <> " Parent: " <> encodeToText (ObjectEncoded $ _parentHeader p)
                <> " Cuthashes: " <> encodeToText (cutToCutHashes Nothing c)
            pure Nothing
        Just adj -> do
            -- This is an expensive call --
            payload <- trace logFun "Chainweb.Miner.Coordinator.newWork.newBlock" () 1
                (_pactNewBlock pact miner p)
            pure . Just $ T2 payload adj

chainChoice :: Cut -> ChainChoice -> IO ChainId
chainChoice c choice = case choice of
    Anything -> randomChainIdAt c (maxChainHeight c + 1)
    Suggestion cid -> pure cid
    TriedLast cid -> loop cid
  where
    loop :: ChainId -> IO ChainId
    loop cid = do
        new <- randomChainIdAt c (maxChainHeight c + 1)
        bool (pure new) (loop cid) $ new == cid

-- | Accepts a "solved" `BlockHeader` from some external source (e.g. a remote
-- mining client), attempts to reassociate it with the current best `Cut`, and
-- publishes the result to the `Cut` network.
--
-- There are a number of "fail fast" conditions which will kill the candidate
-- `BlockHeader` before it enters the Cut pipeline.
--
publish :: LogFunction -> MiningState -> CutDb cas -> BlockHeader -> IO ()
publish lf (MiningState ms) cdb bh = do
    c <- _cut cdb
    let !phash = _blockPayloadHash bh
        !bpar = _blockParent bh
    now <- getCurrentTimeIntegral
    res <- runExceptT $ do
        -- Fail Early: If a `BlockHeader` comes in that isn't associated with any
        -- Payload we know about, reject it.
        --
        T3 m pl _ <- M.lookup (T2 bpar phash) ms
            ?? T2 "Unknown" "No associated Payload"

        let !miner = m ^. minerId . _Unwrapped

        -- Fail Early: If a `BlockHeader`'s injected Nonce (and thus its POW
        -- Hash) is trivially incorrect, reject it.
        --
        unless (prop_block_pow bh) . hoistEither .
            Left $ T2 miner "Invalid POW hash"

        -- Fail Early: If the `BlockHeader` is already stale and can't be
        -- appended to the best `Cut` we know about, reject it.
        --
        c' <- tryMonotonicCutExtension c bh
            !? T2 miner "Mined block for outdated Cut"

        lift $ do
            -- Publish the new Cut into the CutDb (add to queue).
            --
            addCutHashes cdb $ cutToCutHashes Nothing c'
                & set cutHashesHeaders (HM.singleton (_blockHash bh) bh)
                & set cutHashesPayloads (HM.singleton phash (payloadWithOutputsToPayloadData pl))

            -- Log mining success.
            --
            let bytes = foldl' (\acc (Transaction bs, _) -> acc + BS.length bs) 0 $
                        _payloadWithOutputsTransactions pl

            pure $ NewMinedBlock
                { _minedBlockHeader = ObjectEncoded bh
                , _minedBlockTrans = int . V.length $ _payloadWithOutputsTransactions pl
                , _minedBlockSize = int bytes
                , _minedBlockMiner = miner
                , _minedBlockDiscoveredAt = now }
    case res of
        -- The solution is already stale, so we can do whatever work we want to
        -- here.
        Left (T2 mnr msg) -> do
            let !p = c ^?! ixg (_chainId bh)
            lf Info $ JsonLog OrphanedBlock
                { _orphanedHeader = ObjectEncoded bh
                , _orphanedBestOnCut = ObjectEncoded p
                , _orphanedDiscoveredAt = now
                , _orphanedMiner = mnr
                , _orphanedReason = msg
                }
        Right r -> lf Info $ JsonLog r

-- | Try to assemble the adjacent hashes for a new block header that is minded
-- on the given header.
--
-- FIXME: this function assumes that the Graph of the given parent header
-- is the same as for the new header!
--
getAdjacentParents
    :: HasCallStack
    => Cut
        -- ^ the cut which is to be extended
    -> ParentHeader
        -- ^ the header onto which the new block is created. It is expected
        -- that this header is contained in the cut.
    -> Maybe BlockHashRecord
getAdjacentParents c p = BlockHashRecord <$> newAdjHashes
  where
    parentHeight = _blockHeight $ _parentHeader p
    graph = chainGraphAt_ p (parentHeight + 1)

    -- | Try to get all adjacent hashes dependencies.
    --
    newAdjHashes :: Maybe (HM.HashMap ChainId BlockHash)
    newAdjHashes =
        imapM (\xcid _ -> c ^?! ixg xcid . to tryAdj)
        $ HS.toMap
        $ adjacentChainIds graph p

    -- TODO add test that this conforms with the braiding checks in Chainweb.Cut, or
    -- use a function from that module.
    --
    tryAdj :: BlockHeader -> Maybe BlockHash
    tryAdj b
        -- When the block is behind, we can move ahead
        | _blockHeight b == parentHeight + 1 = Just $! _blockParent b

        -- if the block is ahead it's blocked
        | _blockHeight b + 1 == parentHeight = Nothing -- chain is blocked

        -- If this is a graph transition we have to wait for all chains to catchup.
        -- When all chains have caught up @isTransitionCut c@ is @False@.
        | isTransitionCut c = Nothing

        -- If it's not a graph transition we can move ahead
        | _blockHeight b == parentHeight = Just $! _blockHash b

        -- The cut is invalid
        | _blockHeight b > parentHeight + 1 = error $ T.unpack
            $ "getAdjacentParents: detected invalid cut (adjacent parent too far ahead)."
            <> " Parent: " <> encodeToText (ObjectEncoded $ _parentHeader p)
            <> " Conflict: " <> encodeToText (ObjectEncoded b)
        | _blockHeight b + 1 < parentHeight = error $ T.unpack
            $ "getAdjacentParents: detected invalid cut (adjacent parent too far behind)."
            <> " Parent: " <> encodeToText (ObjectEncoded $ _parentHeader p)
            <> " Conflict: " <> encodeToText (ObjectEncoded b)
        | otherwise = error "Chainweb.Miner.Coordinator.getAdjacentParents: internal code invariant violation"
