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
-- Copyright: Copyright © 2019 Kadena LLC.
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
  , CachedPayload
  , MinerStatus(..), minerStatus
    -- * Functions
  , newWork
  , publish
  ) where

import Data.Aeson (ToJSON)
import Data.Bool (bool)
import Data.Coerce (coerce)

import Control.Concurrent.STM.TVar
import Control.DeepSeq (NFData)
import Control.Error.Util (hoistEither, (!?), (??))
import Control.Lens (iforM, set, to, (^.), (^?!))
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)

import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import Data.Generics.Wrapped (_Unwrapped)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Ratio ((%))
import Data.Tuple.Strict (T2(..), T3(..))
import qualified Data.Vector as V

import GHC.Generics (Generic)

import Numeric.Natural (Natural)

import System.LogLevel (LogLevel(..))

-- internal modules

import Chainweb.BlockHash (BlockHash, BlockHashRecord(..))
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation (prop_block_pow)
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Difficulty
import Chainweb.Logging.Miner
import Chainweb.Miner.Pact (Miner(..), MinerId(..), minerId)
import Chainweb.Payload
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (Micros(..), getCurrentTimeIntegral)
import Chainweb.Utils hiding (check)
import Chainweb.Version

import Data.LogMessage (JsonLog(..), LogFunction)

import Utils.Logging.Trace (trace)

-- -------------------------------------------------------------------------- --
-- Miner

-- | Precached payloads for Private Miners. This allows new work requests to be
-- made as often as desired, without clogging the Pact queue.
--
newtype PrimedWork =
    PrimedWork (HM.HashMap MinerId (HM.HashMap ChainId (Maybe CachedPayload)))
    deriving newtype (Semigroup, Monoid)

type CachedPayload = T2 PayloadWithOutputs BlockCreationTime

-- | Data shared between the mining threads represented by `newWork` and
-- `publish`.
--
newtype MiningState = MiningState
    (M.Map (T2 BlockCreationTime BlockPayloadHash) (T3 Miner PrevTime PayloadWithOutputs))
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
    -> IO (T3 PrevTime BlockHeader PayloadWithOutputs)
newWork logFun choice eminer pact tpw c = do
    -- Randomly pick a chain to mine on, unless the caller specified a specific
    -- one.
    --
    cid <- chainChoice c choice

    -- The parent block the mine on. Any given chain will always
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
        Nothing -> newWork logFun (TriedLast cid) eminer pact tpw c
        Just (T2 (T2 payload creationTime) adjParents) -> do
            -- Assemble a candidate `BlockHeader` without a specific `Nonce`
            -- value. `Nonce` manipulation is assumed to occur within the
            -- core Mining logic.
            --
            let !phash = _payloadWithOutputsPayloadHash payload
                !header = newBlockHeader adjParents phash (Nonce 0) creationTime p
            pure $ T3 (PrevTime . _blockCreationTime $ coerce p) header payload
  where
    primed
        :: Miner
        -> ChainId
        -> ParentHeader
        -> PrimedWork
        -> Maybe (T2 CachedPayload BlockHashRecord)
    primed (Miner mid _) cid (ParentHeader p) (PrimedWork pw) = T2
        <$> (HM.lookup mid pw >>= HM.lookup cid >>= id)
        <*> getAdjacentParents c p

    public :: ParentHeader -> Miner -> IO (Maybe (T2 CachedPayload BlockHashRecord))
    public (ParentHeader p) miner = case getAdjacentParents c p of
        Nothing -> pure Nothing
        Just adj -> do
            creationTime <- BlockCreationTime <$> getCurrentTimeIntegral
            -- This is an expensive call --
            payload <- trace logFun "Chainweb.Miner.Coordinator.newWork.newBlock" () 1
                (_pactNewBlock pact miner p creationTime)
            pure . Just $ T2 (T2 payload creationTime) adj

chainChoice :: Cut -> ChainChoice -> IO ChainId
chainChoice c choice = case choice of
    Anything -> randomChainId c
    Suggestion cid -> pure cid
    TriedLast cid -> loop cid
  where
    loop :: ChainId -> IO ChainId
    loop cid = do
      new <- randomChainId c
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
        !bct = _blockCreationTime bh
    res <- runExceptT $ do
        -- Fail Early: If a `BlockHeader` comes in that isn't associated with any
        -- Payload we know about, reject it.
        --
        T3 m p pl <- M.lookup (T2 bct phash) ms
            ?? OrphanedBlock (ObjectEncoded bh) "Unknown" "No associated Payload"

        let !miner = m ^. minerId . _Unwrapped

        -- Fail Early: If a `BlockHeader`'s injected Nonce (and thus its POW
        -- Hash) is trivially incorrect, reject it.
        --
        unless (prop_block_pow bh) . hoistEither .
            Left $ OrphanedBlock (ObjectEncoded bh) miner "Invalid POW hash"

        -- Fail Early: If the `BlockHeader` is already stale and can't be
        -- appended to the best `Cut` we know about, reject it.
        --
        c' <- tryMonotonicCutExtension c bh
            !? OrphanedBlock (ObjectEncoded bh) miner "Mined block for outdated Cut"

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

            now <- getCurrentTimeIntegral
            pure $ NewMinedBlock
                { _minedBlockHeader = ObjectEncoded bh
                , _minedBlockTrans = int . V.length $ _payloadWithOutputsTransactions pl
                , _minedBlockSize = int bytes
                , _minedHashAttempts = estimatedHashes p bh
                , _minedBlockMiner = miner
                , _minedBlockDiscoveredAt = now
                }
    either (lf Info . JsonLog) (lf Info . JsonLog) res

-- | The estimated per-second Hash Power of the network, guessed from the time
-- it took to mine this block among all miners on the chain.
--
estimatedHashes :: PrevTime -> BlockHeader -> Natural
estimatedHashes (PrevTime p) b = floor $ (d % t) * 1000000
  where
    t :: Integer
    t = case timeBetween (_blockCreationTime b) p of Micros t' -> int t'

    d :: Integer
    d = case targetToDifficulty $ _blockTarget b of
        HashDifficulty (PowHashNat w) -> int w

getAdjacentParents :: Cut -> BlockHeader -> Maybe BlockHashRecord
getAdjacentParents c p = BlockHashRecord <$> newAdjHashes
  where
    -- | Try to get all adjacent hashes dependencies.
    --
    newAdjHashes :: Maybe (HM.HashMap ChainId BlockHash)
    newAdjHashes = iforM (_getBlockHashRecord $ _blockAdjacentHashes p) $ \xcid _ ->
        c ^?! ixg xcid . to (tryAdj (_blockHeight p))

    tryAdj :: BlockHeight -> BlockHeader -> Maybe BlockHash
    tryAdj h b
        | _blockHeight b == h = Just $! _blockHash b
        | _blockHeight b == h + 1 = Just $! _blockParent b
        | otherwise = Nothing
