{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Miner.Kato
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--

module Chainweb.Miner.Kato
  ( -- * Types
    MiningState(..)
  , PrevBlock(..)
    -- * Functions
  , awaitNewCut
  , newWork
  , publish
  ) where

import Control.Concurrent.STM (TVar, atomically, readTVarIO, retry)
import Control.Lens (iforM, set, to, view, (^?!))
import Control.Monad (when)

import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HM
import Data.Ratio ((%))
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..), T3(..))
import qualified Data.Vector as V

import Numeric.Natural (Natural)

import System.LogLevel (LogLevel(..))

-- internal modules

import Chainweb.BlockHash (BlockHash, BlockHashRecord(..))
import Chainweb.BlockHeader
import Chainweb.ChainId (ChainId)
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Difficulty
import Chainweb.Logging.Miner
import Chainweb.Miner.Pact (Miner)
import Chainweb.NodeId (NodeId, nodeIdFromNodeId)
import Chainweb.Payload
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (Micros(..), getCurrentTimeIntegral)
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.WebPactExecutionService

import Data.LogMessage (JsonLog(..), LogFunction)

-- -------------------------------------------------------------------------- --
-- Miner

-- | Data shared between the mining threads represented by `newWork` and
-- `publish`.
--
newtype MiningState =
    MiningState (HM.HashMap BlockPayloadHash (T2 PrevBlock PayloadWithOutputs))

-- | A `BlockHeader` that's understood to be the parent of some current,
-- "working" `BlockHeader`.
--
newtype PrevBlock = PrevBlock BlockHeader

-- | Construct a new `BlockHeader` to mine on.
--
newWork
    :: forall cas
    .  Miner
    -> NodeId
    -> CutDb cas
    -> IO (T3 PrevBlock BlockHeader PayloadWithOutputs)
newWork miner nid cdb = _cut cdb >>= work
  where
    pact :: PactExecutionService
    pact = _webPactExecutionService . _webBlockPayloadStorePact $ view cutDbPayloadStore cdb

    work :: Cut -> IO (T3 PrevBlock BlockHeader PayloadWithOutputs)
    work c = do
        -- Randomly pick a chain to mine on.
        --
        cid <- randomChainId c

        -- The parent block the mine on. Any given chain will always
        -- contain at least a genesis block, so this otherwise naughty
        -- `^?!` will always succeed.
        --
        let !p = c ^?! ixg cid

        -- Check if the chain can be mined on by determining if adjacent parents
        -- also exist. If they don't, we test other chains on this same `Cut`,
        -- since we still believe this `Cut` to be good.
        --
        case getAdjacentParents c p of
            Nothing -> work c
            Just adjParents -> do
                -- Fetch a Pact Transaction payload. This is an expensive call
                -- that shouldn't be repeated.
                --
                payload <- _pactNewBlock pact miner p

                -- Assemble a candidate `BlockHeader` without a specific `Nonce`
                -- value. `Nonce` manipulation is assumed to occur within the
                -- core Mining logic.
                --
                creationTime <- getCurrentTimeIntegral
                let !phash = _payloadWithOutputsPayloadHash payload
                    !header = newBlockHeader
                        (nodeIdFromNodeId nid cid)
                        adjParents
                        phash
                        (Nonce 0)  -- TODO Confirm that this is okay.
                        creationTime
                        p

                pure $ T3 (PrevBlock p) header payload

-- | THREAD: Accepts a "solved" `BlockHeader` from some external source (likely
-- a remote mining client), attempts to reassociate it with the current best
-- `Cut`, and publishes the result to the `Cut` network.
--
publish :: LogFunction -> TVar MiningState -> CutDb cas -> BlockHeader -> IO ()
publish lf tms cdb bh = do
    -- Reassociate the new `BlockHeader` with the current `Cut`, if possible.
    -- Otherwise, return silently.
    --
    c <- _cut cdb
    MiningState ms <- readTVarIO tms
    let !phash = _blockPayloadHash bh
    case HM.lookup phash ms of
        Nothing -> pure ()  -- Bogus `BlockHeader` given that has no associated Payload.
        Just (T2 p pl) -> tryMonotonicCutExtension c bh >>= \case
            Nothing -> lf @T.Text Info $ "Newly mined block for outdated cut"
            Just c' -> do
                -- Publish the new Cut into the CutDb (add to queue).
                --
                addCutHashes cdb $ cutToCutHashes Nothing c'
                    & set cutHashesHeaders
                        (HM.singleton (_blockHash bh) bh)
                    & set cutHashesPayloads
                        (HM.singleton phash (payloadWithOutputsToPayloadData pl))

                -- Log mining success.
                --
                let bytes = foldl' (\acc (Transaction bs, _) -> acc + BS.length bs) 0 $
                        _payloadWithOutputsTransactions pl
                    !nmb = NewMinedBlock
                           (ObjectEncoded bh)
                           (int . V.length $ _payloadWithOutputsTransactions pl)
                           (int bytes)
                           (estimatedHashes p bh)

                lf @(JsonLog NewMinedBlock) Info $ JsonLog nmb

-- | The estimated per-second Hash Power of the network, guessed from the time
-- it took to mine this block among all miners on the chain.
--
estimatedHashes :: PrevBlock -> BlockHeader -> Natural
estimatedHashes (PrevBlock p) b = floor $ (d % t) * 1000000
  where
    t :: Integer
    t = case timeBetween b p of Micros t' -> int t'

    d :: Integer
    d = case targetToDifficulty $ _blockTarget b of
        HashDifficulty (PowHashNat w) -> int w

awaitNewCut :: CutDb cas -> Cut -> IO Cut
awaitNewCut cdb c = atomically $ do
    c' <- _cutStm cdb
    when (c' == c) retry
    return c'

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
