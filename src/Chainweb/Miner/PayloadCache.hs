{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module: Chainweb.Miner.PayloadCache
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- As payload providers propose new payloads for the latest blocks those are
-- cached by the miner.
--
-- NOTE that consensus (and not the mining coordinator or payload provider)
-- decides when it makes sense to produce payloads ontop of a block. It does so
-- by notifying the payload provider in a syncToBlock call. This way block
-- production and mining can be supressed, for instance, during catchup or deep
-- catch merges.
--
-- Because payloads do not depend on parents on adjacent chains, it always makes
-- sense to compute and cache new payloads because even as chains are still
-- blocked payloads can already be computed.
--
-- The mining coordinator will request payloads for parent headers as chains
-- become unblocked.
--
-- The cache will always provide the most recent payload for a given parent
-- header. However, because mining is asynchronous, it is possible that a miner
-- resolves a block for a non-recent parent header. Hence, all payloads for a
-- given parent header must be kept.
--
-- Payloads can only be deleted when a parent header becomes outdated. Because
-- in Chainweb branches grow non-monotonically, there is no well-defined moment
-- when a payload becomes definitely outdated. However, usually, one or two
-- block heights should be sufficient. The diameter of the graph is a safe
-- choice. For any deeper reorgs payloads would have to be recomputed. Also, we
-- do not expect miners still working on older payloads anyways, so no miner
-- work would be lost.
--
module Chainweb.Miner.PayloadCache
( PayloadCache(..)
, newIO
, clearSTM
, clearIO
, sizeSTM
, sizeIO
, insertSTM
, insertIO
, getLatestSTM
, getLatestIO
, awaitLatestSTM
, awaitLatestIO
, lookupSTM
, lookupIO

-- * Misc Tools
, pruneSTM
, pruneIO
, payloadHashesSTM
, payloadHashesIO
) where

import Chainweb.BlockHash
import Chainweb.BlockPayloadHash
import Chainweb.PayloadProvider
import Control.Concurrent.STM
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Numeric.Natural
import Chainweb.BlockHeight

-- | A new payload cache for a chain.
--
-- Payloads are pruned from this cache when a new payload is received that is
-- more than graph diameter block heights ahead. Only items that have a block
-- height that is smaller than most recent item are guaranteed to be cached.
-- Items with larger block height may or may not be  pruned.
--
-- TODO: we should probably also limit the size, either globally or per block
-- height.
--
-- TODO: This could be implemented more efficiently by using a ring buffer with
-- one map per block height. The buffer size would have to change when the graph
-- changes. However, graph changes are rare events and it is fine to always use
-- the largest graph defined by a fork point, even if the graph transition has
-- not yet occurred.
--
data PayloadCache = PayloadCache
    { _payloadCacheDepth :: !Natural
    , _payloadCacheMap :: !(TVar (M.Map (RankedBlockHash, Int) NewPayload))
    }

-- NOTE that we provide separate @STM@ and @IO@ versions for functions. The
-- reason for this is that @readTVarIO v@ is more efficient than @atomically
-- (readTVar v)@. In many cases the @IO@ version can benefit from that.

newIO
    :: Natural
        -- ^ depth
    -> IO PayloadCache
newIO n = PayloadCache n <$> newTVarIO mempty

clearSTM :: PayloadCache -> STM ()
clearSTM pc = writeTVar (_payloadCacheMap pc) mempty

clearIO :: PayloadCache -> IO ()
clearIO = atomically . clearSTM

sizeSTM :: PayloadCache -> STM Natural
sizeSTM pc = fromIntegral . M.size <$> readTVar (_payloadCacheMap pc)

sizeIO :: PayloadCache -> IO Natural
sizeIO pc = fromIntegral . M.size <$> readTVarIO (_payloadCacheMap pc)

payloadHashesSTM :: PayloadCache -> STM [BlockPayloadHash]
payloadHashesSTM pc = fmap _newPayloadBlockPayloadHash . M.elems
    <$> readTVar (_payloadCacheMap pc)

payloadHashesIO :: PayloadCache -> IO [BlockPayloadHash]
payloadHashesIO pc = fmap _newPayloadBlockPayloadHash . M.elems
    <$> readTVarIO (_payloadCacheMap pc)

-- | Get the most recent payload for the given parent hash
--
getLatestSTM
    :: PayloadCache
    -> RankedBlockHash
    -> STM (Maybe NewPayload)
getLatestSTM pc rh = do
    lookupValueGE (rh, minBound) <$> readTVar (_payloadCacheMap pc) >>= \case
        Just x
            | _newPayloadRankedParentHash x == rh -> return (Just x)
        _ -> return Nothing

-- | Get the most recent payload for the given parent hash
--
getLatestIO
    :: PayloadCache
    -> RankedBlockHash
    -> IO (Maybe NewPayload)
getLatestIO pc rh =
    lookupValueGE (rh, minBound) <$> readTVarIO (_payloadCacheMap pc) >>= \case
        Just x
            | _newPayloadRankedParentHash x == rh -> return (Just x)
        _ -> return Nothing

-- | Await the most recent payload for the given parent hash
--
awaitLatestSTM
    :: PayloadCache
    -> RankedBlockHash
    -> STM NewPayload
awaitLatestSTM pc rh = getLatestSTM pc rh >>= maybe retry return

-- | Await the most recent payload for the given parent hash
--
awaitLatestIO
    :: PayloadCache
    -> RankedBlockHash
    -> IO NewPayload
awaitLatestIO pc = atomically . awaitLatestSTM pc

-- | Lookup a specific new payload by its block payload hash
--
lookupSTM
    :: PayloadCache
    -> RankedBlockHash
    -> BlockPayloadHash
    -> STM (Maybe NewPayload)
lookupSTM pc rh pld = do
    m <- readTVar (_payloadCacheMap pc)

    -- Focus on the ranked payload hash search for the payload hash starting
    -- with the most recent value.
    return
        . L.find match
        . fmap snd
        . M.toDescList
        . fst
        . M.split (rh, maxBound)
        . snd
        . M.split (rh, minBound)
        $ m
  where
    match = (== pld) . _newPayloadBlockPayloadHash

-- | Lookup a specific new payload by its block payload hash
--
lookupIO
    :: PayloadCache
    -> RankedBlockHash
    -> BlockPayloadHash
    -> IO (Maybe NewPayload)
lookupIO pc rh pld = do
    m <- readTVarIO (_payloadCacheMap pc)

    -- Focus on the ranked payload hash search for the payload hash starting
    -- with the most recent value.
    return
        . L.find match
        . fmap snd
        . M.toDescList
        . fst
        . M.split (rh, maxBound)
        . snd
        . M.split (rh, minBound)
        $ m
  where
    match = (== pld) . _newPayloadBlockPayloadHash

-- | Insert a new payload into the cache
--
insertSTM
    :: PayloadCache
    -> NewPayload
    -> STM ()
insertSTM pc pld =
    modifyTVar' (_payloadCacheMap pc) $
        M.insert key pld . prune (_payloadCacheDepth pc) h
  where
    h = _newPayloadParentHeight pld
    p = _newPayloadParentHash pld
    key = (RankedBlockHash h p, _newPayloadNumber pld)

-- | Insert a new payload into the cache. The cache is pruned before the new
-- item is inserted.
--
insertIO
    :: PayloadCache
    -> NewPayload
    -> IO ()
insertIO pc = atomically . insertSTM pc

-- | Prune the cache for the given block height. Pruning is also performed each
-- time a new item is inserted into the cache
--
pruneSTM
    :: PayloadCache
    -> BlockHeight
    -> STM()
pruneSTM pc h = modifyTVar' (_payloadCacheMap pc) $!
    prune (_payloadCacheDepth pc) h

-- | Prune the cache for the given block height. Pruning is also performed each
-- time a new item is inserted into the cache
--
pruneIO
    :: PayloadCache
    -> BlockHeight
    -> IO ()
pruneIO pc = atomically . pruneSTM pc

-- -------------------------------------------------------------------------- --
-- Utils

lookupValueGE :: Ord k => k -> M.Map k v -> Maybe v
lookupValueGE k m = snd <$> M.lookupGE k m

prune
    :: Natural
    -> BlockHeight
    -> M.Map (RankedBlockHash, Int) v
    -> M.Map (RankedBlockHash, Int) v
prune d h m
    | h < fromIntegral d = m
    | otherwise = snd $ M.split pivot m
  where
    pivot = (RankedBlockHash (h - fromIntegral d) nullBlockHash, minBound)

