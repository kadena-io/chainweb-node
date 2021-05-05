{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Version.Utils
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Utilities for computing BlockHeights, CutHeights and BlockCounts based on
-- chainweb versions and chainweb graphs.
--
module Chainweb.Version.Utils
(
-- * Chain Ids
  randomChainId
, someChainId

-- * Chain Graph Properties By Block Height
, chainGraphs
, chainGraphsAt
, lastGraphChange
, nextGraphChange
, chainCountAt
, diameterAt
, degreeAt
, chainIdsAt
, randomChainIdAt
, someChainIdAt
, blockCountAt
, globalBlockCountAt
, globalBlockRateAt
, isGraphChange

-- * Chain Graph Properties By Cut Height
, avgCutHeightAt
, chainGraphsByCutHeight
, chainGraphAtCutHeight
, chainCountAtCutHeight
, diameterAtCutHeight
, degreeAtCutHeight
, blockCountAtCutHeight
, avgBlockHeightAtCutHeight
, lastGraphChangeByCutHeight
, nextGraphChangeByCutHeight

-- * Expected Block and Cut Heights
, expectedBlockHeightAfterSeconds
, expectedCutHeightAfterSeconds
, expectedBlockCountAfterSeconds
, expectedGlobalBlockCountAfterSeconds
) where

import Chainweb.BlockHeight
import Chainweb.Time

import Data.Foldable
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M

import GHC.Stack

import Numeric.Natural

import System.Random

-- internal modules

import Chainweb.Graph
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
--  Utils

limitHeight :: BlockHeight -> M.Map BlockHeight a -> M.Map BlockHeight a
limitHeight h = M.takeWhileAntitone (h >=)
{-# INLINE limitHeight #-}

-- | This is an internal function and must not be exported.
--
-- Precondition: the is non-empty maps and has an entry 0.
--
-- NOTE: only use when its safe. DO NOT EXPORT this function.
--
atHeight :: HasCallStack => BlockHeight -> M.Map BlockHeight a -> a
atHeight h = snd . fromJuste . M.lookupLE h
{-# INLINE atHeight #-}

-- | This is an internal function and must not be exported.
--
-- Precondition: the is non-empty maps and has an entry 0.
--
atCutHeight :: HasCallStack => CutHeight -> M.Map CutHeight a -> a
atCutHeight h = snd . fromJuste . M.lookupLE h
{-# INLINE atCutHeight #-}

-- -------------------------------------------------------------------------- --
-- Chain Graph Properties By Block Height

-- | BlockHeight intervals for the chain graphs of a chainweb version
--
-- Post-condition:
--
-- @
-- 0 == minimum $ M.keys $ chainGraphs v
-- @
--
chainGraphs :: HasChainwebVersion v => v -> M.Map BlockHeight ChainGraph
chainGraphs v = case _chainwebVersion v of
    Mainnet01 -> mainnet01GraphMap
    Testnet04 -> testnet04GraphMap
    Development -> developmentGraphMap
    x -> M.fromList . toList $ chainwebGraphs x

-- | Memoized mainnet01 chainweb graphs map
--
mainnet01GraphMap :: M.Map BlockHeight ChainGraph
mainnet01GraphMap = M.fromList . toList $ chainwebGraphs Mainnet01
{-# NOINLINE mainnet01GraphMap #-}

-- | Memoized testnet04 chainweb graphs map
--
testnet04GraphMap :: M.Map BlockHeight ChainGraph
testnet04GraphMap = M.fromList . toList $ chainwebGraphs Testnet04
{-# NOINLINE testnet04GraphMap #-}

-- | Memoized devnet chainweb graphs map
--
developmentGraphMap :: M.Map BlockHeight ChainGraph
developmentGraphMap = M.fromList . toList $ chainwebGraphs Development
{-# NOINLINE developmentGraphMap #-}

-- | BlockHeight intervals for the chain graphs of a chainweb version up to a
-- given block height.
--
-- Post-condition:
--
-- @
-- 0 == minimum $ M.keys $ chainGraphs v
-- @
--
chainGraphsAt
    :: HasChainwebVersion v
    => v
    -> BlockHeight
    -> M.Map BlockHeight ChainGraph
chainGraphsAt v h = limitHeight h (chainGraphs v)
{-# INLINE chainGraphsAt #-}

lastGraphChange
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> BlockHeight
    -> BlockHeight
lastGraphChange v h = fst . fromJuste . M.lookupLE h $ chainGraphs v
{-# INLINE lastGraphChange #-}

nextGraphChange
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> BlockHeight
    -> BlockHeight
nextGraphChange v h = fst . fromJuste . M.lookupGT h $ chainGraphs v
{-# INLINE nextGraphChange #-}

chainCountAt
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> BlockHeight
    -> Natural
chainCountAt v h = order $ atHeight h $ chainGraphs v
{-# INLINE chainCountAt #-}

degreeAt
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> BlockHeight
    -> Natural
degreeAt v h = degree $ atHeight h $ chainGraphs v
{-# INLINE degreeAt #-}

diameterAt
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> BlockHeight
    -> Natural
diameterAt v h = diameter $ atHeight h $ chainGraphs v
{-# INLINE diameterAt #-}

chainIdsAt
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> BlockHeight
    -> HS.HashSet ChainId
chainIdsAt v h = graphChainIds $ atHeight h $ chainGraphs v
{-# INLINE chainIdsAt #-}

-- | Uniformily get a random ChainId at the top of the current chainweb
--
randomChainId :: HasChainwebVersion v => v -> IO ChainId
randomChainId v = randomChainIdAt v maxBound
{-# INLINE randomChainId #-}

-- | Uniformily get a random ChainId at the given height of the chainweb
--
randomChainIdAt :: HasChainwebVersion v => v -> BlockHeight -> IO ChainId
randomChainIdAt v h = (!!) (toList cs) <$> randomRIO (0, length cs - 1)
  where
    cs = chainIdsAt v h
{-# INLINE randomChainIdAt #-}

-- | Sometimes, in particular for testing and examples, some fixed chain id is
-- needed, but it doesn't matter which one. This function provides some valid
-- chain ids for the top of the current chainweb.
--
someChainId :: HasCallStack => HasChainwebVersion v => v -> ChainId
someChainId v = someChainIdAt v maxBound
{-# INLINE someChainId #-}

-- | Sometimes, in particular for testing and examples, some fixed chain id is
-- needed, but it doesn't matter which one. This function provides some valid
-- chain ids for the chainweb at the given height.
--
someChainIdAt :: HasCallStack => HasChainwebVersion v => v -> BlockHeight -> ChainId
someChainIdAt v h = head . toList $ chainIdsAt v h
    -- 'head' is guaranteed to succeed because the empty graph isn't a valid chain
    -- graph.
{-# INLINE someChainIdAt #-}

isGraphChange :: HasChainwebVersion v => v -> BlockHeight -> Bool
isGraphChange v h = M.member h (chainGraphs v)
{-# INLINE isGraphChange #-}

-- -------------------------------------------------------------------------- --
-- Block Count

-- | The numbers of blocks on a chain up to the given height
--
-- Precondition: h > genesisHeight
--
blockCountAt
    :: HasCallStack
    => HasChainwebVersion v
    => HasChainId cid
    => v
    -> cid
    -> BlockHeight
    -> Natural
blockCountAt v cid h
    | h < gh = 0
    | otherwise = 1 + int h - int gh
  where
    gh = genesisHeight (_chainwebVersion v) (_chainId cid)

-- | The block count accross all chains at a given block height
--
globalBlockCountAt
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> BlockHeight
    -> Natural
globalBlockCountAt v h = sum
    $ fmap (\c -> blockCountAt v c h)
    $ toList
    $ chainIdsAt v h

globalBlockRateAt
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> BlockHeight
    -> Double
globalBlockRateAt v h = int r / int (chainCountAt v h)
  where
    BlockRate r = blockRate (_chainwebVersion v)

-- -------------------------------------------------------------------------- --
-- Cut Heights

-- | The the average over all possible cuts for which at least one chain has the
-- given block height.
--
-- Note, that the result isn't accurate for block heights around a chain graph
-- change.
--
avgCutHeightAt :: HasChainwebVersion v => v -> BlockHeight -> CutHeight
avgCutHeightAt v h = int $ int h * chainCountAt v h
{-# INLINE avgCutHeightAt #-}

-- | Cut height intervals for the chain graphs of a chainweb version
--
-- Post-condition:
--
-- @
-- 0 == minimum $ M.keys $ cutHeights v
-- @
--
chainGraphsByCutHeight
    :: HasChainwebVersion v
    => v
    -> M.Map CutHeight ChainGraph
chainGraphsByCutHeight = M.fromAscList
    . fmap (\(h,g) -> (int h * int (order g), g))
    . M.toAscList
    . chainGraphs
{-# INLINE chainGraphsByCutHeight #-}

-- chainGraphAtCutHeight = atCutHeight h

-- | The chain graph at the given cut height
--
-- Note, that the result isn't accurate during a chain graph change
--
chainGraphAtCutHeight :: HasChainwebVersion v => v -> CutHeight -> ChainGraph
chainGraphAtCutHeight v h = atCutHeight h $ chainGraphsByCutHeight v
{-# INLINE chainGraphAtCutHeight #-}

-- | The number of chains that exist at the given cut height
--
-- Note, that the result isn't accurate during a chain graph change
--
chainCountAtCutHeight :: HasChainwebVersion v => v -> CutHeight -> Natural
chainCountAtCutHeight v = order . chainGraphAtCutHeight v
{-# INLINE chainCountAtCutHeight #-}

-- | The diameter of the chain graph at the given cut height
--
-- Note, that the result isn't accurate during a chain graph change
--
diameterAtCutHeight :: HasChainwebVersion v => v -> CutHeight -> Natural
diameterAtCutHeight v = diameter . chainGraphAtCutHeight v
{-# INLINE diameterAtCutHeight #-}

-- | The degree of the chain graph at the given cut height
--
-- Note, that the result isn't accurate during a chain graph change
--
degreeAtCutHeight :: HasChainwebVersion v => v -> CutHeight -> Natural
degreeAtCutHeight v = degree . chainGraphAtCutHeight v
{-# INLINE degreeAtCutHeight #-}

-- | The average chain height at a given cut height.
--
-- Note, that the result isn't accurate for block heights around a chain graph
-- change.
--
avgBlockHeightAtCutHeight :: HasChainwebVersion v => v -> CutHeight -> Double
avgBlockHeightAtCutHeight v h = int h / int (chainCountAtCutHeight v h)
{-# INLINE avgBlockHeightAtCutHeight #-}

-- | The global number of blocks that exist at the given cut height.
--
-- Note, that the result isn't accurate for block heights around a chain graph
-- change.
--
blockCountAtCutHeight
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> CutHeight
    -> Natural
blockCountAtCutHeight v h
    = globalBlockCountAt v (int k `div` int (order g)) + int (h - k)
  where
   (k, g) = fromJuste $ M.lookupLE h $ chainGraphsByCutHeight v
{-# INLINE blockCountAtCutHeight #-}

lastGraphChangeByCutHeight
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> CutHeight
    -> CutHeight
lastGraphChangeByCutHeight v h
    = fst $ fromJuste $ M.lookupLE h $ chainGraphsByCutHeight v
{-# INLINE lastGraphChangeByCutHeight #-}

nextGraphChangeByCutHeight
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> CutHeight
    -> CutHeight
nextGraphChangeByCutHeight v h
    = fst $ fromJuste $ M.lookupGT h $ chainGraphsByCutHeight v
{-# INLINE nextGraphChangeByCutHeight #-}

-- -------------------------------------------------------------------------- --
-- Expected Block Count, Block Heights, and Cut Heights

-- | This function is useful for performance testing when calculating the
-- expected number of mined blocks during a test on a given chain.
--
expectedBlockCountAfterSeconds
    :: HasCallStack
    => HasChainwebVersion v
    => HasChainId cid
    => v
    -> cid
    -> Seconds
    -> Double
expectedBlockCountAfterSeconds v cid s = max 0 (1 + (int s / int r) - int gh)
    -- The `max 0` term is required for chains that were added during graph transitions
    -- and thus have `genesisHeight > 0`
  where
    BlockRate r = blockRate (_chainwebVersion v)
    gh = genesisHeight (_chainwebVersion v) (_chainId cid)

-- | This function is useful for performance testing when calculating the
-- expected number of mined blocks during a test accross all chains.
--
-- The sum of count for all chains is multiplied by 0.4 to compensate for the
-- fact that chains are blocked about 60% of the time on small graphs when used with
-- chainweb versions with fixed expected solve times and no difficulty adjustment.
--
expectedGlobalBlockCountAfterSeconds
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> Seconds
    -> Double
expectedGlobalBlockCountAfterSeconds v s = (* 0.4)
    $ sum
    $ fmap (\c -> expectedBlockCountAfterSeconds v c s)
    $ toList
    $ chainIdsAt v (round eh)
  where
    eh = expectedBlockHeightAfterSeconds v s

-- | The expected BlockHeight after the given number of seconds has passed.
--
-- This function is useful for performance testing.
--
expectedBlockHeightAfterSeconds
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> Seconds
    -> Double
expectedBlockHeightAfterSeconds v s = (int s / int r)
  where
    BlockRate r = blockRate (_chainwebVersion v)

-- | The expected CutHeight after the given number of seconds has passed.
--
-- This function is useful for performance testing.
--
expectedCutHeightAfterSeconds
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> Seconds
    -> Double
expectedCutHeightAfterSeconds v s = eh * int (chainCountAt v (round eh))
  where
    eh = expectedBlockHeightAfterSeconds v s
