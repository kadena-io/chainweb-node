{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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
, globalBlockDelayAt
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

-- * Verifiers
, verifiersAt
) where

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Time
import Chainweb.VerifierPlugin
import qualified Chainweb.VerifierPlugin.Allow
import qualified Chainweb.VerifierPlugin.Hyperlane.Announcement
import qualified Chainweb.VerifierPlugin.Hyperlane.Message

import Control.Lens
import Data.Foldable
import qualified Data.HashSet as HS
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

import GHC.Stack

import Numeric.Natural

import System.Random

-- internal modules

import Chainweb.Graph
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version
import Chainweb.Version.Mainnet

import Pact.Core.Names (VerifierName(..))

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
-- 0 == minimum $ M.keys chainGraphs
-- @
--
chainGraphs :: HasVersion => M.Map BlockHeight ChainGraph
chainGraphs
    | _versionCode implicitVersion == _versionCode mainnet = mainnetGraphs
    | otherwise = M.fromDistinctDescList . toList . ruleElems $ _versionGraphs implicitVersion
    where
    mainnetGraphs = M.fromDistinctDescList . toList . ruleElems $ _versionGraphs mainnet

-- | BlockHeight intervals for the chain graphs of a chainweb version up to a
-- given block height.
--
-- Post-condition:
--
-- @
-- 0 == minimum $ M.keys chainGraphs
-- @
--
chainGraphsAt
    :: HasVersion
    => BlockHeight
    -> M.Map BlockHeight ChainGraph
chainGraphsAt h = limitHeight h chainGraphs
{-# INLINE chainGraphsAt #-}

lastGraphChange
    :: (HasCallStack, HasVersion)
    => BlockHeight
    -> BlockHeight
lastGraphChange h = fst . fromJuste . M.lookupLE h $ chainGraphs
{-# INLINE lastGraphChange #-}

nextGraphChange
    :: (HasCallStack, HasVersion)
    => BlockHeight
    -> BlockHeight
nextGraphChange h = fst . fromJuste . M.lookupGT h $ chainGraphs
{-# INLINE nextGraphChange #-}

chainCountAt
    :: (HasCallStack, HasVersion)
    => BlockHeight
    -> Natural
chainCountAt h = order $ atHeight h $ chainGraphs
{-# INLINE chainCountAt #-}

degreeAt
    :: (HasCallStack, HasVersion)
    => BlockHeight
    -> Natural
degreeAt h = degree $ atHeight h $ chainGraphs
{-# INLINE degreeAt #-}

diameterAt
    :: (HasCallStack, HasVersion)
    => BlockHeight
    -> Natural
diameterAt h = diameter $ atHeight h $ chainGraphs
{-# INLINE diameterAt #-}

chainIdsAt
    :: (HasCallStack, HasVersion)
    => BlockHeight
    -> HS.HashSet ChainId
chainIdsAt h = graphChainIds $ atHeight h $ chainGraphs
{-# INLINE chainIdsAt #-}

-- | Uniformily get a random ChainId at the top of the current chainweb
--
randomChainId :: HasVersion => IO ChainId
randomChainId = randomChainIdAt maxBound
{-# INLINE randomChainId #-}

-- | Uniformily get a random ChainId at the given height of the chainweb
--
randomChainIdAt :: HasVersion => BlockHeight -> IO ChainId
randomChainIdAt h = (!!) (toList cs) <$> randomRIO (0, length cs - 1)
  where
    cs = chainIdsAt h
{-# INLINE randomChainIdAt #-}

-- | Sometimes, in particular for testing and examples, some fixed chain id is
-- needed, but it doesn't matter which one. This function provides some valid
-- chain ids for the top of the current chainweb.
--
someChainId :: HasVersion => ChainId
someChainId = someChainIdAt maxBound
{-# INLINE someChainId #-}

-- | Sometimes, in particular for testing and examples, some fixed chain id is
-- needed, but it doesn't matter which one. This function provides some valid
-- chain ids for the chainweb at the given height.
--
someChainIdAt :: HasVersion => BlockHeight -> ChainId
someChainIdAt h = minimum $ chainIdsAt h
    -- guaranteed to succeed because the empty graph isn't a valid chain graph.
{-# INLINE someChainIdAt #-}

isGraphChange :: HasVersion => BlockHeight -> Bool
isGraphChange h = M.member h chainGraphs
{-# INLINE isGraphChange #-}

-- -------------------------------------------------------------------------- --
-- Block Count

-- | The numbers of blocks on a chain up to the given height
--
-- Precondition: h > genesisHeight
--
blockCountAt
    :: (HasCallStack, HasVersion)
    => HasChainId cid
    => cid
    -> BlockHeight
    -> Natural
blockCountAt cid h
    | h < gh = 0
    | otherwise = 1 + int h - int gh
  where
    gh = genesisBlockHeight (_chainId cid)

-- | The block count accross all chains at a given block height
--
globalBlockCountAt
    :: (HasCallStack, HasVersion)
    => BlockHeight
    -> Natural
globalBlockCountAt h = sum
    $ fmap (\c -> blockCountAt c h)
    $ toList
    $ chainIdsAt h

globalBlockDelayAt
    :: (HasCallStack, HasVersion)
    => BlockHeight
    -> Double
globalBlockDelayAt h = (int r / 1_000_000) / int (chainCountAt h)
  where
    BlockDelay r = _versionBlockDelay implicitVersion

-- -------------------------------------------------------------------------- --
-- Cut Heights

-- | The the average over all possible cuts for which at least one chain has the
-- given block height.
--
-- Note, that the result isn't accurate for block heights around a chain graph
-- change.
--
avgCutHeightAt :: HasVersion => BlockHeight -> CutHeight
avgCutHeightAt h = int $ int h * chainCountAt h
{-# INLINE avgCutHeightAt #-}

-- | Cut height intervals for the chain graphs of a chainweb version
--
-- Post-condition:
--
-- @
-- 0 == minimum $ M.keys cutHeights
-- @
--
chainGraphsByCutHeight
    :: HasVersion
    => M.Map CutHeight ChainGraph
chainGraphsByCutHeight = M.fromList
    . fmap (\(h,g) -> (int h * int (order g), g))
    . M.toAscList
    $ chainGraphs
{-# INLINE chainGraphsByCutHeight #-}

-- | The chain graph at the given cut height
--
-- Note, that the result isn't accurate during a chain graph change
--
chainGraphAtCutHeight :: HasVersion => CutHeight -> ChainGraph
chainGraphAtCutHeight h = atCutHeight h $ chainGraphsByCutHeight

-- | The number of chains that exist at the given cut height
--
-- Note, that the result isn't accurate during a chain graph change
--
chainCountAtCutHeight :: HasVersion => CutHeight -> Natural
chainCountAtCutHeight = order . chainGraphAtCutHeight

-- | The diameter of the chain graph at the given cut height
--
-- Note, that the result isn't accurate during a chain graph change
--
diameterAtCutHeight :: HasVersion => CutHeight -> Natural
diameterAtCutHeight = diameter . chainGraphAtCutHeight

-- | The degree of the chain graph at the given cut height
--
-- Note, that the result isn't accurate during a chain graph change
--
degreeAtCutHeight :: HasVersion => CutHeight -> Natural
degreeAtCutHeight = degree . chainGraphAtCutHeight

-- | The average chain height at a given cut height.
--
-- Note, that the result isn't accurate for block heights around a chain graph
-- change.
--
avgBlockHeightAtCutHeight :: HasVersion => CutHeight -> Double
avgBlockHeightAtCutHeight h = int h / int (chainCountAtCutHeight h)

-- | The global number of blocks that exist at the given cut height.
--
-- Note, that the result isn't accurate for block heights around a chain graph
-- change.
--
blockCountAtCutHeight
    :: (HasCallStack, HasVersion)
    => CutHeight
    -> Natural
blockCountAtCutHeight h
    = globalBlockCountAt (int k `div` int (order g)) + int (h - k)
  where
   (k, g) = fromJuste $ M.lookupLE h $ chainGraphsByCutHeight

lastGraphChangeByCutHeight
    :: (HasCallStack, HasVersion)
    => CutHeight
    -> CutHeight
lastGraphChangeByCutHeight h
    = fst $ fromJuste $ M.lookupLE h $ chainGraphsByCutHeight

nextGraphChangeByCutHeight
    :: (HasCallStack, HasVersion)
    => CutHeight
    -> CutHeight
nextGraphChangeByCutHeight h
    = fst $ fromJuste $ M.lookupGT h $ chainGraphsByCutHeight

-- -------------------------------------------------------------------------- --
-- Expected Block Count, Block Heights, and Cut Heights

-- | This function is useful for performance testing when calculating the
-- expected number of mined blocks during a test on a given chain.
--
expectedBlockCountAfterSeconds
    :: (HasCallStack, HasVersion)
    => HasChainId cid
    => cid
    -> Seconds
    -> Double
expectedBlockCountAfterSeconds cid s = max 0 (1 + (int s / (int r / 1_000_000)) - int gh)
    -- The `max 0` term is required for chains that were added during graph transitions
    -- and thus have `genesisHeight > 0`
  where
    BlockDelay r = _versionBlockDelay implicitVersion
    gh = genesisBlockHeight (_chainId cid)

-- | This function is useful for performance testing when calculating the
-- expected number of mined blocks during a test accross all chains.
--
-- The sum of count for all chains is multiplied by 0.4 to compensate for the
-- fact that chains are blocked about 60% of the time on small graphs when used with
-- chainweb versions with fixed expected solve times and no difficulty adjustment.
--
expectedGlobalBlockCountAfterSeconds
    :: (HasCallStack, HasVersion)
    => Seconds
    -> Double
expectedGlobalBlockCountAfterSeconds s = (* 0.4)
    $ sum
    $ fmap (\c -> expectedBlockCountAfterSeconds c s)
    $ toList
    $ chainIdsAt (round eh)
  where
    eh = expectedBlockHeightAfterSeconds s

-- | The expected BlockHeight after the given number of seconds has passed.
--
-- This function is useful for performance testing.
--
expectedBlockHeightAfterSeconds
    :: (HasCallStack, HasVersion)
    => Seconds
    -> Double
expectedBlockHeightAfterSeconds s = int s / (int r / 1_000_000)
  where
    BlockDelay r = _versionBlockDelay implicitVersion

-- | The expected CutHeight after the given number of seconds has passed.
--
-- This function is useful for performance testing.
--
expectedCutHeightAfterSeconds
    :: (HasCallStack, HasVersion)
    => Seconds
    -> Double
expectedCutHeightAfterSeconds s = eh * int (chainCountAt (round eh))
  where
    eh = expectedBlockHeightAfterSeconds s

-- | The verifier plugins enabled for a particular block.
verifiersAt :: HasVersion => ChainId -> BlockHeight -> Map VerifierName VerifierPlugin
verifiersAt cid bh =
    M.restrictKeys allVerifierPlugins activeVerifierNames
    where
    activeVerifierNames
        = snd
        $ ruleZipperHere
        $ snd
        $ ruleSeek (\h _ -> bh >= h)
        $ _versionVerifierPluginNames implicitVersion ^?! atChain cid

-- the mappings from names to verifier plugins is global. the list of verifier
-- plugins active in any particular block validation context is the only thing
-- that varies. this pedantry is only so that ChainwebVersion is plain data
-- with no functions inside.
allVerifierPlugins :: Map VerifierName VerifierPlugin
allVerifierPlugins = M.fromList $ map (over _1 VerifierName)
    [ ("allow", Chainweb.VerifierPlugin.Allow.plugin)

    , ("hyperlane_v3_announcement", Chainweb.VerifierPlugin.Hyperlane.Announcement.plugin)
    , ("hyperlane_v3_message", Chainweb.VerifierPlugin.Hyperlane.Message.plugin)
    ]
