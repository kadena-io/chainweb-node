{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Version.Guards
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Edmund Noble <edmund@kadena.io>
-- Stability: experimental
--
-- Functions which dictate changes in block validation at different BlockHeights, based on
-- chainweb versions.
--
-- Changes either activate at a certain block height and for all subsequent blocks,
-- activate for all subsequent blocks after a certain block height,
-- or activate for all previous blocks before a certain block height.
--

module Chainweb.Version.Guards
    (
    -- ** Payload Validation Guards
    vuln797Fix
    , pactBackCompat_v16
    , skipTxTimingValidation
    , enableModuleNameFix
    , enableModuleNameFix2
    , enablePactEvents
    , enableSPVBridge
    , pact4Coin3
    , pact420
    , enforceKeysetFormats
    , doCheckTxHash
    , chainweb213Pact
    , chainweb214Pact
    , chainweb215Pact
    , chainweb216Pact
    , chainweb217Pact
    , cleanModuleCache
    , chainweb218Pact
    , pact44NewTrans
    , pactParserVersion
    , maxBlockGasLimit

    -- ** BlockHeader Validation Guards
    , slowEpochGuard
    , oldTargetGuard
    , skipFeatureFlagValidationGuard
    , oldDaGuard
  ) where

import Control.Lens
import Numeric.Natural

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Transaction
import Chainweb.Version
import Chainweb.Utils.Rule

getForkHeight :: Fork -> ChainwebVersion -> ChainId -> BlockHeight
getForkHeight fork v cid = v ^?! versionForks . at fork . _Just . onChain cid

checkFork
    :: (BlockHeight -> BlockHeight -> Bool)
    -> Fork -> ChainwebVersion -> ChainId -> BlockHeight -> Bool
checkFork p f v cid h = p h (getForkHeight f v cid)

-- -------------------------------------------------------------------------- --
-- Header Validation Guards
--
-- The guards in this section encode when changes to validation rules for data
-- on the chain become effective.
--
-- Only the following types are allowed as parameters for guards
--
-- * BlockHeader,
-- * ParentHeader,
-- * BlockCreationTime, and
-- * ParentCreationTime
--
-- The result is a simple 'Bool'.
--
-- Guards should have meaningful names and should be used in a way that all
-- places in the code base that depend on the guard should reference the
-- respective guard. That way all dependent code can be easily identified using
-- ide tools, like for instance @grep@.
--
-- Each guard should have a description that provides background for the change
-- and provides all information needed for maintaining the code or code that
-- depends on it.
--

-- | Turn off slow epochs (emergency DA) for blocks.
--
-- Emergency DA is considered a misfeature.
--
-- It's intended purpose is to prevent chain hopping attacks, where an attacker
-- temporarily adds a large amount of hash power, thus increasing the
-- difficulty. When the hash power is removed, the remaining hash power may not
-- be enough to reach the next block in reasonable time.
--
-- In practice, emergency DAs cause more problems than they solve. In
-- particular, they increase the chance of deep forks. Also they make the
-- behavior of the system unpredictable in states of emergency, when stability
-- is usually more important than throughput.
--
slowEpochGuard
    :: ChainwebVersion
    -> ChainId
    -> BlockHeight
        -- ^ BlockHeight of parent Header
    -> Bool
slowEpochGuard = checkFork (<) SlowEpoch

-- | Use the current block time for computing epoch start date and
-- target.
--
-- When this guard is switched off, there will be a single epoch of just 119
-- blocks. The target computation won't compensate for that, since the effects
-- are marginal.
--
oldTargetGuard :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
oldTargetGuard = checkFork (<) OldTargetGuard

-- | Skip validation of feature flags.
--
-- Unused feature flag bits are supposed to be set to 0. As of Chainweb 1.7, the
-- Feature Flag bytes and Nonce bytes have switched places in `BlockHeader`. For
-- live chains, enforcing the following condition must be ignored for the
-- historical blocks for which both the Nonce and Flags could be anything.
--
skipFeatureFlagValidationGuard :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
skipFeatureFlagValidationGuard = checkFork (<) SkipFeatureFlagValidation

oldDaGuard :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
oldDaGuard = checkFork (<) OldDAGuard

-----------------
-- Payload validation guards

vuln797Fix :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
vuln797Fix = checkFork (>=) Vuln797Fix

-- | Preserve Pact bugs pre-1.6 chainweb.
pactBackCompat_v16 :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
pactBackCompat_v16 = checkFork (<) PactBackCompat_v16

-- | Early versions of chainweb used the creation time of the current header
-- for validation of pact tx creation time and TTL. Nowadays the times of
-- the parent header a used.
--
-- When this guard is enabled timing validation is skipped.
--
skipTxTimingValidation :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
skipTxTimingValidation = checkFork (<) SkipTxTimingValidation

-- | Checks height after which module name fix in effect.
--
enableModuleNameFix :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
enableModuleNameFix = checkFork (>=) ModuleNameFix

-- | Related, later fix (Pact #801).
--
enableModuleNameFix2 :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
enableModuleNameFix2 = checkFork (>=) ModuleNameFix2

-- | Turn on pact events in command output.
enablePactEvents :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
enablePactEvents = checkFork (>=) PactEvents

-- | Bridge support: ETH and event SPV.
enableSPVBridge :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
enableSPVBridge = checkFork (>=) SPVBridge

enforceKeysetFormats :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
enforceKeysetFormats = checkFork (>=) EnforceKeysetFormats

doCheckTxHash :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
doCheckTxHash = checkFork (>=) CheckTxHash

-- | Fork for musl trans funs
pact44NewTrans :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
pact44NewTrans = checkFork (>=) Pact44NewTrans

pact4Coin3 :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
pact4Coin3 = checkFork (>) Pact4Coin3

pact420 :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
pact420 = checkFork (>=) Pact420

chainweb213Pact :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
chainweb213Pact = checkFork (>=) Chainweb213Pact

chainweb214Pact :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
chainweb214Pact = checkFork (>) Chainweb214Pact

chainweb215Pact :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
chainweb215Pact = checkFork (>) Chainweb215Pact

chainweb216Pact :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
chainweb216Pact = checkFork (>) Chainweb216Pact

chainweb217Pact :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
chainweb217Pact = checkFork (>) Chainweb217Pact

cleanModuleCache :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
cleanModuleCache = checkFork (==) Chainweb217Pact

chainweb218Pact :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
chainweb218Pact = checkFork (>=) Chainweb218Pact

pactParserVersion :: ChainwebVersion -> ChainId -> BlockHeight -> PactParserVersion
pactParserVersion v cid bh
    | chainweb213Pact v cid bh = PactParserChainweb213
    | otherwise = PactParserGenesis

maxBlockGasLimit :: ChainwebVersion -> BlockHeight -> Maybe Natural
maxBlockGasLimit v bh = case measureRule bh $ _versionMaxBlockGasLimit v of
    Bottom limit -> limit
    Top (_, limit) -> limit
    Between (_, limit) _ -> limit
