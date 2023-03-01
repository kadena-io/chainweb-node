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

module Chainweb.Version.Guards 
    -- ** Payload Validation Guards
    ( vuln797Fix
    -- , coinV2Upgrade
    -- , to20ChainRebalance
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
    , pact44NewTrans
    , pactParserVersion

    -- ** BlockHeader Validation Guards
    , slowEpochGuard
    , oldTargetGuard
    , skipFeatureFlagValidationGuard
    , oldDaGuard
  ) where

import Control.Lens

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Transaction
import Chainweb.Version

getForkHeight :: Fork -> ChainwebVersion -> ChainId -> BlockHeight
getForkHeight fork v cid = v ^?! versionForks . at fork . _Just . onChain cid

checkFork
    :: (BlockHeight -> BlockHeight -> Bool)
    -> Fork -> ChainwebVersion -> ChainId -> BlockHeight -> Bool
checkFork p f v cid h = p h (getForkHeight f v cid)

vuln797Fix :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
vuln797Fix = checkFork (>=) Vuln797Fix
pactBackCompat_v16 :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
pactBackCompat_v16 = checkFork (<) PactBackCompat_v16
skipTxTimingValidation :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
skipTxTimingValidation = checkFork (<) SkipTxTimingValidation
enableModuleNameFix :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
enableModuleNameFix = checkFork (>=) ModuleNameFix
enableModuleNameFix2 :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
enableModuleNameFix2 = checkFork (>=) ModuleNameFix2
enablePactEvents :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
enablePactEvents = checkFork (>=) PactEvents
enableSPVBridge :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
enableSPVBridge = checkFork (>=) SPVBridge
enforceKeysetFormats :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
enforceKeysetFormats = checkFork (>=) EnforceKeysetFormats
doCheckTxHash :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
doCheckTxHash = checkFork (>=) CheckTxHash
pact44NewTrans :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
pact44NewTrans = checkFork (>=) Pact44NewTrans

slowEpochGuard
    :: ChainwebVersion
    -> ChainId
    -> BlockHeight
        -- ^ BlockHeight of parent Header
    -> Bool
slowEpochGuard = checkFork (<) SlowEpoch

oldTargetGuard :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
oldTargetGuard = checkFork (<) OldTargetGuard
skipFeatureFlagValidationGuard :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
skipFeatureFlagValidationGuard = checkFork (<) SkipFeatureFlagValidation
oldDaGuard :: ChainwebVersion -> ChainId -> BlockHeight -> Bool
oldDaGuard = checkFork (<) OldDAGuard
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

pactParserVersion :: ChainwebVersion -> ChainId -> BlockHeight -> PactParserVersion
pactParserVersion v cid bh
    | chainweb213Pact v cid bh = PactParserChainweb213
    | otherwise = PactParserGenesis
