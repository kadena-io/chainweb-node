{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
    , pact42
    , enforceKeysetFormats
    , doCheckTxHash
    , chainweb213Pact
    , chainweb214Pact
    , chainweb215Pact
    , chainweb216Pact
    , chainweb217Pact
    , chainweb218Pact
    , chainweb219Pact
    , chainweb220Pact
    , chainweb221Pact
    , chainweb222Pact
    , chainweb223Pact
    , chainweb224Pact
    , chainweb225Pact
    , chainweb226Pact
    , chainweb228Pact
    , chainweb229Pact
    , chainweb230Pact
    , pact5
    , pact44NewTrans
    , maxBlockGasLimit
    , validPPKSchemes
    , validKeyFormats
    , pact5Serialiser

    -- ** BlockHeader Validation Guards
    , slowEpochGuard
    , oldTargetGuard
    , skipFeatureFlagValidationGuard
    , oldDaGuard
    , hashedAdjacentRecord
    ) where

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Utils.Rule
import Chainweb.Version
import Control.Lens
import Numeric.Natural
import Pact.Core.Builtin qualified as Pact
import Pact.Core.Info qualified as Pact
import Pact.Core.Serialise qualified as Pact
import Pact.Types.KeySet (PublicKeyText, ed25519HexFormat, webAuthnFormat)
import Pact.Types.Scheme (PPKScheme(ED25519, WebAuthn))

getForkHeight :: HasVersion => Fork -> ChainId -> ForkHeight
getForkHeight fork cid = implicitVersion ^?! versionForks . at fork . _Just . atChain cid

checkFork
    :: HasVersion
    => (BlockHeight -> ForkHeight -> Bool)
    -> Fork -> ChainId -> BlockHeight -> Bool
checkFork p f cid h = p h (getForkHeight f cid)

after :: BlockHeight -> ForkHeight -> Bool
after bh (ForkAtBlockHeight bh') = bh > bh'
after _ ForkAtGenesis = True
after _ ForkNever = False

atOrAfter :: BlockHeight -> ForkHeight -> Bool
atOrAfter bh (ForkAtBlockHeight bh') = bh >= bh'
atOrAfter _ ForkAtGenesis = True
atOrAfter _ ForkNever = False

before :: BlockHeight -> ForkHeight -> Bool
before bh (ForkAtBlockHeight bh') = bh < bh'
before _ ForkAtGenesis = False
before _ ForkNever = True

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
    :: HasVersion
    => ChainId
    -> BlockHeight
        -- ^ BlockHeight of parent Header
    -> Bool
slowEpochGuard = checkFork before SlowEpoch

-- | Use the current block time for computing epoch start date and
-- target.
--
-- When this guard is switched off, there will be a single epoch of just 119
-- blocks. The target computation won't compensate for that, since the effects
-- are marginal.
--
oldTargetGuard :: HasVersion => ChainId -> BlockHeight -> Bool
oldTargetGuard = checkFork before OldTargetGuard

-- | Skip validation of feature flags.
--
-- Unused feature flag bits are supposed to be set to 0. As of Chainweb 1.7, the
-- Feature Flag bytes and Nonce bytes have switched places in `BlockHeader`. For
-- live chains, enforcing the following condition must be ignored for the
-- historical blocks for which both the Nonce and Flags could be anything.
--
skipFeatureFlagValidationGuard :: HasVersion => ChainId -> BlockHeight -> Bool
skipFeatureFlagValidationGuard = checkFork before SkipFeatureFlagValidation

oldDaGuard :: HasVersion => ChainId -> BlockHeight -> Bool
oldDaGuard = checkFork before OldDAGuard

-----------------
-- Payload validation guards

vuln797Fix :: HasVersion => ChainId -> BlockHeight -> Bool
vuln797Fix = checkFork atOrAfter Vuln797Fix

-- | Preserve Pact bugs pre-1.6 chainweb.
pactBackCompat_v16 :: HasVersion => ChainId -> BlockHeight -> Bool
pactBackCompat_v16 = checkFork before PactBackCompat_v16

-- | Early versions of chainweb used the creation time of the current header
-- for validation of pact tx creation time and TTL. Nowadays the time of
-- the parent header is used.
--
-- When this guard is enabled timing validation is skipped.
--
skipTxTimingValidation :: HasVersion => ChainId -> BlockHeight -> Bool
skipTxTimingValidation = checkFork before SkipTxTimingValidation

-- | Checks height after which module name fix in effect.
--
enableModuleNameFix :: HasVersion => ChainId -> BlockHeight -> Bool
enableModuleNameFix = checkFork atOrAfter ModuleNameFix

-- | Related, later fix (Pact #801).
--
enableModuleNameFix2 :: HasVersion => ChainId -> BlockHeight -> Bool
enableModuleNameFix2 = checkFork atOrAfter ModuleNameFix2

-- | Turn on pact events in command output.
enablePactEvents :: HasVersion => ChainId -> BlockHeight -> Bool
enablePactEvents = checkFork atOrAfter PactEvents

-- | Bridge support: ETH and event SPV.
enableSPVBridge :: HasVersion => ChainId -> BlockHeight -> Bool
enableSPVBridge = checkFork atOrAfter SPVBridge

enforceKeysetFormats :: HasVersion => ChainId -> BlockHeight -> Bool
enforceKeysetFormats = checkFork atOrAfter EnforceKeysetFormats

doCheckTxHash :: HasVersion => ChainId -> BlockHeight -> Bool
doCheckTxHash = checkFork atOrAfter CheckTxHash

-- | Fork for musl trans funs
pact44NewTrans :: HasVersion => ChainId -> BlockHeight -> Bool
pact44NewTrans = checkFork atOrAfter Pact44NewTrans

pact4Coin3 :: HasVersion => ChainId -> BlockHeight -> Bool
pact4Coin3 = checkFork after Pact4Coin3

pact42 :: HasVersion => ChainId -> BlockHeight -> Bool
pact42 = checkFork atOrAfter Pact42

pact5 :: HasVersion => ChainId -> BlockHeight -> Bool
pact5 = checkFork atOrAfter Pact5Fork

chainweb213Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb213Pact = checkFork atOrAfter Chainweb213Pact

chainweb214Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb214Pact = checkFork after Chainweb214Pact

chainweb215Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb215Pact = checkFork after Chainweb215Pact

chainweb216Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb216Pact = checkFork after Chainweb216Pact

chainweb217Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb217Pact = checkFork after Chainweb217Pact

chainweb218Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb218Pact = checkFork atOrAfter Chainweb218Pact

chainweb219Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb219Pact = checkFork atOrAfter Chainweb219Pact

chainweb220Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb220Pact = checkFork atOrAfter Chainweb220Pact

chainweb221Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb221Pact = checkFork atOrAfter Chainweb221Pact

chainweb222Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb222Pact = checkFork atOrAfter Chainweb222Pact

chainweb223Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb223Pact = checkFork atOrAfter Chainweb223Pact

chainweb224Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb224Pact = checkFork atOrAfter Chainweb224Pact

chainweb225Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb225Pact = checkFork atOrAfter Chainweb225Pact

chainweb226Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb226Pact = checkFork atOrAfter Chainweb226Pact

chainweb228Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb228Pact = checkFork atOrAfter Chainweb228Pact

chainweb229Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb229Pact = checkFork atOrAfter Chainweb229Pact

chainweb230Pact :: HasVersion => ChainId -> BlockHeight -> Bool
chainweb230Pact = checkFork atOrAfter Chainweb230Pact

pact5Serialiser :: HasVersion => ChainId -> BlockHeight -> Pact.PactSerialise Pact.CoreBuiltin Pact.LineInfo
pact5Serialiser cid bh
    | chainweb228Pact cid bh = Pact.serialisePact_lineinfo_pact51
    | otherwise              = Pact.serialisePact_lineinfo_pact50

maxBlockGasLimit :: HasVersion => BlockHeight -> Maybe Natural
maxBlockGasLimit bh = snd $ ruleZipperHere $ snd
    $ ruleSeek (\h _ -> bh >= h) (_versionMaxBlockGasLimit implicitVersion)

hashedAdjacentRecord :: HasVersion => ChainId -> BlockHeight -> Bool
hashedAdjacentRecord = checkFork atOrAfter HashedAdjacentRecord

-- | Different versions of Chainweb allow different PPKSchemes.
--
validPPKSchemes :: HasVersion => ChainId -> BlockHeight -> [PPKScheme]
validPPKSchemes cid bh =
  if chainweb221Pact cid bh
  then [ED25519, WebAuthn]
  else [ED25519]

validKeyFormats :: HasVersion => ChainId -> BlockHeight -> [PublicKeyText -> Bool]
validKeyFormats cid bh =
  if chainweb222Pact cid bh
  then [ed25519HexFormat, webAuthnFormat]
  else [ed25519HexFormat]
