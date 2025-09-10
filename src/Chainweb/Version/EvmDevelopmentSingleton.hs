{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.EvmDevelopmentSingleton
( evmDevnetSingleton
, pattern EvmDevelopmentSingleton
, evmDevnetPair
, pattern EvmDevelopmentPair
) where

import qualified Data.Set as Set

import Chainweb.BlockCreationTime
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version

import Pact.Core.Names

pattern EvmDevelopmentSingleton :: ChainwebVersion
pattern EvmDevelopmentSingleton <- ((== evmDevnetSingleton) -> True) where
    EvmDevelopmentSingleton = evmDevnetSingleton

pattern EvmDevelopmentPair :: ChainwebVersion
pattern EvmDevelopmentPair <- ((== evmDevnetPair) -> True) where
    EvmDevelopmentPair = evmDevnetPair

-- How to compute the hashes:
--
-- Mininal Payload Provider:
--
-- @
-- -- create dummy payload hashes
-- import Chainweb.PayloadProvider.Minimal.Payload
-- import Chainweb.Version.EvmDevelopmentSingleton
--
-- mapM_ (\i -> T.putStrLn (sshow i <> " " <> encodeToText (view payloadHash $ genesisPayload EvmDevelopment $ unsafeChainId i))) [25..97]
-- @
--
-- EVM Payload Provider:
--
-- @
-- cabal run evm-genesis -- evm-development
-- @
--
-- Pact Provider:
--
-- TODO (use ea?)

evmDevnetPair :: ChainwebVersion
evmDevnetPair = withVersion evmDevnetPair $ ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x0000_000e
    , _versionName = ChainwebVersionName "evm-development-pair"
    , _versionForks = tabulateHashMap $ const $ onAllChains ForkAtGenesis
    , _versionUpgrades = onAllChains mempty
    , _versionGraphs = Bottom (minBound, pairChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = []
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = onAllChains $ HashTarget (maxBound `div` 10_000)
        , _genesisTime = onChains
            -- FIXME: is the creation time for the pact headers correct?
            [ (unsafeChainId 0, BlockCreationTime [timeMicrosQQ| 2025-01-01T00:00:00.000000 |])
            , (unsafeChainId 1, BlockCreationTime (Time (secondsToTimeSpan 0x684c5d2a)))
            ]
        , _genesisBlockPayload = onChains $
            -- Pact Payload Provider
            [ (unsafeChainId 0, unsafeFromText "QzxVHFZ5go4PYd3QeAZhxP61hsVnICPw4BB9h-T3PDM")
            -- EVM Payload Provider
            , (unsafeChainId 1, unsafeFromText "E8Z96yfRFs9dHYlml71XhWmZHmWzK5TRAmpXLvPj5rQ")
            ]
        }

    -- still the *default* block gas limit is set, see
    -- defaultChainwebConfiguration._configBlockGasLimit
    , _versionMaxBlockGasLimit = Bottom (minBound, Nothing)
    , _versionCheats = VersionCheats
        { _disablePow = True
        , _fakeFirstEpochStart = True
        , _disablePact = False
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = True
        , _disableMempoolSync = False
        }
    , _versionVerifierPluginNames = onAllChains $ Bottom
        (minBound, Set.fromList $ map VerifierName ["hyperlane_v3_message", "allow"])
    , _versionQuirks = noQuirks
    , _versionServiceDate = Nothing

    -- FIXME make this safe for graph changes
    , _versionPayloadProviderTypes = onChains
        [ (unsafeChainId 0, PactProvider)
        , (unsafeChainId 1, EvmProvider 1789)
        ]
    }

evmDevnetSingleton :: ChainwebVersion
evmDevnetSingleton = withVersion evmDevnetSingleton $ ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x0000_000c
    , _versionName = ChainwebVersionName "evm-development-singleton"
    , _versionForks = tabulateHashMap $ const $ onAllChains ForkAtGenesis
    , _versionUpgrades = onAllChains mempty
    , _versionGraphs = Bottom (minBound, singletonChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = []
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = onAllChains $ HashTarget (maxBound `div` 10_000)
        , _genesisTime = onChains
            [ (unsafeChainId 0, BlockCreationTime (Time (secondsToTimeSpan 0x684c5d2a))) ]
        , _genesisBlockPayload = onChains $
            -- EVM Payload Provider
            [ (unsafeChainId 0, unsafeFromText "ogx6zpVYf2jh1WHwYXFxCt211b1TspNymq0B7p2i3KI") ]
        }

    -- still the *default* block gas limit is set, see
    -- defaultChainwebConfiguration._configBlockGasLimit
    , _versionMaxBlockGasLimit = Bottom (minBound, Nothing)
    , _versionCheats = VersionCheats
        { _disablePow = True
        , _fakeFirstEpochStart = True
        , _disablePact = False
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = True
        , _disableMempoolSync = False
        }
    , _versionVerifierPluginNames = onAllChains $ Bottom
        (minBound, Set.fromList $ map VerifierName ["hyperlane_v3_message", "allow"])
    , _versionQuirks = noQuirks
    , _versionServiceDate = Nothing

    -- FIXME make this safe for graph changes
    , _versionPayloadProviderTypes = onChains
        [ (unsafeChainId 0, EvmProvider 1789) ]
    }
