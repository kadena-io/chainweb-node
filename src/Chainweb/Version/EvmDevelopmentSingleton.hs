{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.EvmDevelopmentSingleton
( evmDevnetSingleton
, pattern EvmDevelopmentSingleton
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

evmDevnetSingleton :: ChainwebVersion
evmDevnetSingleton = withVersion evmDevnetSingleton $ ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x0000_000b
    , _versionName = ChainwebVersionName "evm-development-singleton"
    , _versionForks = tabulateHashMap $ const $ onAllChains ForkAtGenesis
    , _versionUpgrades = onAllChains mempty
    , _versionGraphs = Bottom (minBound, pairChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = []
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = onAllChains $ HashTarget (maxBound `div` 100_000)
        , _genesisTime = onChains
            -- FIXME: is the creation time for the pact headers correct?
            $ [ (unsafeChainId i, BlockCreationTime [timeMicrosQQ| 2025-01-01T00:00:00.000000 |]) | i <- [0..19] ]
            <> [ (unsafeChainId i, BlockCreationTime (Time (secondsToTimeSpan 1687223762))) | i <- [20..24] ]
            <> [ (unsafeChainId i, BlockCreationTime [timeMicrosQQ| 2025-01-01T00:00:00.000000 |]) | i <- [25..97] ]
        , _genesisBlockPayload = onChains $
            -- Pact Payload Provider
            [ (unsafeChainId 0, unsafeFromText "QzxVHFZ5go4PYd3QeAZhxP61hsVnICPw4BB9h-T3PDM")
            -- EVM Payload Provider
            , (unsafeChainId 1, unsafeFromText "FAxLDjtb8r_0S0Rfr8rD47EQwO-Ma-fmEynZccHvn5o")
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
        $ [ (unsafeChainId i, PactProvider) | i <- [0] ]
        <> [ (unsafeChainId i, EvmProvider (1789 - 20 + int i)) | i <- [1] ]
    }
