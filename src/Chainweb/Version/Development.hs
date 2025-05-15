{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.Development(devnet, pattern Development) where

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

pattern Development :: ChainwebVersion
pattern Development <- ((== devnet) -> True) where
    Development = devnet

devnet :: ChainwebVersion
devnet = withVersion devnet $ ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x00000002
    , _versionName = ChainwebVersionName "development"
    , _versionForks = tabulateHashMap $
        \_ -> onAllChains ForkAtGenesis
    , _versionUpgrades = onAllChains mempty
    , _versionGraphs = Bottom (minBound, twentyChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = []
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = onAllChains $ HashTarget (maxBound `div` 100_000)
        , _genesisTime = onAllChains $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
        , _genesisBlockPayload = onChains
            [ (unsafeChainId 0, unsafeFromText "QzxVHFZ5go4PYd3QeAZhxP61hsVnICPw4BB9h-T3PDM")
            , (unsafeChainId 1, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 2, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 3, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 4, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 5, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 6, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 7, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 8, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 9, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 10, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 11, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 12, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 13, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 14, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 15, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 16, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 17, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 18, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            , (unsafeChainId 19, unsafeFromText "66JSEmDIl6AqWTKN29LprukaeUmK0OOd4RufVO8e6-4")
            ]
        }

    -- still the *default* block gas limit is set, see
    -- defaultChainwebConfiguration._configBlockGasLimit
    , _versionMaxBlockGasLimit = Bottom (minBound, Nothing)
    , _versionCheats = VersionCheats
        { _disablePow = True
        , _fakeFirstEpochStart = True
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = True
        , _disableMempoolSync = False
        }
    , _versionVerifierPluginNames = onAllChains $ Bottom
        (minBound, Set.fromList $ map VerifierName ["hyperlane_v3_message", "allow"])
    , _versionQuirks = noQuirks
    , _versionServiceDate = Nothing
    , _versionPayloadProviderTypes = onAllChains PactProvider
    }
