{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.RecapDevelopment(recapDevnet, pattern RecapDevelopment) where

import qualified Data.Set as Set

import Chainweb.BlockCreationTime
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version

import Pact.Core.Names

to20ChainsHeight :: BlockHeight
to20ChainsHeight = 60

pattern RecapDevelopment :: ChainwebVersion
pattern RecapDevelopment <- ((== recapDevnet) -> True) where
    RecapDevelopment = recapDevnet

recapDevnet :: ChainwebVersion
recapDevnet = withVersion recapDevnet ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x0000_0001
    , _versionName = ChainwebVersionName "recap-development"

    , _versionForks = tabulateHashMap $ \case
        SlowEpoch -> onAllChains ForkAtGenesis
        Vuln797Fix -> onAllChains ForkAtGenesis
        CoinV2 -> onAllChains ForkAtGenesis
        PactBackCompat_v16 -> onAllChains ForkAtGenesis
        SkipTxTimingValidation -> onAllChains ForkAtGenesis
        OldTargetGuard -> onAllChains ForkAtGenesis
        SkipFeatureFlagValidation -> onAllChains ForkAtGenesis
        ModuleNameFix -> onAllChains ForkAtGenesis
        ModuleNameFix2 -> onAllChains ForkAtGenesis
        OldDAGuard -> onAllChains ForkAtGenesis
        PactEvents -> onAllChains ForkAtGenesis
        SPVBridge -> onAllChains ForkAtGenesis
        Pact4Coin3 -> onAllChains ForkAtGenesis
        EnforceKeysetFormats -> onAllChains ForkAtGenesis
        Pact42 -> onAllChains ForkAtGenesis
        CheckTxHash -> onAllChains ForkAtGenesis
        Chainweb213Pact -> onAllChains ForkAtGenesis
        Chainweb214Pact -> onAllChains ForkAtGenesis
        Chainweb215Pact -> onAllChains ForkAtGenesis
        Pact44NewTrans -> onAllChains ForkAtGenesis
        Chainweb216Pact -> onAllChains ForkAtGenesis
        Chainweb217Pact -> onAllChains ForkAtGenesis
        Chainweb218Pact -> onAllChains ForkAtGenesis
        Chainweb219Pact -> onAllChains ForkAtGenesis
        Chainweb220Pact -> onAllChains ForkAtGenesis
        Chainweb221Pact -> onAllChains ForkAtGenesis
        Chainweb222Pact -> onAllChains ForkAtGenesis
        Chainweb223Pact -> onAllChains ForkAtGenesis
        Chainweb224Pact -> onAllChains ForkAtGenesis
        Chainweb225Pact -> onAllChains ForkAtGenesis
        Chainweb226Pact -> onAllChains ForkAtGenesis
        Pact5Fork -> onAllChains $ ForkAtGenesis
        Chainweb228Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 10
        Chainweb229Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 20
        Chainweb230Pact -> onAllChains $ ForkAtBlockHeight $ BlockHeight 30
        HashedAdjacentRecord -> onAllChains $ ForkAtBlockHeight $ BlockHeight 40
    , _versionUpgrades = onChains []

    , _versionGraphs =
        (to20ChainsHeight, twentyChainGraph) `Above`
        Bottom (minBound, petersenChainGraph)

    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = []
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = onChains $ concat
            [ [(unsafeChainId i, HashTarget $ maxBound `div` 100_000) | i <- [0..9]]
            , [(unsafeChainId i, HashTarget 0x0000088f99632cadf39b0db7655be62cb7dbc84ebbd9a90e5b5756d3e7d9196c) | i <- [10..19]]
            ]
        , _genesisTime = onAllChains $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
        , _genesisBlockPayload = onChains
            [ (unsafeChainId 0, unsafeFromText "5TWTF5R6Vc85vWHqcklTY91ljkV6mJ1wYfDJShooTCw")
            , (unsafeChainId 1, unsafeFromText "yZ6Syxl34TTrGGKxVHInV0S29BH8v-C8VZTbJr2eK2k")
            , (unsafeChainId 2, unsafeFromText "yZ6Syxl34TTrGGKxVHInV0S29BH8v-C8VZTbJr2eK2k")
            , (unsafeChainId 3, unsafeFromText "yZ6Syxl34TTrGGKxVHInV0S29BH8v-C8VZTbJr2eK2k")
            , (unsafeChainId 4, unsafeFromText "yZ6Syxl34TTrGGKxVHInV0S29BH8v-C8VZTbJr2eK2k")
            , (unsafeChainId 5, unsafeFromText "yZ6Syxl34TTrGGKxVHInV0S29BH8v-C8VZTbJr2eK2k")
            , (unsafeChainId 6, unsafeFromText "yZ6Syxl34TTrGGKxVHInV0S29BH8v-C8VZTbJr2eK2k")
            , (unsafeChainId 7, unsafeFromText "yZ6Syxl34TTrGGKxVHInV0S29BH8v-C8VZTbJr2eK2k")
            , (unsafeChainId 8, unsafeFromText "yZ6Syxl34TTrGGKxVHInV0S29BH8v-C8VZTbJr2eK2k")
            , (unsafeChainId 9, unsafeFromText "yZ6Syxl34TTrGGKxVHInV0S29BH8v-C8VZTbJr2eK2k")
            , (unsafeChainId 10, unsafeFromText "OXFBWONFKY7fTY4MH3GaOOELd_cPCMn9GpIOPBYMsvM")
            , (unsafeChainId 11, unsafeFromText "OXFBWONFKY7fTY4MH3GaOOELd_cPCMn9GpIOPBYMsvM")
            , (unsafeChainId 12, unsafeFromText "OXFBWONFKY7fTY4MH3GaOOELd_cPCMn9GpIOPBYMsvM")
            , (unsafeChainId 13, unsafeFromText "OXFBWONFKY7fTY4MH3GaOOELd_cPCMn9GpIOPBYMsvM")
            , (unsafeChainId 14, unsafeFromText "OXFBWONFKY7fTY4MH3GaOOELd_cPCMn9GpIOPBYMsvM")
            , (unsafeChainId 15, unsafeFromText "OXFBWONFKY7fTY4MH3GaOOELd_cPCMn9GpIOPBYMsvM")
            , (unsafeChainId 16, unsafeFromText "OXFBWONFKY7fTY4MH3GaOOELd_cPCMn9GpIOPBYMsvM")
            , (unsafeChainId 17, unsafeFromText "OXFBWONFKY7fTY4MH3GaOOELd_cPCMn9GpIOPBYMsvM")
            , (unsafeChainId 18, unsafeFromText "OXFBWONFKY7fTY4MH3GaOOELd_cPCMn9GpIOPBYMsvM")
            , (unsafeChainId 19, unsafeFromText "OXFBWONFKY7fTY4MH3GaOOELd_cPCMn9GpIOPBYMsvM")
            ]
        }

    , _versionMaxBlockGasLimit = Bottom (minBound, Just 180_000)
    , _versionCheats = VersionCheats
        { _disablePow = False
        , _fakeFirstEpochStart = True
        , _disablePact = False
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = True
        , _disableMempoolSync = False
        }
    , _versionVerifierPluginNames = onAllChains $
        (600, Set.fromList $ map VerifierName ["hyperlane_v3_message", "allow"]) `Above`
        Bottom (minBound, mempty)
    , _versionQuirks = noQuirks
    , _versionServiceDate = Nothing
    , _versionPayloadProviderTypes = onAllChains PactProvider
    }
