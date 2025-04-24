{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.Testnet05(testnet05, pattern Testnet05) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

import Chainweb.BlockCreationTime
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version
import P2P.BootstrapNodes

import Pact.Core.Names

pattern Testnet05 :: ChainwebVersion
pattern Testnet05 <- ((== testnet05) -> True) where
    Testnet05 = testnet05

testnet05 :: ChainwebVersion
testnet05 = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x00000009
    , _versionName = ChainwebVersionName "testnet05"
    , _versionForks = tabulateHashMap $ \case
        SlowEpoch -> onAllChains testnet05 ForkAtGenesis
        Vuln797Fix -> onAllChains testnet05 ForkAtGenesis
        CoinV2 -> onAllChains testnet05 ForkAtGenesis
        PactBackCompat_v16 -> onAllChains testnet05 ForkAtGenesis
        ModuleNameFix -> onAllChains testnet05 ForkAtGenesis
        SkipTxTimingValidation -> onAllChains testnet05 ForkAtGenesis
        OldTargetGuard -> onAllChains testnet05 ForkAtGenesis
        SkipFeatureFlagValidation -> onAllChains testnet05 ForkAtGenesis
        ModuleNameFix2 -> onAllChains testnet05 ForkAtGenesis
        OldDAGuard -> onAllChains testnet05 ForkAtGenesis
        PactEvents -> onAllChains testnet05 ForkAtGenesis
        SPVBridge -> onAllChains testnet05 ForkAtGenesis
        Pact4Coin3 -> onAllChains testnet05 ForkAtGenesis
        EnforceKeysetFormats -> onAllChains testnet05 ForkAtGenesis
        Pact42 -> onAllChains testnet05 ForkAtGenesis
        CheckTxHash -> onAllChains testnet05 ForkAtGenesis
        Chainweb213Pact -> onAllChains testnet05 ForkAtGenesis
        Chainweb214Pact -> onAllChains testnet05 ForkAtGenesis
        Chainweb215Pact -> onAllChains testnet05 ForkAtGenesis
        Pact44NewTrans -> onAllChains testnet05 ForkAtGenesis
        Chainweb216Pact -> onAllChains testnet05 ForkAtGenesis
        Chainweb217Pact -> onAllChains testnet05 ForkAtGenesis
        Chainweb218Pact -> onAllChains testnet05 ForkAtGenesis
        Chainweb219Pact -> onAllChains testnet05 ForkAtGenesis
        Chainweb220Pact -> onAllChains testnet05 ForkAtGenesis
        Chainweb221Pact -> onAllChains testnet05 ForkAtGenesis
        Chainweb222Pact -> onAllChains testnet05 ForkAtGenesis
        Chainweb223Pact -> onAllChains testnet05 ForkAtGenesis
        Chainweb224Pact -> onAllChains testnet05 ForkAtGenesis
        Chainweb225Pact -> onAllChains testnet05 ForkAtGenesis
        Chainweb226Pact -> onAllChains testnet05 ForkAtGenesis
        Pact5Fork -> onAllChains testnet05 ForkAtGenesis
        Chainweb228Pact -> onAllChains testnet05 ForkAtGenesis
        Chainweb229Pact -> onAllChains testnet05 ForkNever

    , _versionGraphs =
        Bottom (minBound, twentyChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionMaxBlockGasLimit =
        Bottom (minBound, Just 180_000)
    , _versionBootstraps = domainAddr2PeerInfo testnet05BootstrapHosts
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = ChainMap $ HM.fromList $ concat
            [ [(unsafeChainId i, maxTarget) | i <- [0..19]]
            ]
        , _genesisTime = onAllChains testnet05 $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
        , _genesisBlockPayload = onChains
            [ (unsafeChainId 0, unsafeFromText "Gbu_Tf-PJP2VyptN3m0AnTsXRfiFpnxV8iWZcimPZq4")
            , (unsafeChainId 1, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 2, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 3, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 4, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 5, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 6, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 7, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 8, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 9, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 10, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 11, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 12, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 13, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 14, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 15, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 16, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 17, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 18, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            , (unsafeChainId 19, unsafeFromText "c33AN8j0AKMwQO9BoCGCtinIQT_3JWyNc-fsqdt41Go")
            ]
        }
    , _versionUpgrades = onAllChains testnet05 mempty
    , _versionCheats = VersionCheats
        { _disablePow = False
        , _fakeFirstEpochStart = False
        , _disablePact = False
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = False
        , _disableMempoolSync = False
        }
    , _versionVerifierPluginNames = onAllChains testnet05 $
        Bottom (minBound, Set.fromList $ map VerifierName ["hyperlane_v3_message"])
    , _versionQuirks = noQuirks testnet05
    , _versionServiceDate = Nothing
    , _versionPayloadProviderTypes = onAllChains testnet05 PactProvider
    }
