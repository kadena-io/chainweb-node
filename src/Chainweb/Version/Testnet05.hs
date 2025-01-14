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

import Pact.Types.Verifier

pattern Testnet05 :: ChainwebVersion
pattern Testnet05 <- ((== testnet05) -> True) where
    Testnet05 = testnet05

testnet05 :: ChainwebVersion
testnet05 = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x00000009
    , _versionName = ChainwebVersionName "testnet05"
    , _versionForks = tabulateHashMap $ \case
        SlowEpoch -> AllChains ForkAtGenesis
        Vuln797Fix -> AllChains ForkAtGenesis
        CoinV2 -> AllChains ForkAtGenesis
        PactBackCompat_v16 -> AllChains ForkAtGenesis
        ModuleNameFix -> AllChains ForkAtGenesis
        SkipTxTimingValidation -> AllChains ForkAtGenesis
        OldTargetGuard -> AllChains ForkAtGenesis
        SkipFeatureFlagValidation -> AllChains ForkAtGenesis
        ModuleNameFix2 -> AllChains ForkAtGenesis
        OldDAGuard -> AllChains ForkAtGenesis
        PactEvents -> AllChains ForkAtGenesis
        SPVBridge -> AllChains ForkAtGenesis
        Pact4Coin3 -> AllChains ForkAtGenesis
        EnforceKeysetFormats -> AllChains ForkAtGenesis
        Pact42 -> AllChains ForkAtGenesis
        CheckTxHash -> AllChains ForkAtGenesis
        Chainweb213Pact -> AllChains ForkAtGenesis
        Chainweb214Pact -> AllChains ForkAtGenesis
        Chainweb215Pact -> AllChains ForkAtGenesis
        Pact44NewTrans -> AllChains ForkAtGenesis
        Chainweb216Pact -> AllChains ForkAtGenesis
        Chainweb217Pact -> AllChains ForkAtGenesis
        Chainweb218Pact -> AllChains ForkAtGenesis
        Chainweb219Pact -> AllChains ForkAtGenesis
        Chainweb220Pact -> AllChains ForkAtGenesis
        Chainweb221Pact -> AllChains ForkAtGenesis
        Chainweb222Pact -> AllChains ForkAtGenesis
        Chainweb223Pact -> AllChains ForkAtGenesis
        Chainweb224Pact -> AllChains ForkAtGenesis
        Chainweb225Pact -> AllChains ForkAtGenesis
        Chainweb226Pact -> AllChains ForkAtGenesis
        Pact5Fork -> AllChains ForkAtGenesis
        Chainweb228Pact -> AllChains ForkAtGenesis
        Chainweb229Pact -> AllChains ForkNever

    , _versionGraphs =
        Bottom (minBound, twentyChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionMaxBlockGasLimit =
        Bottom (minBound, Just 180_000)
    , _versionBootstraps = domainAddr2PeerInfo testnet05BootstrapHosts
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = OnChains $ HM.fromList $ concat
            [ [(unsafeChainId i, maxTarget) | i <- [0..19]]
            ]
        , _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
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
    , _versionUpgrades = AllChains mempty
    , _versionCheats = VersionCheats
        { _disablePow = False
        , _fakeFirstEpochStart = False
        , _disablePact = False
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = False
        , _disableMempoolSync = False
        }
    , _versionVerifierPluginNames = AllChains $
        Bottom (minBound, Set.fromList $ map VerifierName ["hyperlane_v3_message"])
    , _versionQuirks = noQuirks
    , _versionServiceDate = Nothing
    , _versionPayloadProviderTypes = AllChains PactProvider
    }
