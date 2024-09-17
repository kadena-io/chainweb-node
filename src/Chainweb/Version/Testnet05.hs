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

import qualified Chainweb.BlockHeader.Genesis.Testnet050Payload as PN0
import qualified Chainweb.BlockHeader.Genesis.Testnet051to19Payload as PNN

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
        Pact5Fork -> AllChains ForkAtGenesis

    , _versionGraphs =
        End twentyChainGraph
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionMaxBlockGasLimit =
        End (Just 180_000)
    , _versionBootstraps = domainAddr2PeerInfo testnet05BootstrapHosts
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = OnChains $ HM.fromList $ concat
            [ [(unsafeChainId i, maxTarget) | i <- [0..19]]
            ]
        , _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
        , _genesisBlockPayload = OnChains $ HM.fromList $ concat
            [ [ (unsafeChainId 0, PN0.payloadBlock)
              ]
            , [(unsafeChainId i, PNN.payloadBlock) | i <- [1..19]]
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
        End (Set.fromList $ map VerifierName ["hyperlane_v3_message"])
    , _versionQuirks = VersionQuirks
        { _quirkGasFees = mempty
        }
    , _versionServiceDate = Nothing
    }
