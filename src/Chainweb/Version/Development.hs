{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.Development(devnet, pattern Development) where

import qualified Data.HashMap.Strict as HM

import Chainweb.BlockCreationTime
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version

import qualified Chainweb.BlockHeader.Genesis.Development0Payload as DN0
import qualified Chainweb.BlockHeader.Genesis.Development1to9Payload as DNN
import qualified Chainweb.BlockHeader.Genesis.Development10to19Payload as DNKAD
import qualified Chainweb.Pact.Transactions.DevelopmentTransactions as Devnet
import qualified Chainweb.Pact.Transactions.CoinV3Transactions as CoinV3
import qualified Chainweb.Pact.Transactions.CoinV4Transactions as CoinV4
import qualified Chainweb.Pact.Transactions.CoinV5Transactions as CoinV5
import qualified Chainweb.Pact.Transactions.MainnetKADTransactions as MNKAD

to20ChainsDevelopment :: BlockHeight
to20ChainsDevelopment = 60

pattern Development :: ChainwebVersion
pattern Development <- ((== devnet) -> True) where
    Development = devnet

devnet :: ChainwebVersion
devnet = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x00000001
    , _versionName = ChainwebVersionName "development"

    , _versionForks = tabulateHashMap $ \case
            SlowEpoch -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
            Vuln797Fix -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
            CoinV2 -> onChains $ [(unsafeChainId 0, ForkAtBlockHeight $ BlockHeight 3)] <> [(unsafeChainId i, ForkAtBlockHeight $ BlockHeight 4) | i <- [1..19]]
            PactBackCompat_v16 -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
            SkipTxTimingValidation -> AllChains $ ForkAtBlockHeight $ BlockHeight 2
            OldTargetGuard -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
            SkipFeatureFlagValidation -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
            ModuleNameFix -> AllChains $ ForkAtBlockHeight $ BlockHeight 2
            ModuleNameFix2 -> AllChains $ ForkAtBlockHeight $ BlockHeight 2
            OldDAGuard -> AllChains $ ForkAtBlockHeight $ BlockHeight 13
            PactEvents -> AllChains $ ForkAtBlockHeight $ BlockHeight 40
            SPVBridge -> AllChains $ ForkAtBlockHeight $ BlockHeight 50
            Pact4Coin3 -> AllChains $ ForkAtBlockHeight $ BlockHeight 80
            EnforceKeysetFormats -> AllChains $ ForkAtBlockHeight $ BlockHeight 100
            Pact42 -> AllChains $ ForkAtBlockHeight $ BlockHeight 90
            CheckTxHash -> AllChains $ ForkAtBlockHeight $ BlockHeight 110
            Chainweb213Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 95
            Chainweb214Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 115
            Chainweb215Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 165
            Pact44NewTrans -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
            Chainweb216Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 215
            Chainweb217Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 470
            Chainweb218Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 500
            Chainweb219Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 550
            Chainweb220Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 560
            Chainweb221Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 580
            Chainweb222Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 590
            Chainweb223Pact -> AllChains ForkNever
            EnableVerifiers -> AllChains ForkNever
            Hyperlane -> AllChains ForkNever

    , _versionUpgrades = foldr (chainZip HM.union) (AllChains mempty)
        [ forkUpgrades devnet
            [ (CoinV2, onChains [(unsafeChainId i, upgrade Devnet.transactions) | i <- [0..9]])
            , (Pact4Coin3, AllChains (Upgrade CoinV3.transactions True))
            , (Chainweb214Pact, AllChains (Upgrade CoinV4.transactions True))
            , (Chainweb215Pact, AllChains (Upgrade CoinV5.transactions True))
            ]
        , onChains [(unsafeChainId 0, HM.singleton to20ChainsDevelopment (upgrade MNKAD.transactions))]
        ]

    , _versionGraphs =
        (to20ChainsDevelopment, twentyChainGraph) `Above`
        End petersonChainGraph

    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = []
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = onChains $ concat
            [ [(unsafeChainId i, HashTarget $ maxBound `div` 100_000) | i <- [0..9]]
            , [(unsafeChainId i, HashTarget 0x0000088f99632cadf39b0db7655be62cb7dbc84ebbd9a90e5b5756d3e7d9196c) | i <- [10..19]]
            ]
        , _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
        , _genesisBlockPayload = onChains $ concat
            [ [(unsafeChainId 0, DN0.payloadBlock)]
            , [(unsafeChainId i, DNN.payloadBlock) | i <- [1..9]]
            , [(unsafeChainId i, DNKAD.payloadBlock) | i <- [10..19]]
            ]
        }

    , _versionMaxBlockGasLimit = End (Just 180_000)
    , _versionCheats = VersionCheats
        { _disablePow = False
        , _fakeFirstEpochStart = True
        , _disablePact = False
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = True
        , _disableMempoolSync = False
        }
    , _versionVerifierPlugins = AllChains $ End $ mempty
    }
