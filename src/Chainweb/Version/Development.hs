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
import qualified Chainweb.BlockHeader.Genesis.DevelopmentNPayload as DNN
import qualified Chainweb.BlockHeader.Genesis.DevelopmentKADPayload as DNKAD
import qualified Chainweb.Pact.Transactions.DevelopmentTransactions as Devnet
import qualified Chainweb.Pact.Transactions.CoinV3Transactions as CoinV3
import qualified Chainweb.Pact.Transactions.CoinV4Transactions as CoinV4
import qualified Chainweb.Pact.Transactions.CoinV5Transactions as CoinV5
import qualified Chainweb.Pact.Transactions.MainnetKADTransactions as MNKAD

to20ChainsDevelopment :: BlockHeight
to20ChainsDevelopment = 12

pattern Development :: ChainwebVersion
pattern Development <- ((== devnet) -> True) where
    Development = devnet

devnet :: ChainwebVersion
devnet = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x00000001
    , _versionName = ChainwebVersionName "development"

    , _versionForks = tabulateHashMap $ \case
            Vuln797Fix -> AllChains 1
            SlowEpoch -> AllChains 1
            OldTargetGuard -> AllChains 1
            EnforceKeysetFormats -> AllChains 1
            SkipFeatureFlagValidation -> AllChains 1
            OldDAGuard -> AllChains 1
            CheckTxHash -> AllChains 1
            PactEvents -> AllChains 1
            SkipTxTimingValidation -> AllChains 1
            SPVBridge -> AllChains 1
            ModuleNameFix -> AllChains 1
            ModuleNameFix2 -> AllChains 1
            PactBackCompat_v16 -> AllChains 1
            Pact44NewTrans -> AllChains 1
            Pact420 -> AllChains $ BlockHeight 1
            CoinV2 -> onChains $ concat
                [ [(unsafeChainId 0, BlockHeight 3)]
                , [(unsafeChainId i, BlockHeight 4) | i <- [1..19]]
                ]
            Pact4Coin3 -> AllChains $ BlockHeight 14
            Chainweb213Pact -> AllChains $ BlockHeight 15
            Chainweb214Pact -> AllChains $ BlockHeight 16
            Chainweb215Pact -> AllChains $ BlockHeight 17
            Chainweb216Pact -> AllChains $ BlockHeight 18
            Chainweb217Pact -> AllChains $ BlockHeight 18
            Chainweb218Pact -> AllChains $ BlockHeight 18

    , _versionUpgrades = foldr (chainZip HM.union) (AllChains mempty)
        [ forkUpgrades devnet
            [ (CoinV2, onChains $ concat
                [ [(unsafeChainId 0, upgrade Devnet.transactions)]
                , [(unsafeChainId i, upgrade Devnet.transactions) | i <- [1..9]]
                ])
            , (Pact4Coin3, AllChains (Upgrade CoinV3.transactions True))
            , (Chainweb214Pact, AllChains (Upgrade CoinV4.transactions True))
            , (Chainweb215Pact, AllChains (Upgrade CoinV5.transactions True))
            ]
        , onChains [(unsafeChainId 0, HM.singleton to20ChainsDevelopment (upgrade MNKAD.transactions))]
        ]

    , _versionGraphs =
        (to20ChainsDevelopment, twentyChainGraph) `Above`
        End petersonChainGraph

    , _versionBlockRate = BlockRate 30_000_000
    , _versionWindow = Just $ WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionFakeFirstEpochStart = True
    , _versionBootstraps = []
    , _versionGenesis = ChainwebGenesis
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
    , _versionCheats = Cheats
        { _disablePeerValidation = True
        , _disablePow = False
        , _disablePact = False
        , _disableMempool = False
        }
    }
