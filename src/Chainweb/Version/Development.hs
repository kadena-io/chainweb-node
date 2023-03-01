{-# language NumericUnderscores #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}

module Chainweb.Version.Development(devnet, pattern Development) where

import qualified Data.HashMap.Strict as HM

import Chainweb.BlockCreationTime
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Time
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

    , _versionForks = HM.unions
        [ HM.fromList
            [ (Chainweb217Pact, AllChains $ BlockHeight 20)
            , (Chainweb216Pact, AllChains $ BlockHeight 19)
            , (Chainweb215Pact, AllChains $ BlockHeight 18)
            , (Chainweb214Pact, AllChains $ BlockHeight 17)
            , (Chainweb213Pact, AllChains $ BlockHeight 16)
            , (Pact420, AllChains $ BlockHeight 15)
            , (Pact4Coin3, AllChains $ BlockHeight 14)
            , (CoinV2, onChains $ concat
                [ [(unsafeChainId 0, BlockHeight 3)]
                , [(unsafeChainId i, BlockHeight 4) | i <- [1..19]]
                ])
            ]
        -- all unspecified forks start at block 1
        , HM.fromList [(fork, AllChains 1) | fork <- [minBound..maxBound]]
        ]

    , _versionUpgrades = chainZip HM.union
        (forkUpgrades devnet
        [ (CoinV2, onChains $ concat
            [ [(unsafeChainId 0, upgrade Devnet.transactions)]
            , [(unsafeChainId i, upgrade Devnet.transactions) | i <- [1..9]]
            ])
        , (Pact4Coin3, AllChains (upgrade CoinV3.transactions))
        , (Chainweb214Pact, AllChains (upgrade CoinV4.transactions))
        , (Chainweb215Pact, AllChains (upgrade CoinV5.transactions))
        ])
        (onChains [(unsafeChainId 0, HM.singleton to20ChainsDevelopment (upgrade MNKAD.transactions))])

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
