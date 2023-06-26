{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.FastDevelopment(fastDevnet, pattern FastDevelopment) where

import Chainweb.BlockCreationTime
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version

import qualified Chainweb.BlockHeader.Genesis.FastDevelopment0Payload as FDN0
import qualified Chainweb.BlockHeader.Genesis.FastDevelopment1to19Payload as FDNN

pattern FastDevelopment :: ChainwebVersion
pattern FastDevelopment <- ((== fastDevnet) -> True) where
    FastDevelopment = fastDevnet

fastDevnet :: ChainwebVersion
fastDevnet = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x00000002
    , _versionName = ChainwebVersionName "fast-development"
    , _versionForks = tabulateHashMap $ \_ -> AllChains ForkAtGenesis
    , _versionUpgrades = AllChains mempty
    , _versionGraphs = End twentyChainGraph
    , _versionBlockRate = BlockRate 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = []
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = AllChains $ HashTarget (maxBound `div` 100_000)
        , _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
        , _genesisBlockPayload = onChains $ concat
            [ [(unsafeChainId 0, FDN0.payloadBlock)]
            , [(unsafeChainId i, FDNN.payloadBlock) | i <- [1..19]]
            ]
        }

    -- still the *default* block gas limit is set, see
    -- defaultChainwebConfiguration._configBlockGasLimit
    , _versionMaxBlockGasLimit = End Nothing
    , _versionCheats = VersionCheats
        { _disablePow = True
        , _fakeFirstEpochStart = True
        , _disablePact = False
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = True
        , _disableMempoolSync = False
        }
    }
