{-# language LambdaCase #-}
{-# language ImportQualifiedPost #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.Pact5Development(pact5Devnet, pattern Pact5Development) where

import Chainweb.BlockCreationTime
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version
import Data.Set qualified as Set
import Pact.Types.Verifier
import qualified Chainweb.BlockHeader.Genesis.Pact5Development0Payload as DN0
import qualified Chainweb.BlockHeader.Genesis.Pact5Development1to19Payload as DNN

pattern Pact5Development :: ChainwebVersion
pattern Pact5Development <- ((== pact5Devnet) -> True) where
    Pact5Development = pact5Devnet

pact5Devnet :: ChainwebVersion
pact5Devnet = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x00000008
    , _versionName = ChainwebVersionName "pact5-development"
    , _versionForks = tabulateHashMap $ \case
        _ -> AllChains ForkAtGenesis
    , _versionUpgrades = AllChains mempty
    , _versionGraphs = Bottom (minBound, twentyChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = []
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = AllChains $ HashTarget (maxBound `div` 100_000)
        , _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
        , _genesisBlockPayload = onChains $ concat
            [ [(unsafeChainId 0, DN0.payloadBlock)]
            , [(unsafeChainId i, DNN.payloadBlock) | i <- [1..19]]
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
    , _versionVerifierPluginNames = AllChains $ Bottom
        ( minBound
        , Set.fromList $ map VerifierName ["hyperlane_v3_message", "allow"]
        )
    , _versionQuirks = noQuirks
    , _versionServiceDate = Nothing
    }
