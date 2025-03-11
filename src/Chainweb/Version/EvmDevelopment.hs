{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.EvmDevelopment
( evmDevnet
, pattern EvmDevelopment
) where

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

pattern EvmDevelopment :: ChainwebVersion
pattern EvmDevelopment <- ((== evmDevnet) -> True) where
    EvmDevelopment = evmDevnet

evmDevnet :: ChainwebVersion
evmDevnet = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x0000_000a
    , _versionName = ChainwebVersionName "evm-development"
    , _versionForks = tabulateHashMap $ \case
        -- TODO: for now, Pact 5 is never enabled on EVM devnet.
        -- this will change as it stabilizes.
        Pact5Fork -> AllChains ForkNever
        _ -> AllChains ForkAtGenesis
    , _versionUpgrades = AllChains mempty
    , _versionGraphs = Bottom (minBound, twentyChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = []
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = AllChains $ HashTarget (maxBound `div` 500_000)
        , _genesisTime = onChains
            $ (unsafeChainId 0, BlockCreationTime (Time (secondsToTimeSpan 1687223762)))
            : (unsafeChainId 1, BlockCreationTime (Time (secondsToTimeSpan 1687223762)))
            : [ (unsafeChainId i, BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]) | i <- [2..19] ]
        , _genesisBlockPayload = onChains $
            [ (unsafeChainId 0, unsafeFromText "cRFtAlQ2aQn3xzMlieY3UGixp2a7z2eXdl4N4w6HRLg")
            , (unsafeChainId 1, unsafeFromText "1lkrseazRVRa4-TYmVV2XcOjyY3Ba0KA-IX4r1bwwtI")
            , (unsafeChainId 2, unsafeFromText "Gnh6QWze67ODyy4BoV4ZOeih72e_Cqos2BJM41sMgVc")
            , (unsafeChainId 3, unsafeFromText "Ta08GYak3xnTr0HvJq9e37RTigd56N7m2aj_cxI1oC0")
            , (unsafeChainId 4, unsafeFromText "eliqzAQ0JGxPD_73dwO7mXsX_tEOz6HJuLsDNJxqSd4")
            , (unsafeChainId 5, unsafeFromText "F7-cmj0XXGKxjKm-dDSmMSpD9jwCjzrdQmwQgsjPj2g")
            , (unsafeChainId 6, unsafeFromText "VK7rBExdlAUo9maErP19WJSgVMTc37xpEa_VXWELF74")
            , (unsafeChainId 7, unsafeFromText "CnAxuzvToxp-bNZ_lnhCAJCEU4hzXuNJmGRMJx5bBWE")
            , (unsafeChainId 8, unsafeFromText "abMl-fqTLY1EmiiFILE_orgsbAB_kKAshAx-zIQFoEM")
            , (unsafeChainId 9, unsafeFromText "o5G8VKfB7I1Qv_Y8paCFHIS6vZuMYUgYBtV6-fDqzYA")
            , (unsafeChainId 10, unsafeFromText "1DJiGUHIrXDNS7M3RJlUorw_07gjLRetb1q9tlt7aZs")
            , (unsafeChainId 11, unsafeFromText "ot-tryHcaC7uvHkT1MqYsbLDV1ApZa2KGvjzddIonYU")
            , (unsafeChainId 12, unsafeFromText "MjGGd7Osb09dEE2mfpDFISrAfWNnDqEqEYuRm5nFXSs")
            , (unsafeChainId 13, unsafeFromText "ljaDzDMfFMOO4AZYC63_KIPAINZwRXUzKZ5FyB1pDjs")
            , (unsafeChainId 14, unsafeFromText "W9lvCyH_NAJx89mnCQcwl5OknI89IM_5rn_TR6KkobQ")
            , (unsafeChainId 15, unsafeFromText "PGKV488wnfsEgv28CtuAT16JNWmMRB-42TDMIr4jRGQ")
            , (unsafeChainId 16, unsafeFromText "8J1yMti75X1Gnjn2AEpWMw-8nzOK6ysHo5c4SBIiNGo")
            , (unsafeChainId 17, unsafeFromText "XlIsxdG3YnbxapDq71wY85-ghlIK3c5vfD_WEXgKcRM")
            , (unsafeChainId 18, unsafeFromText "968Xg-0Jqm1nTgFi69or2yuFprmg7_SDKcrcWIItW74")
            , (unsafeChainId 19, unsafeFromText "7CRqrZPgJ9JYuCWAQtO2bqxlFDUw_i2Hlk52duk8A0s")
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
        (minBound, Set.fromList $ map VerifierName ["hyperlane_v3_message", "allow"])
    , _versionQuirks = noQuirks
    , _versionServiceDate = Nothing

    -- FIXME make this safe for graph changes
    , _versionPayloadProviderTypes = onChains
        $ (unsafeChainId 0, EvmProvider 1789)
        : (unsafeChainId 1, EvmProvider 1790)
        : [ (unsafeChainId i, MinimalProvider) | i <- [2..19] ]
    }
