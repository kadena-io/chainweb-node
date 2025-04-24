{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.Mainnet(mainnet, pattern Mainnet01) where

import Control.Lens
import qualified Data.HashMap.Strict as HM
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
import P2P.BootstrapNodes

import Pact.Core.Gas(Gas(..))
import Pact.Core.Names

-- | Initial hash target for mainnet 20-chain transition. Difficulty on the new
-- chains is 1/4 of the current difficulty. It is based on the following header
-- from 2020-07-09. This value should be double checked after the testnet04
-- transition and before the release of chainweb node version 2.1.
--
-- @
-- {
--   "creationTime": 1594319266887602,
--   "parent": "aSIkDjuJQGGOwJW-60T_1WRK9KPJm1rz63a4SW8WtSc",
--   "height": 731382,
--   "hash": "Ua_pSMMo-szlMpXMuSYWTcVlaSIf01TxJvBCmFkmhBM",
--   "chainId": 0,
--   "weight": "xo3dabqEYpUPAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
--   "featureFlags": 0,
--   "epochStart": 1594316109999615,
--   "adjacents": {
--     "2": "KuuujcD6yeZ9jRXwlRE0ed5dHc3x_akIz1REmKXuDtk",
--     "5": "qFU32Qmlj-syzuZ2awCvyoW6Jex3TQqGTzd-Dchn1gc",
--     "3": "Lgu1FgiCw4qPpptoVRmijn8WKG2OcAUAp1Ha7KFbrWg"
--   },
--   "payloadHash": "MV079yClHYSYBW74WySK-15AUVQg8QMKHJZbtzTCbgA",
--   "chainwebVersion": "mainnet01",
--   "target": "DOordl9cgfs4ZTBdFnbjRW5th-hW-pL33DIAAAAAAAA",
--   "nonce": "149742924667593745"
-- }
-- @
--
-- It holds that:
--
-- prop> Just mainnet20InitialHashTarget == HashTarget . (4 *) <$> (runGet decodePowHashNat =<< decodeB64UrlNoPaddingText "DOordl9cgfs4ZTBdFnbjRW5th-hW-pL33DIAAAAAAAA")
--
mainnet20InitialHashTarget :: HashTarget
mainnet20InitialHashTarget = HashTarget 0x000000000000cb73de4be95ba21db5b9178dd85974c194e3ee05717dd8afa830

to20ChainsMainnet :: BlockHeight
to20ChainsMainnet = 852_054 -- 2020-08-20 16:00:00

pattern Mainnet01 :: ChainwebVersion
pattern Mainnet01 <- ((== mainnet) -> True) where
    Mainnet01 = mainnet

mainnet :: ChainwebVersion
mainnet = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x00000005
    , _versionName = ChainwebVersionName "mainnet01"
    , _versionForks = tabulateHashMap $ \case
        SlowEpoch -> onAllChains mainnet (ForkAtBlockHeight $ BlockHeight 80_000)
        Vuln797Fix -> onChains $
            [ (unsafeChainId 0, ForkAtBlockHeight $ BlockHeight 121_452) -- 2019-12-10T21:00:00.0
            , (unsafeChainId 1, ForkAtBlockHeight $ BlockHeight 121_452)
            , (unsafeChainId 2, ForkAtBlockHeight $ BlockHeight 121_452)
            , (unsafeChainId 3, ForkAtBlockHeight $ BlockHeight 121_451)
            , (unsafeChainId 4, ForkAtBlockHeight $ BlockHeight 121_451)
            , (unsafeChainId 5, ForkAtBlockHeight $ BlockHeight 121_452)
            , (unsafeChainId 6, ForkAtBlockHeight $ BlockHeight 121_452)
            , (unsafeChainId 7, ForkAtBlockHeight $ BlockHeight 121_451)
            , (unsafeChainId 8, ForkAtBlockHeight $ BlockHeight 121_452)
            , (unsafeChainId 9, ForkAtBlockHeight $ BlockHeight 121_451)
            ] <> [(unsafeChainId i, ForkAtGenesis) | i <- [10..19]]
        CoinV2 -> onChains $
            [ (unsafeChainId 0, ForkAtBlockHeight $ BlockHeight 140_808)
            , (unsafeChainId 1, ForkAtBlockHeight $ BlockHeight 140_809)
            , (unsafeChainId 2, ForkAtBlockHeight $ BlockHeight 140_808)
            , (unsafeChainId 3, ForkAtBlockHeight $ BlockHeight 140_809)
            , (unsafeChainId 4, ForkAtBlockHeight $ BlockHeight 140_808)
            , (unsafeChainId 5, ForkAtBlockHeight $ BlockHeight 140_808)
            , (unsafeChainId 6, ForkAtBlockHeight $ BlockHeight 140_808)
            , (unsafeChainId 7, ForkAtBlockHeight $ BlockHeight 140_809)
            , (unsafeChainId 8, ForkAtBlockHeight $ BlockHeight 140_808)
            , (unsafeChainId 9, ForkAtBlockHeight $ BlockHeight 140_808)
            ] <> [(unsafeChainId i, ForkAtGenesis) | i <- [10..19]]
        PactBackCompat_v16 -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 328_000
        ModuleNameFix -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 448_501
        SkipTxTimingValidation -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 449_940
        OldTargetGuard -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 452_820 -- ~ 2020-04-04T00:00:00Z
        SkipFeatureFlagValidation -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 530_500 -- ~ 2020-05-01T00:00:xxZ
        ModuleNameFix2 -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 752_214
        OldDAGuard -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 771_414 -- ~ 2020-07-23 16:00:00
        PactEvents -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 1_138_000
        SPVBridge -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 1_275_000
        Pact4Coin3 -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 1_722_500      -- 2021-06-19T03:34:05+00:00
        EnforceKeysetFormats -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 2_162_000 -- 2022-01-17T17:51:12
        Pact42 -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 2_334_500 -- 2022-01-17T17:51:12+00:00
        CheckTxHash -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 2_349_800 -- 2022-01-23T02:53:38
        Chainweb213Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 2_447_315 -- 2022-02-26T00:00:00+00:00
        Chainweb214Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 2_605_663 -- 2022-04-22T00:00:00+00:00
        Chainweb215Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 2_766_630 -- 2022-06-17T00:00:00+00:00
        Pact44NewTrans -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 2_939_323 -- Todo: add date
        Chainweb216Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 2_988_324 -- 2022-09-02T00:00:00+00:00
        Chainweb217Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 3_250_348 -- 2022-12-02T00:00:00+00:00
        Chainweb218Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 3_512_363 -- 2023-03-03 00:00:00+00:00
        Chainweb219Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 3_774_423 -- 2023-06-02 00:00:00+00:00
        Chainweb220Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 4_056_499 -- 2023-09-08 00:00:00+00:00
        Chainweb221Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 4_177_889 -- 2023-10-20 00:00:00+00:00
        Chainweb222Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 4_335_753 -- 2023-12-14 00:00:00+00:00
        Chainweb223Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 4_577_530 -- 2024-03-07 00:00:00+00:00
        Chainweb224Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 4_819_246 -- 2024-05-30 00:00:00+00:00
        Chainweb225Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 5_060_924 -- 2024-08-22 00:00:00+00:00
        Chainweb226Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 5_302_559 -- 2024-11-14 00:00:00+00:00
        Pact5Fork -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 5_555_698       -- 2025-02-10 00:00:00+00:00
        Chainweb228Pact -> onAllChains mainnet $ ForkAtBlockHeight $ BlockHeight 5_659_280 -- 2025-03-18 00:00:00+00:00
        Chainweb229Pact -> onAllChains mainnet $ ForkNever

    , _versionGraphs =
        (to20ChainsMainnet, twentyChainGraph) `Above`
        Bottom (minBound, petersonChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionMaxBlockGasLimit =
        (succ $ mainnet ^?! versionForks . at Chainweb216Pact . _Just . atChain (unsafeChainId 0) . _ForkAtBlockHeight, Just 180_000) `Above`
        Bottom (minBound, Nothing)
    , _versionBootstraps = domainAddr2PeerInfo mainnetBootstrapHosts
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = ChainMap $ HM.fromList $ concat
            [ [(unsafeChainId i, maxTarget) | i <- [0..9]]
            , [(unsafeChainId i, mainnet20InitialHashTarget) | i <- [10..19]]
            ]
        , _genesisTime = onAllChains mainnet $ BlockCreationTime [timeMicrosQQ| 2019-10-30T00:01:00.0 |]
        , _genesisBlockPayload = onChains
            [ ( unsafeChainId 0, unsafeFromText "k1H3DsInAPvJ0W_zPxnrpkeSNdPUT0S9U8bqDLG739o")
            , ( unsafeChainId 1, unsafeFromText "kClp_Tw7keCLXMfaCyjH-gToAGmLvRQqiNRmhWUCbxs")
            , ( unsafeChainId 2, unsafeFromText "4DVp1dkSLYkFD0zbFF7e5Ba0ezKmnZhEVzlc5ZqqAT0")
            , ( unsafeChainId 3, unsafeFromText "fCHKOS9SF9G3Gy5iGZnOmQxjGYf32yM8PklrcBD_dmQ")
            , ( unsafeChainId 4, unsafeFromText "cTZ_29TkjXDlG_GlW8iB-va3mofpt_UjthLcOoqtlaQ")
            , ( unsafeChainId 5, unsafeFromText "MS_wu92H4GVPCziB6g8rOAc3x2uyegXH4Yl_0TfpI7U")
            , ( unsafeChainId 6, unsafeFromText "DVs3k9omI_WHMOJ8tUZ1Bt9Q1DdNmqC3V51iZDGmWwM")
            , ( unsafeChainId 7, unsafeFromText "Faa4TQZGFkFumLfHnMIJBAu_PSmGY4F-YraLVFrxr7Y")
            , ( unsafeChainId 8, unsafeFromText "YWgbJ4K5VET4WnG3H0Y0HPgzZ_qSnTgqxyB1kpMLJTQ")
            , ( unsafeChainId 9, unsafeFromText "w_6Spsw-jdCi9hBTlM6v0C1P7XoglU_AqNG9rcOwAZ0")
            , ( unsafeChainId 10, unsafeFromText "i-MN4AoxsaPds4M_MzwNSUygAkGnPZoCDvahfckowt4")
            , ( unsafeChainId 11, unsafeFromText "i-MN4AoxsaPds4M_MzwNSUygAkGnPZoCDvahfckowt4")
            , ( unsafeChainId 12, unsafeFromText "i-MN4AoxsaPds4M_MzwNSUygAkGnPZoCDvahfckowt4")
            , ( unsafeChainId 13, unsafeFromText "i-MN4AoxsaPds4M_MzwNSUygAkGnPZoCDvahfckowt4")
            , ( unsafeChainId 14, unsafeFromText "i-MN4AoxsaPds4M_MzwNSUygAkGnPZoCDvahfckowt4")
            , ( unsafeChainId 15, unsafeFromText "i-MN4AoxsaPds4M_MzwNSUygAkGnPZoCDvahfckowt4")
            , ( unsafeChainId 16, unsafeFromText "i-MN4AoxsaPds4M_MzwNSUygAkGnPZoCDvahfckowt4")
            , ( unsafeChainId 17, unsafeFromText "i-MN4AoxsaPds4M_MzwNSUygAkGnPZoCDvahfckowt4")
            , ( unsafeChainId 18, unsafeFromText "i-MN4AoxsaPds4M_MzwNSUygAkGnPZoCDvahfckowt4")
            , ( unsafeChainId 19, unsafeFromText "i-MN4AoxsaPds4M_MzwNSUygAkGnPZoCDvahfckowt4")
            ]
        }
    , _versionUpgrades = onChains []
    , _versionCheats = VersionCheats
        { _disablePow = False
        , _fakeFirstEpochStart = False
        , _disablePact = False
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = False
        , _disableMempoolSync = False
        }
    , _versionVerifierPluginNames = onAllChains mainnet $
        (4_577_530, Set.fromList $ map VerifierName ["hyperlane_v3_message"]) `Above`
        Bottom (minBound, mempty)
    , _versionQuirks = VersionQuirks
        { _quirkGasFees = onChains
            [ (unsafeChainId 0, HM.fromList [((BlockHeight 4585419, TxBlockIdx 0), Gas 67_618)])
            , (unsafeChainId 9, HM.fromList [((BlockHeight 4594049, TxBlockIdx 0), Gas 69_092)])
            ]
        }
    , _versionServiceDate = Just "2025-04-30T00:00:00Z"
    , _versionPayloadProviderTypes = onAllChains mainnet PactProvider
    }
