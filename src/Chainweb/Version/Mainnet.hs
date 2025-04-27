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

import Pact.Types.Runtime (Gas(..))
import Pact.Types.Verifier

import qualified Chainweb.BlockHeader.Genesis.Mainnet0Payload as MN0
import qualified Chainweb.BlockHeader.Genesis.Mainnet1Payload as MN1
import qualified Chainweb.BlockHeader.Genesis.Mainnet2Payload as MN2
import qualified Chainweb.BlockHeader.Genesis.Mainnet3Payload as MN3
import qualified Chainweb.BlockHeader.Genesis.Mainnet4Payload as MN4
import qualified Chainweb.BlockHeader.Genesis.Mainnet5Payload as MN5
import qualified Chainweb.BlockHeader.Genesis.Mainnet6Payload as MN6
import qualified Chainweb.BlockHeader.Genesis.Mainnet7Payload as MN7
import qualified Chainweb.BlockHeader.Genesis.Mainnet8Payload as MN8
import qualified Chainweb.BlockHeader.Genesis.Mainnet9Payload as MN9
import qualified Chainweb.BlockHeader.Genesis.Mainnet10to19Payload as MNKAD
import qualified Chainweb.Pact.Transactions.CoinV3Transactions as CoinV3
import qualified Chainweb.Pact.Transactions.CoinV4Transactions as CoinV4
import qualified Chainweb.Pact.Transactions.CoinV5Transactions as CoinV5
import qualified Chainweb.Pact.Transactions.CoinV6Transactions as CoinV6
import qualified Chainweb.Pact.Transactions.Mainnet0Transactions as MN0
import qualified Chainweb.Pact.Transactions.Mainnet1Transactions as MN1
import qualified Chainweb.Pact.Transactions.Mainnet2Transactions as MN2
import qualified Chainweb.Pact.Transactions.Mainnet3Transactions as MN3
import qualified Chainweb.Pact.Transactions.Mainnet4Transactions as MN4
import qualified Chainweb.Pact.Transactions.Mainnet5Transactions as MN5
import qualified Chainweb.Pact.Transactions.Mainnet6Transactions as MN6
import qualified Chainweb.Pact.Transactions.Mainnet7Transactions as MN7
import qualified Chainweb.Pact.Transactions.Mainnet8Transactions as MN8
import qualified Chainweb.Pact.Transactions.Mainnet9Transactions as MN9
import qualified Chainweb.Pact.Transactions.MainnetKADTransactions as MNKAD

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
        SlowEpoch -> AllChains (ForkAtBlockHeight $ BlockHeight 80_000)
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
        PactBackCompat_v16 -> AllChains (ForkAtBlockHeight $ BlockHeight 328_000)
        ModuleNameFix -> AllChains (ForkAtBlockHeight $ BlockHeight 448_501)
        SkipTxTimingValidation -> AllChains (ForkAtBlockHeight $ BlockHeight 449_940)
        OldTargetGuard -> AllChains (ForkAtBlockHeight $ BlockHeight 452_820) -- ~ 2020-04-04T00:00:00Z
        SkipFeatureFlagValidation -> AllChains (ForkAtBlockHeight $ BlockHeight 530_500) -- ~ 2020-05-01T00:00:xxZ
        ModuleNameFix2 -> AllChains (ForkAtBlockHeight $ BlockHeight 752_214)
        OldDAGuard -> AllChains (ForkAtBlockHeight $ BlockHeight 771_414) -- ~ 2020-07-23 16:00:00
        PactEvents -> AllChains (ForkAtBlockHeight $ BlockHeight 1_138_000)
        SPVBridge -> AllChains (ForkAtBlockHeight $ BlockHeight 1_275_000)
        Pact4Coin3 -> AllChains (ForkAtBlockHeight $ BlockHeight 1_722_500)      -- 2021-06-19T03:34:05+00:00
        EnforceKeysetFormats -> AllChains (ForkAtBlockHeight $ BlockHeight 2_162_000) -- 2022-01-17T17:51:12
        Pact42 -> AllChains (ForkAtBlockHeight $ BlockHeight 2_334_500) -- 2022-01-17T17:51:12+00:00
        CheckTxHash -> AllChains (ForkAtBlockHeight $ BlockHeight 2_349_800) -- 2022-01-23T02:53:38
        Chainweb213Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 2_447_315) -- 2022-02-26T00:00:00+00:00
        Chainweb214Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 2_605_663) -- 2022-04-22T00:00:00+00:00
        Chainweb215Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 2_766_630) -- 2022-06-17T00:00:00+00:00
        Pact44NewTrans -> AllChains (ForkAtBlockHeight $ BlockHeight 2_939_323) -- Todo: add date
        Chainweb216Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 2_988_324) -- 2022-09-02T00:00:00+00:00
        Chainweb217Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 3_250_348) -- 2022-12-02T00:00:00+00:00
        Chainweb218Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 3_512_363) -- 2023-03-03 00:00:00+00:00
        Chainweb219Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 3_774_423) -- 2023-06-02 00:00:00+00:00
        Chainweb220Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 4_056_499) -- 2023-09-08 00:00:00+00:00
        Chainweb221Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 4_177_889) -- 2023-10-20 00:00:00+00:00
        Chainweb222Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 4_335_753) -- 2023-12-14 00:00:00+00:00
        Chainweb223Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 4_577_530) -- 2024-03-07 00:00:00+00:00
        Chainweb224Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 4_819_246) -- 2024-05-30 00:00:00+00:00
        Chainweb225Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 5_060_924) -- 2024-08-22 00:00:00+00:00
        Chainweb226Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 5_302_559) -- 2024-11-14 00:00:00+00:00
        Pact5Fork -> AllChains (ForkAtBlockHeight $ BlockHeight 5_555_698)       -- 2025-02-10 00:00:00+00:00
        Chainweb228Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 5_659_280) -- 2025-03-18 00:00:00+00:00
        Chainweb229Pact -> AllChains (ForkAtBlockHeight $ BlockHeight 5_785_923) -- 2025-05-01 00:00:00+00:00
        Chainweb230Pact -> AllChains ForkNever

    , _versionGraphs =
        (to20ChainsMainnet, twentyChainGraph) `Above`
        Bottom (minBound, petersenChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionMaxBlockGasLimit =
        (succ $ mainnet ^?! versionForks . at Chainweb216Pact . _Just . atChain (unsafeChainId 0) . _ForkAtBlockHeight, Just 180_000) `Above`
        Bottom (minBound, Nothing)
    , _versionBootstraps = domainAddr2PeerInfo mainnetBootstrapHosts
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = OnChains $ HM.fromList $ concat
            [ [(unsafeChainId i, maxTarget) | i <- [0..9]]
            , [(unsafeChainId i, mainnet20InitialHashTarget) | i <- [10..19]]
            ]
        , _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2019-10-30T00:01:00.0 |]
        , _genesisBlockPayload = OnChains $ HM.fromList $ concat
            [
                [ (unsafeChainId 0, MN0.payloadBlock)
                , (unsafeChainId 1, MN1.payloadBlock)
                , (unsafeChainId 2, MN2.payloadBlock)
                , (unsafeChainId 3, MN3.payloadBlock)
                , (unsafeChainId 4, MN4.payloadBlock)
                , (unsafeChainId 5, MN5.payloadBlock)
                , (unsafeChainId 6, MN6.payloadBlock)
                , (unsafeChainId 7, MN7.payloadBlock)
                , (unsafeChainId 8, MN8.payloadBlock)
                , (unsafeChainId 9, MN9.payloadBlock)
                ]
            , [(unsafeChainId i, MNKAD.payloadBlock) | i <- [10..19]]
            ]
        }
    , _versionUpgrades = chainZip HM.union
        (indexByForkHeights mainnet
        [ (CoinV2, onChains
            [ (unsafeChainId 0, pact4Upgrade MN0.transactions)
            , (unsafeChainId 1, pact4Upgrade MN1.transactions)
            , (unsafeChainId 2, pact4Upgrade MN2.transactions)
            , (unsafeChainId 3, pact4Upgrade MN3.transactions)
            , (unsafeChainId 4, pact4Upgrade MN4.transactions)
            , (unsafeChainId 5, pact4Upgrade MN5.transactions)
            , (unsafeChainId 6, pact4Upgrade MN6.transactions)
            , (unsafeChainId 7, pact4Upgrade MN7.transactions)
            , (unsafeChainId 8, pact4Upgrade MN8.transactions)
            , (unsafeChainId 9, pact4Upgrade MN9.transactions)
            ])
        , (Pact4Coin3, AllChains $ Pact4Upgrade CoinV3.transactions True)
        , (Chainweb214Pact, AllChains $ Pact4Upgrade CoinV4.transactions True)
        , (Chainweb215Pact, AllChains $ Pact4Upgrade CoinV5.transactions True)
        , (Chainweb223Pact, AllChains $ pact4Upgrade CoinV6.transactions)
        ])
        (onChains [(unsafeChainId 0, HM.singleton to20ChainsMainnet (pact4Upgrade MNKAD.transactions))])
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
        (4_577_530, Set.fromList $ map VerifierName ["hyperlane_v3_message"]) `Above`
        Bottom (minBound, mempty)
    , _versionQuirks = VersionQuirks
        { _quirkGasFees = onChains
            [ (unsafeChainId 0, HM.fromList [((BlockHeight 4585419, TxBlockIdx 0), Gas 67_618)])
            , (unsafeChainId 9, HM.fromList [((BlockHeight 4594049, TxBlockIdx 0), Gas 69_092)])
            ]
        }
    , _versionServiceDate = Just "2025-07-23T00:00:00Z"
    }
