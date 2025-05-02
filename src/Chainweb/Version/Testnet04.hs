{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.Testnet04(testnet04, pattern Testnet04) where

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

import Pact.Core.Gas (Gas(..))
import Pact.Core.Names

-- | Initial hash target for testnet04 20-chain transition. Based on the following
-- header from recap devnet running with 5 GPUs hash power. Using this target unchanged
-- means, that we should do to the transition with the hash power of about
-- 5 - 50 GPUs in the system for a smooth transition.
--
-- The value for the initial target is 38 times smaller larger than value of an
-- successful test run on recap devnet with 5 GPUs. During that test the initial
-- target was about 32 times larger than the actual target at the time of the
-- transition.
--
-- @
-- {
--   "creationTime": 1594433454304125,
--   "parent": "DHSarVwhj6Xvu0KewCI1nRdGcNSWKFoOUy7us27mDac",
--   "height": 200,
--   "hash": "DC8HV9W0JM5gzliwDupjG10Lnwav09xWtxy01kGPTLM",
--   "chainId": 0,
--   "weight": "ReZ2aCAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
--   "featureFlags": 0,
--   "epochStart": 1594430808323849,
--   "adjacents": {
--     "2": "JPbz_YjWIvDgdGxdemkU6vVRimZZawxY_j0Hwo0pzb0",
--     "5": "wMFfoFrQ1GWOFj6jCNGRa3SiuFRGOCmS06F7HfmLnNw",
--     "3": "9WIBnxDGGZsy9FCCorvAUa4SlE5Rqs-cTLEsWCPOVbQ"
--   },
--   "payloadHash": "AOYQdE5xl_YueZSppW4MoadasjF149K28CON2GuH9Mc",
--   "chainwebVersion": "recap-development",
--   "target": "NZIklpW6xujSPrX3gyhXInfxxOS6JDjkW_GbGwAAAAA",
--   "nonce": "5805155470630695"
-- }
-- @
--
-- It holds that:
--
-- prop> Just testnet20InitialHashTarget == HashTarget <$> (runGet decodePowHashNat =<< decodeB64UrlNoPaddingText "NZIklpW6xujSPrX3gyhXInfxxOS6JDjkW_GbGwAAAAA")
-- prop> _hashTarget testnet20InitialHashTarget `div` _hashTarget mainnet20InitialHashTarget == PowHashNat 8893
-- prop> _hashTarget (genesisBlockTarget RecapDevelopment (unsafeChainId 10)) `div` _hashTarget testnet20InitialHashTarget == PowHashNat 38
--
testnet20InitialHashTarget :: HashTarget
testnet20InitialHashTarget = HashTarget 0x000000001b9bf15be43824bae4c4f17722572883f7b53ed2e8c6ba9596249235

-- | The block height of the 20-chain transition.
to20ChainsTestnet :: BlockHeight
to20ChainsTestnet = 332_604 -- 2020-07-28 16:00:00

pattern Testnet04 :: ChainwebVersion
pattern Testnet04 <- ((== testnet04) -> True) where
    Testnet04 = testnet04

testnet04 :: ChainwebVersion
testnet04 = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x00000007
    , _versionName = ChainwebVersionName "testnet04"
    , _versionForks = tabulateHashMap $ \case
        SlowEpoch -> onAllChains testnet04 ForkAtGenesis
        Vuln797Fix -> onAllChains testnet04 ForkAtGenesis
        CoinV2 -> onChains $ concat
            [ [(unsafeChainId i, ForkAtBlockHeight $ BlockHeight 1) | i <- [0..9]]
            , [(unsafeChainId i, ForkAtBlockHeight $ BlockHeight 337_000) | i <- [10..19]]
            ]
        PactBackCompat_v16 -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 0
        ModuleNameFix -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 2
        SkipTxTimingValidation -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 1
        OldTargetGuard -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 0
        SkipFeatureFlagValidation -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 0
        ModuleNameFix2 -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 289_966 -- ~ 2020-07-13
        OldDAGuard -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 318_204 -- ~ 2020-07-23 16:00:00
        PactEvents -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 660_000
        SPVBridge -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 820_000 -- 2021-01-14T17:12:02
        Pact4Coin3 -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 1_261_000  -- 2021-06-17T15:54:14
        EnforceKeysetFormats -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 1_701_000 -- 2021-11-18T17:54:36
        Pact42 -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 1_862_000  -- 2021-06-19T03:34:05
        CheckTxHash -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 1_889_000 -- 2022-01-24T04:19:24
        Chainweb213Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 1_974_556  -- 2022-02-25 00:00:00
        Chainweb214Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 2_134_331  -- 2022-04-21T12:00:00Z
        Chainweb215Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 2_295_437  -- 2022-06-16T12:00:00+00:00
        Pact44NewTrans -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 2_500_369 -- Todo: add date
        Chainweb216Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 2_516_739 -- 2022-09-01 12:00:00+00:00
        Chainweb217Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 2_777_367 -- 2022-12-01 12:00:00+00:00
        Chainweb218Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 3_038_343 -- 2023-03-02 12:00:00+00:00
        Chainweb219Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 3_299_753 -- 2023-06-01 12:00:00+00:00
        Chainweb220Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 3_580_964 -- 2023-09-08 12:00:00+00:00
        Chainweb221Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 3_702_250 -- 2023-10-19 12:00:00+00:00
        Chainweb222Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 3_859_808 -- 2023-12-13 12:00:00+00:00
        Chainweb223Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 4_100_681 -- 2024-03-06 12:00:00+00:00
        Chainweb224Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 4_333_587 -- 2024-05-29 12:00:00+00:00
        Chainweb225Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 4_575_072 -- 2024-08-21 12:00:00+00:00
        Chainweb226Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 4_816_925 -- 2024-11-13 12:00:00+00:00
        Pact5Fork -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 5_058_738       -- 2025-02-05 12:00:00+00:00
        Chainweb228Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 5_155_146 -- 2025-03-11 00:00:00+00:00
        Chainweb229Pact -> onAllChains testnet04 $ ForkAtBlockHeight $ BlockHeight 5_300_466 -- 2025-04-30 12:00:00+00:00
        Chainweb230Pact -> onAllChains testnet04 ForkNever

    , _versionGraphs =
        (to20ChainsTestnet, twentyChainGraph) `Above`
        Bottom (minBound, petersenChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionMaxBlockGasLimit =
        (succ $ testnet04 ^?! versionForks . at Chainweb216Pact . _Just . atChain (unsafeChainId 0) . _ForkAtBlockHeight, Just 180_000) `Above`
        Bottom (minBound, Nothing)
    , _versionBootstraps = domainAddr2PeerInfo testnet04BootstrapHosts
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = ChainMap $ HM.fromList $ concat
            [ [(unsafeChainId i, maxTarget) | i <- [0..9]]
            , [(unsafeChainId i, testnet20InitialHashTarget) | i <- [10..19]]
            ]
        , _genesisTime = onAllChains testnet04 $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
        , _genesisBlockPayload = onChains
            [ (unsafeChainId 0, unsafeFromText "nfYm3e_fk2ICws0Uowos6OMuqfFg5Nrl_zqXVx9v_ZQ")
            , (unsafeChainId 1, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 2, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 3, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 4, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 5, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 6, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 7, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 8, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 9, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 10, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 11, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 12, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 13, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 14, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 15, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 16, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 17, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 18, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
            , (unsafeChainId 19, unsafeFromText "HU-ZhdfsQCiTrfxjtbkr5MHmjoukOt6INqB2vuYiF3g")
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
    , _versionVerifierPluginNames = onAllChains testnet04 $ (4_100_681, Set.fromList $ map VerifierName ["hyperlane_v3_message"]) `Above`
        Bottom (minBound, mempty)
    , _versionQuirks = VersionQuirks
        { _quirkGasFees = onChains
            [ (unsafeChainId 1, HM.fromList [((BlockHeight 4104500, TxBlockIdx 0), Gas 66_239)])
            , (unsafeChainId 2, HM.fromList [((BlockHeight 4108311, TxBlockIdx 0), Gas 65_130)])
            ]
        }
    , _versionServiceDate = Just "2025-07-23T00:00:00Z"
    , _versionPayloadProviderTypes = onAllChains testnet04 PactProvider
    }
