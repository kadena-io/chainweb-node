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

import Pact.Types.Runtime (Gas(..))
import Pact.Types.Verifier

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
import qualified Chainweb.BlockHeader.Genesis.Testnet040Payload as PN0
import qualified Chainweb.BlockHeader.Genesis.Testnet041to19Payload as PNN

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
        SlowEpoch -> AllChains ForkAtGenesis
        Vuln797Fix -> AllChains ForkAtGenesis
        CoinV2 -> onChains $ concat
            [ [(unsafeChainId i, ForkAtBlockHeight $ BlockHeight 1) | i <- [0..9]]
            , [(unsafeChainId i, ForkAtBlockHeight $ BlockHeight 337_000) | i <- [10..19]]
            ]
        PactBackCompat_v16 -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
        ModuleNameFix -> AllChains $ ForkAtBlockHeight $ BlockHeight 2
        SkipTxTimingValidation -> AllChains $ ForkAtBlockHeight $ BlockHeight 1
        OldTargetGuard -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
        SkipFeatureFlagValidation -> AllChains $ ForkAtBlockHeight $ BlockHeight 0
        ModuleNameFix2 -> AllChains $ ForkAtBlockHeight $ BlockHeight 289_966 -- ~ 2020-07-13
        OldDAGuard -> AllChains $ ForkAtBlockHeight $ BlockHeight 318_204 -- ~ 2020-07-23 16:00:00
        PactEvents -> AllChains $ ForkAtBlockHeight $ BlockHeight 660_000
        SPVBridge -> AllChains $ ForkAtBlockHeight $ BlockHeight 820_000 -- 2021-01-14T17:12:02
        Pact4Coin3 -> AllChains $ ForkAtBlockHeight $ BlockHeight 1_261_000  -- 2021-06-17T15:54:14
        EnforceKeysetFormats -> AllChains $ ForkAtBlockHeight $ BlockHeight 1_701_000 -- 2021-11-18T17:54:36
        Pact42 -> AllChains $ ForkAtBlockHeight $ BlockHeight 1_862_000  -- 2021-06-19T03:34:05
        CheckTxHash -> AllChains $ ForkAtBlockHeight $ BlockHeight 1_889_000 -- 2022-01-24T04:19:24
        Chainweb213Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 1_974_556  -- 2022-02-25 00:00:00
        Chainweb214Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 2_134_331  -- 2022-04-21T12:00:00Z
        Chainweb215Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 2_295_437  -- 2022-06-16T12:00:00+00:00
        Pact44NewTrans -> AllChains $ ForkAtBlockHeight $ BlockHeight 2_500_369 -- Todo: add date
        Chainweb216Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 2_516_739 -- 2022-09-01 12:00:00+00:00
        Chainweb217Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 2_777_367 -- 2022-12-01 12:00:00+00:00
        Chainweb218Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 3_038_343 -- 2023-03-02 12:00:00+00:00
        Chainweb219Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 3_299_753 -- 2023-06-01 12:00:00+00:00
        Chainweb220Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 3_580_964 -- 2023-09-08 12:00:00+00:00
        Chainweb221Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 3_702_250 -- 2023-10-19 12:00:00+00:00
        Chainweb222Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 3_859_808 -- 2023-12-13 12:00:00+00:00
        Chainweb223Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 4_100_681 -- 2024-03-06 12:00:00+00:00
        Chainweb224Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 4_333_587 -- 2024-05-29 12:00:00+00:00
        Chainweb225Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 4_575_072 -- 2024-08-21 12:00:00+00:00
        Chainweb226Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 4_816_925 -- 2024-11-13 12:00:00+00:00
        Pact5Fork -> AllChains $ ForkAtBlockHeight $ BlockHeight 5_058_738       -- 2025-02-05 12:00:00+00:00
        Chainweb228Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 5_155_146 -- 2025-03-11 00:00:00+00:00
        Chainweb229Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 5_300_466 -- 2025-04-30 12:00:00+00:00
        Chainweb230Pact -> AllChains ForkNever

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
        { _genesisBlockTarget = OnChains $ HM.fromList $ concat
            [ [(unsafeChainId i, maxTarget) | i <- [0..9]]
            , [(unsafeChainId i, testnet20InitialHashTarget) | i <- [10..19]]
            ]
        , _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
        , _genesisBlockPayload = OnChains $ HM.fromList $ concat
            [ [(unsafeChainId 0, PN0.payloadBlock)]
            , [(unsafeChainId i, PNN.payloadBlock) | i <- [1..19]]
            ]
        }
    , _versionUpgrades = chainZip HM.union
        (indexByForkHeights testnet04
        [ (CoinV2, onChains $
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
        , (Pact4Coin3, AllChains (Pact4Upgrade CoinV3.transactions True))
        , (Chainweb214Pact, AllChains (Pact4Upgrade CoinV4.transactions True))
        , (Chainweb215Pact, AllChains (Pact4Upgrade CoinV5.transactions True))
        , (Chainweb223Pact, AllChains (pact4Upgrade CoinV6.transactions))
        ])
        (onChains [(unsafeChainId 0, HM.singleton to20ChainsTestnet (pact4Upgrade MNKAD.transactions))])
    , _versionCheats = VersionCheats
        { _disablePow = False
        , _fakeFirstEpochStart = False
        , _disablePact = False
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = False
        , _disableMempoolSync = False
        }
    , _versionVerifierPluginNames = AllChains $ (4_100_681, Set.fromList $ map VerifierName ["hyperlane_v3_message"]) `Above`
        Bottom (minBound, mempty)
    , _versionQuirks = VersionQuirks
        { _quirkGasFees = onChains
            [ (unsafeChainId 1, HM.fromList [((BlockHeight 4104500, TxBlockIdx 0), Gas 66_239)])
            , (unsafeChainId 2, HM.fromList [((BlockHeight 4108311, TxBlockIdx 0), Gas 65_130)])
            ]
        }
    , _versionServiceDate = Just "2025-07-23T00:00:00Z"
    }
