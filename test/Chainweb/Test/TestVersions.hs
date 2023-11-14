{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chainweb.Test.TestVersions
    ( barebonesTestVersion
    , fastForkingCpmTestVersion
    , noBridgeCpmTestVersion
    , slowForkingCpmTestVersion
    , timedConsensusVersion
    ) where

import Control.Lens hiding (elements)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as List
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM0Payload as TN0
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM1to9Payload as TNN

import System.IO.Unsafe

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeight
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Pact.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version
import Chainweb.Version.Registry
import P2P.Peer

import qualified Chainweb.Pact.Transactions.CoinV3Transactions as CoinV3
import qualified Chainweb.Pact.Transactions.CoinV4Transactions as CoinV4
import qualified Chainweb.Pact.Transactions.CoinV5Transactions as CoinV5
import qualified Chainweb.Pact.Transactions.MainnetKADTransactions as MNKAD
import qualified Chainweb.Pact.Transactions.OtherTransactions as Other

testBootstrapPeerInfos :: PeerInfo
testBootstrapPeerInfos =
    PeerInfo
#if WITH_ED25519
        { _peerId = Just $ unsafeFromText "BMe2hSdSEGCzLwvoYXPuB1BqYEH5wiV5AvacutSGWmg"
#else
        { _peerId = Just $ unsafeFromText "9LkpIG95q5cs0YJg0d-xdR2YLeW_puv1PjS2kEfmEuQ"
#endif
            -- this is the fingerprint of the certificate and key that is stored
            -- in ./scripts/test-bootstrap-node.config". For programatic use of
            -- the same certificate is also available at
            -- "Chainweb.Test.P2P.Peer.BootstrapConfig". It is intended for
            -- testing purposes only.

        , _peerAddr = HostAddress
            { _hostAddressHost = localhost
            , _hostAddressPort = 1789
            }
        }

type VersionBuilder = ChainwebVersion -> ChainwebVersion

-- | Executes a `VersionBuilder` to build a `ChainwebVersion`, by taking its
-- fixed point. Additionally registers it in the global version registry.
buildTestVersion :: VersionBuilder -> ChainwebVersion
buildTestVersion f =
    unsafeDupablePerformIO (v <$ registerVersion v) & versionName .~ v ^. versionName
    where
    v = f v

-- | All testing `ChainwebVersion`s *must* have unique names and *must* be
-- included in this list to be assigned a version code, and also registered via
-- `buildTestVersion` into the global version registry. Failure to do so will
-- result in runtime errors from `Chainweb.Version.Registry`.
testVersions :: [ChainwebVersionName]
testVersions = _versionName <$> concat
    [ [ fastForkingCpmTestVersion (knownChainGraph g)
      | g :: KnownGraph <- [minBound..maxBound]
      ]
    , [ slowForkingCpmTestVersion (knownChainGraph g)
      | g :: KnownGraph <- [minBound..maxBound]
      ]
    , [ barebonesTestVersion (knownChainGraph g)
      | g :: KnownGraph <- [minBound..maxBound]
      ]
    , [ noBridgeCpmTestVersion (knownChainGraph g)
      | g :: KnownGraph <- [minBound..maxBound]
      ]
    , [ timedConsensusVersion (knownChainGraph g1) (knownChainGraph g2)
      | g1 :: KnownGraph <- [minBound..maxBound]
      , g2 :: KnownGraph <- [minBound..maxBound]
      ]
    ]

-- | Details common to all test versions thus far.
-- Using this, a `ChainwebVersion`'s `versionCode` is set to the version's
-- index in `testVersions`, to ensure that all test versions have unique codes
-- in the global version registry in `Chainweb.Version.Registry`.
testVersionTemplate :: VersionBuilder
testVersionTemplate v = v
    & versionCode .~ ChainwebVersionCode (int (fromJuste $ List.elemIndex (_versionName v) testVersions) + 0x80000000)
    & versionHeaderBaseSizeBytes .~ 318 - 110
    & versionWindow .~ WindowWidth 120
    & versionMaxBlockGasLimit .~ End (Just 2_000_000)
    & versionBootstraps .~ [testBootstrapPeerInfos]
    & versionVerifierPlugins .~ AllChains (End mempty)

-- | A set of fork heights which are relatively fast, but not fast enough to break anything.
fastForks :: HashMap Fork (ChainMap ForkHeight)
fastForks = tabulateHashMap $ \case
    SlowEpoch -> AllChains ForkAtGenesis
    OldTargetGuard -> AllChains ForkAtGenesis
    SkipFeatureFlagValidation -> AllChains ForkAtGenesis
    OldDAGuard -> AllChains ForkAtGenesis
    Vuln797Fix -> AllChains ForkAtGenesis
    PactBackCompat_v16 -> AllChains ForkAtGenesis
    SPVBridge -> AllChains ForkAtGenesis
    EnforceKeysetFormats -> AllChains ForkAtGenesis
    CheckTxHash -> AllChains ForkAtGenesis
    Pact44NewTrans -> AllChains ForkAtGenesis
    Chainweb213Pact -> AllChains ForkAtGenesis
    PactEvents -> AllChains ForkAtGenesis
    CoinV2 -> AllChains $ ForkAtBlockHeight $ BlockHeight 1
    Pact42 -> AllChains $ ForkAtBlockHeight $ BlockHeight 1
    SkipTxTimingValidation -> AllChains $ ForkAtBlockHeight $ BlockHeight 2
    ModuleNameFix -> AllChains $ ForkAtBlockHeight $ BlockHeight 2
    ModuleNameFix2 -> AllChains $ ForkAtBlockHeight $ BlockHeight 2
    Pact4Coin3 -> AllChains $ ForkAtBlockHeight $ BlockHeight 4
    Chainweb214Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 5
    Chainweb215Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 10
    Chainweb216Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 11
    Chainweb217Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 20
    Chainweb218Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 20
    Chainweb219Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 27
    Chainweb220Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 30
    Chainweb221Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 33
    Chainweb222Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 36
    Chainweb223Pact -> AllChains ForkNever
    EnableVerifiers -> AllChains $ ForkAtBlockHeight $ BlockHeight 35
    Hyperlane -> AllChains ForkNever

-- | A test version without Pact or PoW, with only one chain graph.
barebonesTestVersion :: ChainGraph -> ChainwebVersion
barebonesTestVersion g = buildTestVersion $ \v ->
    testVersionTemplate v
        & versionWindow .~ WindowWidth 120
        & versionBlockDelay .~ BlockDelay 1_000_000
        & versionName .~ ChainwebVersionName ("test-" <> toText g)
        & versionGraphs .~ End g
        & versionCheats .~ VersionCheats
            { _disablePow = True
            , _fakeFirstEpochStart = True
            , _disablePact = True
            }
        & versionDefaults .~ VersionDefaults
            { _disableMempoolSync = True
            , _disablePeerValidation = True
            }
        & versionGenesis .~ VersionGenesis
            { _genesisBlockPayload = AllChains emptyPayload
            , _genesisBlockTarget = AllChains maxTarget
            , _genesisTime = AllChains $ BlockCreationTime epoch
            }
        & versionForks .~ HM.fromList [ (f, AllChains ForkAtGenesis) | f <- [minBound..maxBound] ]
        & versionUpgrades .~ AllChains HM.empty

-- | A test version without Pact or PoW, with a chain graph upgrade at block height 8.
timedConsensusVersion :: ChainGraph -> ChainGraph -> ChainwebVersion
timedConsensusVersion g1 g2 = buildTestVersion $ \v -> v
    & testVersionTemplate
    & versionName .~ ChainwebVersionName ("timedConsensus-" <> toText g1 <> "-" <> toText g2)
    & versionBlockDelay .~ BlockDelay 1_000_000
    & versionWindow .~ WindowWidth 120
    & versionForks .~ tabulateHashMap (\case
        SkipTxTimingValidation -> AllChains $ ForkAtBlockHeight (BlockHeight 2)
        -- pact is disabled, we don't care about pact forks
        _ -> AllChains ForkAtGenesis
    )
    & versionUpgrades .~ AllChains HM.empty
    & versionGraphs .~ (BlockHeight 8, g2) `Above` (End g1)
    & versionCheats .~ VersionCheats
        { _disablePow = True
        , _fakeFirstEpochStart = True
        , _disablePact = True
        }
    & versionDefaults .~ VersionDefaults
        { _disableMempoolSync = True
        , _disablePeerValidation = True
        }
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, TN0.payloadBlock) :
            [(n, TNN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` chainIds v)]
        , _genesisBlockTarget = AllChains maxTarget
        , _genesisTime = AllChains $ BlockCreationTime epoch
        }

-- | A family of versions each with Pact enabled and PoW disabled.
cpmTestVersion :: ChainGraph -> VersionBuilder
cpmTestVersion g v = v
    & testVersionTemplate
    & versionWindow .~ WindowWidth 120
    & versionBlockDelay .~ BlockDelay (Micros 100_000)
    & versionGraphs .~ End g
    & versionCheats .~ VersionCheats
        { _disablePow = True
        , _fakeFirstEpochStart = True
        , _disablePact = False
        }
    & versionDefaults .~ VersionDefaults
        { _disableMempoolSync = False
        , _disablePeerValidation = True
        }
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, TN0.payloadBlock) :
            [(n, TNN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` chainIds v)]
        , _genesisBlockTarget = AllChains maxTarget
        , _genesisTime = AllChains $ BlockCreationTime epoch
        }
    & versionUpgrades .~ chainZip HM.union
        (forkUpgrades v
            [ (CoinV2, AllChains (upgrade Other.transactions))
            , (Pact4Coin3, AllChains (Upgrade CoinV3.transactions True))
            , (Chainweb214Pact, AllChains (Upgrade CoinV4.transactions True))
            , (Chainweb215Pact, AllChains (Upgrade CoinV5.transactions True))
            ])
        (onChains [(unsafeChainId 3, HM.singleton (BlockHeight 2) (Upgrade MNKAD.transactions False))])

-- | CPM version (see `cpmTestVersion`) with forks and upgrades slowly enabled.
slowForkingCpmTestVersion :: ChainGraph -> ChainwebVersion
slowForkingCpmTestVersion g = buildTestVersion $ \v -> v
    & cpmTestVersion g
    & versionName .~ ChainwebVersionName ("slowfork-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap \case
        SlowEpoch -> AllChains ForkAtGenesis
        OldTargetGuard -> AllChains ForkAtGenesis
        SkipFeatureFlagValidation -> AllChains ForkAtGenesis
        OldDAGuard -> AllChains ForkAtGenesis
        Vuln797Fix -> AllChains ForkAtGenesis
        PactBackCompat_v16 -> AllChains ForkAtGenesis
        SPVBridge -> AllChains ForkAtGenesis
        Pact44NewTrans -> AllChains ForkAtGenesis
        CoinV2 -> AllChains $ ForkAtBlockHeight (BlockHeight 1)
        SkipTxTimingValidation -> AllChains $ ForkAtBlockHeight (BlockHeight 2)
        ModuleNameFix -> AllChains $ ForkAtBlockHeight (BlockHeight 2)
        ModuleNameFix2 -> AllChains $ ForkAtBlockHeight (BlockHeight 2)
        Pact42 -> AllChains $ ForkAtBlockHeight (BlockHeight 5)
        CheckTxHash -> AllChains $ ForkAtBlockHeight (BlockHeight 7)
        EnforceKeysetFormats -> AllChains $ ForkAtBlockHeight (BlockHeight 10)
        PactEvents -> AllChains $ ForkAtBlockHeight (BlockHeight 10)
        Pact4Coin3 -> AllChains $ ForkAtBlockHeight (BlockHeight 20)
        Chainweb213Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 26)
        Chainweb214Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 30)
        Chainweb215Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 35)
        Chainweb216Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 53)
        Chainweb217Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 55)
        Chainweb218Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 60)
        Chainweb219Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 71)
        Chainweb220Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 85)
        Chainweb221Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 100)
        Chainweb222Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 115)
        Chainweb223Pact -> AllChains ForkNever
        EnableVerifiers -> AllChains $ ForkAtBlockHeight (BlockHeight 110)
        Hyperlane -> AllChains ForkNever

-- | CPM version (see `cpmTestVersion`) with forks and upgrades quickly enabled.
fastForkingCpmTestVersion :: ChainGraph -> ChainwebVersion
fastForkingCpmTestVersion g = buildTestVersion $ \v -> v
    & cpmTestVersion g
    & versionName .~ ChainwebVersionName ("fastfork-CPM-" <> toText g)
    & versionForks .~ fastForks

-- | CPM version (see `cpmTestVersion`) with forks and upgrades quickly enabled
-- but with no SPV bridge.
noBridgeCpmTestVersion :: ChainGraph -> ChainwebVersion
noBridgeCpmTestVersion g = buildTestVersion $ \v -> v
    & cpmTestVersion g
    & versionName .~ ChainwebVersionName ("nobridge-CPM-" <> toText g)
    & versionForks .~ (fastForks & at SPVBridge ?~ AllChains ForkNever)
