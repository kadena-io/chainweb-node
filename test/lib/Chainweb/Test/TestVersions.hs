{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.TestVersions
    ( barebonesTestVersion
    , fastForkingCpmTestVersion
    , noBridgeCpmTestVersion
    , slowForkingCpmTestVersion
    , quirkedGasInstantCpmTestVersion
    , quirkedGasPact5InstantCpmTestVersion
    , timedConsensusVersion
    , instantCpmTestVersion
    , pact5InstantCpmTestVersion
    , pact5CheckpointerTestVersion
    , pact5SlowCpmTestVersion
    , instantCpmTransitionTestVersion
    , pact53TransitionCpmTestVersion
    ) where

import Control.Lens hiding (elements)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM0Payload as TN0
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM1to9Payload as TNN
import qualified Chainweb.BlockHeader.Genesis.InstantTimedCPM0Payload as IN0
import qualified Chainweb.BlockHeader.Genesis.InstantTimedCPM1to9Payload as INN
import qualified Chainweb.BlockHeader.Genesis.Pact5InstantTimedCPM0Payload as PIN0
import qualified Chainweb.BlockHeader.Genesis.Pact5InstantTimedCPM1to9Payload as PINN
import qualified Chainweb.BlockHeader.Genesis.Pact53TransitionTimedCPM0Payload as PIT0
import qualified Chainweb.BlockHeader.Genesis.Pact53TransitionTimedCPM1to9Payload as PITN
import qualified Chainweb.BlockHeader.Genesis.QuirkedGasPact5InstantTimedCPM0Payload as QPIN0
import qualified Chainweb.BlockHeader.Genesis.QuirkedGasPact5InstantTimedCPM1to9Payload as QPINN

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

import qualified Pact.Types.Gas as P
import Chainweb.Test.Pact5.Utils (pactTxFrom4To5)

import Pact.Types.Verifier

import qualified Chainweb.Pact.Transactions.CoinV3Transactions as CoinV3
import qualified Chainweb.Pact.Transactions.CoinV4Transactions as CoinV4
import qualified Chainweb.Pact.Transactions.CoinV5Transactions as CoinV5
import qualified Chainweb.Pact.Transactions.CoinV6Transactions as CoinV6
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
    unsafePerformIO (v <$ registerVersion v) & versionName .~ v ^. versionName
    where
    v = f v
{-# noinline buildTestVersion #-}

-- | All testing `ChainwebVersion`s *must* have unique names and *must* be
-- included in this list to be assigned a version code, and also registered via
-- `buildTestVersion` into the global version registry. Failure to do so will
-- result in runtime errors from `Chainweb.Version.Registry`.
testVersions :: [ChainwebVersionName]
testVersions = _versionName <$> concat
    [   [ fastForkingCpmTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
        ]
    ,   [ slowForkingCpmTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
        ]
    ,   [ barebonesTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
        ]
    ,   [ noBridgeCpmTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
        ]
    ,   [ timedConsensusVersion (knownChainGraph g1) (knownChainGraph g2)
        | g1 :: KnownGraph <- [minBound..maxBound]
        , g2 :: KnownGraph <- [minBound..maxBound]
        ]
    ,   [ quirkedGasInstantCpmTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
        ]
    ,   [ quirkedGasPact5InstantCpmTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
        ]
    ,   [ instantCpmTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
        ]
    ,   [ pact5InstantCpmTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
        ]
    ,   [ pact5CheckpointerTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
        ]
    ,   [ pact5SlowCpmTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
        ]
    ,   [ instantCpmTransitionTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
        ]
    ,   [ pact53TransitionCpmTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
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
    & versionMaxBlockGasLimit .~ Bottom (minBound, Just 2_000_000)
    & versionMinimumBlockHeaderHistory .~ Bottom (minBound, Just 20)
    & versionBootstraps .~ [testBootstrapPeerInfos]
    & versionVerifierPluginNames .~ AllChains (Bottom (minBound, mempty))
    & versionForkNumber .~ 0

-- | A test version without Pact or PoW, with only one chain graph.
barebonesTestVersion :: ChainGraph -> ChainwebVersion
barebonesTestVersion g = buildTestVersion $ \v ->
    testVersionTemplate v
        & versionWindow .~ WindowWidth 120
        & versionBlockDelay .~ BlockDelay 1_000_000
        & versionName .~ ChainwebVersionName ("test-" <> toText g)
        & versionGraphs .~ Bottom (minBound, g)
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
        & versionQuirks .~ noQuirks
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
    & versionQuirks .~ noQuirks
    & versionUpgrades .~ AllChains HM.empty
    & versionGraphs .~ (BlockHeight 8, g2) `Above` Bottom (minBound, g1)
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

-- | A test version without Pact or PoW.
pact5CheckpointerTestVersion :: ChainGraph -> ChainwebVersion
pact5CheckpointerTestVersion g1 = buildTestVersion $ \v -> v
    & testVersionTemplate
    & versionName .~ ChainwebVersionName ("pact5-checkpointertest-" <> toText g1)
    & versionBlockDelay .~ BlockDelay 1_000_000
    & versionWindow .~ WindowWidth 120
    & versionForks .~ tabulateHashMap (\case
        SkipTxTimingValidation -> AllChains $ ForkAtBlockHeight (BlockHeight 2)
        -- pact is disabled, we don't care about pact forks
        _ -> AllChains ForkAtGenesis
    )
    & versionQuirks .~ noQuirks
    & versionUpgrades .~ AllChains HM.empty
    & versionGraphs .~ Bottom (minBound, g1)
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
        { _genesisBlockPayload = onChains [ (n, emptyPayload) | n <- HS.toList (chainIds v) ]
        , _genesisBlockTarget = AllChains maxTarget
        , _genesisTime = AllChains $ BlockCreationTime epoch
        }

-- | A family of versions each with Pact enabled and PoW disabled.
cpmTestVersion :: ChainGraph -> VersionBuilder
cpmTestVersion g v = v
    & testVersionTemplate
    & versionWindow .~ WindowWidth 120
    & versionBlockDelay .~ BlockDelay (Micros 100_000)
    & versionGraphs .~ Bottom (minBound, g)
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
        (indexByForkHeights v
            [ (CoinV2, AllChains (pact4Upgrade Other.transactions))
            , (Pact4Coin3, AllChains (Pact4Upgrade CoinV3.transactions True))
            , (Chainweb214Pact, AllChains (Pact4Upgrade CoinV4.transactions True))
            , (Chainweb215Pact, AllChains (Pact4Upgrade CoinV5.transactions True))
            , (Chainweb223Pact, AllChains (pact4Upgrade CoinV6.transactions))
            ])
        (onChains [(unsafeChainId 3, HM.singleton (BlockHeight 2) (Pact4Upgrade MNKAD.transactions False))])

slowForks :: HashMap Fork (ChainMap ForkHeight)
slowForks = tabulateHashMap \case
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
    Chainweb223Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 120)
    Chainweb224Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 125)
    Chainweb225Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 130)
    Chainweb226Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 135)
    Pact5Fork -> AllChains ForkNever
    Chainweb228Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 145)
    Chainweb229Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 150)
    Chainweb230Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 155)
    Chainweb231Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 160)
    Chainweb232Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 165)

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
    Chainweb223Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 38
    Chainweb224Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 40
    Chainweb225Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 42
    Chainweb226Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 44
    Pact5Fork -> AllChains $ ForkAtBlockHeight $ BlockHeight 46
    Chainweb228Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 48
    Chainweb229Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 50
    Chainweb230Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 52
    Chainweb231Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 54
    Chainweb232Pact -> AllChains $ ForkAtBlockHeight $ BlockHeight 56

-- | CPM version (see `cpmTestVersion`) with forks and upgrades slowly enabled.
slowForkingCpmTestVersion :: ChainGraph -> ChainwebVersion
slowForkingCpmTestVersion g = buildTestVersion $ \v -> v
    & cpmTestVersion g
    & versionName .~ ChainwebVersionName ("slowfork-CPM-" <> toText g)
    & versionForks .~ slowForks
    & versionVerifierPluginNames .~ AllChains
        (Bottom (minBound, Set.fromList $ map VerifierName ["allow", "hyperlane_v3_announcement", "hyperlane_v3_message"]))
    & versionQuirks .~ noQuirks

-- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled,
-- and with a gas fee quirk.
quirkedGasInstantCpmTestVersion :: ChainGraph -> ChainwebVersion
quirkedGasInstantCpmTestVersion g = buildTestVersion $ \v -> v
    & cpmTestVersion g
    & versionName .~ ChainwebVersionName ("quirked-instant-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        Pact5Fork -> AllChains ForkNever
        _ -> AllChains ForkAtGenesis)
    & versionQuirks .~ VersionQuirks
        { _quirkGasFees = onChain (unsafeChainId 0)
            $ HM.singleton (BlockHeight 2, TxBlockIdx 0) (P.Gas 1)
        }
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, IN0.payloadBlock) :
            [(n, INN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
        , _genesisBlockTarget = AllChains maxTarget
        , _genesisTime = AllChains $ BlockCreationTime epoch
        }
    & versionUpgrades .~ AllChains mempty
    & versionVerifierPluginNames .~ AllChains (Bottom (minBound, mempty))

-- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled,
-- and with a gas fee quirk.
quirkedGasPact5InstantCpmTestVersion :: ChainGraph -> ChainwebVersion
quirkedGasPact5InstantCpmTestVersion g = buildTestVersion $ \v -> v
    & cpmTestVersion g
    & versionName .~ ChainwebVersionName ("quirked-pact5-instant-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        _ -> AllChains ForkAtGenesis)
    & versionQuirks .~ VersionQuirks
        { _quirkGasFees = onChain (unsafeChainId 0)
            $ HM.singleton (BlockHeight 1, TxBlockIdx 0) (P.Gas 1)
        }
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, QPIN0.payloadBlock) :
            [(n, QPINN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
        , _genesisBlockTarget = AllChains maxTarget
        , _genesisTime = AllChains $ BlockCreationTime epoch
        }
    & versionUpgrades .~ AllChains mempty
    & versionVerifierPluginNames .~ AllChains (Bottom (minBound, mempty))

-- | CPM version (see `cpmTestVersion`) with forks and upgrades quickly enabled.
fastForkingCpmTestVersion :: ChainGraph -> ChainwebVersion
fastForkingCpmTestVersion g = buildTestVersion $ \v -> v
    & cpmTestVersion g
    & versionName .~ ChainwebVersionName ("fastfork-CPM-" <> toText g)
    & versionForks .~ fastForks
    & versionQuirks .~ noQuirks

-- | CPM version (see `cpmTestVersion`) with forks and upgrades quickly enabled
-- but with no SPV bridge.
noBridgeCpmTestVersion :: ChainGraph -> ChainwebVersion
noBridgeCpmTestVersion g = buildTestVersion $ \v -> v
    & cpmTestVersion g
    & versionName .~ ChainwebVersionName ("nobridge-CPM-" <> toText g)
    & versionForks .~ (fastForks & at SPVBridge ?~ AllChains ForkNever)
    & versionQuirks .~ noQuirks

-- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled
-- at genesis EXCEPT Pact 5.
instantCpmTestVersion :: ChainGraph -> ChainwebVersion
instantCpmTestVersion g = buildTestVersion $ \v -> v
    & cpmTestVersion g
    & versionName .~ ChainwebVersionName ("instant-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        -- pact 5 is off
        Pact5Fork -> AllChains ForkNever
        _ -> AllChains ForkAtGenesis
        )
    & versionQuirks .~ noQuirks
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, IN0.payloadBlock) :
            [(n, INN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
        , _genesisBlockTarget = AllChains maxTarget
        , _genesisTime = AllChains $ BlockCreationTime epoch
        }
    & versionUpgrades .~ AllChains mempty
    & versionVerifierPluginNames .~ AllChains
        (Bottom
            ( minBound
            , Set.fromList $ map VerifierName ["allow", "hyperlane_v3_announcement", "hyperlane_v3_message"]
            )
        )

pact5InstantCpmTestVersion :: ChainGraph -> ChainwebVersion
pact5InstantCpmTestVersion g = buildTestVersion $ \v -> v
    & cpmTestVersion g
    & versionName .~ ChainwebVersionName ("instant-pact5-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        -- SPV Bridge is not in effect for Pact 5 yet.
        SPVBridge -> AllChains ForkNever

        _ -> AllChains ForkAtGenesis
        )
    & versionQuirks .~ noQuirks
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, PIN0.payloadBlock) :
            [(n, PINN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
        , _genesisBlockTarget = AllChains maxTarget
        , _genesisTime = AllChains $ BlockCreationTime epoch
        }
    & versionUpgrades .~ AllChains mempty
    & versionVerifierPluginNames .~ AllChains
        (Bottom
            ( minBound
            , Set.fromList $ map VerifierName ["allow", "hyperlane_v3_announcement", "hyperlane_v3_message","signed_list"]
            )
        )

pact53TransitionCpmTestVersion :: ChainGraph -> ChainwebVersion
pact53TransitionCpmTestVersion g = buildTestVersion $ \v -> v
    & cpmTestVersion g
    & versionName .~ ChainwebVersionName ("pact53-transition-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        -- SPV Bridge is not in effect for Pact 5 yet.
        SPVBridge -> AllChains ForkNever

        Chainweb230Pact -> AllChains $ ForkAtBlockHeight (BlockHeight 5)

        _ -> AllChains ForkAtGenesis
        )
    & versionQuirks .~ noQuirks
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, PIT0.payloadBlock) :
            [(n, PITN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
        , _genesisBlockTarget = AllChains maxTarget
        , _genesisTime = AllChains $ BlockCreationTime epoch
        }
    & versionUpgrades .~ AllChains mempty
    & versionVerifierPluginNames .~ AllChains
        (Bottom
            ( minBound
            , Set.fromList $ map VerifierName ["allow", "hyperlane_v3_announcement", "hyperlane_v3_message"]
            )
        )

-- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled
-- at genesis. We also have an upgrade after genesis that redeploys Coin v5 as
-- a Pact 5 module.
pact5SlowCpmTestVersion :: ChainGraph -> ChainwebVersion
pact5SlowCpmTestVersion g = buildTestVersion $ \v -> v
    & cpmTestVersion g
    & versionName .~ ChainwebVersionName ("pact5-slow-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        -- genesis blocks are not ever run with Pact 5
        Pact5Fork -> onChains [ (cid, ForkAtBlockHeight (succ $ genesisBlockHeight v cid)) | cid <- HS.toList $ graphChainIds g ]
        -- SPV Bridge is not in effect for Pact 5 yet.
        SPVBridge -> AllChains ForkNever
        _ -> AllChains ForkAtGenesis
        )
    & versionQuirks .~ noQuirks
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, IN0.payloadBlock) :
            [(n, INN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
        , _genesisBlockTarget = AllChains maxTarget
        , _genesisTime = AllChains $ BlockCreationTime epoch
        }
    & versionUpgrades .~ indexByForkHeights v
        [ (Pact5Fork, AllChains (Pact5Upgrade (List.map pactTxFrom4To5 CoinV6.transactions)))
        ]
    & versionVerifierPluginNames .~ AllChains
        (Bottom
            ( minBound
            , Set.fromList $ map VerifierName ["allow", "hyperlane_v3_announcement", "hyperlane_v3_message"]
            )
        )

-- | ChainwebVersion that transitions between Pact4 and Pact5 at block height 20.
instantCpmTransitionTestVersion :: ChainGraph -> ChainwebVersion
instantCpmTransitionTestVersion g = buildTestVersion $ \v -> v
    & cpmTestVersion g
    & versionName .~ ChainwebVersionName ("instant-CPM-transition-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        -- pact 5 is off until here
        Pact5Fork -> AllChains $ ForkAtBlockHeight $ BlockHeight 20

        -- SPV Bridge is not in effect for Pact 5 yet.
        SPVBridge -> AllChains ForkNever

        _ -> AllChains ForkAtGenesis
        )
    & versionQuirks .~ noQuirks
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, IN0.payloadBlock) :
            [(n, INN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
        , _genesisBlockTarget = AllChains maxTarget
        , _genesisTime = AllChains $ BlockCreationTime epoch
        }
    & versionUpgrades .~ AllChains mempty
    & versionVerifierPluginNames .~ AllChains
        (Bottom
            ( minBound
            , Set.fromList $ map VerifierName ["allow", "hyperlane_v3_announcement", "hyperlane_v3_message"]
            )
        )
