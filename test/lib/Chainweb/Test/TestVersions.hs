{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Chainweb.Test.TestVersions
    ( barebonesTestVersion
    -- , fastForkingCpmTestVersion
    -- , noBridgeCpmTestVersion
    -- , slowForkingCpmTestVersion
    , quirkedGasInstantCpmTestVersion
    , quirkedGasPact5InstantCpmTestVersion
    , timedConsensusVersion
    , instantCpmTestVersion
    , checkpointerTestVersion
    , testVersions
    ) where

import Control.Lens hiding (elements)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Chainweb.BlockHeader.Genesis.InstantTimedCPM0Payload as IN0
import qualified Chainweb.BlockHeader.Genesis.InstantTimedCPM1to9Payload as INN
-- import qualified Chainweb.BlockHeader.Genesis.QuirkedGasPact5InstantTimedCPM0Payload as QPIN0
-- import qualified Chainweb.BlockHeader.Genesis.QuirkedGasPact5InstantTimedCPM1to9Payload as QPINN

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

import Chainweb.Payload (PayloadWithOutputs_(_payloadWithOutputsPayloadHash), PayloadWithOutputs)
import qualified Pact.Core.Names as Pact
import qualified Pact.Core.Gas as Pact

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

type VersionBuilder = HasVersion => ChainwebVersion

-- | Executes a `VersionBuilder` to build a `ChainwebVersion`, by taking its
-- fixed point. Additionally registers it in the global version registry.
buildTestVersion :: VersionBuilder -> ChainwebVersion
buildTestVersion f =
    v
    where
    v = withVersion v f
{-# noinline buildTestVersion #-}

-- | All testing `ChainwebVersion`s *must* have unique names and *must* be
-- included in this list to be assigned a version code, and also registered via
-- `buildTestVersion` into the global version registry. Failure to do so will
-- result in runtime errors from `Chainweb.Version.Registry`.
testVersions :: [ChainwebVersionName]
testVersions = _versionName <$> concat
    -- [   [ fastForkingCpmTestVersion (knownChainGraph g)
    --     | g :: KnownGraph <- [minBound..maxBound]
    --     ]
    -- ,   [ slowForkingCpmTestVersion (knownChainGraph g)
    --     | g :: KnownGraph <- [minBound..maxBound]
    --     ]
    [   [ barebonesTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
        ]
    -- ,   [ noBridgeCpmTestVersion (knownChainGraph g)
    --     | g :: KnownGraph <- [minBound..maxBound]
    --     ]
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
    -- ,   [ pact5InstantCpmTestVersion (knownChainGraph g)
    --     | g :: KnownGraph <- [minBound..maxBound]
    --     ]
    ,   [ checkpointerTestVersion (knownChainGraph g)
        | g :: KnownGraph <- [minBound..maxBound]
        ]
    ]

-- | Details common to all test versions thus far.
-- Using this, a `ChainwebVersion`'s `versionCode` is set to the version's
-- index in `testVersions`, to ensure that all test versions have unique codes
-- in the global version registry in `Chainweb.Version.Registry`.
testVersionTemplate :: VersionBuilder
testVersionTemplate = implicitVersion
    & versionCode .~ ChainwebVersionCode (int (fromJuste $ List.elemIndex (_versionName implicitVersion) testVersions) + 0x80000000)
    & versionHeaderBaseSizeBytes .~ 318 - 110
    & versionWindow .~ WindowWidth 120
    & versionMaxBlockGasLimit .~ Bottom (minBound, Just 2_000_000)
    & versionBootstraps .~ [testBootstrapPeerInfos]
    & versionVerifierPluginNames .~ onAllChains (Bottom (minBound, mempty))
    & versionServiceDate .~ Nothing

-- | A test version without Pact or PoW, with only one chain graph.
barebonesTestVersion :: ChainGraph -> ChainwebVersion
barebonesTestVersion g = buildTestVersion $
    testVersionTemplate
        & versionWindow .~ WindowWidth 120
        & versionBlockDelay .~ BlockDelay 1_000_000
        & versionName .~ ChainwebVersionName ("test-" <> toText g)
        & versionGraphs .~ Bottom (minBound, g)
        & versionCheats .~ VersionCheats
            { _disablePow = True
            , _fakeFirstEpochStart = True
            }
        & versionDefaults .~ VersionDefaults
            { _disableMempoolSync = True
            , _disablePeerValidation = True
            }
        & versionGenesis .~ VersionGenesis
            { _genesisBlockPayload = onAllChains $ _payloadWithOutputsPayloadHash emptyPayload
            , _genesisBlockTarget = onAllChains maxTarget
            , _genesisTime = onAllChains $ BlockCreationTime epoch
            }
        & versionForks .~ HM.fromList [ (f, onAllChains ForkAtGenesis) | f <- [minBound..maxBound] ]
        & versionQuirks .~ noQuirks
        & versionUpgrades .~ onAllChains HM.empty

-- | A test version without Pact or PoW, with a chain graph upgrade at block height 8.
timedConsensusVersion :: ChainGraph -> ChainGraph -> ChainwebVersion
timedConsensusVersion g1 g2 = buildTestVersion $
    testVersionTemplate
    & versionName .~ ChainwebVersionName ("timedConsensus-" <> toText g1 <> "-" <> toText g2)
    & versionBlockDelay .~ BlockDelay 1_000_000
    & versionWindow .~ WindowWidth 120
    & versionForks .~ tabulateHashMap (\case
        SkipTxTimingValidation -> onAllChains $ ForkAtBlockHeight (BlockHeight 2)
        -- pact is disabled, we don't care about pact forks
        _ -> onAllChains ForkAtGenesis
    )
    & versionQuirks .~ noQuirks
    & versionUpgrades .~ onAllChains HM.empty
    & versionGraphs .~ (BlockHeight 8, g2) `Above` Bottom (minBound, g1)
    & versionCheats .~ VersionCheats
        { _disablePow = True
        , _fakeFirstEpochStart = True
        }
    & versionDefaults .~ VersionDefaults
        { _disableMempoolSync = True
        , _disablePeerValidation = True
        }
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $ [] -- TODO: PP
            -- (unsafeChainId 0, TN0.payloadBlock) :
            -- [(n, TNN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` chainIds v)]
        , _genesisBlockTarget = onAllChains maxTarget
        , _genesisTime = onAllChains $ BlockCreationTime epoch
        }

-- | A test version without Pact or PoW.
checkpointerTestVersion :: ChainGraph -> ChainwebVersion
checkpointerTestVersion g1 = buildTestVersion $
    testVersionTemplate
    & versionName .~ ChainwebVersionName ("pact5-checkpointertest-" <> toText g1)
    & versionBlockDelay .~ BlockDelay 1_000_000
    & versionWindow .~ WindowWidth 120
    & versionForks .~ tabulateHashMap (\case
        SkipTxTimingValidation -> onAllChains $ ForkAtBlockHeight (BlockHeight 2)
        -- pact is disabled, we don't care about pact forks
        _ -> onAllChains ForkAtGenesis
    )
    & versionQuirks .~ noQuirks
    & versionUpgrades .~ onAllChains HM.empty
    & versionGraphs .~ Bottom (minBound, g1)
    & versionCheats .~ VersionCheats
        { _disablePow = True
        , _fakeFirstEpochStart = True
        }
    & versionDefaults .~ VersionDefaults
        { _disableMempoolSync = True
        , _disablePeerValidation = True
        }
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains [ (n, _payloadWithOutputsPayloadHash emptyPayload) | n <- HS.toList chainIds ]
        , _genesisBlockTarget = onAllChains maxTarget
        , _genesisTime = onAllChains $ BlockCreationTime epoch
        }
    & versionPayloadProviderTypes .~ onAllChains PactProvider

-- | A family of versions each with Pact enabled and PoW disabled.
cpmTestVersion :: ChainGraph -> VersionBuilder
cpmTestVersion g =
    testVersionTemplate
    & versionWindow .~ WindowWidth 120
    & versionBlockDelay .~ BlockDelay (Micros 100_000)
    & versionGraphs .~ Bottom (minBound, g)
    & versionCheats .~ VersionCheats
        { _disablePow = True
        , _fakeFirstEpochStart = True
        }
    & versionDefaults .~ VersionDefaults
        { _disableMempoolSync = False
        , _disablePeerValidation = True
        }
    & versionUpgrades .~ onAllChains mempty
    & versionPayloadProviderTypes .~ onAllChains PactProvider

-- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled,
-- and with a gas fee quirk.
quirkedGasInstantCpmTestVersion :: ChainGraph -> ChainwebVersion
quirkedGasInstantCpmTestVersion g = buildTestVersion $
    cpmTestVersion g
    & versionName .~ ChainwebVersionName ("quirked-instant-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        _ -> onAllChains ForkAtGenesis)
    & versionQuirks .~ VersionQuirks
        { _quirkGasFees = onChain (unsafeChainId 0)
            $ HM.singleton (BlockHeight 2, TxBlockIdx 0) (Pact.Gas 1)
        }
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $ [] -- TODO: PP
            -- (unsafeChainId 0, IN0.payloadBlock) :
            -- [(n, INN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
        , _genesisBlockTarget = onAllChains maxTarget
        , _genesisTime = onAllChains $ BlockCreationTime epoch
        }
    & versionUpgrades .~ onAllChains mempty
    & versionVerifierPluginNames .~ onAllChains (Bottom (minBound, mempty))

-- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled,
-- and with a gas fee quirk.
quirkedGasPact5InstantCpmTestVersion :: ChainGraph -> ChainwebVersion
quirkedGasPact5InstantCpmTestVersion g = buildTestVersion $
    cpmTestVersion g
    & versionName .~ ChainwebVersionName ("quirked-pact5-instant-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        _ -> onAllChains ForkAtGenesis)
    & versionQuirks .~ VersionQuirks
        { _quirkGasFees = onChain (unsafeChainId 0)
            $ HM.singleton (BlockHeight 1, TxBlockIdx 0) (Pact.Gas 1)
        }
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, _payloadWithOutputsPayloadHash IN0.payloadBlock) :
            [(n, _payloadWithOutputsPayloadHash INN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
        , _genesisBlockTarget = onAllChains maxTarget
        , _genesisTime = onAllChains $ BlockCreationTime epoch
        }
    & versionUpgrades .~ onAllChains mempty
    & versionVerifierPluginNames .~ onAllChains (Bottom (minBound, mempty))

-- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled
-- at genesis EXCEPT Pact 5.
instantCpmTestVersion :: ChainGraph -> ChainwebVersion
instantCpmTestVersion g = buildTestVersion $
    cpmTestVersion g
    & versionName .~ ChainwebVersionName ("instant-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        _ -> onAllChains ForkAtGenesis
        )
    & versionQuirks .~ noQuirks
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, _payloadWithOutputsPayloadHash IN0.payloadBlock) :
            [(n, _payloadWithOutputsPayloadHash INN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
        , _genesisBlockTarget = onAllChains maxTarget
        , _genesisTime = onAllChains $ BlockCreationTime epoch
        }
    & versionUpgrades .~ onAllChains mempty
    & versionVerifierPluginNames .~ onAllChains
        (Bottom
            ( minBound
            , Set.fromList $ map Pact.VerifierName ["allow", "hyperlane_v3_announcement", "hyperlane_v3_message"]
            )
        )

-- -- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled
-- -- at genesis. We also have an upgrade after genesis that redeploys Coin v5 as
-- -- a Pact 5 module.
-- pact5SlowCpmTestVersion :: ChainGraph -> ChainwebVersion
-- pact5SlowCpmTestVersion g = buildTestVersion $ \v -> v
--     & cpmTestVersion g
--     & versionName .~ ChainwebVersionName ("pact5-slow-CPM-" <> toText g)
--     & versionForks .~ tabulateHashMap (\case
--         -- genesis blocks are not ever run with Pact 5
--         Pact5Fork -> onChains [ (cid, ForkAtBlockHeight (succ $ genesisBlockHeight v cid)) | cid <- HS.toList $ graphChainIds g ]
--         -- SPV Bridge is not in effect for Pact 5 yet.
--         SPVBridge -> onAllChains ForkNever
--         _ -> onAllChains ForkAtGenesis
--         )
--     & versionQuirks .~ noQuirks
--     & versionGenesis .~ VersionGenesis
--         { _genesisBlockPayload = onChains $ [] -- TODO: PP
--             -- (unsafeChainId 0, IN0.payloadBlock) :
--             -- [(n, INN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
--         , _genesisBlockTarget = onAllChains maxTarget
--         , _genesisTime = onAllChains $ BlockCreationTime epoch
--         }
--     & versionUpgrades .~ indexByForkHeights v
--         -- TODO: PP
--         -- [ (Pact5Fork, onAllChains (Pact5Upgrade (List.map pactTxFrom4To5 CoinV6.transactions)))
--         [
--         ]
--     & versionVerifierPluginNames .~ onAllChains
--         (Bottom
--             ( minBound
--             , Set.fromList $ map Pact.VerifierName ["allow", "hyperlane_v3_announcement", "hyperlane_v3_message"]
--             )
--         )
