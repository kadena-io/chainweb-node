{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

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
import qualified Data.List as L

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

-- | Details common to all test versions thus far.
testVersionTemplate :: Rule BlockHeight ChainGraph -> ChainwebVersion
testVersionTemplate gs = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x80000000
    , _versionGraphs = gs
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionWindow = WindowWidth 120
    , _versionMaxBlockGasLimit = Bottom (minBound, Just 2_000_000)
    , _versionBootstraps = [testBootstrapPeerInfos]
    , _versionVerifierPluginNames = (Bottom (minBound, mempty) <$ cids)
    , _versionServiceDate = Nothing
    }
    where cids = ChainMap $ HS.toMap $ graphChainIds $ snd $ ruleHead gs

-- | A test version without Pact or PoW, with only one chain graph.
barebonesTestVersion :: ChainGraph -> ChainwebVersion
barebonesTestVersion g =
    testVersionTemplate gs
        & versionWindow .~ WindowWidth 120
        & versionBlockDelay .~ BlockDelay 1_000_000
        & versionName .~ ChainwebVersionName ("test-" <> toText g)
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
            { _genesisBlockPayload = _payloadWithOutputsPayloadHash emptyPayload <$ cids
            , _genesisBlockTarget = maxTarget <$ cids
            , _genesisTime = BlockCreationTime epoch <$ cids
            }
        & versionForks .~ HM.fromList [ (f, ForkAtGenesis <$ cids) | f <- [minBound..maxBound] ]
        & versionQuirks .~ VersionQuirks
            { _quirkGasFees = HM.empty <$ cids
            }
        & versionUpgrades .~ (HM.empty <$ cids)
    where
    gs = Bottom (minBound, g)
    cids = ChainMap $ HS.toMap $ graphChainIds $ snd $ ruleHead gs

-- | A test version without Pact or PoW, with a chain graph upgrade at block height 8.
timedConsensusVersion :: ChainGraph -> ChainGraph -> ChainwebVersion
timedConsensusVersion g1 g2 =
    testVersionTemplate gs
        & versionName .~ ChainwebVersionName ("timedConsensus-" <> toText g1 <> "-" <> toText g2)
        & versionBlockDelay .~ BlockDelay 1_000_000
        & versionWindow .~ WindowWidth 120
        & versionForks .~ tabulateHashMap (\case
            SkipTxTimingValidation -> ForkAtBlockHeight (BlockHeight 2) <$ cids
            -- pact is disabled, we don't care about pact forks
            _ -> ForkAtGenesis <$ cids
        )
        & versionQuirks .~ VersionQuirks
            { _quirkGasFees = HM.empty <$ cids
            }
        & versionUpgrades .~ (HM.empty <$ cids)
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
                (unsafeChainId 0, _payloadWithOutputsPayloadHash $ IN0.payloadBlock) :
                    [ (n, _payloadWithOutputsPayloadHash INN.payloadBlock)
                    | n <- unsafeChainId 0 `L.delete` fmap fst (itoList cids)
                    ]
            , _genesisBlockTarget = maxTarget <$ cids
            , _genesisTime = BlockCreationTime epoch <$ cids
            }
        & versionPayloadProviderTypes .~ (PactProvider <$ cids)
    where
    gs = (BlockHeight 8, g2) `Above` Bottom (minBound, g1)
    cids = ChainMap $ HS.toMap $ graphChainIds $ snd $ ruleHead gs

-- | A test version without Pact or PoW.
checkpointerTestVersion :: ChainGraph -> ChainwebVersion
checkpointerTestVersion g1 =
    testVersionTemplate gs
        & versionName .~ ChainwebVersionName ("pact5-checkpointertest-" <> toText g1)
        & versionBlockDelay .~ BlockDelay 1_000_000
        & versionWindow .~ WindowWidth 120
        & versionForks .~ tabulateHashMap (\case
            SkipTxTimingValidation -> ForkAtBlockHeight (BlockHeight 2) <$ cids
            -- pact is disabled, we don't care about pact forks
            _ -> ForkAtGenesis <$ cids
        )
        & versionQuirks .~ VersionQuirks
            { _quirkGasFees = HM.empty <$ cids
            }
        & versionUpgrades .~ (HM.empty <$ cids)
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
            { _genesisBlockPayload = onChains
                [ (n, _payloadWithOutputsPayloadHash emptyPayload)
                | n <- fst <$> itoList cids
                ]
            , _genesisBlockTarget = maxTarget <$ cids
            , _genesisTime = BlockCreationTime epoch <$ cids
            }
        & versionPayloadProviderTypes .~ (PactProvider <$ cids)
    where
    gs = Bottom (minBound, g1)
    cids = ChainMap $ HS.toMap $ graphChainIds $ snd $ ruleHead gs

-- | A family of versions each with Pact enabled and PoW disabled.
cpmTestVersion :: Rule BlockHeight ChainGraph -> ChainwebVersion
cpmTestVersion gs =
    testVersionTemplate gs
        & versionWindow .~ WindowWidth 120
        & versionBlockDelay .~ BlockDelay (Micros 100_000)
        & versionCheats .~ VersionCheats
            { _disablePow = True
            , _fakeFirstEpochStart = True
            , _disablePact = False
            }
        & versionDefaults .~ VersionDefaults
            { _disableMempoolSync = False
            , _disablePeerValidation = True
            }
        & versionUpgrades .~ (mempty <$ cids)
        & versionPayloadProviderTypes .~ (PactProvider <$ cids)
    where
    cids = ChainMap $ HS.toMap $ graphChainIds $ snd $ ruleHead gs

-- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled,
-- and with a gas fee quirk.
quirkedGasInstantCpmTestVersion :: ChainGraph -> ChainwebVersion
quirkedGasInstantCpmTestVersion g =
    cpmTestVersion gs
    & versionName .~ ChainwebVersionName ("quirked-instant-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        _ -> ForkAtGenesis <$ cids)
    & versionQuirks .~ VersionQuirks
        { _quirkGasFees = onChain (unsafeChainId 0)
            $ HM.singleton (BlockHeight 2, TxBlockIdx 0) (Pact.Gas 1)
        }
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, _payloadWithOutputsPayloadHash IN0.payloadBlock) :
                [ (n, _payloadWithOutputsPayloadHash INN.payloadBlock)
                | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)
                ]
        , _genesisBlockTarget = maxTarget <$ cids
        , _genesisTime = BlockCreationTime epoch <$ cids
        }
    & versionUpgrades .~ (mempty <$ cids)
    & versionVerifierPluginNames .~ (Bottom (minBound, mempty) <$ cids)
    where
    gs = Bottom (minBound, g)
    cids = ChainMap $ HS.toMap $ graphChainIds $ snd $ ruleHead gs

-- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled,
-- and with a gas fee quirk.
quirkedGasPact5InstantCpmTestVersion :: ChainGraph -> ChainwebVersion
quirkedGasPact5InstantCpmTestVersion g =
    cpmTestVersion gs
    & versionName .~ ChainwebVersionName ("quirked-pact5-instant-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        _ -> ForkAtGenesis <$ cids)
    & versionQuirks .~ VersionQuirks
        { _quirkGasFees = onChain (unsafeChainId 0)
            $ HM.singleton (BlockHeight 1, TxBlockIdx 0) (Pact.Gas 1)
        }
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, _payloadWithOutputsPayloadHash IN0.payloadBlock) :
            [(n, _payloadWithOutputsPayloadHash INN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
        , _genesisBlockTarget = maxTarget <$ cids
        , _genesisTime = BlockCreationTime epoch <$ cids
        }
    & versionUpgrades .~ (mempty <$ cids)
    & versionVerifierPluginNames .~ (Bottom (minBound, mempty) <$ cids)
    where
    gs = Bottom (minBound, g)
    cids = ChainMap $ HS.toMap $ graphChainIds $ snd $ ruleHead gs

-- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled
-- at genesis EXCEPT Pact 5.
instantCpmTestVersion :: ChainGraph -> ChainwebVersion
instantCpmTestVersion g =
    cpmTestVersion gs
    & versionName .~ ChainwebVersionName ("instant-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        _ -> ForkAtGenesis <$ cids
        )
    & versionQuirks .~ VersionQuirks
        { _quirkGasFees = mempty <$ cids
        }
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, _payloadWithOutputsPayloadHash IN0.payloadBlock) :
            [(n, _payloadWithOutputsPayloadHash INN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
        , _genesisBlockTarget = maxTarget <$ cids
        , _genesisTime = BlockCreationTime epoch <$ cids
        }
    & versionUpgrades .~ ChainMap mempty
    & versionVerifierPluginNames .~
        (Bottom
            ( minBound
            , Set.fromList $ map Pact.VerifierName ["allow", "hyperlane_v3_announcement", "hyperlane_v3_message"]
            )
        <$ cids)
    where
    gs = Bottom (minBound, g)
    cids = ChainMap $ HS.toMap $ graphChainIds $ snd $ ruleHead gs

-- -- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled
-- -- at genesis. We also have an upgrade after genesis that redeploys Coin v5 as
-- -- a Pact 5 module.
-- pact5SlowCpmTestVersion :: ChainGraph -> ChainwebVersion
-- pact5SlowCpmTestVersion g =
--     & cpmTestVersion g
--     & versionName .~ ChainwebVersionName ("pact5-slow-CPM-" <> toText g)
--     & versionForks .~ tabulateHashMap (\case
--         -- genesis blocks are not ever run with Pact 5
--         Pact5Fork -> onChains [ (cid, ForkAtBlockHeight (succ $ genesisBlockHeight v cid)) | cid <- HS.toList $ graphChainIds g ]
--         -- SPV Bridge is not in effect for Pact 5 yet.
--         SPVBridge -> ChainMap ForkNever
--         _ -> ChainMap ForkAtGenesis
--         )
--     & versionQuirks .~ noQuirks
--     & versionGenesis .~ VersionGenesis
--         { _genesisBlockPayload = onChains $ [] -- TODO: PP
--             -- (unsafeChainId 0, IN0.payloadBlock) :
--             -- [(n, INN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)]
--         , _genesisBlockTarget = ChainMap maxTarget
--         , _genesisTime = ChainMap $ BlockCreationTime epoch
--         }
--     & versionUpgrades .~ indexByForkHeights v
--         -- TODO: PP
--         -- [ (Pact5Fork, ChainMap (Pact5Upgrade (List.map pactTxFrom4To5 CoinV6.transactions)))
--         [
--         ]
--     & versionVerifierPluginNames .~ ChainMap
--         (Bottom
--             ( minBound
--             , Set.fromList $ map Pact.VerifierName ["allow", "hyperlane_v3_announcement", "hyperlane_v3_message"]
--             )
--         )
