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
    -- , quirkedGasInstantCpmTestVersion
    , quirkedGasPact5InstantCpmTestVersion
    , timedConsensusVersion
    , instantCpmTestVersion
    -- , pact5InstantCpmTestVersion
    -- , pact5SlowCpmTestVersion
    -- , instantCpmTransitionTestVersion
    , pact53TransitionCpmTestVersion
    , checkpointerTestVersion
    ) where

import Control.Lens hiding (elements)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Set as Set
import qualified Chainweb.BlockHeader.Genesis.InstantTimedCPM0Payload as IN0
import qualified Chainweb.BlockHeader.Genesis.InstantTimedCPM1to9Payload as INN
-- import qualified Chainweb.BlockHeader.Genesis.Pact5InstantTimedCPM0Payload as PIN0
-- import qualified Chainweb.BlockHeader.Genesis.Pact5InstantTimedCPM1to9Payload as PINN
import qualified Chainweb.BlockHeader.Genesis.Pact53TransitionTimedCPM0Payload as PIT0
import qualified Chainweb.BlockHeader.Genesis.Pact53TransitionTimedCPM1to9Payload as PITN
-- import qualified Chainweb.BlockHeader.Genesis.QuirkedGasPact5InstantTimedCPM0Payload as QPIN0
-- import qualified Chainweb.BlockHeader.Genesis.QuirkedGasPact5InstantTimedCPM1to9Payload as QPINN
-- import qualified Chainweb.BlockHeader.Genesis.QuirkedGasPact5InstantTimedCPM0Payload as QPIN0
-- import qualified Chainweb.BlockHeader.Genesis.QuirkedGasPact5InstantTimedCPM1to9Payload as QPINN

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
import P2P.Peer

import Chainweb.Pact.Payload(PayloadWithOutputs_(_payloadWithOutputsPayloadHash))
import qualified Pact.Core.Names as Pact
import qualified Pact.Core.Gas as Pact
import qualified Data.List as L
import qualified Chainweb.Pact.Transactions.CoinV3Transactions as CoinV3
import qualified Chainweb.Pact.Transactions.CoinV4Transactions as CoinV4
import qualified Chainweb.Pact.Transactions.CoinV5Transactions as CoinV5
import qualified Chainweb.Pact.Transactions.CoinV6Transactions as CoinV6
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM0Payload as TN0
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM1to9Payload as TNN
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
    , _versionMinimumBlockHeaderHistory = Bottom (minBound, Just 20)
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
        & versionPayloadProviderTypes .~ (MinimalProvider <$ cids)
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
        & versionPayloadProviderTypes .~ (MinimalProvider <$ cids)
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
cpmTestVersion :: ChainGraph -> ChainwebVersion
cpmTestVersion g = withVersion (cpmTestVersion g) $
    testVersionTemplate gs
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
            (unsafeChainId 0, _payloadWithOutputsPayloadHash TN0.payloadBlock) :
            [(n, _payloadWithOutputsPayloadHash TNN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` cids)]
        , _genesisBlockTarget = onAllChains maxTarget
        , _genesisTime = onAllChains $ BlockCreationTime epoch
        }
    & versionUpgrades .~ chainZip HM.union
        (indexByForkHeights
            [ (CoinV2, onAllChains (pact4Upgrade Other.transactions))
            , (Pact4Coin3, onAllChains (Pact4Upgrade CoinV3.transactions True))
            , (Chainweb214Pact, onAllChains (Pact4Upgrade CoinV4.transactions True))
            , (Chainweb215Pact, onAllChains (Pact4Upgrade CoinV5.transactions True))
            , (Chainweb223Pact, onAllChains (pact4Upgrade CoinV6.transactions))
            ])
        (onChains [(unsafeChainId 3, HM.singleton (BlockHeight 2) (Pact4Upgrade MNKAD.transactions False))])
    & versionPayloadProviderTypes .~ (PactProvider <$ ChainMap (HS.toMap cids))
    where
    gs = Bottom (minBound, g)
    cids = graphChainIds $ snd $ ruleHead gs

-- -- | CPM version (see `cpmTestVersion`) with forks and upgrades slowly enabled.
-- slowForkingCpmTestVersion :: ChainGraph -> ChainwebVersion
-- slowForkingCpmTestVersion g = buildTestVersion $ \v -> v
--     & cpmTestVersion g
--     & versionName .~ ChainwebVersionName ("slowfork-CPM-" <> toText g)
--     & versionForks .~ slowForks
--     & versionVerifierPluginNames .~ onAllChains
--         (Bottom (minBound, Set.fromList $ map Pact.VerifierName ["allow", "hyperlane_v3_announcement", "hyperlane_v3_message"]))
--     & versionQuirks .~ noQuirks

-- -- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled,
-- -- and with a gas fee quirk.
-- quirkedGasInstantCpmTestVersion :: ChainGraph -> ChainwebVersion
-- quirkedGasInstantCpmTestVersion g =
--     cpmTestVersion gs
--     & versionName .~ ChainwebVersionName ("quirked-instant-CPM-" <> toText g)
--     & versionForks .~ tabulateHashMap (\case
--         _ -> ForkAtGenesis <$ cids)
--     & versionQuirks .~ VersionQuirks
--         { _quirkGasFees = onChain (unsafeChainId 0)
--             $ HM.singleton (BlockHeight 2, TxBlockIdx 0) (Pact.Gas 1)
--         }
--     & versionGenesis .~ VersionGenesis
--         { _genesisBlockPayload = onChains $
--             (unsafeChainId 0, _payloadWithOutputsPayloadHash IN0.payloadBlock) :
--                 [ (n, _payloadWithOutputsPayloadHash INN.payloadBlock)
--                 | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)
--                 ]
--         , _genesisBlockTarget = maxTarget <$ cids
--         , _genesisTime = BlockCreationTime epoch <$ cids
--         }
--     & versionUpgrades .~ (mempty <$ cids)
--     & versionVerifierPluginNames .~ (Bottom (minBound, mempty) <$ cids)
--     where
--     gs = Bottom (minBound, g)
--     cids = ChainMap $ HS.toMap $ graphChainIds $ snd $ ruleHead gs

-- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled,
-- and with a gas fee quirk.
quirkedGasPact5InstantCpmTestVersion :: ChainGraph -> ChainwebVersion
quirkedGasPact5InstantCpmTestVersion g =
    cpmTestVersion g
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
    cids = ChainMap $ HS.toMap $ graphChainIds g

-- | CPM version (see `cpmTestVersion`) with forks and upgrades instantly enabled
-- at genesis.
instantCpmTestVersion :: ChainGraph -> ChainwebVersion
instantCpmTestVersion g = withVersion (instantCpmTestVersion g) $
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
            , Set.fromList $ map Pact.VerifierName
                ["allow", "hyperlane_v3_announcement", "hyperlane_v3_message","signed_list"]
            )
        )

pact53TransitionCpmTestVersion :: ChainGraph -> ChainwebVersion
pact53TransitionCpmTestVersion g = withVersion (pact53TransitionCpmTestVersion g) $
    cpmTestVersion g
    & versionName .~ ChainwebVersionName ("pact53-transition-CPM-" <> toText g)
    & versionForks .~ tabulateHashMap (\case
        -- SPV Bridge is not in effect for Pact 5 yet.
        SPVBridge -> onAllChains ForkNever

        Chainweb230Pact -> onAllChains $ ForkAtBlockHeight (BlockHeight 5)
        HashedAdjacentRecord -> onAllChains ForkNever
        Chainweb231Pact -> onAllChains ForkNever

        _ -> onAllChains ForkAtGenesis
        )
    & versionQuirks .~ noQuirks
    & versionGenesis .~ VersionGenesis
        { _genesisBlockPayload = onChains $
            (unsafeChainId 0, _payloadWithOutputsPayloadHash PIT0.payloadBlock) :
            [ (n, _payloadWithOutputsPayloadHash PITN.payloadBlock)
            | n <- HS.toList (unsafeChainId 0 `HS.delete` graphChainIds g)
            ]
        , _genesisBlockTarget = onAllChains maxTarget
        , _genesisTime = onAllChains $ BlockCreationTime epoch
        }
    & versionUpgrades .~ onAllChains mempty
    & versionVerifierPluginNames .~ onAllChains
        (Bottom
            ( minBound
            , Set.fromList $ map Pact.VerifierName
                ["allow", "hyperlane_v3_announcement", "hyperlane_v3_message", "signed_list"]
            )
        )
