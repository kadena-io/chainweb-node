{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chainweb.Test.TestVersions
    ( legalizeTestVersion
    , barebonesTestVersion
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
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPMNPayload as TNN

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

legalizeTestVersion :: VersionBuilder -> ChainwebVersion
legalizeTestVersion f = unsafePerformIO $ do
    let v = f v
    registerVersion v
    return v

-- | All chainweb versions used in tests *must* be included in this list to be assigned
-- a version code, and also registered via legalizeTestVersion into the version registry.
-- Failure to do so will result in runtime errors.
testRegistry :: [ChainwebVersionName]
testRegistry = concat
    [ [ _versionName $ fastForkingCpmTestVersion' (knownChainGraph g) undefined
      | g :: KnownGraph <- [minBound..maxBound]
      ]
    , [ _versionName $ slowForkingCpmTestVersion' (knownChainGraph g) undefined
      | g :: KnownGraph <- [minBound..maxBound]
      ]
    , [ _versionName $ barebonesTestVersion' (knownChainGraph g) undefined
      | g :: KnownGraph <- [minBound..maxBound]
      ]
    , [ _versionName $ noBridgeCpmTestVersion' (knownChainGraph g) undefined
      | g :: KnownGraph <- [minBound..maxBound]
      ]
    , [ _versionName $ timedConsensusVersion' (knownChainGraph g1) (knownChainGraph g2) undefined
      | g1 :: KnownGraph <- [minBound..maxBound]
      , g2 :: KnownGraph <- [minBound..maxBound]
      ]
    ]

testVersionTemplate :: VersionBuilder
testVersionTemplate v = v
    & versionCode .~ ChainwebVersionCode (int (fromJuste $ List.findIndex (\vn -> vn == _versionName v) testRegistry) + 0x80000000)
    & versionHeaderBaseSizeBytes .~ 318 - 110
    & versionWindow .~ Nothing
    & versionFakeFirstEpochStart .~ False -- DA is already disabled with _versionWindow = Nothing
    & versionMaxBlockGasLimit .~ End (Just 2_000_000)
    & versionBootstraps .~ [testBootstrapPeerInfos]

fastForks :: HashMap Fork (ChainMap BlockHeight)
fastForks = tabulateHashMap $ \case
    Pact420 -> AllChains (BlockHeight 0)
    SlowEpoch -> AllChains (BlockHeight 0)
    OldTargetGuard -> AllChains (BlockHeight 0)
    SkipFeatureFlagValidation -> AllChains (BlockHeight 0)
    OldDAGuard -> AllChains (BlockHeight 0)
    Vuln797Fix -> AllChains (BlockHeight 0)
    PactBackCompat_v16 -> AllChains (BlockHeight 0)
    SPVBridge -> AllChains (BlockHeight 0)
    EnforceKeysetFormats -> AllChains (BlockHeight 0)
    CheckTxHash -> AllChains (BlockHeight 0)
    Pact44NewTrans -> AllChains (BlockHeight 0)
    Chainweb213Pact -> AllChains (BlockHeight 0)
    PactEvents -> AllChains (BlockHeight 0)
    CoinV2 -> AllChains (BlockHeight 1)
    SkipTxTimingValidation -> AllChains (BlockHeight 2)
    ModuleNameFix -> AllChains (BlockHeight 2)
    ModuleNameFix2 -> AllChains (BlockHeight 2)
    Pact4Coin3 -> AllChains (BlockHeight 4)
    Chainweb214Pact -> AllChains (BlockHeight 5)
    Chainweb215Pact -> AllChains (BlockHeight 10)
    Chainweb216Pact -> AllChains (BlockHeight 16)
    Chainweb217Pact -> AllChains (BlockHeight 20)
    Chainweb218Pact -> AllChains (BlockHeight 21)

barebonesTestVersion :: ChainGraph -> ChainwebVersion
barebonesTestVersion g = legalizeTestVersion (barebonesTestVersion' g)

barebonesTestVersion' :: ChainGraph -> VersionBuilder
barebonesTestVersion' g v =
    testVersionTemplate v
        & versionWindow .~ Nothing
        & versionBlockRate .~ BlockRate 0
        & versionName .~ ChainwebVersionName ("test-" <> toText g)
        & versionGraphs .~ End g
        & versionCheats .~ Cheats
            { _disablePow = False -- PoW is effectively disabled with _versionWindow = Nothing
            , _disablePact = True
            , _disableMempool = True
            , _disablePeerValidation = True
            }
        & versionGenesis .~ ChainwebGenesis
            { _genesisBlockPayload = AllChains emptyPayload
            , _genesisBlockTarget = AllChains maxTarget
            , _genesisTime = AllChains $ BlockCreationTime epoch
            }
        & versionForks .~ fastForks
        & versionUpgrades .~ forkUpgrades v
            [ (CoinV2, AllChains (upgrade Other.transactions))
            , (Pact4Coin3, AllChains (upgrade CoinV3.transactions))
            , (Chainweb214Pact, AllChains (upgrade CoinV4.transactions))
            , (Chainweb215Pact, AllChains (upgrade CoinV5.transactions))
            ]

cpmTestVersion :: ChainGraph -> VersionBuilder
cpmTestVersion g v = v
    & versionWindow .~ Nothing
    & versionBlockRate .~ BlockRate (Micros 100_000)
    & versionGraphs .~ End g
    & versionCheats .~ Cheats
        { _disablePow = False -- PoW is effectively disabled with _versionWindow = Nothing
        , _disablePact = False
        , _disableMempool = False
        , _disablePeerValidation = True
        }
    & versionGenesis .~ ChainwebGenesis
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

slowForkingCpmTestVersion :: ChainGraph -> ChainwebVersion
slowForkingCpmTestVersion g = legalizeTestVersion (slowForkingCpmTestVersion' g)

slowForkingCpmTestVersion' :: ChainGraph -> VersionBuilder
slowForkingCpmTestVersion' g v =
    cpmTestVersion g (testVersionTemplate v)
        & versionName .~ ChainwebVersionName ("slowfork-CPM-" <> toText g)
        & versionForks .~ HM.fromList
            [ (SlowEpoch, AllChains (BlockHeight 0))
            , (OldTargetGuard, AllChains (BlockHeight 0))
            , (SkipFeatureFlagValidation, AllChains (BlockHeight 0))
            , (OldDAGuard, AllChains (BlockHeight 0))
            , (Vuln797Fix, AllChains (BlockHeight 0))
            , (PactBackCompat_v16, AllChains (BlockHeight 0))
            , (SPVBridge, AllChains (BlockHeight 0))
            , (Pact44NewTrans, AllChains (BlockHeight 0))
            , (CoinV2, AllChains (BlockHeight 1))
            , (SkipTxTimingValidation, AllChains (BlockHeight 2))
            , (ModuleNameFix, AllChains (BlockHeight 2))
            , (ModuleNameFix2, AllChains (BlockHeight 2))
            , (Pact420, AllChains (BlockHeight 5))
            , (CheckTxHash, AllChains (BlockHeight 7))
            , (EnforceKeysetFormats, AllChains (BlockHeight 10))
            , (PactEvents, AllChains (BlockHeight 10))
            , (Pact4Coin3, AllChains (BlockHeight 20))
            , (Chainweb213Pact, AllChains (BlockHeight 26))
            , (Chainweb214Pact, AllChains (BlockHeight 30))
            , (Chainweb215Pact, AllChains (BlockHeight 35))
            , (Chainweb216Pact, AllChains (BlockHeight 53))
            , (Chainweb217Pact, AllChains (BlockHeight 55))
            , (Chainweb218Pact, AllChains (BlockHeight 60))
            ]

fastForkingCpmTestVersion :: ChainGraph -> ChainwebVersion
fastForkingCpmTestVersion g = legalizeTestVersion (fastForkingCpmTestVersion' g)

fastForkingCpmTestVersion' :: ChainGraph -> VersionBuilder
fastForkingCpmTestVersion' g v =
    cpmTestVersion g (testVersionTemplate v)
        & versionName .~ ChainwebVersionName ("fastfork-CPM-" <> toText g)
        & versionForks .~ fastForks

noBridgeCpmTestVersion :: ChainGraph -> ChainwebVersion
noBridgeCpmTestVersion g = legalizeTestVersion (noBridgeCpmTestVersion' g)

noBridgeCpmTestVersion' :: ChainGraph -> VersionBuilder
noBridgeCpmTestVersion' g v =
    cpmTestVersion g (testVersionTemplate v)
        & versionName .~ ChainwebVersionName ("nobridge-CPM-" <> toText g)
        & versionForks .~ (fastForks & at SPVBridge .~ Just (AllChains maxBound))

timedConsensusVersion :: ChainGraph -> ChainGraph -> ChainwebVersion
timedConsensusVersion g1 g2 = legalizeTestVersion (timedConsensusVersion' g1 g2)

timedConsensusVersion' :: ChainGraph -> ChainGraph -> VersionBuilder
timedConsensusVersion' g1 g2 v =
    testVersionTemplate v
        & versionName .~ ChainwebVersionName ("timedConsensus-" <> toText g1 <> "-" <> toText g2)
        & versionBlockRate .~ BlockRate 1_000_000
        & versionWindow .~ Nothing
        & versionForks .~ tabulateHashMap (\case
            SkipTxTimingValidation -> AllChains (BlockHeight 2)
            -- pact is disabled, we don't care about pact forks
            _ -> AllChains (BlockHeight 0)
        )
        & versionUpgrades .~ AllChains HM.empty
        & versionWindow .~ Nothing
        & versionGraphs .~ Above (BlockHeight 8, g2) (End g1)
        & versionCheats .~ Cheats
            { _disablePow = False -- PoW is effectively disabled with _versionWindow = Nothing
            , _disablePact = True
            , _disableMempool = True
            , _disablePeerValidation = True
            }
        & versionGenesis .~ ChainwebGenesis
            { _genesisBlockPayload = onChains $
                (unsafeChainId 0, TN0.payloadBlock) :
                [(n, TNN.payloadBlock) | n <- HS.toList (unsafeChainId 0 `HS.delete` chainIds v)]
            , _genesisBlockTarget = AllChains maxTarget
            , _genesisTime = AllChains $ BlockCreationTime epoch
            }

