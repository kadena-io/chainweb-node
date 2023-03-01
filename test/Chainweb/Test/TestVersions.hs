{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Bits
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Word
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPM0Payload as TN0
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPMNPayload as TNN

import System.IO.Unsafe

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeight
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.HostAddress
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

data GraphPos = P1 | P2 deriving (Bounded, Enum)

graphToCodeN :: GraphPos -> KnownGraph -> Word32
graphToCodeN p g = shiftL (graphToCode g) (4 * (4 + fromEnum p))
  where
    graphToCode :: KnownGraph -> Word32
    graphToCode Singleton = 0x00000001
    graphToCode Pair = 0x00000002
    graphToCode Triangle = 0x00000003
    graphToCode Peterson = 0x00000004
    graphToCode Twenty = 0x00000005
    graphToCode HoffmanSingleton = 0x00000006

legalizeTestVersion :: (ChainwebVersion -> ChainwebVersion) -> ChainwebVersion
legalizeTestVersion f = unsafePerformIO $ do
    let v = f v
    registerVersion v
    return v

-- edtodo: test registry list for codes
testRegistry :: [ChainwebVersion]
testRegistry = concat
    [ [ fastForkingCpmTestVersion (knownChainGraph g) | g :: KnownGraph <- [minBound..maxBound] ]
    , [ slowForkingCpmTestVersion (knownChainGraph g) | g :: KnownGraph <- [minBound..maxBound] ]
    , [ barebonesTestVersion (knownChainGraph g) | g :: KnownGraph <- [minBound..maxBound] ]
    , [ noBridgeCpmTestVersion (knownChainGraph g) | g :: KnownGraph <- [minBound..maxBound] ]
    , [ timedConsensusVersion (knownChainGraph g1) (knownChainGraph g2) | g1 :: KnownGraph <- [minBound..maxBound], g2 :: KnownGraph <- [minBound..maxBound] ]
    ]

testVersionTemplate :: ChainwebVersion -> ChainwebVersion
testVersionTemplate v = v
    & versionHeaderBaseSizeBytes .~ 318 - 110
    & versionWindow .~ Nothing
    & versionFakeFirstEpochStart .~ False -- DA is already disabled with window = Nothing
    & versionMaxBlockGasLimit .~ End (Just 2_000_000)
    & versionBootstraps .~ [testBootstrapPeerInfos]

fastForks :: HashMap Fork (ChainMap BlockHeight)
fastForks = HM.fromList
    [ (Pact420, AllChains (BlockHeight 0))
    , (SlowEpoch, AllChains (BlockHeight 0))
    , (OldTargetGuard, AllChains (BlockHeight 0))
    , (SkipFeatureFlagValidation, AllChains (BlockHeight 0))
    , (OldDAGuard, AllChains (BlockHeight 0))
    , (Vuln797Fix, AllChains (BlockHeight 0))
    , (PactBackCompat_v16, AllChains (BlockHeight 0))
    , (SPVBridge, AllChains (BlockHeight 0))
    , (EnforceKeysetFormats, AllChains (BlockHeight 0))
    , (CheckTxHash, AllChains (BlockHeight 0))
    , (Pact44NewTrans, AllChains (BlockHeight 0))
    , (Chainweb213Pact, AllChains (BlockHeight 0))
    , (PactEvents, AllChains (BlockHeight 0))
    , (CoinV2, AllChains (BlockHeight 1))
    , (SkipTxTimingValidation, AllChains (BlockHeight 2))
    , (ModuleNameFix, AllChains (BlockHeight 2))
    , (ModuleNameFix2, AllChains (BlockHeight 2))
    , (Pact4Coin3, AllChains (BlockHeight 4))
    , (Chainweb214Pact, AllChains (BlockHeight 5))
    , (Chainweb215Pact, AllChains (BlockHeight 10))
    , (Chainweb216Pact, AllChains (BlockHeight 16))
    , (Chainweb217Pact, AllChains (BlockHeight 20))
    ]

barebonesTestVersion :: ChainGraph -> ChainwebVersion
barebonesTestVersion g = legalizeTestVersion $ \v ->
    testVersionTemplate v
        & versionCode .~ ChainwebVersionCode
            (0x80000000 .|. graphToCodeN P1 (view chainGraphKnown g))
        & versionWindow .~ Nothing
        & versionBlockRate .~ BlockRate 0
        & versionName .~ ChainwebVersionName ("test-" <> toText g)
        & versionGraphs .~ End g
        & versionCheats .~ Cheats
            { _disablePow = False -- PoW is effectively disabled with window = Nothing? edtodo
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

cpmTestVersion :: ChainGraph -> ChainwebVersion -> ChainwebVersion
cpmTestVersion g v = v
    & versionWindow .~ Nothing
    & versionBlockRate .~ BlockRate (Micros 200_000)
    & versionGraphs .~ End g
    & versionCheats .~ Cheats
        { _disablePow = False -- PoW is effectively disabled with window = Nothing? edtodo
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
slowForkingCpmTestVersion g = legalizeTestVersion $ \v ->
    cpmTestVersion g (testVersionTemplate v)
        & versionName .~ ChainwebVersionName ("slowfork-CPM-test-" <> toText g)
        & versionCode .~ ChainwebVersionCode
            (0x80000003 .|. graphToCodeN P1 (view chainGraphKnown g))
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
            ]

fastForkingCpmTestVersion :: ChainGraph -> ChainwebVersion
fastForkingCpmTestVersion g = legalizeTestVersion $ \v ->
    cpmTestVersion g (testVersionTemplate v)
        & versionName .~ ChainwebVersionName ("fastfork-CPM-" <> toText g)
        & versionCode .~ ChainwebVersionCode
            (0x80000004 .|. graphToCodeN P1 (view chainGraphKnown g))
        & versionForks .~ fastForks

noBridgeCpmTestVersion :: ChainGraph -> ChainwebVersion
noBridgeCpmTestVersion g = legalizeTestVersion $ \v ->
    cpmTestVersion g (testVersionTemplate v)
        & versionName .~ ChainwebVersionName ("nobridge-CPM-" <> toText g)
        & versionCode .~ ChainwebVersionCode
            (0x80000005 .|. graphToCodeN P1 (view chainGraphKnown g))
        & versionForks .~ (fastForks & at SPVBridge .~ Just (AllChains maxBound))

timedConsensusVersion :: ChainGraph -> ChainGraph -> ChainwebVersion
timedConsensusVersion g1 g2 = legalizeTestVersion $ \v ->
    testVersionTemplate v
        & versionName .~ ChainwebVersionName ("timedConsensus-" <> toText g1 <> "-" <> toText g2)
        & versionCode .~ ChainwebVersionCode
            (foldl' (.|.) 0x80000001
                [ graphToCodeN P1 (view chainGraphKnown g1)
                , graphToCodeN P2 (view chainGraphKnown g2)
                ])
        & versionBlockRate .~ BlockRate 1_000_000
        & versionWindow .~ Nothing
        & versionForks .~ HM.fromList
            [ (Vuln797Fix, AllChains (BlockHeight 0))
            , (SkipTxTimingValidation, AllChains (BlockHeight 2))
            , (CheckTxHash, AllChains (BlockHeight 0))
            , (SlowEpoch, AllChains (BlockHeight 0))
            , (OldTargetGuard, AllChains (BlockHeight 0))
            , (SkipFeatureFlagValidation, AllChains (BlockHeight 0))
            , (OldDAGuard, AllChains (BlockHeight 0))
            ]
        & versionUpgrades .~ AllChains HM.empty
        & versionWindow .~ Nothing
        & versionGraphs .~ Above (BlockHeight 8, g2) (End g1)
        & versionCheats .~ Cheats
            { _disablePow = False -- PoW is effectively disabled with window = Nothing? edtodo
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