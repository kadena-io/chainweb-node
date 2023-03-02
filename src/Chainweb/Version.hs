{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Version
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Properties of Chainweb Versions; properties which should be consistent
-- between all nodes running on the same network.
--
module Chainweb.Version
    ( Fork(..)
    , ChainwebGenesis(..)
    , Cheats(..)
    , disableMempool
    , disablePact
    , disablePeerValidation
    , disablePow
    , ChainwebVersionCode(..)
    , ChainwebVersionName(..)
    , ChainwebVersion(..)
    , Upgrade(..)
    , upgrade
    , versionForks
    , versionBlockRate
    , versionCheats
    , versionUpgrades
    , versionBootstraps
    , versionCode
    , versionFakeFirstEpochStart
    , versionGraphs
    , versionHeaderBaseSizeBytes
    , versionMaxBlockGasLimit
    , versionName
    , versionWindow
    , window
    , blockRate
    , versionGenesis
    , genesisBlockPayload
    , genesisBlockPayloadHash
    , genesisBlockTarget
    , genesisTime
    , emptyPayload
    , forkUpgrades
    , latestBehaviorAt
    , domainAddr2PeerInfo
    , encodeChainwebVersionCode
    , decodeChainwebVersionCode

    -- * Properties of Chainweb Version
    -- ** POW
    , BlockRate(..)
    , WindowWidth(..)
    -- ** Payload Validation Parameters
    , maxBlockGasLimit

    -- * Typelevel ChainwebVersion
    , ChainwebVersionT(..)
    , ChainwebVersionSymbol
    , chainwebVersionSymbolVal
    , SomeChainwebVersionT(..)
    , KnownChainwebVersionSymbol
    , someChainwebVersionVal

    -- * Singletons
    , Sing(SChainwebVersion)
    , SChainwebVersion
    , pattern FromSingChainwebVersion

    -- * HasChainwebVersion
    , HasChainwebVersion(..)
    , mkChainId
    , chainIds

    -- * ChainId
    , module Chainweb.ChainId

    -- * Re-exports from Chainweb.ChainGraph

    -- ** Chain Graph
    , ChainGraph
    , HasChainGraph(..)
    , adjacentChainIds
    , chainGraphAt
    , chainGraphAt_
    , chainwebGraphsAt

    -- ** Graph Properties
    , order
    , diameter
    , degree
    , shortestPath

    -- ** Undirected Edges
    , AdjPair
    , _getAdjPair
    , pattern Adj
    , adjs
    , adjsOfVertex
    , checkAdjacentChainIds

    -- * Internal. Don't use. Exported only for testing
    -- , headerSizes
    -- , headerBaseSizeBytes
    ) where

import Control.DeepSeq
import Control.Lens hiding ((.=), (<.>), index)
import Control.Monad.Catch

import Data.Aeson hiding (pairs)
import Data.Aeson.Types
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Proxy
import qualified Data.Text as T
import Data.Word

import GHC.Generics(Generic)
import GHC.Stack
import GHC.TypeLits

import Numeric.Natural

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.MerkleUniverse
import Chainweb.Miner.Pact
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Utils.Serialization

import Data.Singletons

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Bootstrap Peer Info

-- | Official testnet bootstrap nodes
--
domainAddr2PeerInfo :: [HostAddress] -> [PeerInfo]
domainAddr2PeerInfo = fmap (PeerInfo Nothing)

data Fork
    = Vuln797Fix
    | SlowEpoch
    | OldTargetGuard
    | EnforceKeysetFormats
    | SkipFeatureFlagValidation
    | OldDAGuard
    | CheckTxHash
    | PactEvents
    | SkipTxTimingValidation
    | SPVBridge
    | ModuleNameFix
    | ModuleNameFix2
    | PactBackCompat_v16
    | CoinV2
    | Pact4Coin3
    | Pact420
    | Chainweb213Pact
    | Chainweb214Pact
    | Chainweb215Pact
    | Chainweb216Pact
    | Chainweb217Pact
    | Chainweb218Pact
    | Pact44NewTrans
    -- always add new forks at the end, not in the middle of the constructors.
    deriving (Bounded, Generic, NFData, Hashable, Eq, Enum, Ord, Show)

instance HasTextRepresentation Fork where
  toText Vuln797Fix = "vuln797Fix"
  toText CoinV2 = "coinV2"
  toText SlowEpoch = "slowEpoch"
  toText OldTargetGuard = "oldTargetGuard"
  toText EnforceKeysetFormats = "enforceKeysetFormats"
  toText SkipFeatureFlagValidation = "skipFeatureFlagValidation"
  toText OldDAGuard = "oldDaGuard"
  toText CheckTxHash = "checkTxHash"
  toText Pact4Coin3 = "pact4Coin3"
  toText PactEvents = "pactEvents"
  toText SkipTxTimingValidation = "skipTxTimingValidation"
  toText SPVBridge = "spvBridge"
  toText PactBackCompat_v16 = "pactBackCompat_v16"
  toText ModuleNameFix = "moduleNameFix"
  toText ModuleNameFix2 = "moduleNameFix2"
  toText Pact420 = "pact420"
  toText Chainweb213Pact = "chainweb213Pact"
  toText Chainweb214Pact = "chainweb214Pact"
  toText Chainweb215Pact = "chainweb215Pact"
  toText Chainweb216Pact = "chainweb216Pact"
  toText Chainweb217Pact = "chainweb217Pact"
  toText Chainweb218Pact = "chainweb218Pact"
  toText Pact44NewTrans = "pact44NewTrans"

  fromText "vuln797Fix" = return Vuln797Fix
  fromText "coinV2" = return CoinV2
  fromText "slowEpoch" = return SlowEpoch
  fromText "oldTargetGuard" = return OldTargetGuard
  fromText "enforceKeysetFormats" = return EnforceKeysetFormats
  fromText "skipFeatureFlagValidation" = return SkipFeatureFlagValidation
  fromText "oldDaGuard" = return OldDAGuard
  fromText "checkTxHash" = return CheckTxHash
  fromText "pact4Coin3" = return Pact4Coin3
  fromText "pactEvents" = return PactEvents
  fromText "skipTxTimingValidation" = return SkipTxTimingValidation
  fromText "spvBridge" = return SPVBridge
  fromText "pactBackCompat_v16" = return PactBackCompat_v16
  fromText "moduleNameFix" = return ModuleNameFix
  fromText "moduleNameFix2" = return ModuleNameFix2
  fromText "pact420" = return Pact420
  fromText "chainweb213Pact" = return Chainweb213Pact
  fromText "chainweb214Pact" = return Chainweb214Pact
  fromText "chainweb215Pact" = return Chainweb215Pact
  fromText "chainweb216Pact" = return Chainweb216Pact
  fromText "chainweb217Pact" = return Chainweb217Pact
  fromText "chainweb218Pact" = return Chainweb218Pact
  fromText "pact44NewTrans" = return Pact44NewTrans
  fromText t = throwM . TextFormatException $ "Unknown Chainweb fork: " <> t

instance ToJSON Fork where
    toJSON = toJSON . toText
instance ToJSONKey Fork where
    toJSONKey = toJSONKeyText toText
instance FromJSON Fork where
    parseJSON = parseJsonFromText "Fork"
instance FromJSONKey Fork where
    fromJSONKey = FromJSONKeyTextParser $ either fail return . eitherFromText

newtype ChainwebVersionName =
    ChainwebVersionName { getChainwebVersionName :: T.Text }
    deriving stock (Generic, Eq, Ord)
    deriving newtype (Show, ToJSON, FromJSON)
    deriving anyclass (Hashable, NFData)

newtype ChainwebVersionCode =
    ChainwebVersionCode { getChainwebVersionCode :: Word32 }
    deriving stock (Generic, Eq, Ord)
    deriving newtype (Show, ToJSON, FromJSON)
    deriving anyclass (Hashable, NFData)

data Upgrade = Upgrade
    { _upgradeTransactions :: [ChainwebTransaction]
    , _legacyUpgradeIsPrecocious :: Bool
    -- ^ when set to `True`, the upgrade transactions are executed using the
    -- forks of the next block, rather than the block the upgrade transactions
    -- are included in.  do not use this for new upgrades unless you are sure
    -- you need it, this mostly exists for old upgrades.
    }
    deriving stock (Generic, Eq)
    deriving anyclass (NFData)

upgrade :: [ChainwebTransaction] -> Upgrade
upgrade txs = Upgrade txs False

data ChainwebVersion
    = ChainwebVersion
    { _versionCode :: ChainwebVersionCode
    , _versionName :: ChainwebVersionName
    , _versionGraphs :: Rule BlockHeight ChainGraph
    , _versionForks :: HashMap Fork (ChainMap BlockHeight)
    , _versionUpgrades :: ChainMap (HashMap BlockHeight Upgrade)
    , _versionBlockRate :: BlockRate
    , _versionWindow :: Maybe WindowWidth
    , _versionHeaderBaseSizeBytes :: Natural
    -- ^ The size in bytes of the constant portion of the serialized header. This is
    -- the header /without/ the adjacent hashes.
    --
    -- NOTE: This is internal. For the actual size of the serialized header
    -- use 'headerSizeBytes'.
    , _versionMaxBlockGasLimit :: Rule BlockHeight (Maybe Natural)
    , _versionFakeFirstEpochStart :: Bool
    , _versionBootstraps :: [PeerInfo]
    , _versionGenesis :: ChainwebGenesis
    , _versionCheats :: Cheats
    }
    deriving stock (Generic)
    deriving anyclass NFData

instance Show ChainwebVersion where
    show = T.unpack . getChainwebVersionName . _versionName

window :: ChainwebVersion -> Maybe WindowWidth
window = _versionWindow

blockRate :: ChainwebVersion -> BlockRate
blockRate = _versionBlockRate

instance Ord ChainwebVersion where
    v `compare` v' = fold
        [ _versionCode v `compare` _versionCode v'
        , _versionName v `compare` _versionName v'
        , _versionGraphs v `compare` _versionGraphs v'
        , _versionForks v `compare` _versionForks v'
        -- upgrades cannot be ordered because Payload in Pact cannot be ordered
        -- , _versionUpgrades v `compare` _versionUpgrades v'
        , _versionBlockRate v `compare` _versionBlockRate v'
        , _versionWindow v `compare` _versionWindow v'
        , _versionHeaderBaseSizeBytes v `compare` _versionHeaderBaseSizeBytes v'
        , _versionMaxBlockGasLimit v `compare` _versionMaxBlockGasLimit v'
        , _versionFakeFirstEpochStart v `compare` _versionFakeFirstEpochStart v'
        , _versionBootstraps v `compare` _versionBootstraps v'
        , _versionCheats v `compare` _versionCheats v'
        ]

instance Eq ChainwebVersion where
    v == v' = and
        [ compare v v' == EQ
        , _versionUpgrades v == _versionUpgrades v'
        ]

data Cheats = Cheats
    { _disablePow :: Bool
    , _disablePact :: Bool
    , _disablePeerValidation :: Bool
    , _disableMempool :: Bool
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass (ToJSON, FromJSON, NFData)

data ChainwebGenesis = ChainwebGenesis
    { _genesisBlockTarget :: ChainMap HashTarget
    , _genesisBlockPayload :: ChainMap PayloadWithOutputs
    , _genesisTime :: ChainMap BlockCreationTime
    }
    deriving stock (Generic, Eq)
    deriving anyclass NFData

instance Show ChainwebGenesis where
    show _ = "<genesis>"

makeLensesWith (lensRules & generateLazyPatterns .~ True) 'ChainwebVersion
makeLensesWith (lensRules & generateLazyPatterns .~ True) 'ChainwebGenesis
makeLensesWith (lensRules & generateLazyPatterns .~ True) 'Cheats

genesisBlockPayloadHash :: ChainwebVersion -> ChainId -> BlockPayloadHash
genesisBlockPayloadHash v cid = v ^?! versionGenesis . genesisBlockPayload . onChain cid . to _payloadWithOutputsPayloadHash

-- | Empty payload marking no-op transaction payloads for deprecated
-- versions.
--
emptyPayload :: PayloadWithOutputs
emptyPayload = PayloadWithOutputs mempty miner coinbase h i o
  where
    BlockPayload h i o = newBlockPayload miner coinbase mempty
    miner = MinerData $ encodeToByteString noMiner
    coinbase = noCoinbaseOutput

encodeChainwebVersionCode :: ChainwebVersionCode -> Put
encodeChainwebVersionCode = putWord32le . getChainwebVersionCode

decodeChainwebVersionCode :: Get ChainwebVersionCode
decodeChainwebVersionCode = ChainwebVersionCode <$> getWord32le

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag ChainwebVersionCode where
    type Tag ChainwebVersionCode = 'ChainwebVersionTag
    toMerkleNode = encodeMerkleInputNode encodeChainwebVersionCode
    fromMerkleNode = decodeMerkleInputNode decodeChainwebVersionCode

instance HasTextRepresentation ChainwebVersionName where
    toText = getChainwebVersionName
    fromText = pure . ChainwebVersionName

-- -- -------------------------------------------------------------------------- --
-- -- Type level ChainwebVersion

newtype ChainwebVersionT = ChainwebVersionT Symbol

data SomeChainwebVersionT = forall (a :: ChainwebVersionT)
        . KnownChainwebVersionSymbol a => SomeChainwebVersionT (Proxy a)

class KnownSymbol (ChainwebVersionSymbol n) => KnownChainwebVersionSymbol (n :: ChainwebVersionT) where
    type ChainwebVersionSymbol n :: Symbol
    chainwebVersionSymbolVal :: Proxy n -> T.Text

instance (KnownSymbol n) => KnownChainwebVersionSymbol ('ChainwebVersionT n) where
    type ChainwebVersionSymbol ('ChainwebVersionT n) = n
    chainwebVersionSymbolVal _ = T.pack $ symbolVal (Proxy @n)

someChainwebVersionVal :: ChainwebVersion -> SomeChainwebVersionT
someChainwebVersionVal v = someChainwebVersionVal' (_versionName v)

someChainwebVersionVal' :: ChainwebVersionName -> SomeChainwebVersionT
someChainwebVersionVal' v = case someSymbolVal (sshow v) of
    (SomeSymbol (Proxy :: Proxy v)) -> SomeChainwebVersionT (Proxy @('ChainwebVersionT v))

-- -- -------------------------------------------------------------------------- --
-- -- Singletons

data instance Sing (v :: ChainwebVersionT) where
    SChainwebVersion :: KnownChainwebVersionSymbol v => Sing v

type SChainwebVersion (v :: ChainwebVersionT) = Sing v

instance KnownChainwebVersionSymbol v => SingI (v :: ChainwebVersionT) where
    sing = SChainwebVersion

instance SingKind ChainwebVersionT where
    type Demote ChainwebVersionT = ChainwebVersionName

    fromSing (SChainwebVersion :: Sing v) = unsafeFromText
        . chainwebVersionSymbolVal $ Proxy @v

    toSing n = case someChainwebVersionVal' n of
        SomeChainwebVersionT p -> SomeSing (singByProxy p)

pattern FromSingChainwebVersion :: Sing (n :: ChainwebVersionT) -> ChainwebVersion
pattern FromSingChainwebVersion sng <- ((\v -> withSomeSing (_versionName v) SomeSing) -> SomeSing sng)
{-# COMPLETE FromSingChainwebVersion #-}

-- -- -------------------------------------------------------------------------- --
-- -- HasChainwebVersion Class
--
class HasChainwebVersion a where
    _chainwebVersion :: a -> ChainwebVersion
    _chainwebVersion = view chainwebVersion

    chainwebVersion :: Getter a ChainwebVersion
    chainwebVersion = to _chainwebVersion

    {-# MINIMAL _chainwebVersion | chainwebVersion #-}

instance HasChainwebVersion ChainwebVersion where
    _chainwebVersion = id

-- | All known chainIds. This includes chains that are not yet "active".
--
chainIds :: HasChainwebVersion v => v -> HS.HashSet ChainId
chainIds = graphChainIds . snd . ruleHead . _versionGraphs . _chainwebVersion

-- | Creates a map from fork heights to upgrades.
forkUpgrades
    :: ChainwebVersion
    -> [(Fork, ChainMap Upgrade)]
    -> ChainMap (HashMap BlockHeight Upgrade)
forkUpgrades v = OnChains . foldl' go (HM.empty <$ HS.toMap (chainIds v))
    where
    conflictError fork h =
        error $ "conflicting upgrades at block height " <> show h <> " when adding upgrade for fork " <> show fork
    emptyUpgradeError fork =
        error $ "empty set of upgrade transactions for fork " <> show fork
    go acc (fork, txsPerChain) =
        HM.unionWith
            (HM.unionWithKey (conflictError fork))
            acc newTxs
        where
        newTxs = HM.fromList $
            [ (cid, HM.singleton forkHeight upg)
            | cid <- HM.keys acc
            , Just upg <- [txsPerChain ^? onChain cid]
            , not (null $ _upgradeTransactions upg) || emptyUpgradeError fork
            , let forkHeight = v ^?! versionForks . at fork . _Just . onChain cid
            , forkHeight /= maxBound
            ]

-- | The block height at all chains at which the latest known behavior changes
-- will have taken effect: forks, upgrade transactions, or graph changes.
latestBehaviorAt :: ChainwebVersion -> BlockHeight
latestBehaviorAt v =
    foldlOf' changes max 0 v + 1
    where
    changes = fold
        [ versionForks . folded . folded
        , versionUpgrades . folded . ifolded . asIndex
        , versionGraphs . to ruleHead . _1 . _Just
        ] . filtered (/= maxBound)

mkChainId
    :: (MonadThrow m, HasChainwebVersion v)
    => v -> BlockHeight -> Word32 -> m ChainId
mkChainId v h i = cid
    <$ checkWebChainId (chainGraphAt (_chainwebVersion v) h) cid
  where
    cid = unsafeChainId i

-- -- -------------------------------------------------------------------------- --
-- -- Properties of Chainweb Versions
-- -- -------------------------------------------------------------------------- --

-- | Return the Graph History at a given block height in descending order.
--
-- The functions provided in 'Chainweb.Version.Utils' are generally more
-- convenient to use than this function.
--
-- This function is safe because of invariants provided by 'chainwebGraphs'.
-- (There are also unit tests the confirm this.)
--
chainwebGraphsAt
    :: HasCallStack
    => ChainwebVersion
    -> BlockHeight
    -> Rule BlockHeight ChainGraph
chainwebGraphsAt v h =
    ruleDropWhile (> h) (_versionGraphs v)

-- | The 'ChainGraph' for the given 'BlockHeight'
--
chainGraphAt :: HasCallStack => ChainwebVersion -> BlockHeight -> ChainGraph
chainGraphAt v = snd . ruleHead . chainwebGraphsAt v

-- | The 'ChainGraph' for the given 'BlockHeight'
--
chainGraphAt_
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> BlockHeight
    -> ChainGraph
chainGraphAt_ = chainGraphAt . _chainwebVersion

instance HasChainGraph (ChainwebVersion, BlockHeight) where
    _chainGraph = uncurry chainGraphAt

maxBlockGasLimit :: ChainwebVersion -> BlockHeight -> Maybe Natural
maxBlockGasLimit v bh = case measureRule bh $ _versionMaxBlockGasLimit v of
    Bottom limit -> limit
    Top (_, limit) -> limit
    Between (_, limit) _ -> limit

-- edtodo
-- chainweb218Pact :: ChainwebVersion -> BlockHeight -> Bool
-- chainweb218Pact Testnet04 = (>= 3_038_343) -- 2023-03-02 12:00:00+00:00
-- chainweb218Pact Development = (>= 500)
-- chainweb218Pact (FastTimedCPM g) | g == petersonChainGraph = (> 60)
-- chainweb218Pact _ = (> 24)