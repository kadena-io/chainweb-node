{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
    (
    -- * Properties of Chainweb Version
      Fork(..)
    , VersionGenesis(..)
    , VersionCheats(..)
    , VersionDefaults(..)
    , disablePow
    , fakeFirstEpochStart
    , disablePact
    , disablePeerValidation
    , disableMempoolSync
    , ChainwebVersionCode(..)
    , encodeChainwebVersionCode
    , decodeChainwebVersionCode
    , ChainwebVersionName(..)
    , ChainwebVersion(..)
    , Upgrade(..)
    , upgrade
    , versionForks
    , versionBlockRate
    , versionCheats
    , versionDefaults
    , versionUpgrades
    , versionBootstraps
    , versionCode
    , versionGraphs
    , versionHeaderBaseSizeBytes
    , versionMaxBlockGasLimit
    , versionName
    , versionWindow
    , versionGenesis
    , genesisBlockPayload
    , genesisBlockPayloadHash
    , genesisBlockTarget
    , genesisTime

    -- * Typelevel ChainwebVersionName
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

    -- * ChainId re-export
    , module Chainweb.ChainId

    -- * Re-exports from Chainweb.ChainGraph

    -- ** Chain Graph
    , ChainGraph
    , HasChainGraph(..)
    , adjacentChainIds
    , chainGraphAt
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

    -- ** Utilities for constructing Chainweb Version
    , forkUpgrades
    , latestBehaviorAt
    , domainAddr2PeerInfo

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
import GHC.TypeLits

#if !MIN_VERSION_base(4,16,0)
import Numeric.Natural
#endif

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Utils.Serialization

import Data.Singletons

import P2P.Peer

-- | Data type representing changes to block validation, whether in the payload
-- or in the header. Always add new forks at the end, not in the middle of the
-- constructors.
--
data Fork
    = SlowEpoch
    | Vuln797Fix
    | CoinV2
    | PactBackCompat_v16
    | ModuleNameFix
    | SkipTxTimingValidation
    | OldTargetGuard
    | SkipFeatureFlagValidation
    | ModuleNameFix2
    | OldDAGuard
    | PactEvents
    | SPVBridge
    | Pact4Coin3
    | EnforceKeysetFormats
    | Pact420
    | CheckTxHash
    | Chainweb213Pact
    | Chainweb214Pact
    | Chainweb215Pact
    | Pact44NewTrans
    | Chainweb216Pact
    | Chainweb217Pact
    | Chainweb218Pact
    | Chainweb219Pact
    -- always add new forks at the end, not in the middle of the constructors.
    deriving stock (Bounded, Generic, Eq, Enum, Ord, Show)
    deriving anyclass (NFData, Hashable)

instance HasTextRepresentation Fork where
    toText SlowEpoch = "slowEpoch"
    toText Vuln797Fix = "vuln797Fix"
    toText CoinV2 = "coinV2"
    toText PactBackCompat_v16 = "pactBackCompat_v16"
    toText ModuleNameFix = "moduleNameFix"
    toText SkipTxTimingValidation = "skipTxTimingValidation"
    toText OldTargetGuard = "oldTargetGuard"
    toText SkipFeatureFlagValidation = "skipFeatureFlagValidation"
    toText ModuleNameFix2 = "moduleNameFix2"
    toText OldDAGuard = "oldDaGuard"
    toText PactEvents = "pactEvents"
    toText SPVBridge = "spvBridge"
    toText Pact4Coin3 = "pact4Coin3"
    toText EnforceKeysetFormats = "enforceKeysetFormats"
    toText Pact420 = "pact420"
    toText CheckTxHash = "checkTxHash"
    toText Chainweb213Pact = "chainweb213Pact"
    toText Chainweb214Pact = "chainweb214Pact"
    toText Chainweb215Pact = "chainweb215Pact"
    toText Pact44NewTrans = "pact44NewTrans"
    toText Chainweb216Pact = "chainweb216Pact"
    toText Chainweb217Pact = "chainweb217Pact"
    toText Chainweb218Pact = "chainweb218Pact"
    toText Chainweb219Pact = "chainweb219Pact"

    fromText "slowEpoch" = return SlowEpoch
    fromText "vuln797Fix" = return Vuln797Fix
    fromText "coinV2" = return CoinV2
    fromText "pactBackCompat_v16" = return PactBackCompat_v16
    fromText "moduleNameFix" = return ModuleNameFix
    fromText "skipTxTimingValidation" = return SkipTxTimingValidation
    fromText "oldTargetGuard" = return OldTargetGuard
    fromText "skipFeatureFlagValidation" = return SkipFeatureFlagValidation
    fromText "moduleNameFix2" = return ModuleNameFix2
    fromText "oldDaGuard" = return OldDAGuard
    fromText "pactEvents" = return PactEvents
    fromText "spvBridge" = return SPVBridge
    fromText "pact4Coin3" = return Pact4Coin3
    fromText "enforceKeysetFormats" = return EnforceKeysetFormats
    fromText "pact420" = return Pact420
    fromText "checkTxHash" = return CheckTxHash
    fromText "chainweb213Pact" = return Chainweb213Pact
    fromText "chainweb214Pact" = return Chainweb214Pact
    fromText "chainweb215Pact" = return Chainweb215Pact
    fromText "pact44NewTrans" = return Pact44NewTrans
    fromText "chainweb216Pact" = return Chainweb216Pact
    fromText "chainweb217Pact" = return Chainweb217Pact
    fromText "chainweb218Pact" = return Chainweb218Pact
    fromText "chainweb219Pact" = return Chainweb219Pact
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
    deriving newtype (ToJSON, FromJSON)
    deriving anyclass (Hashable, NFData)

instance Show ChainwebVersionName where show = T.unpack . getChainwebVersionName

newtype ChainwebVersionCode =
    ChainwebVersionCode { getChainwebVersionCode :: Word32 }
    deriving stock (Generic, Eq, Ord)
    deriving newtype (Show, ToJSON, FromJSON)
    deriving anyclass (Hashable, NFData)

encodeChainwebVersionCode :: ChainwebVersionCode -> Put
encodeChainwebVersionCode = putWord32le . getChainwebVersionCode

decodeChainwebVersionCode :: Get ChainwebVersionCode
decodeChainwebVersionCode = ChainwebVersionCode <$> getWord32le

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag ChainwebVersionCode where
    type Tag ChainwebVersionCode = 'ChainwebVersionTag
    toMerkleNode = encodeMerkleInputNode encodeChainwebVersionCode
    fromMerkleNode = decodeMerkleInputNode decodeChainwebVersionCode

-- The type of upgrades, which are sets of transactions to run at certain block
-- heights during coinbase.
--
data Upgrade = Upgrade
    { _upgradeTransactions :: [ChainwebTransaction]
    , _legacyUpgradeIsPrecocious :: Bool
        -- ^ when set to `True`, the upgrade transactions are executed using the
        -- forks of the next block, rather than the block the upgrade
        -- transactions are included in.  do not use this for new upgrades
        -- unless you are sure you need it, this mostly exists for old upgrades.
    }
    deriving stock (Generic, Eq)
    deriving anyclass (NFData)

upgrade :: [ChainwebTransaction] -> Upgrade
upgrade txs = Upgrade txs False

-- | Chainweb versions are sets of properties that must remain consistent among
-- all nodes on the same network. For examples see `Chainweb.Version.Mainnet`,
-- `Chainweb.Version.Testnet`, `Chainweb.Version.Development`, and
-- `Chainweb.Test.TestVersions`.
--
-- NOTE: none of the fields should be strict at any level, because of how we
-- use them in `Chainweb.Test.TestVersions`. However, all versions are
-- evaluated to normal form by `Chainweb.Version.Registry` before use. We also
-- explicitly produce lazy optics which are required by
-- `Chainweb.Test.TestVersions`. Only the mutation side of the optics is lazy,
-- and mutating a chainweb version during normal operation of a node is
-- completely illegal.
--
data ChainwebVersion
    = ChainwebVersion
    { _versionCode :: ChainwebVersionCode
        -- ^ The numeric code identifying the Version, must be unique. See
        -- `Chainweb.Version.Registry`.
    , _versionName :: ChainwebVersionName
        -- ^ The textual name of the Version, used in almost all REST endpoints.
    , _versionGraphs :: Rule BlockHeight ChainGraph
        -- ^ The chain graphs in the history and at which block heights they apply.
    , _versionForks :: HashMap Fork (ChainMap BlockHeight)
        -- ^ The block heights on each chain to apply behavioral changes.
        -- Interpretation of these is up to the functions in
        -- `Chainweb.Version.Guards`.
    , _versionUpgrades :: ChainMap (HashMap BlockHeight Upgrade)
        -- ^ The upgrade transactions to execute on each chain at certain block
        -- heights.
    , _versionBlockRate :: BlockRate
        -- ^ The Proof-of-Work `BlockRate` for each `ChainwebVersion`. This is
        -- the number of microseconds we expect to pass while a miner mines on
        -- various chains, eventually succeeding on one.
    , _versionWindow :: WindowWidth
        -- ^ The Proof-of-Work `WindowWidth` for each `ChainwebVersion`.
    , _versionHeaderBaseSizeBytes :: Natural
        -- ^ The size in bytes of the constant portion of the serialized header.
        -- This is the header /without/ the adjacent hashes.
        --
        -- NOTE: This is internal. For the actual size of the serialized header
        -- use 'headerSizeBytes'.
    , _versionMaxBlockGasLimit :: Rule BlockHeight (Maybe Natural)
        -- ^ The maximum gas limit for an entire block.
    , _versionBootstraps :: [PeerInfo]
        -- ^ The locations of the bootstrap peers.
    , _versionGenesis :: VersionGenesis
        -- ^ The information used to construct the genesis blocks.
    , _versionCheats :: VersionCheats
        -- ^ Whether to disable any core functionality.
    , _versionDefaults :: VersionDefaults
        -- ^ Version-specific defaults that can be overridden elsewhere.
    }
    deriving stock (Generic)
    deriving anyclass NFData

instance Show ChainwebVersion where
    show = show . _versionName

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
        , _versionBootstraps v `compare` _versionBootstraps v'
        -- genesis cannot be ordered because Payload in Pact cannot be ordered
        -- , _versionGenesis v `compare` _versionGenesis v'
        , _versionCheats v `compare` _versionCheats v'
        ]

instance Eq ChainwebVersion where
    v == v' = and
        [ compare v v' == EQ
        , _versionUpgrades v == _versionUpgrades v'
        , _versionGenesis v == _versionGenesis v'
        ]

data VersionDefaults = VersionDefaults
    { _disablePeerValidation :: Bool
        -- ^ should we try to check that a peer is valid? See `P2P.Peer.validatePeerConfig`
    , _disableMempoolSync :: Bool
        -- ^ should we disable mempool sync entirely?
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass (ToJSON, FromJSON, NFData)

data VersionCheats = VersionCheats
    { _disablePow :: Bool
        -- ^ should we stop checking proof of work?
    , _fakeFirstEpochStart :: Bool
        -- ^ should we fake the start time of the first epoch? See `Chainweb.BlockHeader.epochStart`.
    , _disablePact :: Bool
        -- ^ Should we replace the pact service with a dummy that always makes empty blocks?
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass (ToJSON, FromJSON, NFData)

data VersionGenesis = VersionGenesis
    { _genesisBlockTarget :: ChainMap HashTarget
    , _genesisBlockPayload :: ChainMap PayloadWithOutputs
    , _genesisTime :: ChainMap BlockCreationTime
    }
    deriving stock (Generic, Eq)
    deriving anyclass NFData

instance Show VersionGenesis where
    show _ = "<genesis>"

makeLensesWith (lensRules & generateLazyPatterns .~ True) 'ChainwebVersion
makeLensesWith (lensRules & generateLazyPatterns .~ True) 'VersionGenesis
makeLensesWith (lensRules & generateLazyPatterns .~ True) 'VersionCheats
makeLensesWith (lensRules & generateLazyPatterns .~ True) 'VersionDefaults

genesisBlockPayloadHash :: ChainwebVersion -> ChainId -> BlockPayloadHash
genesisBlockPayloadHash v cid = v ^?! versionGenesis . genesisBlockPayload . onChain cid . to _payloadWithOutputsPayloadHash

instance HasTextRepresentation ChainwebVersionName where
    toText = getChainwebVersionName
    fromText = pure . ChainwebVersionName

-------------------------------------------------------------------------- --
-- Type level ChainwebVersion

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
someChainwebVersionVal' v = case someSymbolVal (show v) of
    (SomeSymbol (Proxy :: Proxy v)) -> SomeChainwebVersionT (Proxy @('ChainwebVersionT v))

-------------------------------------------------------------------------- --
-- Singletons

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
pattern FromSingChainwebVersion sng <- (\v -> withSomeSing (_versionName v) SomeSing -> SomeSing sng)
{-# COMPLETE FromSingChainwebVersion #-}

-------------------------------------------------------------------------- --
-- HasChainwebVersion Class

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

mkChainId
    :: (MonadThrow m, HasChainwebVersion v)
    => v -> BlockHeight -> Word32 -> m ChainId
mkChainId v h i = cid
    <$ checkWebChainId (chainGraphAt (_chainwebVersion v) h) cid
  where
    cid = unsafeChainId i

-------------------------------------------------------------------------- --
-- Properties of Chainweb Versions
-------------------------------------------------------------------------- --

-- | Return the Graph History at a given block height in descending order.
--
-- The functions provided in 'Chainweb.Version.Utils' are generally more
-- convenient to use than this function.
--
-- This function is safe because of invariants provided by 'chainwebGraphs'.
-- (There are also unit tests the confirm this.)
chainwebGraphsAt
    :: ChainwebVersion
    -> BlockHeight
    -> Rule BlockHeight ChainGraph
chainwebGraphsAt v h =
    ruleDropWhile (> h) (_versionGraphs v)

-- | The 'ChainGraph' for the given 'BlockHeight'
chainGraphAt :: HasChainwebVersion v => v -> BlockHeight -> ChainGraph
chainGraphAt v = snd . ruleHead . chainwebGraphsAt (_chainwebVersion v)

instance HasChainGraph (ChainwebVersion, BlockHeight) where
    _chainGraph = uncurry chainGraphAt

-------------------------------------------------------------------------- --
-- Utilities for constructing chainweb versions

domainAddr2PeerInfo :: [HostAddress] -> [PeerInfo]
domainAddr2PeerInfo = fmap (PeerInfo Nothing)

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
latestBehaviorAt v = foldlOf' behaviorChanges max 0 v + 1
    where
    behaviorChanges = fold
        [ versionForks . folded . folded
        , versionUpgrades . folded . ifolded . asIndex
        , versionGraphs . to ruleHead . _1 . _Just
        ] . filtered (/= maxBound)
