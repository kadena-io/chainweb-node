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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE LambdaCase #-}

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
    , ForkHeight(..)
    , _ForkAtBlockHeight
    , _ForkAtGenesis
    , _ForkNever
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
    , pact4Upgrade
    , TxIdxInBlock(..)
    , _TxBlockIdx
    , VersionQuirks(..)
    , noQuirks
    , quirkGasFees
    , versionForks
    , versionBlockDelay
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
    , versionVerifierPluginNames
    , versionQuirks
    , versionServiceDate
    , genesisBlockPayload
    , genesisBlockPayloadHash
    , genesisBlockTarget
    , genesisTime
    , genesisBlockHeight
    , genesisHeightAndGraph

    , PactUpgrade(..)
    , PactVersion(..)
    , PactVersionT(..)
    , ForBothPactVersions(..)
    , ForSomePactVersion(..)
    , pattern ForPact4
    , _ForPact4
    , pattern ForPact5
    , _ForPact5
    , forAnyPactVersion

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
    , indexByForkHeights
    , latestBehaviorAt
    , onAllChains
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
import Data.Set(Set)
import Data.Proxy
import qualified Data.Text as T
import Data.Word

import GHC.Generics(Generic)
import GHC.TypeLits
import GHC.Stack

-- internal modules

import Pact.Types.Runtime (Gas)

import Chainweb.BlockCreationTime
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.MerkleUniverse
import Chainweb.Payload
import qualified Chainweb.Pact4.Transaction as Pact4
import qualified Chainweb.Pact5.Transaction as Pact5
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Utils.Serialization

import Pact.Types.Verifier

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
    | Pact42
    -- always add new forks at the end, not in the middle of the constructors.
    | CheckTxHash
    | Chainweb213Pact
    | Chainweb214Pact
    | Chainweb215Pact
    | Pact44NewTrans
    | Chainweb216Pact
    | Chainweb217Pact
    | Chainweb218Pact
    | Chainweb219Pact
    | Chainweb220Pact
    | Chainweb221Pact
    | Chainweb222Pact
    | Chainweb223Pact
    | Chainweb224Pact
    | Chainweb225Pact
    | Chainweb226Pact
    | Pact5Fork
    | Chainweb228Pact
    | Chainweb229Pact
    | Chainweb230Pact
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
    toText Pact42 = "Pact42"
    toText CheckTxHash = "checkTxHash"
    toText Chainweb213Pact = "chainweb213Pact"
    toText Chainweb214Pact = "chainweb214Pact"
    toText Chainweb215Pact = "chainweb215Pact"
    toText Pact44NewTrans = "pact44NewTrans"
    toText Chainweb216Pact = "chainweb216Pact"
    toText Chainweb217Pact = "chainweb217Pact"
    toText Chainweb218Pact = "chainweb218Pact"
    toText Chainweb219Pact = "chainweb219Pact"
    toText Chainweb220Pact = "chainweb220Pact"
    toText Chainweb221Pact = "chainweb221Pact"
    toText Chainweb222Pact = "chainweb222Pact"
    toText Chainweb223Pact = "chainweb223Pact"
    toText Chainweb224Pact = "chainweb224Pact"
    toText Chainweb225Pact = "chainweb225Pact"
    toText Chainweb226Pact = "chainweb226Pact"
    toText Pact5Fork = "pact5"
    toText Chainweb228Pact = "chainweb228Pact"
    toText Chainweb229Pact = "chainweb229Pact"
    toText Chainweb230Pact = "chainweb230Pact"

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
    fromText "Pact42" = return Pact42
    fromText "checkTxHash" = return CheckTxHash
    fromText "chainweb213Pact" = return Chainweb213Pact
    fromText "chainweb214Pact" = return Chainweb214Pact
    fromText "chainweb215Pact" = return Chainweb215Pact
    fromText "pact44NewTrans" = return Pact44NewTrans
    fromText "chainweb216Pact" = return Chainweb216Pact
    fromText "chainweb217Pact" = return Chainweb217Pact
    fromText "chainweb218Pact" = return Chainweb218Pact
    fromText "chainweb219Pact" = return Chainweb219Pact
    fromText "chainweb220Pact" = return Chainweb220Pact
    fromText "chainweb221Pact" = return Chainweb221Pact
    fromText "chainweb222Pact" = return Chainweb222Pact
    fromText "chainweb223Pact" = return Chainweb223Pact
    fromText "chainweb224Pact" = return Chainweb224Pact
    fromText "chainweb225Pact" = return Chainweb225Pact
    fromText "chainweb226Pact" = return Chainweb226Pact
    fromText "pact5" = return Pact5Fork
    fromText "chainweb228Pact" = return Chainweb228Pact
    fromText "chainweb229Pact" = return Chainweb229Pact
    fromText "chainweb230Pact" = return Chainweb230Pact
    fromText t = throwM . TextFormatException $ "Unknown Chainweb fork: " <> t

instance ToJSON Fork where
    toJSON = toJSON . toText
instance ToJSONKey Fork where
    toJSONKey = toJSONKeyText toText
instance FromJSON Fork where
    parseJSON = parseJsonFromText "Fork"
instance FromJSONKey Fork where
    fromJSONKey = FromJSONKeyTextParser $ either fail return . eitherFromText

data ForkHeight = ForkAtBlockHeight !BlockHeight | ForkAtGenesis | ForkNever
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass (Hashable, NFData)

makePrisms ''ForkHeight

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

data PactVersion = Pact4 | Pact5
  deriving stock (Eq, Show)
data PactVersionT (v :: PactVersion) where
    Pact4T :: PactVersionT Pact4
    Pact5T :: PactVersionT Pact5
deriving stock instance Eq (PactVersionT v)
deriving stock instance Show (PactVersionT v)
instance NFData (PactVersionT v) where
    rnf Pact4T = ()
    rnf Pact5T = ()

data ForSomePactVersion f = forall pv. ForSomePactVersion (PactVersionT pv) (f pv)
forAnyPactVersion :: (forall pv. f pv -> a) -> ForSomePactVersion f -> a
forAnyPactVersion k (ForSomePactVersion _ f) = k f
instance (forall pv. Eq (f pv)) => Eq (ForSomePactVersion f) where
    ForSomePactVersion Pact4T f == ForSomePactVersion Pact4T f' = f == f'
    ForSomePactVersion Pact5T f == ForSomePactVersion Pact5T f' = f == f'
    ForSomePactVersion _ _ == ForSomePactVersion _ _ = False
deriving stock instance (forall pv. Show (f pv)) => Show (ForSomePactVersion f)
instance (forall pv. NFData (f pv)) => NFData (ForSomePactVersion f) where
    rnf (ForSomePactVersion pv f) = rnf pv `seq` rnf f
pattern ForPact4 :: f Pact4 -> ForSomePactVersion f
pattern ForPact4 x = ForSomePactVersion Pact4T x
_ForPact4 :: Prism' (ForSomePactVersion f) (f Pact4)
_ForPact4 = prism' ForPact4 $ \case
    ForPact4 x -> Just x
    _ -> Nothing
_ForPact5 :: Prism' (ForSomePactVersion f) (f Pact5)
_ForPact5 = prism' ForPact5 $ \case
    ForPact5 x -> Just x
    _ -> Nothing
pattern ForPact5 :: f Pact5 -> ForSomePactVersion f
pattern ForPact5 x = ForSomePactVersion Pact5T x
{-# COMPLETE ForPact4, ForPact5 #-}
data ForBothPactVersions f = ForBothPactVersions
    { _forPact4 :: (f Pact4)
    , _forPact5 :: (f Pact5)
    }
deriving stock instance (Eq (f Pact4), Eq (f Pact5)) => Eq (ForBothPactVersions f)
deriving stock instance (Show (f Pact4), Show (f Pact5)) => Show (ForBothPactVersions f)
instance (NFData (f Pact4), NFData (f Pact5)) => NFData (ForBothPactVersions f) where
    rnf b = rnf (_forPact4 b) `seq` rnf (_forPact5 b)

-- The type of upgrades, which are sets of transactions to run at certain block
-- heights during coinbase.

data PactUpgrade where
    Pact4Upgrade ::
        { _pact4UpgradeTransactions :: [Pact4.Transaction]
        , _legacyUpgradeIsPrecocious :: Bool
        -- ^ when set to `True`, the upgrade transactions are executed using the
        -- forks of the next block, rather than the block the upgrade
        -- transactions are included in.  do not use this for new upgrades
        -- unless you are sure you need it, this mostly exists for old upgrades.
        } -> PactUpgrade
    Pact5Upgrade ::
        { _pact5UpgradeTransactions :: [Pact5.Transaction]
        } -> PactUpgrade

instance Eq PactUpgrade where
    Pact4Upgrade txs precocious == Pact4Upgrade txs' precocious' =
        txs == txs' && precocious == precocious'
    Pact5Upgrade txs == Pact5Upgrade txs' =
        txs == txs'
    _ == _ = False

instance Show PactUpgrade where
    show Pact4Upgrade {} = "<pact4 upgrade>"
    show Pact5Upgrade {} = "<pact5 upgrade>"

instance NFData PactUpgrade where
    rnf (Pact4Upgrade txs precocious) = rnf txs `seq` rnf precocious
    rnf (Pact5Upgrade txs) = rnf txs

pact4Upgrade :: [Pact4.Transaction] -> PactUpgrade
pact4Upgrade txs = Pact4Upgrade txs False

data TxIdxInBlock = TxBlockIdx Word
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

makePrisms ''TxIdxInBlock

-- The type of quirks, i.e. special validation behaviors that are in some
-- sense one-offs which can't be expressed as upgrade transactions and must be
-- preserved.
data VersionQuirks = VersionQuirks
    { _quirkGasFees :: !(ChainMap (HashMap (BlockHeight, TxIdxInBlock) Gas))
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

noQuirks :: VersionQuirks
noQuirks = VersionQuirks
    { _quirkGasFees = AllChains HM.empty
    }

-- | Chainweb versions are sets of properties that must remain consistent among
-- all nodes on the same network. For examples see `Chainweb.Version.Mainnet`,
-- `Chainweb.Version.Testnet`, `Chainweb.Version.RecapDevelopment`, and
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
    , _versionForks :: HashMap Fork (ChainMap ForkHeight)
        -- ^ The block heights on each chain to apply behavioral changes.
        -- Interpretation of these is up to the functions in
        -- `Chainweb.Version.Guards`.
    , _versionUpgrades :: ChainMap (HashMap BlockHeight PactUpgrade)
        -- ^ The Pact upgrade transactions to execute on each chain at certain block
        -- heights.
    , _versionBlockDelay :: BlockDelay
        -- ^ The Proof-of-Work `BlockDelay` for each `ChainwebVersion`. This is
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
    , _versionVerifierPluginNames :: ChainMap (Rule BlockHeight (Set VerifierName))
        -- ^ Verifier plugins that can be run to verify transaction contents.
    , _versionQuirks :: VersionQuirks
        -- ^ Modifications to behavior at particular blockheights
    , _versionServiceDate :: Maybe String
        -- ^ The node service date for this version.
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
        , _versionBlockDelay v `compare` _versionBlockDelay v'
        , _versionWindow v `compare` _versionWindow v'
        , _versionHeaderBaseSizeBytes v `compare` _versionHeaderBaseSizeBytes v'
        , _versionMaxBlockGasLimit v `compare` _versionMaxBlockGasLimit v'
        , _versionBootstraps v `compare` _versionBootstraps v'
        -- genesis cannot be ordered because Payload in Pact cannot be ordered
        -- , _versionGenesis v `compare` _versionGenesis v'
        , _versionCheats v `compare` _versionCheats v'
        , _versionVerifierPluginNames v `compare` _versionVerifierPluginNames v'
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
makeLensesWith (lensRules & generateLazyPatterns .~ True) 'VersionQuirks

genesisBlockPayloadHash :: ChainwebVersion -> ChainId -> BlockPayloadHash
genesisBlockPayloadHash v cid = v ^?! versionGenesis . genesisBlockPayload . atChain cid . to _payloadWithOutputsPayloadHash

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

-- | The genesis block height for a given chain.
--
-- Raises an error if a non-existing chainid is provided.
-- (We generally assume that this invariant holds throughout the code base.
-- It is enforced via the 'mkChainId' smart constructor for ChainId.)
--
genesisBlockHeight :: HasCallStack => ChainwebVersion -> ChainId -> BlockHeight
genesisBlockHeight v c = fst $ genesisHeightAndGraph v c
{-# inlinable genesisBlockHeight #-}

-- | The genesis graph for a given Chain
--
-- Invariant:
--
-- * The given ChainId exists in the first graph of the graph history.
--   (We generally assume that this invariant holds throughout the code base.
--   It is enforced via the 'mkChainId' smart constructor for ChainId.)
--
genesisHeightAndGraph
    :: HasCallStack
    => HasChainwebVersion v
    => HasChainId c
    => v
    -> c
    -> (BlockHeight, ChainGraph)
genesisHeightAndGraph v c =
    case ruleSeek (\_ g -> not (isWebChain g c)) (_versionGraphs (_chainwebVersion v)) of
        -- the chain was in every graph down to the bottom,
        -- so the bottom has the genesis graph
        (False, z) -> ruleZipperHere z
        (True, (BetweenZipper _ above))
            -- the chain is not in this graph, and there is no graph above
            -- which could have it
            | [] <- above -> missingChainError
            -- the chain is not in this graph but the graph above does have it
            | (t:_) <- above -> t
    where
    missingChainError = error
        $ "Invalid ChainId " <> show (_chainId c)
        <> " for chainweb version " <> show (_versionName (_chainwebVersion v))
{-# inlinable genesisHeightAndGraph #-}

-------------------------------------------------------------------------- --
-- Utilities for constructing chainweb versions

domainAddr2PeerInfo :: [HostAddress] -> [PeerInfo]
domainAddr2PeerInfo = fmap (PeerInfo Nothing)

-- | A utility to allow indexing behavior by forks, returning that behavior
-- indexed by the block heights of those forks.
indexByForkHeights
    :: ChainwebVersion
    -> [(Fork, ChainMap a)]
    -> ChainMap (HashMap BlockHeight a)
indexByForkHeights v = OnChains . foldl' go (HM.empty <$ HS.toMap (chainIds v))
  where
    conflictError fork h =
        error $ "conflicting behavior at block height " <> show h <> " when adding behavior for fork " <> show fork
    go acc (fork, txsPerChain) =
        HM.unionWith
            (HM.unionWithKey (conflictError fork))
            acc newTxs
      where
        newTxs = HM.fromList $
            [ (cid, HM.singleton forkHeight upg)
            | cid <- HM.keys acc
            , Just upg <- [txsPerChain ^? atChain cid]
            , ForkAtBlockHeight forkHeight <- [v ^?! versionForks . at fork . _Just . atChain cid]
            , forkHeight /= maxBound
            ]

-- | The block height at all chains at which the latest known behavior changes
-- will have taken effect: forks, upgrade transactions, or graph changes.
latestBehaviorAt :: ChainwebVersion -> BlockHeight
latestBehaviorAt v = foldlOf' behaviorChanges max 0 v + 1
    where
    behaviorChanges = fold
        [ versionForks . folded . folded . _ForkAtBlockHeight
        , versionUpgrades . folded . ifolded . asIndex
        , versionGraphs . to ruleHead . _1
        ]

-- | Easy construction of a `ChainMap` with entries for every chain
-- in a `ChainwebVersion`.
onAllChains :: Applicative m => ChainwebVersion -> (ChainId -> m a) -> m (ChainMap a)
onAllChains v f = OnChains <$>
    HM.traverseWithKey
        (\cid () -> f cid)
        (HS.toMap (chainIds v))
