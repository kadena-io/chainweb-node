{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveTraversable #-}

-- |
-- Module: Chainweb.ChainId
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- The defininitions in this module are also exported via "Chainweb.Version".
--
module Chainweb.ChainId
( ChainIdException(..)
, ChainId
, HasChainId(..)
, checkChainId
, chainIdToText
, chainIdFromText

-- * Serialization
, encodeChainId
, decodeChainId
, decodeChainIdChecked

-- * Typelevel ChainID
, ChainIdT(..)
, ChainIdSymbol
, chainIdSymbolVal
, SomeChainIdT(..)
, KnownChainIdSymbol
, someChainIdVal

-- * Singletons
, Sing(SChainId)
, type SChainId
, pattern FromSingChainId

-- * Testing
, unsafeChainId
, chainIdInt

-- * Mapping from chain IDs to values
, ChainMap(..)
, atChain
, onChains
, onChain
, chainZip
) where

import Control.Applicative
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad.Catch (Exception, MonadThrow)

import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Kind
import Data.Proxy
import Data.Semialign
import qualified Data.Text as T
import Data.These
import Data.Word (Word32)

import GHC.Generics (Generic)
import GHC.TypeLits

-- internal imports

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Utils
import Chainweb.Utils.Serialization

import Data.Singletons hiding (Index)

-- -------------------------------------------------------------------------- --
-- Exceptions

data ChainIdException
    = ChainIdMismatch (Expected ChainId) (Actual ChainId)
    deriving (Show, Eq, Ord, Generic)

instance Exception ChainIdException

-- -------------------------------------------------------------------------- --
-- ChainId

-- | ChainId /within the context of a Chainweb instance/.
--
-- The set of valid ChainIds is determined by the 'ChainwebVersion'. In almost
-- all use cases there should be a context that is an instance of
-- 'HasChainwebVersion' can be used get the set of chain ids.
--
-- In the context of a particular chain the respective 'ChainId' can be obtained
-- via instances of 'HasChainId'.
--
-- /How to create values of type 'ChainId'/
--
-- * To fold or traverse over all chain ids, use 'chainIds'.
-- * To deserialize a chain id, use 'mkChainId'.
-- * For a random chain id consider using 'randomChainId'.
-- * For some arbitrary but fixed chain id consider using 'someChainId'.
--
newtype ChainId :: Type where
    ChainId :: Word32 -> ChainId
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving anyclass (Hashable, ToJSON, FromJSON, NFData)

instance ToJSONKey ChainId where
    toJSONKey = toJSONKeyText toText
    {-# INLINE toJSONKey #-}

instance FromJSONKey ChainId where
    fromJSONKey = FromJSONKeyTextParser (either fail return . eitherFromText)
    {-# INLINE fromJSONKey #-}

class HasChainId a where
    _chainId :: a -> ChainId
    _chainId = view chainId
    {-# INLINE _chainId #-}

    chainId :: Getter a ChainId
    chainId = to _chainId
    {-# INLINE chainId #-}

    {-# MINIMAL _chainId | chainId #-}

instance HasChainId ChainId where
    _chainId = id
    {-# INLINE _chainId #-}

instance HasChainId a => HasChainId (Expected a) where
    _chainId = _chainId . getExpected
    {-# INLINE _chainId #-}

instance HasChainId a => HasChainId (Actual a) where
    _chainId = _chainId . getActual
    {-# INLINE _chainId #-}

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag ChainId where
    type Tag ChainId = 'ChainIdTag
    toMerkleNode = encodeMerkleInputNode encodeChainId
    fromMerkleNode = decodeMerkleInputNode decodeChainId
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

checkChainId
    :: MonadThrow m
    => HasChainId expected
    => HasChainId actual
    => Expected expected
    -> Actual actual
    -> m ChainId
checkChainId expected actual = _chainId
    <$> check ChainIdMismatch (_chainId <$> expected) (_chainId <$> actual)
{-# INLINE checkChainId #-}

chainIdToText :: ChainId -> T.Text
chainIdToText (ChainId i) = sshow i
{-# INLINE chainIdToText #-}

chainIdFromText :: MonadThrow m => T.Text -> m ChainId
chainIdFromText = fmap ChainId . treadM
{-# INLINE chainIdFromText #-}

instance HasTextRepresentation ChainId where
    toText = chainIdToText
    {-# INLINE toText #-}
    fromText = chainIdFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Serialization

encodeChainId :: ChainId -> Put
encodeChainId (ChainId w32) = putWord32le w32
{-# INLINE encodeChainId #-}

decodeChainId :: Get ChainId
decodeChainId = ChainId <$> getWord32le
{-# INLINE decodeChainId #-}

decodeChainIdChecked
    :: HasChainId p
    => Expected p
    -> Get ChainId
decodeChainIdChecked p = checkChainId p . Actual =<< decodeChainId
{-# INLINE decodeChainIdChecked #-}

-- -------------------------------------------------------------------------- --
-- Type Level ChainId

-- it's easier to use Symbol here. If we wanted to use Nat we'd need a
-- typelevel function Nat -> Symbol
--
newtype ChainIdT = ChainIdT Symbol

data SomeChainIdT = forall (a :: ChainIdT)
    . KnownChainIdSymbol a => SomeChainIdT (Proxy a)

class KnownSymbol (ChainIdSymbol n) => KnownChainIdSymbol (n :: ChainIdT) where
    type ChainIdSymbol n :: Symbol
    chainIdSymbolVal :: Proxy n -> T.Text

instance KnownSymbol n => KnownChainIdSymbol ('ChainIdT n) where
    type ChainIdSymbol ('ChainIdT n) = n
    chainIdSymbolVal _ = T.pack $ symbolVal (Proxy @n)

someChainIdVal :: ChainId -> SomeChainIdT
someChainIdVal cid = case someSymbolVal (T.unpack (toText cid)) of
    (SomeSymbol (Proxy :: Proxy v)) -> SomeChainIdT (Proxy @('ChainIdT v))

-- -------------------------------------------------------------------------- --
-- Singletons

data instance Sing (n :: ChainIdT) where
    SChainId :: KnownChainIdSymbol n => Sing n

type SChainId (n :: ChainIdT) = Sing n

instance KnownChainIdSymbol n => SingI (n :: ChainIdT) where
    sing = SChainId

instance SingKind ChainIdT where
    type Demote ChainIdT = ChainId
    fromSing (SChainId :: Sing n) = unsafeFromText $ chainIdSymbolVal (Proxy @n)
    toSing n = case someChainIdVal n of
        SomeChainIdT p -> SomeSing (singByProxy p)

    {-# INLINE fromSing #-}
    {-# INLINE toSing #-}

pattern FromSingChainId :: Sing (n :: ChainIdT) -> ChainId
pattern FromSingChainId sng <- ((\cid -> withSomeSing cid SomeSing) -> SomeSing sng)
  where FromSingChainId sng = fromSing sng
{-# COMPLETE FromSingChainId #-}

-- -------------------------------------------------------------------------- --
-- Misc

-- | This function should be be rarely needed. Please consult the documentation
-- of 'ChainId' for alternative ways to obtain 'ChainId' values.
--
unsafeChainId :: Word32 -> ChainId
unsafeChainId = ChainId
{-# INLINE unsafeChainId #-}

chainIdInt :: Integral i => ChainId -> i
chainIdInt (ChainId cid) = int cid
{-# INLINE chainIdInt #-}

-- -------------------------------------------------------------------------- --
-- ChainMap

-- TODO: Shouldn't this type guarantee that the map is total, i.e. that there
-- exists a value for each chain?

-- | Values keyed by `ChainId`s, or a single value that applies for all chains.
data ChainMap a = AllChains a | OnChains (HashMap ChainId a)
    deriving stock (Eq, Functor, Foldable, Traversable, Generic, Ord, Show)
    deriving anyclass (Hashable, NFData)

-- TODO: fix this. This is not a legal instance, because `align` can change the
-- shape from `AllChains` to `OnChains`. This breaks the "alignedness" law.
instance Semialign ChainMap where
    align (OnChains l) (OnChains r) = OnChains $ align l r
    align (OnChains l) (AllChains r) = OnChains $ fmap (`These` r) l
    align (AllChains l) (OnChains r) = OnChains $ fmap (l `These`) r
    align (AllChains l) (AllChains r) = AllChains $ These l r

-- | A smart constructor, @onChains = OnChains . HM.fromList@.
onChains :: [(ChainId, a)] -> ChainMap a
onChains = OnChains . HM.fromList

-- | A smart constructor, @onChain c a = OnChains (HM.singleton c a)@.
onChain :: ChainId -> a -> ChainMap a
onChain c a = OnChains (HM.singleton c a)

-- | Zips two `ChainMap`s on their chain IDs.
chainZip :: (a -> a -> a) -> ChainMap a -> ChainMap a -> ChainMap a
chainZip f (OnChains l) (OnChains r) = OnChains $ HM.unionWith f l r
chainZip f (OnChains l) (AllChains r) = OnChains $ fmap (`f` r) l
chainZip f (AllChains l) (OnChains r) = OnChains $ fmap (l `f`) r
chainZip f (AllChains l) (AllChains r) = AllChains $ f l r

instance ToJSON a => ToJSON (ChainMap a) where
    toJSON (AllChains a) = object
        [ "allChains" .= a
        ]
    toJSON (OnChains m) = toJSON m

instance FromJSON a => FromJSON (ChainMap a) where
    parseJSON = withObject "ChainMap" $ \o ->
        (AllChains <$> o .: "allChains") <|> OnChains <$> parseJSON (Object o)

makePrisms ''ChainMap

-- | Provides access to the value at a `ChainId`, if it exists.
atChain :: HasChainId cid => cid -> Fold (ChainMap a) a
atChain cid = folding $ \case
    OnChains m -> m ^. at (_chainId cid)
    AllChains a -> Just a

type instance Index (ChainMap a) = ChainId
type instance IxValue (ChainMap a) = a

instance IxedGet (ChainMap a) where
    ixg i = atChain i
