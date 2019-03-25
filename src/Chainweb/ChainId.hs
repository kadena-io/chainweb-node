{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.ChainId
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.ChainId
( ChainIdException(..)
, ChainId(..)
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

-- * Testing
, accursedUnutterableChainId
, accursedUnutterableGetChainId
) where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Catch (Exception, MonadThrow)

import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Signed
import Data.Hashable (Hashable(..))
import Data.Kind
import Data.Proxy
import qualified Data.Text as T
import Data.Word (Word32)

import GHC.Generics (Generic)
import GHC.TypeLits

import Test.QuickCheck (Arbitrary(..))

-- internal imports

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Utils

import Data.Singletons

-- -------------------------------------------------------------------------- --
-- Exceptions

data ChainIdException
    = ChainIdMismatch (Expected ChainId) (Actual ChainId)
    deriving (Show, Eq, Ord, Generic)

instance Exception ChainIdException

-- -------------------------------------------------------------------------- --
-- ChainId

-- | ChainId /within a Chainweb/.
--
-- Generally a block chain is /globally/ uniquely identified by its genesis hash.
-- This type only uniquely identifies the chain /locally/ within the context of
-- a chainweb.
--
-- However, the chainweb context is globally uniquely identified by the
-- 'ChainwebVersion', which in turn is determined by the identities of the
-- chains in the chainweb. Since the chainweb topology is statically represented
-- on the type level, while the Chainweb version is a runtime value, we break
-- the cycle by using the Chainweb version as globally unique identifier and
-- include the 'ChainwebVersion' and the 'ChainId' into the genesis hash.
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

instance IsMerkleLogEntry ChainwebHashTag ChainId where
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

encodeChainId :: MonadPut m => ChainId -> m ()
encodeChainId (ChainId i32) = putWord32le $ unsigned i32
{-# INLINE encodeChainId #-}

decodeChainId :: MonadGet m => m ChainId
decodeChainId = ChainId <$> getWord32le
{-# INLINE decodeChainId #-}

decodeChainIdChecked
    :: MonadGet m
    => MonadThrow m
    => HasChainId p
    => Expected p
    -> m ChainId
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

-- -------------------------------------------------------------------------- --
-- Testing

-- | Generally, the 'ChainId' is determined by the genesis block of a chain for
-- a given 'Chainweb.Version'. This constructor is only for testing.
--
accursedUnutterableChainId :: Word32 -> ChainId
accursedUnutterableChainId = ChainId
{-# INLINE accursedUnutterableChainId #-}

accursedUnutterableGetChainId :: ChainId -> Word32
accursedUnutterableGetChainId (ChainId cid) = cid

instance Arbitrary ChainId where
    arbitrary = accursedUnutterableChainId <$> arbitrary
