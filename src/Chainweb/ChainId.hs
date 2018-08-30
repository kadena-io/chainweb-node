{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

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
, ChainId
, HasChainId(..)
, checkChainId
, prettyChainId

-- * Serialization

, encodeChainId
, decodeChainId
, decodeChainIdChecked

-- * Testing
, testChainId
) where

import Control.Lens
import Control.Monad.Catch (Exception, MonadThrow)

import Data.Aeson (ToJSON(..), FromJSON(..), ToJSONKey(..), FromJSONKey(..))
import Data.Aeson.Types (toJSONKeyText, FromJSONKeyFunction(..))
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Signed
import Data.Hashable (Hashable(..))
import Data.Int
import Data.Kind
import qualified Data.Text as T

import GHC.Generics (Generic)

import Prelude.Unicode

-- internal imports

import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Exceptions

data ChainIdException
    = ChainIdMissmatch (Expected ChainId) (Actual ChainId)
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
newtype ChainId ∷ Type where
    ChainId ∷ Int32 → ChainId
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving anyclass (Hashable, ToJSON, FromJSON)

instance ToJSONKey ChainId where
    toJSONKey = toJSONKeyText sshow
    {-# INLINE toJSONKey #-}

instance FromJSONKey ChainId where
    fromJSONKey = FromJSONKeyText (ChainId ∘ read ∘ T.unpack)
    {-# INLINE fromJSONKey #-}

class HasChainId a where
    _chainId ∷ a → ChainId
    _chainId = view chainId
    {-# INLINE _chainId #-}

    chainId ∷ Getter a ChainId
    chainId = to _chainId
    {-# INLINE chainId #-}

    {-# MINIMAL _chainId | chainId #-}

instance HasChainId ChainId where
    _chainId = id
    {-# INLINE _chainId #-}

instance HasChainId a ⇒ HasChainId (Expected a) where
    _chainId = _chainId ∘ getExpected
    {-# INLINE _chainId #-}

instance HasChainId a ⇒ HasChainId (Actual a) where
    _chainId = _chainId ∘ getActual
    {-# INLINE _chainId #-}

checkChainId
    ∷ MonadThrow m
    ⇒ HasChainId expected
    ⇒ HasChainId actual
    ⇒ Expected expected
    → Actual actual
    → m ChainId
checkChainId expected actual = _chainId
    <$> check ChainIdMissmatch (_chainId <$> expected) (_chainId <$> actual)
{-# INLINE checkChainId #-}

prettyChainId ∷ ChainId → T.Text
prettyChainId (ChainId i) = sshow i
{-# INLINE prettyChainId #-}

-- -------------------------------------------------------------------------- --
-- $Serialization
--
-- ChainIds can be deserialized in two ways:
--

encodeChainId ∷ MonadPut m ⇒ ChainId → m ()
encodeChainId (ChainId i32) = putWord32le $ unsigned i32
{-# INLINE encodeChainId #-}

decodeChainId ∷ MonadGet m ⇒ m ChainId
decodeChainId = ChainId ∘ signed <$> getWord32le
{-# INLINE decodeChainId #-}

decodeChainIdChecked
    ∷ MonadGet m
    ⇒ MonadThrow m
    ⇒ HasChainId p
    ⇒ Expected p
    → m ChainId
decodeChainIdChecked p = checkChainId p ∘ Actual =<< decodeChainId
{-# INLINE decodeChainIdChecked #-}

-- -------------------------------------------------------------------------- --
-- Testing

-- | Generally, the 'ChainId' is determined by the genesis block of a chain for
-- a given 'Chainweb.Version'. This constructor is only for testing.
--
testChainId ∷ Int32 → ChainId
testChainId = ChainId
{-# INLINE testChainId #-}
