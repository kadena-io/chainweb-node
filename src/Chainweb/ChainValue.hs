{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.ChainValue
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.ChainValue
( ChainValue(..)
, chainValueChainId
, chainValueValue
, chainValue
, chainLookup
, chainLookupM
, type ChainValueCasLookup
) where

import Control.DeepSeq
import Chainweb.ChainId
import Chainweb.Storage.Table
import Chainweb.Utils (HasTextRepresentation(..))
import Control.Lens
import Data.Hashable
import GHC.Generics
import Data.Text qualified as T

-- -------------------------------------------------------------------------- --
-- Tag Values With a ChainId

data ChainValue a = ChainValue
    { _chainValueChainId :: !ChainId
    , _chainValueValue :: !a
    }
    deriving (Show, Eq, Ord, Generic)
    deriving (Functor, Foldable, Traversable)
    deriving anyclass (NFData, Hashable)

makeLenses ''ChainValue

instance TraversableWithIndex ChainId ChainValue where
  itraverse f (ChainValue cid v) = ChainValue cid <$> f cid v
  {-# INLINE itraverse #-}

instance FoldableWithIndex ChainId ChainValue
instance FunctorWithIndex ChainId ChainValue

instance HasTextRepresentation a => HasTextRepresentation (ChainValue a) where
    toText (ChainValue cid a) = toText cid <> ":" <> toText a
    fromText t = case T.breakOn ":" t of
        (c, r) -> ChainValue <$> fromText c <*> fromText (T.tail r)
    {-# INLINE toText #-}

-- | If a type is already an instance of 'IsCasValue', adding the chain does
-- preserve this property. By also wrapping the key it is possible to shard
-- the CAS by chain value.
--
instance IsCasValue a => IsCasValue (ChainValue a) where
    type CasKeyType (ChainValue a) = ChainValue (CasKeyType a)
    casKey (ChainValue cid a) = ChainValue cid (casKey a)
    {-# INLINE casKey #-}

instance HasChainId (ChainValue a) where
    _chainId (ChainValue cid _) = cid
    {-# INLINE _chainId #-}

chainValue :: HasChainId a => a -> ChainValue a
chainValue a = ChainValue (_chainId a) a
{-# INLINE chainValue #-}

chainLookup
    :: ReadableCas db (ChainValue a)
    => db
    -> CasKeyType (ChainValue a)
    -> IO (Maybe a)
chainLookup db = fmap (fmap _chainValueValue) . tableLookup db
{-# INLINE chainLookup #-}

chainLookupM
    :: ReadableCas db (ChainValue a)
    => db
    -> CasKeyType (ChainValue a)
    -> IO a
chainLookupM db = fmap _chainValueValue . tableLookupM db
{-# INLINE chainLookupM #-}

type ChainValueCasLookup a b = ReadableCas a (ChainValue b)

