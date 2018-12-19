{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

-- |
-- Module: Chainweb.NodeId
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.NodeId
(
-- * Chain Node Id
  ChainNodeId(..)
, chainNodeIdChain
, chainNodeIdId
, encodeChainNodeId
, decodeChainNodeId
, decodeChainNodeIdChecked
, chainNodeIdToText
, chainNodeIdFromText

-- * Chainweb Node Id
, ChainwebNodeId(..)
, chainwebNodeIdId
, encodeChainwebNodeId
, decodeChainwebNodeId
, chainwebNodeIdToText
, chainwebNodeIdFromText
, nodeIdFromChainwebNodeId
) where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Catch

import Data.Aeson
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Hashable
import qualified Data.Text as T
import Data.Word

import GHC.Generics

import Test.QuickCheck

-- Internal imports

import Chainweb.ChainId
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Chain NodeId

data ChainNodeId = ChainNodeId
    { _chainNodeIdChain :: !ChainId
    , _chainNodeIdId :: !Word64
    }
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData, FromJSON, ToJSON)

makeLenses ''ChainNodeId

instance HasChainId ChainNodeId where
    _chainId = _chainNodeIdChain
    {-# INLINE _chainId #-}

encodeChainNodeId :: MonadPut m => ChainNodeId -> m ()
encodeChainNodeId (ChainNodeId cid i) = encodeChainId cid >> putWord64le i
{-# INLINE encodeChainNodeId #-}

decodeChainNodeId :: MonadGet m => m ChainNodeId
decodeChainNodeId = ChainNodeId <$> decodeChainId <*> getWord64le
{-# INLINE decodeChainNodeId #-}

decodeChainNodeIdChecked
    :: MonadThrow m
    => MonadGet m
    => HasChainId p
    => Expected p
    -> m ChainNodeId
decodeChainNodeIdChecked p = ChainNodeId <$> decodeChainIdChecked p <*> getWord64le
{-# INLINE decodeChainNodeIdChecked #-}

chainNodeIdToText :: ChainNodeId -> T.Text
chainNodeIdToText (ChainNodeId c i) = sshow i <> "/" <> chainIdToText c
{-# INLINE chainNodeIdToText #-}

chainNodeIdFromText :: MonadThrow m => T.Text -> m ChainNodeId
chainNodeIdFromText t = case T.break (== '/') t of
    (a, b)
        | not (T.null b) -> ChainNodeId <$> fromText (T.drop 1 b) <*> treadM a
        | otherwise -> throwM . TextFormatException
            $ "Missing '/' in \"" <> a <> "\"."

instance HasTextRepresentation ChainNodeId where
    toText = chainNodeIdToText
    {-# INLINE toText #-}
    fromText = chainNodeIdFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Chainweb NodeId

newtype ChainwebNodeId = ChainwebNodeId
    { _chainwebNodeIdId :: Word64
    }
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData, FromJSON, ToJSON)
    deriving newtype (Arbitrary)

makeLenses ''ChainwebNodeId

encodeChainwebNodeId :: MonadPut m => ChainwebNodeId -> m ()
encodeChainwebNodeId (ChainwebNodeId i) = putWord64le i
{-# INLINE encodeChainwebNodeId #-}

decodeChainwebNodeId :: MonadGet m => m ChainwebNodeId
decodeChainwebNodeId = ChainwebNodeId <$> getWord64le
{-# INLINE decodeChainwebNodeId #-}

chainwebNodeIdToText :: ChainwebNodeId -> T.Text
chainwebNodeIdToText (ChainwebNodeId i) = sshow i
{-# INLINE chainwebNodeIdToText #-}

chainwebNodeIdFromText :: MonadThrow m => T.Text -> m ChainwebNodeId
chainwebNodeIdFromText = fmap ChainwebNodeId . treadM

instance HasTextRepresentation ChainwebNodeId where
    toText = chainwebNodeIdToText
    {-# INLINE toText #-}
    fromText = chainwebNodeIdFromText
    {-# INLINE fromText #-}

nodeIdFromChainwebNodeId :: ChainwebNodeId -> ChainId -> ChainNodeId
nodeIdFromChainwebNodeId (ChainwebNodeId i) cid = ChainNodeId cid i
{-# INLINE nodeIdFromChainwebNodeId #-}

