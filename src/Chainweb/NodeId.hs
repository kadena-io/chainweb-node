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
  NodeId(..)
, nodeIdChain
, nodeIdId
, encodeNodeId
, decodeNodeId
, decodeNodeIdCheckedChain
, nodeIdToText
, nodeIdFromText

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
import Data.Kind
import qualified Data.Text as T
import Data.Word

import GHC.Generics

import Test.QuickCheck

-- Internal imports

import Chainweb.ChainId
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- NodeId

data NodeId :: Type where
    NodeId :: { _nodeIdChain :: !ChainId, _nodeIdId :: !Word64 } -> NodeId
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData, FromJSON, ToJSON)

makeLenses ''NodeId

instance HasChainId NodeId where
    _chainId = _nodeIdChain
    {-# INLINE _chainId #-}

encodeNodeId :: MonadPut m => NodeId -> m ()
encodeNodeId (NodeId cid i) = encodeChainId cid >> putWord64le i
{-# INLINE encodeNodeId #-}

decodeNodeId :: MonadGet m => m NodeId
decodeNodeId = NodeId <$> decodeChainId <*> getWord64le
{-# INLINE decodeNodeId #-}

decodeNodeIdCheckedChain
    :: MonadThrow m
    => MonadGet m
    => HasChainId p
    => Expected p
    -> m NodeId
decodeNodeIdCheckedChain p = NodeId <$> decodeChainIdChecked p <*> getWord64le
{-# INLINE decodeNodeIdCheckedChain #-}

nodeIdToText :: NodeId -> T.Text
nodeIdToText (NodeId c i) = sshow i <> "/" <> chainIdToText c
{-# INLINE nodeIdToText #-}

nodeIdFromText :: MonadThrow m => T.Text -> m NodeId
nodeIdFromText t = case T.break (== '/') t of
    (a, b)
        | not (T.null b) -> NodeId <$> fromText (T.drop 1 b) <*> treadM a
        | otherwise -> throwM . TextFormatException
            $ "Missing '/' in \"" <> a <> "\"."

instance HasTextRepresentation NodeId where
    toText = nodeIdToText
    {-# INLINE toText #-}
    fromText = nodeIdFromText
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

nodeIdFromChainwebNodeId :: ChainwebNodeId -> ChainId -> NodeId
nodeIdFromChainwebNodeId (ChainwebNodeId i) cid = NodeId cid i
{-# INLINE nodeIdFromChainwebNodeId #-}

