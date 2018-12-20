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
, NodeId(..)
, nodeIdId
, encodeNodeId
, decodeNodeId
, nodeIdToText
, nodeIdFromText
, nodeIdFromNodeId
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

newtype NodeId = NodeId
    { _nodeIdId :: Word64
    }
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData, FromJSON, ToJSON)
    deriving newtype (Arbitrary)

makeLenses ''NodeId

encodeNodeId :: MonadPut m => NodeId -> m ()
encodeNodeId (NodeId i) = putWord64le i
{-# INLINE encodeNodeId #-}

decodeNodeId :: MonadGet m => m NodeId
decodeNodeId = NodeId <$> getWord64le
{-# INLINE decodeNodeId #-}

nodeIdToText :: NodeId -> T.Text
nodeIdToText (NodeId i) = sshow i
{-# INLINE nodeIdToText #-}

nodeIdFromText :: MonadThrow m => T.Text -> m NodeId
nodeIdFromText = fmap NodeId . treadM

instance HasTextRepresentation NodeId where
    toText = nodeIdToText
    {-# INLINE toText #-}
    fromText = nodeIdFromText
    {-# INLINE fromText #-}

nodeIdFromNodeId :: NodeId -> ChainId -> ChainNodeId
nodeIdFromNodeId (NodeId i) cid = ChainNodeId cid i
{-# INLINE nodeIdFromNodeId #-}

