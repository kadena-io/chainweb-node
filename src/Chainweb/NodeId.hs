{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
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
( NodeId(..)
, nodeIdChain
, nodeIdId
, encodeNodeId
, decodeNodeId
, decodeNodeIdCheckedChain
, prettyNodeId
) where

import Control.Lens
import Control.Monad.Catch

import Data.Aeson
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Hashable
import Data.Kind
import Data.Monoid
import qualified Data.Text as T
import Data.Word

import GHC.Generics

-- Internal imports

import Chainweb.ChainId
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- NodeId

data NodeId :: Type where
    NodeId :: { _nodeIdChain :: !ChainId, _nodeIdId :: !Word64 } -> NodeId
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving anyclass (Hashable, FromJSON, ToJSON)

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

prettyNodeId :: NodeId -> T.Text
prettyNodeId (NodeId c i) = sshow i <> "/" <> prettyChainId c
{-# INLINE prettyNodeId #-}

