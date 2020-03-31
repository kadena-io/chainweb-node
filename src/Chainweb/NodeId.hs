{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
-- * Chainweb Node Id
  NodeId(..)
, nodeIdId
, encodeNodeId
, decodeNodeId
, nodeIdToText
, nodeIdFromText
) where

import Control.DeepSeq
import Control.Lens
import Control.Monad ((<$!>))
import Control.Monad.Catch

import Data.Aeson
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Hashable
import qualified Data.Text as T
import Data.Word

import GHC.Generics

import Test.QuickCheck

-- internal imports

import Chainweb.Utils

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
decodeNodeId = NodeId <$!> getWord64le
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
