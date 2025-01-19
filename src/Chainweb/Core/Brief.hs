{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module: Chainweb.Core.Brief
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Core.Brief
( Brief(..)
, toTextShort

-- * Utils
, briefJson
, BriefJson
, briefValue
) where

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.PayloadProvider
import Chainweb.Ranked
import Chainweb.Utils
import Control.Lens
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Text qualified as T
import Numeric.Natural
import Data.Aeson

-- -------------------------------------------------------------------------- --
-- Adhoc class for brief logging output

class Brief a where
    brief :: a -> T.Text

toTextShort :: HasTextRepresentation a => a -> T.Text
toTextShort = T.take 6 . toText

-- -------------------------------------------------------------------------- --
-- Generic Instances

instance (Brief a, Brief b) => Brief (a,b) where
    brief (a,b) = "(" <> brief a <> "," <> brief b <> ")"

instance (Brief a, Brief b) => Brief (Either a b) where
    brief (Left a) = "left:" <> brief a
    brief (Right b) = "right:" <> brief b

instance Brief a => Brief (Maybe a) where
    brief (Just a) = brief a
    brief Nothing = "nothing"

instance Brief a => Brief [a] where
    brief l = "[" <> (T.intercalate "," $ brief <$> l) <> "]"

-- -------------------------------------------------------------------------- --
-- Core Chainweb Types

instance Brief a => Brief (Ranked a) where
    brief (Ranked r h) = sshow r <> ":" <> brief h

instance Brief CutHeight where brief = toText . int @_ @Natural
instance Brief BlockHeight where brief = toText . int @_ @Natural
instance Brief CutId where brief = toTextShort
instance Brief ChainId where brief = toText
instance Brief BlockHash where brief = toTextShort
instance Brief BlockPayloadHash where brief = toTextShort
instance Brief BlockHeader where brief = brief . view blockHash
instance Brief ParentHeader where brief = brief . _parentHeader

instance Brief BlockHashWithHeight where
    brief a = brief (_bhwhHeight a) <> ":" <> brief (_bhwhHash a)

instance Brief CutHashes where
    brief c = T.intercalate ":"
        [ brief (_cutHashesId c)
        , brief (_cutHashesHeight c)
        , brief (L.sort $ HM.toList $ _cutHashes c)
        ]

instance Brief Cut where
    brief = brief . cutToCutHashes Nothing

instance Brief NewPayload where
    brief np = brief (_newPayloadChainId np)
        <> ":" <> brief (_newPayloadRankedParentHash np)

instance Brief SyncState where
    brief = briefJson

-- -------------------------------------------------------------------------- --
-- Utils

newtype BriefJson a = BriefJson a

instance ToJSON a => ToJSON (BriefJson a) where
    toJSON (BriefJson a) = briefValue $ toJSON a

briefJson :: ToJSON a => a -> T.Text
briefJson = encodeToText . BriefJson

briefValue :: Value -> Value
briefValue (Object o) = Object (briefValue <$> o)
briefValue (Array a) = Array (briefValue <$> a)
briefValue (String t) = String (toTextShort t)
briefValue n = n

