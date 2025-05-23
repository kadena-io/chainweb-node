{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Chainweb.Core.CryptoHash
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.Parent
import Chainweb.PayloadProvider
import Chainweb.Ranked
import Chainweb.Utils
import Control.Lens
import Data.Aeson
import Data.ByteString qualified as B
import Data.ByteString.Short qualified as BS
import Data.Coerce
import Data.Hash.Class.Mutable
import Data.Hash.SHA2 (Sha2_512_256(..) {- Coercible AdjacentsHashAlgorithm BS.ShortByteString -})
import Data.Foldable
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import Numeric.Natural

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

instance Brief a => Brief (NonEmpty a) where
    brief = brief . toList

instance Brief a => Brief (Parent a) where
    brief = brief . unwrapParent

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
instance Brief (Parent BlockHeader) where brief = brief . unwrapParent
deriving
    via (CryptoHash AdjacentsHashAlgorithm)
    instance Brief AdjacentsHash
instance Brief ConsensusPayload where
    brief = brief . _consensusPayloadHash
instance Brief p => Brief (EvaluationCtx p) where
    brief ec =
        "payload:" <> brief (_evaluationCtxPayload ec)
        <> ":parent:" <> brief (_evaluationCtxRankedParentHash ec)

deriving
    via (BriefText (CryptoHash a))
    instance (IncrementalHash a, Coercible a BS.ShortByteString) => Brief (CryptoHash a)

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
    brief ss = T.intercalate ":"
        [ brief (_syncStateHeight ss)
        , brief (_syncStateBlockHash ss)
        , brief (_syncStateBlockPayloadHash ss)
        ]

instance Brief ConsensusState where
    brief cs =
        "{ \"latest\": \"" <> brief (_consensusStateLatest cs)
        <> "\", \"safe\": \"" <> brief (_consensusStateSafe cs)
        <> "\", \"final\": \"" <> brief (_consensusStateFinal cs)
        <> "\" }"

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

-- -------------------------------------------------------------------------- --
-- Tools for Deriving Via

newtype ShowBrief a = ShowBrief a
instance Show a => Brief (ShowBrief a) where
    brief (ShowBrief a) = T.take 6 $ T.pack $ show a

newtype BriefBase64ByteString = BriefBase64ByteString B.ByteString
instance Brief BriefBase64ByteString where
    brief (BriefBase64ByteString bytes) = T.take 6
        $ encodeB64UrlNoPaddingText
        $ bytes

newtype BriefBase64ShortByteString = BriefBase64ShortByteString BS.ShortByteString
instance Brief BriefBase64ShortByteString where
    brief (BriefBase64ShortByteString bytes) = T.take 6
        $ encodeB64UrlNoPaddingText
        $ BS.fromShort bytes

newtype BriefText a = BriefText a
instance HasTextRepresentation a => Brief (BriefText a) where
    brief (BriefText t) = toTextShort t
