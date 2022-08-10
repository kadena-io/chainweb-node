{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.RestAPI.Orphan
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Orphan type class instances for various types and classes that are used in
-- the definition of the Chainweb REST API.
--
-- This classes don't abstract over inherent (algebraic) properties of the
-- respective types  but instead just abstract over the behavior of a data types
-- in the context of a particular REST API. It therefor makes sense to implement
-- these as orphans in a module that is specific for the particular REST API.
-- The instances are brought into scope only in the modules that implement that
-- API and are out of scope otherwise.
--
module Chainweb.RestAPI.Orphans () where

import Control.Monad

import Data.Bifunctor
import Data.Proxy
import Data.Semigroup (Max(..), Min(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Servant.API

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Utils.Serialization
import Chainweb.Version

import P2P.Peer

import Pact.Parse (ParsedInteger(..))
import Pact.Server.API ()
import Pact.Types.Gas (GasLimit(..))

-- -------------------------------------------------------------------------- --
-- HttpApiData

instance ToHttpApiData GasLimit where
    toUrlPiece (GasLimit (ParsedInteger g)) = toUrlPiece g

instance FromHttpApiData GasLimit where
    parseUrlPiece p = GasLimit . ParsedInteger <$> parseUrlPiece p

instance ToHttpApiData PeerId where
    toUrlPiece = toText

instance FromHttpApiData PeerId where
    parseUrlPiece = first T.pack . eitherFromText

instance FromHttpApiData HostAddress where
    parseUrlPiece = first sshow . readHostAddressBytes . T.encodeUtf8

instance FromHttpApiData BlockHash where
    parseUrlPiece = first sshow
        . (runGetS decodeBlockHash <=< decodeB64UrlNoPaddingText)

instance ToHttpApiData BlockHash where
    toUrlPiece = encodeB64UrlNoPaddingText . runPutS . encodeBlockHash

instance FromHttpApiData BlockPayloadHash where
    parseUrlPiece = first sshow
        . (runGetS decodeBlockPayloadHash <=< decodeB64UrlNoPaddingText)

instance ToHttpApiData BlockPayloadHash where
    toUrlPiece = encodeB64UrlNoPaddingText . runPutS . encodeBlockPayloadHash

instance FromHttpApiData ChainwebVersion where
    parseUrlPiece = first T.pack . eitherFromText

instance ToHttpApiData ChainwebVersion where
    toUrlPiece = toText

instance FromHttpApiData ChainId where
    parseUrlPiece = first sshow . chainIdFromText

instance ToHttpApiData ChainId where
    toUrlPiece = chainIdToText

instance FromHttpApiData BlockHeight where
    parseUrlPiece = fmap BlockHeight . parseUrlPiece

instance ToHttpApiData BlockHeight where
    toUrlPiece (BlockHeight k) = toUrlPiece k

instance FromHttpApiData CutHeight where
    parseUrlPiece = fmap CutHeight . parseUrlPiece

instance ToHttpApiData CutHeight where
    toUrlPiece (CutHeight k) = toUrlPiece k

instance FromHttpApiData MinRank where
    parseUrlPiece = fmap (MinRank . Min) . parseUrlPiece

instance ToHttpApiData MinRank where
    toUrlPiece (MinRank (Min k)) = toUrlPiece k

instance FromHttpApiData MaxRank where
    parseUrlPiece = fmap (MaxRank . Max) . parseUrlPiece

instance ToHttpApiData MaxRank where
    toUrlPiece (MaxRank (Max k)) = toUrlPiece k

instance FromHttpApiData Eos where
    parseUrlPiece = fmap Eos . parseUrlPiece

instance ToHttpApiData Eos where
    toUrlPiece (Eos b) = toUrlPiece b

instance FromHttpApiData Limit where
    parseUrlPiece = fmap Limit . parseUrlPiece

instance ToHttpApiData Limit where
    toUrlPiece (Limit k) = toUrlPiece k

instance ToHttpApiData (NextItem BlockHash) where
    toUrlPiece = toText

instance FromHttpApiData (NextItem BlockHash) where
    parseUrlPiece = first sshow . nextItemFromText

instance ToHttpApiData (NextItem PeerId) where
    toUrlPiece = toText

instance FromHttpApiData (NextItem PeerId) where
    parseUrlPiece = first sshow . fromText

instance ToHttpApiData (NextItem Int) where
    toUrlPiece = toText

instance FromHttpApiData (NextItem Int) where
    parseUrlPiece = first sshow . fromText

instance ToHttpApiData SomeChainwebVersionT where
    toUrlPiece (SomeChainwebVersionT prox) = chainwebVersionSymbolVal prox

instance ToHttpApiData SomeChainIdT where
    toUrlPiece (SomeChainIdT prox) = chainIdSymbolVal prox

instance
    (KnownChainwebVersionSymbol sym, HasLink sub)
    => HasLink (sym :> sub)
  where
    type MkLink (sym :> sub) a = MkLink sub a
    toLink toA _ = toLink toA (Proxy @(ChainwebVersionSymbol sym :> sub))

instance
    (KnownChainIdSymbol sym, HasLink sub)
    => HasLink (sym :> sub)
  where
    type MkLink (sym :> sub) a = MkLink sub a
    toLink toA _ = toLink toA (Proxy @(ChainIdSymbol sym :> sub))

