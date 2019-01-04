{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.RestAPI.Orphan
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.RestAPI.Orphans () where

import Control.Lens
import Control.Monad

import Data.Aeson hiding (decode, encode)
import Data.Bifunctor
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import Data.Semigroup (Max(..), Min(..))
import Data.Serialize (decode, encode)
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UUID as V4

import Numeric.Natural

import Servant.API

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader (BlockHeader)
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.CutDB
import Chainweb.HostAddress hiding (properties)
import Chainweb.TreeDB hiding (properties)
import Chainweb.Utils
import Chainweb.Utils.Paging hiding (properties)
import Chainweb.Version

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- HttpApiData

instance ToHttpApiData PeerId where
    toUrlPiece = toText

instance FromHttpApiData PeerId where
    parseUrlPiece = first T.pack . eitherFromText

instance FromHttpApiData HostAddress where
    parseUrlPiece = first sshow . readHostAddressBytes . T.encodeUtf8

instance FromHttpApiData BlockHash where
    parseUrlPiece = first sshow . (decode <=< first show . decodeB64UrlNoPaddingText)

instance ToHttpApiData BlockHash where
    toUrlPiece = encodeB64UrlNoPaddingText . encode

instance FromHttpApiData ChainwebVersion where
    parseUrlPiece = first T.pack . eitherFromText

instance ToHttpApiData ChainwebVersion where
    toUrlPiece = toText

instance FromHttpApiData ChainId where
    parseUrlPiece = first sshow . chainIdFromText

instance ToHttpApiData ChainId where
    toUrlPiece = chainIdToText

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

-- -------------------------------------------------------------------------- --
-- Swagger ParamSchema

instance ToParamSchema PeerId where
    toParamSchema _ = toParamSchema (Proxy @V4.UUID)

instance ToParamSchema BlockHash where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & format ?~ "byte"
        & maxLength ?~ int blockHashBytesCount
        & minLength ?~ int blockHashBytesCount

instance ToParamSchema BlockHeader where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & format ?~ "byte"

instance ToParamSchema MinRank where
    toParamSchema _ = mempty
        & type_ .~ SwaggerInteger
        & minimum_ ?~ 0
        & exclusiveMinimum ?~ False

instance ToParamSchema MaxRank where
    toParamSchema _ = mempty
        & type_ .~ SwaggerInteger
        & minimum_ ?~ 0
        & exclusiveMinimum ?~ False

instance ToParamSchema Limit where
    toParamSchema _ = mempty
        & type_ .~ SwaggerInteger
        & minimum_ ?~ 0
        & exclusiveMinimum ?~ False

instance ToParamSchema Eos where
    toParamSchema _ = mempty
        & type_ .~ SwaggerBoolean

instance ToParamSchema (NextItem BlockHash) where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & pattern ?~ "(inclusive|exclusive):key"

instance ToParamSchema (NextItem PeerId) where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & pattern ?~ "(inclusive|exclusive):key"

instance ToParamSchema (NextItem Int) where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & pattern ?~ "(inclusive|exclusive):key"

instance ToParamSchema ChainId where
    toParamSchema _ = mempty
        & type_ .~ SwaggerInteger
        & format ?~ "word32"

instance ToParamSchema ChainwebVersion where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & enum_ ?~ (toJSON <$> [Simulation, Test, Testnet00])

-- -------------------------------------------------------------------------- --
-- Swagger Schema

instance ToSchema Swagger where
    declareNamedSchema _ = return $ NamedSchema (Just "Swagger")
        $ sketchSchema ("swagger specification" :: T.Text)

instance ToSchema PeerInfo

instance ToSchema ChainId

instance ToSchema PeerId where
    declareNamedSchema _ = declareNamedSchema (Proxy @V4.UUID)

instance ToSchema HostAddress where
    declareNamedSchema _ = return $ NamedSchema (Just "HostAddress") $ mempty
        & type_ .~ SwaggerString
        & pattern ?~ "<hostname>:<port>"
        & minLength ?~ 3
        & maxLength ?~ 258

instance ToSchema BlockHash where
    declareNamedSchema _ = return $ NamedSchema (Just "Key") $ byteSchema
        & minLength ?~ int blockHashBytesCount
        & maxLength ?~ int blockHashBytesCount

instance ToSchema BlockHeader where
    declareNamedSchema _ = return $ NamedSchema (Just "Entry") byteSchema

instance ToSchema CutHashes where
    declareNamedSchema _ = do
        mapSchema <- declareSchemaRef (Proxy @(HM.HashMap ChainId BlockHash))
        peerSchema <- declareSchemaRef (Proxy @PeerInfo)
        return $ NamedSchema (Just "CutHashes") $ mempty
            & type_ .~ SwaggerObject
            & properties .~
                [ ("hashes", mapSchema)
                , ("origin", peerSchema)
                ]
            & required .~ [ "hashes" ]

instance ToSchema (NextItem k) where
    declareNamedSchema _ = return $ NamedSchema (Just "next") $ mempty
        & type_ .~ SwaggerString
        & pattern ?~ "(inclusive|exclusive):<Key>"
        & minLength ?~ 10 + 1

instance (ToSchema a) => ToSchema (Page k a) where
    declareNamedSchema _ = do
        naturalSchema <- declareSchemaRef (Proxy @Natural)
        keySchema <- declareSchemaRef (Proxy @(NextItem k))
        itemsSchema <- declareSchemaRef (Proxy @[a])
        return $ NamedSchema (Just "Page") $ mempty
            & type_ .~ SwaggerObject
            & properties .~
                [ ("limit", naturalSchema)
                , ("items", itemsSchema)
                , ("next", keySchema)
                ]
            & required .~ [ "limit", "items" ]

instance ToSchema (BranchBounds BlockHeaderDb) where
    declareNamedSchema _ = do
        setSchema <- declareSchemaRef (Proxy @[BlockHash])
        return $ NamedSchema (Just "BranchBounds") $ mempty
            & type_ .~ SwaggerObject
            & properties .~
                [ ("upper", setSchema)
                , ("lower", setSchema)
                ]
            & required .~ [ "upper", "lower" ]
