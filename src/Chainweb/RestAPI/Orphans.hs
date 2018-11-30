{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.ChainDB.RestAPI.Orphan
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

import Data.Aeson hiding (encode, decode)
import Data.Bifunctor
import Data.Proxy
import Data.Semigroup (Min(..), Max(..))
import Data.Serialize (encode, decode)
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UUID as V4

import Servant.API

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader (BlockHeader)
import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.Configuration

-- -------------------------------------------------------------------------- --
-- HttpApiData

deriving newtype instance ToHttpApiData PeerId
deriving newtype instance FromHttpApiData PeerId

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

instance FromHttpApiData Limit where
    parseUrlPiece = fmap Limit . parseUrlPiece

instance ToHttpApiData Limit where
    toUrlPiece (Limit k) = toUrlPiece k

instance FromHttpApiData (Bounds BlockHash) where
    parseUrlPiece t =
        let (a, b) = T.break (== ',') t
        in (\a' b' -> Bounds (LowerBound a') (UpperBound b'))
           <$> parseUrlPiece a
           <*> parseUrlPiece (T.drop 1 b)

instance ToHttpApiData (Bounds BlockHash) where
    toUrlPiece (Bounds (LowerBound l) (UpperBound u)) = toUrlPiece l <> "," <> toUrlPiece u

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
    & type_            .~ SwaggerInteger
    & minimum_         ?~ 0
    & exclusiveMinimum ?~ False

instance ToParamSchema MaxRank where
  toParamSchema _ = mempty
    & type_            .~ SwaggerInteger
    & minimum_         ?~ 0
    & exclusiveMinimum ?~ False

instance ToParamSchema Limit where
  toParamSchema _ = mempty
    & type_            .~ SwaggerInteger
    & minimum_         ?~ 0
    & exclusiveMinimum ?~ False

instance ToParamSchema (Bounds BlockHash) where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & pattern ?~ "key,key"

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
