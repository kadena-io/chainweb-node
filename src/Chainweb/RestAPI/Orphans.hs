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
module Chainweb.RestAPI.Orphans
(
) where

import Control.Lens
import Control.Monad

import Data.Aeson
import Data.Bifunctor
import Data.Proxy
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UUID as V4

import Servant.API

-- internal modules
import Chainweb.BlockHash
import Chainweb.ChainDB
import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.Configuration

-- -------------------------------------------------------------------------- --
-- HttpApiData

deriving newtype instance ToHttpApiData PeerId
deriving newtype instance FromHttpApiData PeerId

instance FromHttpApiData HostAddress where
    parseUrlPiece = first sshow . readHostAddressBytes . T.encodeUtf8

instance FromHttpApiData (Key 'Unchecked) where
    parseUrlPiece = first sshow . (decodeKey <=< decodeB64UrlNoPaddingText)

instance ToHttpApiData (Key 'Unchecked) where
    toUrlPiece = encodeB64UrlNoPaddingText . encodeKey

instance FromHttpApiData ChainwebVersion where
    parseUrlPiece = first T.pack . eitherFromText

instance ToHttpApiData ChainwebVersion where
    toUrlPiece = toText

instance FromHttpApiData ChainId where
    parseUrlPiece = first sshow . chainIdFromText

instance ToHttpApiData ChainId where
    toUrlPiece = chainIdToText

instance FromHttpApiData (Key 'Unchecked, Key 'Unchecked) where
    parseUrlPiece t =
        let (a, b) = T.break (== ',') t
        in (,) <$> parseUrlPiece a <*> parseUrlPiece (T.drop 1 b)

instance ToHttpApiData (Key 'Unchecked, Key 'Unchecked) where
    toUrlPiece (a,b) = toUrlPiece a <> "," <> toUrlPiece b

-- -------------------------------------------------------------------------- --
-- Swagger ParamSchema

instance ToParamSchema PeerId where
    toParamSchema _ = toParamSchema (Proxy @V4.UUID)

instance ToParamSchema (Key t) where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & format ?~ "byte"
        & maxLength ?~ int blockHashBytesCount
        & minLength ?~ int blockHashBytesCount

instance ToParamSchema (Entry t) where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & format ?~ "byte"

instance ToParamSchema (Key t, Key t) where
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

instance ToSchema (Key t) where
    declareNamedSchema _ = return $ NamedSchema (Just "Key") $ byteSchema
        & minLength ?~ int blockHashBytesCount
        & maxLength ?~ int blockHashBytesCount

instance ToSchema (Entry t) where
    declareNamedSchema _ = return $ NamedSchema (Just "Entry") byteSchema

