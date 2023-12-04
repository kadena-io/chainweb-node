{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Pact.RestAPI.SPV
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Types and Utils for the Pact SPV2 API
--
module Chainweb.Pact.RestAPI.SPV
( SpvAlgorithm(..)
, SpvSubjectType(..)
, SpvSubjectIdentifier(..)
, Spv2Request(..)
) where

import Data.Aeson
import qualified Data.Text as T

import GHC.Generics

import Numeric.Natural

import Pact.Types.Command
import Pact.JSON.Legacy.Value

-- internal modules

import Chainweb.ChainId

-- -------------------------------------------------------------------------- --
-- SPV Algorithm

data SpvAlgorithm
    = SpvSHA512t_256
    | SpvKeccak_256
    deriving (Show, Eq, Ord, Generic)

instance ToJSON SpvAlgorithm where
    toJSON SpvSHA512t_256 = "SHA512t_256"
    toJSON SpvKeccak_256 = "Keccak_256"
    {-# INLINE toJSON #-}

    toEncoding SpvSHA512t_256 = toEncoding @String "SHA512t_256"
    toEncoding SpvKeccak_256 = toEncoding @String "Keccak_256"
    {-# INLINE toEncoding #-}

instance FromJSON SpvAlgorithm where
    parseJSON = withText "SpvAlgorithm" $ \case
        "SHA512t_256" -> pure SpvSHA512t_256
        "Keccak_256" -> pure SpvKeccak_256
        t -> fail $ "unrecognized SPV algorithm: " <> T.unpack t

-- -------------------------------------------------------------------------- --
-- SPV Subject Type

data SpvSubjectType
    = SpvSubjectResult
    | SpvSubjectEvents
    deriving (Show, Eq, Ord, Generic)

instance ToJSON SpvSubjectType where
    toJSON SpvSubjectResult = "result"
    toJSON SpvSubjectEvents = "events"
    {-# INLINE toJSON #-}

    toEncoding SpvSubjectResult = toEncoding @String "result"
    toEncoding SpvSubjectEvents = toEncoding @String "events"
    {-# INLINE toEncoding #-}

instance FromJSON SpvSubjectType where
    parseJSON = withText "SpvType" $ \case
        "result" -> pure SpvSubjectResult
        "events" -> pure SpvSubjectEvents
        t -> fail $ "unrecognized SPV Subject type: " <> T.unpack t

-- -------------------------------------------------------------------------- --
-- SPV Subject Identifier

data SpvSubjectIdentifier = SpvSubjectIdentifier
    { _spvSubjectIdType :: !SpvSubjectType
    , _spvSubjectIdChain :: !ChainId
    , _spvSubjectIdReqKey :: !RequestKey
    }
    deriving (Show, Eq, Ord, Generic)

spvSubjectIdentifierProperties :: KeyValue e kv => SpvSubjectIdentifier -> [kv]
spvSubjectIdentifierProperties o =
    [ "type" .= _spvSubjectIdType o
    , "chain" .= _spvSubjectIdChain o
    , "requestKey" .= toLegacyJsonViaEncode (_spvSubjectIdReqKey o)
    ]
{-# INLINE spvSubjectIdentifierProperties #-}

instance ToJSON SpvSubjectIdentifier where
    toJSON = object . spvSubjectIdentifierProperties
    toEncoding = pairs . mconcat . spvSubjectIdentifierProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON SpvSubjectIdentifier where
    parseJSON = withObject "SpvSubjectIdentifier" $ \o -> SpvSubjectIdentifier
        <$> o .: "type"
        <*> o .: "chain"
        <*> o .: "requestKey"

-- -------------------------------------------------------------------------- --
-- SPV2 Request

data Spv2Request = Spv2Request
    { _spv2ReqSubjectIdentifier :: !SpvSubjectIdentifier
    , _spv2ReqMinimalProofDepth :: !(Maybe Natural)
    , _spv2ReqAlgorithm :: !SpvAlgorithm
    }
    deriving (Show, Eq, Ord, Generic)

spv2RequestProperties :: KeyValue e kv => Spv2Request -> [kv]
spv2RequestProperties o =
    [ "subjectIdentifier" .= _spv2ReqSubjectIdentifier o
    , "minimalProofDepth" .= _spv2ReqMinimalProofDepth o
    , "algorithm" .= _spv2ReqAlgorithm o
    ]
{-# INLINE spv2RequestProperties #-}

instance ToJSON Spv2Request where
    toJSON = object . spv2RequestProperties
    toEncoding = pairs . mconcat . spv2RequestProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON Spv2Request where
    parseJSON = withObject "Spv2Request" $ \o -> Spv2Request
        <$> o .: "subjectIdentifier"
        <*> o .:? "minimalProofDepth" .!= Nothing
        <*> o .: "algorithm"

