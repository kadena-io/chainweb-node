{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module: Chainweb.SPV.PayloadProof
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.SPV.PayloadProof
(
-- * Payload Proofs
  PayloadProof(..)
, runPayloadProof

-- ** Some Payload Proof
, SomePayloadProof(..)

-- * Utils
, RequestKeyNotFoundException(..)
) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch

import Data.Hash.Keccak

import Data.Aeson
import Data.MerkleLog (MerkleNodeType(..))
import Data.MerkleLog.V1 qualified as V1
import Data.Text qualified as T

import GHC.Generics

import Pact.Types.Command

-- internal modules

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Utils

newtype RequestKeyNotFoundException = RequestKeyNotFoundException RequestKey
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (NFData)

instance Exception RequestKeyNotFoundException

-- | Internal helper type of holding the ToJSON dictionary for the
-- proof subject encoding.
--
newtype JsonProofSubject a = JsonProofSubject (MerkleNodeType a)

jsonProofSubjectProperties
    :: MerkleHashAlgorithm a
    => KeyValue e kv
    => JsonProofSubject a
    -> [kv]
jsonProofSubjectProperties (JsonProofSubject (TreeNode h)) =
    [ "tree" .= encodeB64UrlNoPaddingText (V1.encodeMerkleRoot h)
    ]
jsonProofSubjectProperties (JsonProofSubject (InputNode bytes)) =
    [ "input" .= encodeB64UrlNoPaddingText bytes
    ]

instance MerkleHashAlgorithm a => ToJSON (JsonProofSubject a) where
    toJSON = object . jsonProofSubjectProperties
    toEncoding = pairs . mconcat . jsonProofSubjectProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

-- -------------------------------------------------------------------------- --
-- PayloadProof

-- | API encoding for SPV proofs.
--
-- The proof only contains information that is authenticated. Any addititional
-- information about the root or the subject of the proof must be obtained from
-- a trusted source.
--
-- This is a new format. The SPV proofs defined in "Chainweb.SPV" still use a
-- legacy format.
--
data PayloadProof a = PayloadProof
    { _payloadProofRootType :: !MerkleRootType
        -- ^ The type of the Merle root.
    , _payloadProofBlob :: !(V1.MerkleProof a)
        -- ^ The Merkle proof blob which coaintains both, the proof object and
        -- the proof subject.
    } deriving (Show, Eq, Generic, NFData)

payloadProofProperties
    :: forall a e kv
    . MerkleHashAlgorithmName a
    => MerkleHashAlgorithm a
    => KeyValue e kv
    => PayloadProof a
    -> [kv]
payloadProofProperties p =
    [ "rootType" .= _payloadProofRootType p
    , "object" .= (obj . V1._merkleProofObject) blob
    , "subject" .= JsonProofSubject (V1._getMerkleProofSubject $ V1._merkleProofSubject blob)
    , "algorithm" .= merkleHashAlgorithmName @a
    ]
  where
    blob = _payloadProofBlob p
    obj = encodeB64UrlNoPaddingText . V1.encodeMerkleProofObject
{-# INLINE payloadProofProperties #-}

instance (MerkleHashAlgorithm a, MerkleHashAlgorithmName a) => ToJSON (PayloadProof a) where
    toJSON = object . payloadProofProperties
    toEncoding = pairs . mconcat . payloadProofProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance (MerkleHashAlgorithm a, MerkleHashAlgorithmName a) => FromJSON (PayloadProof a) where
    parseJSON = withObject "PayloadProof" $ \o -> PayloadProof
        <$ (assertJSON (merkleHashAlgorithmName @a) =<< o .: "algorithm")
        <*> o .: "rootType"
        <*> parse o
      where
        parse o = V1.MerkleProof
            <$> (parseSubject =<< o .: "subject")
            <*> (parseObject =<< o .: "object")

        parseSubject = withObject "ProofSubject" $ \o -> V1.MerkleProofSubject
            <$> ((o .: "tree" >>= parseTreeNode) <|> (o .: "input" >>= parseInputNode))

        parseTreeNode = withText "TreeNode"
            $ fmap TreeNode . parseBinary V1.decodeMerkleRoot

        parseInputNode = withText "InputNode"
            $ fmap InputNode . parseBinary pure

        parseObject = withText "ProofObject"
            $ parseBinary V1.decodeMerkleProofObject

        assertJSON e a = unless (e == a)
            $ fail $ "expected " <> sshow e <> ", got " <> sshow a

        parseBinary p t = either (fail . show) return $
            p =<< decodeB64UrlNoPaddingText t

-- -------------------------------------------------------------------------- --
-- Some Payload Proof

data SomePayloadProof where
    SomePayloadProof :: (MerkleHashAlgorithm a, MerkleHashAlgorithmName a) => !(PayloadProof a) -> SomePayloadProof

deriving instance Show SomePayloadProof

instance ToJSON SomePayloadProof where
    toJSON (SomePayloadProof p) = toJSON p
    toEncoding (SomePayloadProof p) = toEncoding p
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON SomePayloadProof where
    parseJSON v = withObject "SomePayloadProof" (\o -> o .: "algorithm" >>= pick) v
      where
        pick a
            | a == merkleHashAlgorithmName @ChainwebMerkleHashAlgorithm =
                SomePayloadProof @ChainwebMerkleHashAlgorithm <$> parseJSON v
            | a == merkleHashAlgorithmName @Keccak256 =
                SomePayloadProof @Keccak256 <$> parseJSON v
            | otherwise = fail $ "unsupported Merkle hash algorithm: " <> T.unpack a
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Proof Validation

-- | Run the Payload Output Proof. Returns the root and the transaction output
-- that is the proof subject.
--
-- Running the proof reduces that task of authenticating the subject to the
-- (smaller) task of authenticating just the root. Also, all transactions of the
-- a block share the same root. So, given respective proofs, all outputs of the
-- block payload can be authenticated via authentication of a single root.
--
-- NOTE: It is up the caller to validate the authenticity of the returned root
-- hash. The proof only claims that the subject is contained in the root.
--
runPayloadProof
    :: forall a b m
    . MonadThrow m
    => MerkleHashAlgorithm a
    => IsMerkleLogEntry a ChainwebHashTag b
    => PayloadProof a
    -> m (MerkleRootType, MerkleLogHash a, b)
runPayloadProof p = do
    root <- MerkleLogHash <$> V1.runMerkleProof blob
    (_payloadProofRootType p, root,) <$> proofSubject blob
  where
    blob = _payloadProofBlob p

