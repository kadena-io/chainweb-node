{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

-- * Utils
, RequestKeyNotFoundException(..)
) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch

import Data.Aeson
import Data.MerkleLog

import GHC.Generics

import Pact.Types.Command

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Utils

newtype RequestKeyNotFoundException = RequestKeyNotFoundException RequestKey
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (NFData)

instance Exception RequestKeyNotFoundException

-- -------------------------------------------------------------------------- --
-- PayloadProof

data PayloadProof a = PayloadProof
    { _payloadProofChainId :: !ChainId
    , _payloadProofHeight :: !BlockHeight
    , _payloadProofHash :: !BlockHash
    , _payloadProofReqKey :: !RequestKey
    , _payloadProofRootType :: !MerkleRootType
    , _payloadProofBlob :: !(MerkleProof a)
    }
    deriving (Show, Eq, Generic)

instance MerkleHashAlgorithmName a => ToJSON (PayloadProof a) where
    toJSON p = object
        [ "chain" .= _payloadProofChainId p
        , "blockHeight" .= _payloadProofHeight p
        , "blockHash" .= _payloadProofHash p
        , "requestKey" .= _payloadProofReqKey p
        , "algorithm" .= merkleHashAlgorithmName @a
        , "rootType" .= _payloadProofRootType p
        , "object" .= (obj . _merkleProofObject) blob
        , "subject" .= (subj . _getMerkleProofSubject . _merkleProofSubject) blob
        ]
      where
        blob = _payloadProofBlob p
        obj = encodeB64UrlNoPaddingText . encodeMerkleProofObject

        subj (TreeNode h) = object
            [ "tree" .= encodeB64UrlNoPaddingText (encodeMerkleRoot h)
            ]
        subj (InputNode bytes) = object
            [ "input" .= encodeB64UrlNoPaddingText bytes
            ]

instance (MerkleHashAlgorithm a, MerkleHashAlgorithmName a) => FromJSON (PayloadProof a) where
    parseJSON = withObject "PayloadProof" $ \o -> PayloadProof
        <$> o .: "chain"
        <*> o .: "blockHeight"
        <*> o .: "blockHash"
        <*> o .: "requestKey"
        <* (assertJSON (merkleHashAlgorithmName @a) =<< o .: "algorithm")
        <*> o .: "rootType"
        <*> parse o
      where
        parse o = MerkleProof
            <$> (parseSubject =<< o .: "subject")
            <*> (parseObject =<< o .: "object")

        parseSubject = withObject "ProofSubject" $ \o -> MerkleProofSubject
            <$> ((o .: "tree" >>= parseTreeNode) <|> (o .: "input" >>= parseInputNode))

        parseTreeNode = withText "TreeNode"
            $ fmap TreeNode . parseBinary decodeMerkleRoot

        parseInputNode = withText "InputNode"
            $ fmap InputNode . parseBinary pure

        parseObject = withText "ProofObject"
            $ parseBinary decodeMerkleProofObject

        assertJSON e a = unless (e == a)
            $ fail $ "expected " <> sshow e <> ", got " <> sshow a

        parseBinary p t = either (fail . show) return $
            p =<< decodeB64UrlNoPaddingText t

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
runPayloadProof p = (_payloadProofRootType p, root,) <$> proofSubject blob
  where
    root = MerkleLogHash $ runMerkleProof blob
    blob = _payloadProofBlob p

