{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.SPV
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- SPV proofs for the Chainweb Merkle Tree.
--
module Chainweb.SPV
( SpvException(..)
, TransactionOutputProof(..)
, outputProofChainId
, FakeEventProof(..)
) where

import Control.Applicative
import Control.DeepSeq
import Control.Lens (Getter, to)
import Control.Monad
import Control.Monad.Catch

import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.MerkleLog.Common hiding (Expected, Actual)
import qualified Data.MerkleLog.V1 as V1
import qualified Data.Text as T

import GHC.Generics (Generic)

import Numeric.Natural

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.MerkleUniverse
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Exceptions

data SpvException
    = SpvExceptionTargetNotReachable
        { _spvExceptionMsg :: !T.Text
        , _spvExceptionSourceChainId :: !ChainId
        , _spvExceptionSourceHeight :: !BlockHeight
        , _spvExceptionTargetChainId :: !ChainId
        , _spvExceptionTargetHeight :: !BlockHeight
        }
    | SpvExceptionInconsistentPayloadData
        { _spvExceptionMsg :: !T.Text
        , _spvExceptionMsgPayloadHash :: !BlockPayloadHash
        }
    | SpvExceptionVerificationFailed
        { _spvExceptionMsg :: !T.Text
        }
    | SpvExceptionInsufficientProofDepth
        { _spvExceptionMsg :: !T.Text
        , _spvExceptionExpectedDepth :: !(Expected Natural)
        , _spvExceptionActualDepth :: !(Actual Natural)
        }
    deriving (Show, Eq, Ord, Generic, NFData)

instance Exception SpvException

-- -------------------------------------------------------------------------- --
-- JSON encoding utils

-- | This is a legacy encoding that contains only a subset of the properties
-- that are defined in the new proof format in "Chainweb.SPV.PayloadProof".
-- Newly added SPV API endpoints should use the new format. We keep using the
-- legacy format for existing endpoints in order to not break existing clients.
--
proofProperties
    :: forall e kv
    . KeyValue e kv
    => ChainId
    -> V1.MerkleProof ChainwebMerkleHashAlgorithm
    -> [kv]
proofProperties cid p =
    [ "chain" .= cid
    , "object" .= obj (V1._merkleProofObject p)
    , "subject" .= JsonProofSubject (V1._getMerkleProofSubject $ V1._merkleProofSubject p)
    , "algorithm" .= ("SHA512t_256" :: T.Text)
    ]
  where
    obj = encodeB64UrlNoPaddingText . V1.encodeMerkleProofObject

-- | Internal helper type of holding the ToJSON dictionary for the
-- legacy proof subject encoding.
--
newtype JsonProofSubject = JsonProofSubject (MerkleNodeType ChainwebMerkleHashAlgorithm)

jsonProofSubjectProperties
    :: KeyValue e kv
    => JsonProofSubject
    -> [kv]
jsonProofSubjectProperties (JsonProofSubject (TreeNode h)) =
    [ "tree" .= encodeB64UrlNoPaddingText (encodeMerkleRoot h)
    ]
jsonProofSubjectProperties (JsonProofSubject (InputNode bytes)) =
    [ "input" .= encodeB64UrlNoPaddingText bytes
    ]

instance ToJSON JsonProofSubject where
    toJSON = object . jsonProofSubjectProperties
    toEncoding = pairs . mconcat . jsonProofSubjectProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

parseProof
    :: String
    -> (ChainId -> V1.MerkleProof ChainwebMerkleHashAlgorithm -> a)
    -> Value
    -> Aeson.Parser a
parseProof name mkProof = withObject name $ \o -> mkProof
    <$> o .: "chain"
    <*> parse o
    <* (assertJSON ("SHA512t_256" :: T.Text) =<< o .: "algorithm")
  where
    parse o = V1.MerkleProof
        <$> (parseSubject =<< o .: "subject")
        <*> (parseObject =<< o .: "object")

    parseSubject = withObject "ProofSubject" $ \o -> V1.MerkleProofSubject
        <$> ((o .: "tree" >>= parseTreeNode) <|> (o .: "input" >>= parseInputNode))

    parseTreeNode = withText "TreeNode"
        $ fmap TreeNode . parseBinary decodeMerkleRoot

    parseInputNode = withText "InputNode"
        $ fmap InputNode . parseBinary pure

    parseObject = withText "ProofObject"
        $ parseBinary V1.decodeMerkleProofObject

    assertJSON e a = unless (e == a)
        $ fail $ "expected " <> sshow e <> ", got " <> sshow a

    parseBinary p t = either (fail . show) return $
        p =<< decodeB64UrlNoPaddingText t

-- -------------------------------------------------------------------------- --
-- Output Proofs

-- | Witness that a transaction output is included in the head of a chain in a
-- chainweb.
--
data TransactionOutputProof a = TransactionOutputProof
    !ChainId
        -- ^ the target chain of the proof, i.e the chain which contains
        -- the root of the proof.
    !(V1.MerkleProof a)
        -- ^ the Merkle proof blob, which contains both the proof object and
        -- the subject.
    deriving (Show, Eq)

instance ToJSON (TransactionOutputProof ChainwebMerkleHashAlgorithm) where
    toJSON (TransactionOutputProof cid p) = object $ proofProperties cid p
    toEncoding (TransactionOutputProof cid p) = pairs . mconcat $ proofProperties cid p
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON (TransactionOutputProof ChainwebMerkleHashAlgorithm) where
    parseJSON = parseProof "TransactionOutputProof" TransactionOutputProof
    {-# INLINE parseJSON #-}

-- | Getter into the chain id of a 'TransactionOutputProof'
--
outputProofChainId :: Getter (TransactionOutputProof a) ChainId
outputProofChainId = to (\(TransactionOutputProof cid _) -> cid)

-- -------------------------------------------------------------------------- --
-- Fake Event Proof

-- | Fake event proof
--
data FakeEventProof = FakeEventProof
    !ChainId
        -- ^ the target chain of the proof, i.e the chain which contains
        -- the root of the proof.
    !Value
        -- ^ the Merkle proof blob, which contains both the proof object and
        -- the subject.
    deriving (Show, Eq)

instance ToJSON FakeEventProof where
    toJSON = object . fakeEventProofProperties
    toEncoding = pairs . mconcat . fakeEventProofProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON FakeEventProof where
    parseJSON = withObject "FakeEventProof" $ \o -> FakeEventProof
        <$> o .: "chain"
        <*> o .: "subject"
    {-# INLINE parseJSON #-}

fakeEventProofProperties
    :: forall e kv
    . KeyValue e kv
    => FakeEventProof
    -> [kv]
fakeEventProofProperties (FakeEventProof cid v) =
    [ "chain" .= cid
    , "object" .= obj "SNAKEOIL"
    , "subject" .= v
    , "algorithm" .= ("SHA512t_256" :: T.Text)
    ]
  where
    obj = encodeB64UrlNoPaddingText

