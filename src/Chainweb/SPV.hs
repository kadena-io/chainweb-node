{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
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
, TransactionProof(..)
, proofChainId
, TransactionOutputProof(..)
, outputProofChainId
) where


import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Utils
import Control.Applicative
import Control.DeepSeq
import Control.Lens (Getter, to)
import Control.Monad
import Control.Monad.Catch
import Crypto.Hash.Algorithms
import Data.Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as B
import Data.MerkleLog hiding (Expected, Actual)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Numeric.Natural
import Prelude hiding (lookup)

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
    -> MerkleProof SHA512t_256
    -> [kv]
proofProperties cid p =
    [ "chain" .= cid
    , "object" .= obj (_merkleProofObject p)
    , "subject" .= JsonProofSubject (_getMerkleProofSubject $ _merkleProofSubject p)
    , "algorithm" .= ("SHA512t_256" :: T.Text)
    ]
  where
    obj = encodeB64UrlNoPaddingText . encodeMerkleProofObject

-- | Internal helper type of holding the ToJSON dictionary for the
-- legacy proof subject encoding.
--
newtype JsonProofSubject = JsonProofSubject (MerkleNodeType SHA512t_256 B.ByteString)

jsonProofSubjectProperties :: KeyValue e kv => JsonProofSubject -> [kv]
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
    -> (ChainId -> MerkleProof SHA512t_256 -> a)
    -> Value
    -> Aeson.Parser a
parseProof name mkProof = withObject name $ \o -> mkProof
    <$> o .: "chain"
    <*> parse o
    <* (assertJSON ("SHA512t_256" :: T.Text) =<< o .: "algorithm")
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
-- Transaction Proofs

-- | Witness that a transaction is included in the head of a chain in a
-- chainweb.
--
data TransactionProof a = TransactionProof
    !ChainId
        -- ^ the target chain of the proof, i.e the chain which contains
        -- the root of the proof.
    !(MerkleProof a)
        -- ^ the Merkle proof blob, which contains both the proof object and
        -- the subject.
    deriving (Show, Eq)

instance ToJSON (TransactionProof SHA512t_256) where
    toJSON (TransactionProof cid p) = object $ proofProperties cid p
    toEncoding (TransactionProof cid p) = pairs . mconcat $ proofProperties cid p
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON (TransactionProof SHA512t_256) where
    parseJSON = parseProof "TransactionProof" TransactionProof
    {-# INLINE parseJSON #-}

-- | Getter into the chain id of a 'TransactionProof'
--
proofChainId :: Getter (TransactionProof a) ChainId
proofChainId = to (\(TransactionProof cid _) -> cid)

-- -------------------------------------------------------------------------- --
-- Output Proofs

-- | Witness that a transaction output is included in the head of a chain in a
-- chainweb.
--
data TransactionOutputProof a = TransactionOutputProof
    !ChainId
        -- ^ the target chain of the proof, i.e the chain which contains
        -- the root of the proof.
    !(MerkleProof a)
        -- ^ the Merkle proof blob, which contains both the proof object and
        -- the subject.
    deriving (Show, Eq)

instance ToJSON (TransactionOutputProof SHA512t_256) where
    toJSON (TransactionOutputProof cid p) = object $ proofProperties cid p
    toEncoding (TransactionOutputProof cid p) = pairs . mconcat $ proofProperties cid p
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON (TransactionOutputProof SHA512t_256) where
    parseJSON = parseProof "TransactionOutputProof" TransactionOutputProof
    {-# INLINE parseJSON #-}

-- | Getter into the chain id of a 'TransactionOutputProof'
--
outputProofChainId :: Getter (TransactionOutputProof a) ChainId
outputProofChainId = to (\(TransactionOutputProof cid _) -> cid)

