{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Applicative
import Control.DeepSeq
import Control.Lens (Getter, to)
import Control.Monad
import Control.Monad.Catch

import Crypto.Hash.Algorithms

import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.MerkleLog hiding (Expected, Actual)
import qualified Data.Text as T

import GHC.Generics (Generic)

import Numeric.Natural

import Prelude hiding (lookup)

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
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
proofToJson
    :: ChainId
        -- ^ The target chain of the proof, i.e. the chain of the root.
    -> MerkleProof SHA512t_256
        -- ^ The Merkle proof blob which coaintains both, the proof object and
        -- the proof subject.
    -> Value
proofToJson cid p = object
    [ "chain" .= cid
    , "object" .= obj (_merkleProofObject p)
    , "subject" .= (subj . _getMerkleProofSubject . _merkleProofSubject) p
    , "algorithm" .= ("SHA512t_256" :: T.Text)
    ]
  where
    obj = encodeB64UrlNoPaddingText . encodeMerkleProofObject

    subj (TreeNode h) = object
        [ "tree" .= encodeB64UrlNoPaddingText (encodeMerkleRoot h)
        ]
    subj (InputNode bytes) = object
        [ "input" .= encodeB64UrlNoPaddingText bytes
        ]

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
    toJSON (TransactionProof cid p) = proofToJson cid p

instance FromJSON (TransactionProof SHA512t_256) where
    parseJSON = parseProof "TransactionProof" TransactionProof

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
    toJSON (TransactionOutputProof cid p) = proofToJson cid p

instance FromJSON (TransactionOutputProof SHA512t_256) where
    parseJSON = parseProof "TransactionOutputProof" TransactionOutputProof

-- | Getter into the chain id of a 'TransactionOutputProof'
--
outputProofChainId :: Getter (TransactionOutputProof a) ChainId
outputProofChainId = to (\(TransactionOutputProof cid _) -> cid)
