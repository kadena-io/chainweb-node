{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
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
, ProofTarget(..)
, TransactionProof(..)
, proofChainId
, TransactionOutputProof(..)
, outputProofTarget
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
import qualified Data.ByteString as B
import Data.MerkleLog hiding (Expected, Actual)
import Data.Text (Text)
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
        , _spvExceptionTargetChainId :: !ProofTarget
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
    :: forall kv
    . KeyValue kv
    => ProofTarget
    -> MerkleProof SHA512t_256
    -> [kv]
proofProperties tgt p =
    [ "chain" .= tgt
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

jsonProofSubjectProperties :: KeyValue kv => JsonProofSubject -> [kv]
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
    -> (ProofTarget -> MerkleProof SHA512t_256 -> Aeson.Parser a)
    -> Value
    -> Aeson.Parser a
parseProof name mkProof = withObject name $ \o -> join $ mkProof
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
    toJSON (TransactionProof cid p) = object $ proofProperties (ProofTargetChain cid) p
    toEncoding (TransactionProof cid p) = pairs . mconcat $ proofProperties (ProofTargetChain cid) p
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON (TransactionProof SHA512t_256) where
    parseJSON = parseProof "TransactionProof" (\tgt p -> do { ProofTargetChain cid <- return tgt; return (TransactionProof cid p) })
    {-# INLINE parseJSON #-}

-- | Getter into the chain id of a 'TransactionProof'
--
proofChainId :: Getter (TransactionProof a) ChainId
proofChainId = to (\(TransactionProof cid _) -> cid)

-- -------------------------------------------------------------------------- --
-- Output Proofs
--
-- | Targets for proofs, usually a chain ID, but in the case of L2, a network
-- and chain identifier. TODO: determine format, if any.
data ProofTarget = ProofTargetChain !ChainId | ProofTargetCrossNetwork !Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

instance ToJSON ProofTarget where
    toJSON (ProofTargetChain cid) = toJSON (toText cid)
    toJSON (ProofTargetCrossNetwork subtgt) = toJSON ("crossnet:" <> subtgt)
    toEncoding (ProofTargetChain cid) = toEncoding (toText cid)
    toEncoding (ProofTargetCrossNetwork subtgt) = toEncoding ("crossnet:" <> subtgt)

instance FromJSON ProofTarget where
    parseJSON = withText "ProofTarget" $ \case
        (T.stripPrefix "crossnet:" -> Just tgt) -> return $ ProofTargetCrossNetwork tgt
        (chainIdFromText -> Just cid) -> return $ ProofTargetChain cid
        _ -> fail "expected numeric chain ID or crossnet:<some string>"


-- | Witness that a transaction output is included in the head of a chain in a
-- chainweb.
--
data TransactionOutputProof a = TransactionOutputProof
    !ProofTarget
        -- ^ the target chain of the proof, i.e the chain which contains
        -- the root of the proof.
    !(MerkleProof a)
        -- ^ the Merkle proof blob, which contains both the proof object and
        -- the subject.
    deriving (Show, Eq)

instance ToJSON (TransactionOutputProof SHA512t_256) where
    toJSON (TransactionOutputProof tgt p) = object $ proofProperties tgt p
    toEncoding (TransactionOutputProof tgt p) = pairs . mconcat $ proofProperties tgt p
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON (TransactionOutputProof SHA512t_256) where
    parseJSON = parseProof "TransactionOutputProof" (\tgt p -> return (TransactionOutputProof tgt p))
    {-# INLINE parseJSON #-}

-- | Getter into the chain id of a 'TransactionOutputProof'
--
outputProofTarget :: Getter (TransactionOutputProof a) ProofTarget
outputProofTarget = to (\(TransactionOutputProof tgt _) -> tgt)

outputProofChainId :: Getter (TransactionOutputProof a) (Maybe ChainId)
outputProofChainId = to $ \(TransactionOutputProof tgt _) -> case tgt of
  ProofTargetChain cid -> Just cid
  ProofTargetCrossNetwork _ -> Nothing
