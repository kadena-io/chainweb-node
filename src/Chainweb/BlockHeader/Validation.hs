{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.BlockHeader.Validation
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.BlockHeader.Validation
(
-- * Validation Failures
  ValidationFailure(..)
, ValidationFailureType(..)
, definiteValidationFailures
, isDefinite
, ephemeralValidationFailures
, isEphemeral

-- * Validation functions
, validateBlockHeader
, validateBlockHeaderM
, validateIntrinsicM
, validateInductiveM
, isValidBlockHeader
, validateIntrinsic
, validateInductive
, validateBlockParentExists
, validateBlocksM

-- * Intrinsic BlockHeader Properties
, prop_block_pow
, prop_block_hash
, prop_block_genesis_parent
, prop_block_genesis_target
, prop_block_target
, prop_block_featureFlags

-- * Inductive BlockHeader Properties
, prop_block_epoch
, prop_block_height
, prop_block_weight
, prop_block_chainwebVersion
, prop_block_chainId
, prop_block_creationTime
) where

import Control.Monad
import Control.Monad.Catch

import Data.Foldable
import Data.Function
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockTarget, genesisParentBlockHash)
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Time
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- BlockHeader Validation

-- | A data type that describes a failure of validating a block header.
--
data ValidationFailure = ValidationFailure
    { _validateFailureParent :: !(Maybe BlockHeader)
    , _validateFailureHeader :: !BlockHeader
    , _validationFailureFailures :: ![ValidationFailureType]
    }

instance Show ValidationFailure where
    show (ValidationFailure p e ts)
        = "Validation failure"
            <> ".\n Parent: " <> show p
            <> ".\n Header: " <> show e
            <> "\n" <> unlines (map description ts)
      where
        description t = case t of
            MissingParent -> "Parent isn't in the database"
            CreatedBeforeParent -> "Block claims to have been created before its parent"
            VersionMismatch -> "Block uses a version of chainweb different from its parent"
            ChainMismatch -> "Block uses a chaind-id different from its parent"
            IncorrectHash -> "The hash of the block header does not match the one given"
            IncorrectPow -> "The POW hash does not match the POW target of the block"
            IncorrectEpoch -> "The epoch start time of the block is incorrect"
            IncorrectHeight -> "The given height is not one more than the parent height"
            IncorrectWeight -> "The given weight is not the sum of the difficulty target and the parent's weight"
            IncorrectTarget -> "The given target for the block is incorrect for its history"
            IncorrectGenesisParent -> "The block is a genesis block, but doesn't have its parent set to its own hash"
            IncorrectGenesisTarget -> "The block is a genesis block, but doesn't have the correct difficulty target"
            BlockInTheFuture -> "The creation time of the block is in the future"
            IncorrectPayloadHash -> "The payload hash does not match the payload hash that results from payload validation"
            MissingPayload -> "The payload of the block is missing"
            InvalidFeatureFlags -> "The block has an invalid feature flag value"

-- | An enumeration of possible validation failures for a block header.
--
data ValidationFailureType
    = MissingParent
        -- ^ Parent isn't in the database
    | CreatedBeforeParent
        -- ^ Claims to be created at a time prior to its parent's creation
    | VersionMismatch
        -- ^ Claims to use a version of chainweb different from that of its
        -- parent
    | ChainMismatch
        -- ^ Claims to use a chain-id different from that of its parent
    | IncorrectHash
        -- ^ The hash of the header properties as computed by computeBlockHash
        -- does not match the hash given in the header
    | IncorrectPow
        -- ^ The POW hash of the header does not match the POW target of the
        -- block.
    | IncorrectHeight
        -- ^ The given height is not one more than the parent height
    | IncorrectWeight
        -- ^ The given weight is not the sum of the target difficulty and the
        -- parent's weight
    | IncorrectTarget
        -- ^ The given target of the block is not correct. The target of the
        -- first block of an epoch is the adjusted target of the previous epoch.
        -- For all other blocks the target equals the target of the parent
        -- block.
    | IncorrectEpoch
        -- ^ The epoch start time value of the block is incorrect. The epoch
        -- start time of the first block of an epoch equals the block creation
        -- time of that block. For all other blocks the epoch start time equls
        -- the epoch start time of the parent block.
    | IncorrectGenesisParent
        -- ^ The block is a genesis block, but doesn't have its parent set to
        -- its own hash.
    | IncorrectGenesisTarget
        -- ^ The block is a genesis block, but doesn't have the correct
        -- POW target.
    | BlockInTheFuture
        -- ^ The creation time of the block is in the future.
    | IncorrectPayloadHash
        -- ^ The validation of the payload hash failed.
    | MissingPayload
        -- ^ The payload for the block is missing.
    | InvalidFeatureFlags
        -- ^ The block has an invalid feature flag setting
  deriving (Show, Eq, Ord)

instance Exception ValidationFailure

-- | The list of validation failures that are definite and independent of any
-- external context. A block for which validation fails with one of these
-- failures must be dicarded.
--
-- No node on the chainweb-web network should propgate blocks with these
-- failures. If a block is received that causes a definite validation failures
-- is receveived from a chainweb-node, that chainweb-node should be
-- blacklisted/removed in the peer database.
--
definiteValidationFailures :: [ValidationFailureType]
definiteValidationFailures =
    [ CreatedBeforeParent
    , VersionMismatch
    , ChainMismatch
    , IncorrectHash
    , IncorrectPow
    , IncorrectHeight
    , IncorrectWeight
    , IncorrectTarget
    , IncorrectEpoch
    , IncorrectGenesisParent
    , IncorrectGenesisTarget
    , IncorrectPayloadHash
    ]

-- | Predicate that checks whether a validation failure is definite.
--
isDefinite :: [ValidationFailureType] -> Bool
isDefinite failures
    = not . null $ L.intersect failures definiteValidationFailures

-- | The list of ephemaral validation failures. These failure depend on a local
-- context and validation may succeed in the future.
--
-- In case of 'BlockInTheFuture' validation /will/ eventually succeed.
--
ephemeralValidationFailures :: [ValidationFailureType]
ephemeralValidationFailures =
    [ MissingParent
    , MissingPayload
    , BlockInTheFuture
    ]

-- | A predicate that checks whether a validation failure is ephemeral.
--
isEphemeral :: [ValidationFailureType] -> Bool
isEphemeral failures
    = not . null $ L.intersect failures ephemeralValidationFailures

-- -------------------------------------------------------------------------- --
-- Validate BlockHeader

-- | Validate properties of the block header, throwing an exception detailing
-- the failures if any.
--
-- This doesn't include checks for
--
-- * MissingPayload
-- * MissingParent
-- * IncorrectPayloadHash
--
validateBlockHeaderM
    :: MonadThrow m
    => Time Micros
        -- ^ The current clock time
    -> BlockHeader
        -- ^ parent block header. The genesis header is considered its own parent.
    -> BlockHeader
        -- ^ The block header to be checked
    -> m ()
validateBlockHeaderM t p e = unless (null $ failures)
    $ throwM (ValidationFailure (Just p) e failures)
  where
    failures = validateBlockHeader t p e

-- | Validate intrinsic properties of the block header, throwing an exception detailing
-- the failures if any.
--
validateIntrinsicM
    :: MonadThrow m
    => Time Micros
        -- ^ The current clock time
    -> BlockHeader
        -- ^ The block header to be checked
    -> m ()
validateIntrinsicM t e = unless (null $ failures)
    $ throwM (ValidationFailure Nothing e failures)
  where
    failures = validateIntrinsic t e

-- | Validate inductive properties of the block header, throwing an exception detailing
-- the failures if any.
--
validateInductiveM
    :: MonadThrow m
    => BlockHeader
        -- ^ parent block header. The genesis header is considered its own parent.
    -> BlockHeader
        -- ^ The block header to be checked
    -> m ()
validateInductiveM p e = unless (null $ failures)
    $ throwM (ValidationFailure Nothing e failures)
  where
    failures = validateInductive p e

-- | Check whether a BlockHeader satisfies all validaiton properties.
--
-- This doesn't include checks for
--
-- * MissingPayload
-- * MissingParent
-- * IncorrectPayloadHash
--
isValidBlockHeader
    :: Time Micros
        -- ^ The current clock time
    -> BlockHeader
        -- ^ parent block header. The genesis header is considered its own parent.
    -> BlockHeader
        -- ^ The block header to be checked
    -> Bool
isValidBlockHeader t p b = null $ validateBlockHeader t p b

-- | Validate properties of the block header, producing a list of the validation
-- failures.
--
-- This doesn't include checks for
--
-- * MissingPayload
-- * MissingParent
-- * IncorrectPayloadHash
--
validateBlockHeader
    :: Time Micros
        -- ^ The current clock time
    -> BlockHeader
        -- ^ parent block header. The genesis header is considered its own parent.
    -> BlockHeader
        -- ^ The block header to be checked
    -> [ValidationFailureType]
        -- ^ A list of ways in which the block header isn't valid
validateBlockHeader t p b = validateIntrinsic t b <> validateInductive p b

-- | Validates properties of a block which are checkable from the block header
-- without observing the remainder of the database.
--
validateIntrinsic
    :: Time Micros
        -- ^ The current clock time
    -> BlockHeader
        -- ^ block header to be validated
    -> [ValidationFailureType]
        -- ^ A list of ways in which the block header isn't valid
validateIntrinsic t b = concat
    [ [ IncorrectHash | not (prop_block_hash b) ]
    , [ IncorrectPow | not (prop_block_pow b) ]
    , [ IncorrectGenesisParent | not (prop_block_genesis_parent b)]
    , [ IncorrectGenesisTarget | not (prop_block_genesis_target b)]
    , [ BlockInTheFuture | not (prop_block_current t b)]
    , [ InvalidFeatureFlags | not (prop_block_featureFlags b)]
    ]

-- | Validate properties of a block with respect to a given parent.
--
validateInductive
    :: BlockHeader
        -- ^ parent block header. The genesis header is considered its own parent.
    -> BlockHeader
        -- ^ Block header under scrutiny
    -> [ValidationFailureType]
        -- ^ A list of ways in which the block header isn't valid
validateInductive p b = concat
    [ [ IncorrectHeight | not (prop_block_height p b) ]
    , [ IncorrectTarget | not (prop_block_target p b) ]
    , [ IncorrectEpoch | not (prop_block_epoch p b) ]
    , [ VersionMismatch | not (prop_block_chainwebVersion p b) ]
    , [ CreatedBeforeParent | not (prop_block_creationTime p b) ]
    , [ IncorrectWeight | not (prop_block_weight p b) ]
    , [ ChainMismatch | not (prop_block_chainId p b) ]
    -- TODO:
    -- target of block matches the calculate target for the branch
    ]

-- | Validate that the parent exist with the given lookup function.
--
-- Returns the parent if it exists. Throws an error otherwise.
--
validateBlockParentExists
    :: Monad m
    => (BlockHash -> m (Maybe BlockHeader))
    -> BlockHeader
    -> m (Either ValidationFailureType BlockHeader)
validateBlockParentExists lookupParent h
    | isGenesisBlockHeader h = return $ Right h
    | otherwise = lookupParent (_blockParent h) >>= \case
        (Just !p) -> return $ Right p
        Nothing -> return $ Left MissingParent

-- | Validate a set of blocks that may depend on each other.
--
-- * MissingPayload
-- * IncorrectPayloadHash
--
validateBlocksM
    :: MonadThrow m
    => Time Micros
        -- ^ The current clock time
    -> (BlockHash -> m (Maybe BlockHeader))
    -> HM.HashMap BlockHash BlockHeader
    -> m ()
validateBlocksM t lookupParent as
    = traverse_ go as
  where
    lookupParent' h = case HM.lookup h as of
        Just p -> return (Just p)
        Nothing -> lookupParent h

    go h = validateBlockParentExists lookupParent' h >>= \case
        Left e -> throwM $ ValidationFailure Nothing h [e]
        Right p -> validateBlockHeaderM t p h

-- -------------------------------------------------------------------------- --
-- Intrinsic BlockHeader properties

prop_block_pow :: BlockHeader -> Bool
prop_block_pow b = checkTarget (_blockTarget b) (_blockPow b)

prop_block_hash :: BlockHeader -> Bool
prop_block_hash b = _blockHash b == computeBlockHash b

prop_block_genesis_parent :: BlockHeader -> Bool
prop_block_genesis_parent b
    = isGenesisBlockHeader b ==> hasGenesisParentHash b
    && hasGenesisParentHash b ==> isGenesisBlockHeader b
  where
    hasGenesisParentHash b' =
        _blockParent b' == genesisParentBlockHash (_blockChainwebVersion b') (_chainId b')

prop_block_genesis_target :: BlockHeader -> Bool
prop_block_genesis_target b = isGenesisBlockHeader b
    ==> _blockTarget b == genesisBlockTarget

prop_block_current :: Time Micros -> BlockHeader -> Bool
prop_block_current t b = BlockCreationTime t >= _blockCreationTime b

prop_block_featureFlags :: BlockHeader -> Bool
prop_block_featureFlags b = _blockFlags b == mkFeatureFlags

-- -------------------------------------------------------------------------- --
-- Inductive BlockHeader Properties

prop_block_target
    :: BlockHeader
        -- ^ parent header of the checked block header
    -> BlockHeader
        -- ^ the block header that is checked
    -> Bool
prop_block_target p b = _blockTarget b == powTarget p (_blockCreationTime b)

prop_block_epoch
    :: BlockHeader
        -- ^ parent header of the checked block header
    -> BlockHeader
        -- ^ the block header that is checked
    -> Bool
prop_block_epoch p b = _blockEpochStart b == epochStart p (_blockCreationTime b)

prop_block_height
    :: BlockHeader
        -- ^ parent header of the checked block header
    -> BlockHeader
        -- ^ the block header that is checked
    -> Bool
prop_block_height p b
    | isGenesisBlockHeader b = _blockHeight b == _blockHeight p
    | otherwise = _blockHeight b == _blockHeight p + 1

prop_block_chainwebVersion
    :: BlockHeader
        -- ^ parent header of the checked block header
    -> BlockHeader
        -- ^ the block header that is checked
    -> Bool
prop_block_chainwebVersion = (==) `on` _blockChainwebVersion

prop_block_creationTime
    :: BlockHeader
        -- ^ parent header of the checked block header
    -> BlockHeader
        -- ^ the block header that is checked
    -> Bool
prop_block_creationTime p b
    | isGenesisBlockHeader b = _blockCreationTime b == _blockCreationTime p
    | otherwise = _blockCreationTime b > _blockCreationTime p

prop_block_weight
    :: BlockHeader
        -- ^ parent header of the checked block header
    -> BlockHeader
        -- ^ the block header that is checked
    -> Bool
prop_block_weight p b
    | isGenesisBlockHeader b = _blockWeight b == _blockWeight p
    | otherwise = _blockWeight b == int (targetToDifficulty (_blockTarget b)) + _blockWeight p

prop_block_chainId
    :: BlockHeader
        -- ^ parent header of the checked block header
    -> BlockHeader
        -- ^ the block header that is checked
    -> Bool
prop_block_chainId = (==) `on` _blockChainId
