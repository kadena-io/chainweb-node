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

-- * Validation with a DB lookup function
, validateEntryM
, validateAdditionsM

-- * Pure validation functions
, isValidEntry
, validateEntry

-- * Pure Validation with Given Parent Header
, validateIntrinsic
, validateParent

-- * Intrinsic BlockHeader Properties
, prop_block_difficulty
, prop_block_hash
, prop_block_genesis_parent
, prop_block_genesis_target

-- * Inductive BlockHeader Properties
, prop_block_height
, prop_block_weight
, prop_block_chainwebVersion
, prop_block_chainId
, prop_block_creationTime
) where

import Control.Monad.Catch

import Data.Foldable
import Data.Function
import qualified Data.HashMap.Strict as HM

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockTarget, genesisParentBlockHash)
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- BlockHeader Validation

data ValidationFailure = ValidationFailure BlockHeader [ValidationFailureType]

instance Show ValidationFailure where
    show (ValidationFailure e ts) = "Validation failure: " ++ unlines (map description ts) ++ "\n" ++ show e
      where
        description t = case t of
            MissingParent -> "Parent isn't in the database"
            CreatedBeforeParent -> "Block claims to have been created before its parent"
            VersionMismatch -> "Block uses a version of chainweb different from its parent"
            ChainMismatch -> "Block uses a chaind-id different from its parent"
            IncorrectHash -> "The hash of the block header does not match the one given"
            IncorrectHeight -> "The given height is not one more than the parent height"
            IncorrectWeight -> "The given weight is not the sum of the difficulty target and the parent's weight"
            IncorrectTarget -> "The given target difficulty for the following block is incorrect"
            IncorrectGenesisParent -> "The block is a genesis block, but doesn't have its parent set to its own hash"
            IncorrectGenesisTarget -> "The block is a genesis block, but doesn't have the correct difficulty target"

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
    | IncorrectHeight
        -- ^ The given height is not one more than the parent height
    | IncorrectWeight
        -- ^ The given weight is not the sum of the target difficulty and the
        -- parent's weight
    | IncorrectTarget
        -- ^ The given target difficulty for the following block is not correct
        -- (TODO: this isn't yet checked, but
        -- Chainweb.ChainDB.Difficulty.calculateTarget is relevant.)
    | IncorrectGenesisParent
        -- ^ The block is a genesis block, but doesn't have its parent set to
        -- its own hash.
    | IncorrectGenesisTarget
        -- ^ The block is a genesis block, but doesn't have the correct
        -- difficulty target.
  deriving (Show, Eq, Ord)

instance Exception ValidationFailure

-- -------------------------------------------------------------------------- --
-- Validate with Lookup Function in MonadThrow

-- | Validate properties of the block header, throwing an exception detailing
-- the failures if any.
--
validateEntryM
    :: MonadThrow m
    => (BlockHash -> m (Maybe BlockHeader))
    -> BlockHeader
    -> m ()
validateEntryM lookupParent e =
    validateEntry lookupParent e >>= \case
        [] -> return ()
        failures -> throwM (ValidationFailure e failures)

-- | Validate a set of additions that are supposed to be added atomically to
-- the database.
--
validateAdditionsM
    :: MonadThrow m
    => (BlockHash -> m (Maybe BlockHeader))
    -> HM.HashMap BlockHash BlockHeader
    -> m ()
validateAdditionsM lookupParent as = traverse_ (validateEntryM lookupParent') as
  where
    lookupParent' h = case HM.lookup h as of
        Nothing -> lookupParent h
        p -> return p

-- -------------------------------------------------------------------------- --
-- Validate with Lookup Function

isValidEntry
    :: MonadThrow m
    => (BlockHash -> m (Maybe BlockHeader))
    -> BlockHeader
    -> m Bool
isValidEntry lockupParent b = null <$> validateEntry lockupParent b

-- | Validate properties of the block header, producing a list of the validation
-- failures
--
validateEntry
    :: Monad m
    => (BlockHash -> m (Maybe BlockHeader))
        -- ^ parent lookup
    -> BlockHeader
        -- ^ The block header to be checked
    -> m [ValidationFailureType]
        -- ^ A list of ways in which the block header isn't valid
validateEntry lookupParent b = (validateIntrinsic b ++)
    <$> validateInductiveInternal lookupParent b

-- | Validates properties of a block with respect to its parent.
--
validateInductiveInternal
    :: Monad m
    => (BlockHash -> m (Maybe BlockHeader))
    -> BlockHeader
    -> m [ValidationFailureType]
validateInductiveInternal lookupParent b
    | isGenesisBlockHeader b = return (validateParent b b)
    | otherwise = lookupParent (_blockParent b) >>= \case
        Nothing -> return [MissingParent]
        Just p -> return $ validateParent p b

-- -------------------------------------------------------------------------- --
-- Validate BlockHeaders

-- | Validates properties of a block which are checkable from the block header
-- without observing the remainder of the database.
--
validateIntrinsic
    :: BlockHeader -- ^ block header to be validated
    -> [ValidationFailureType]
validateIntrinsic b = concat
    [ [ IncorrectTarget | not (prop_block_difficulty b) ]
    , [ IncorrectHash | not (prop_block_hash b) ]
    , [ IncorrectGenesisParent | not (prop_block_genesis_parent b)]
    , [ IncorrectGenesisTarget | not (prop_block_genesis_target b)]
    ]

-- | Validate properties of a block with respect to a given parent.
--
validateParent
    :: BlockHeader -- ^ Parent block header
    -> BlockHeader -- ^ Block header under scrutiny
    -> [ValidationFailureType]
validateParent p b = concat
    [ [ IncorrectHeight | not (prop_block_height p b) ]
    , [ VersionMismatch | not (prop_block_chainwebVersion p b) ]
    , [ CreatedBeforeParent | not (prop_block_creationTime p b) ]
    , [ IncorrectWeight | not (prop_block_weight p b) ]
    , [ ChainMismatch | not (prop_block_chainId p b) ]
    -- TODO:
    -- target of block matches the calculate target for the branch
    ]

-- -------------------------------------------------------------------------- --
-- Intrinsic BlockHeader properties

prop_block_difficulty :: BlockHeader -> Bool
prop_block_difficulty b = checkTarget (_blockTarget b) (_blockPow b)

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
    ==> _blockTarget b == genesisBlockTarget (_blockChainwebVersion b)

-- -------------------------------------------------------------------------- --
-- Inductive BlockHeader Properties

prop_block_height :: BlockHeader -> BlockHeader -> Bool
prop_block_height p b
    | isGenesisBlockHeader b = _blockHeight b == _blockHeight p
    | otherwise = _blockHeight b == _blockHeight p + 1

prop_block_chainwebVersion :: BlockHeader -> BlockHeader -> Bool
prop_block_chainwebVersion = (==) `on` _blockChainwebVersion

prop_block_creationTime :: BlockHeader -> BlockHeader -> Bool
prop_block_creationTime p b
    | isGenesisBlockHeader b = _blockCreationTime b == _blockCreationTime p
    | otherwise = _blockCreationTime b > _blockCreationTime p

prop_block_weight :: BlockHeader -> BlockHeader -> Bool
prop_block_weight p b
    | isGenesisBlockHeader b = _blockWeight b == _blockWeight p
    | otherwise = _blockWeight b == int (targetToDifficulty (_blockTarget b)) + _blockWeight p

prop_block_chainId :: BlockHeader -> BlockHeader -> Bool
prop_block_chainId = (==) `on` _blockChainId
