{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.BlockHeader.Validation
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Block Header Validation
--
module Chainweb.BlockHeader.Validation
(
-- * Validated BockHeaders
  ValidatedHeader
, _validatedHeader
, ValidatedHeaders
, _validatedHeaders

-- * Input Data Types for Validation Functions
-- $params
, InvalidValidationParameters(..)
, ChainStep
, chainStep
, _chainStepParent
, _chainStepHeader
, WebStep
, webStep
, _webStepAdjs
, _webStepChain
, _webStepParent
, _webStepHeader

-- * Validation Failures
, ValidationFailure(..)
, ValidationFailureType(..)
, chainStepFailure
, webStepFailure
, definiteValidationFailures
, isDefinite
, ephemeralValidationFailures
, isEphemeral

-- * Top-level Validaton Functions
, validateBlockHeaderM
, validateBlockHeadersM

-- * Partial validation functions
--
-- | For failing fast during cut validation
, validateIntrinsicM
, validateInductiveChainM

-- * Validation of Input Parameters
, validateBlockParentExists
, validateAllParentsExist

-- * Pure Top-level Validation functions
, validateBlockHeader
, isValidBlockHeader

-- * Collections of Validation Properties
, validateIntrinsic
, validateInductive
, validateInductiveChainStep
, validateInductiveWebStep

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
, prop_block_adjacent_chainIds
, prop_block_adjacent_parents_version
) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except

import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe (isJust)
import qualified Data.Text as T

import GHC.Generics

import System.Environment
import System.IO.Unsafe

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockTarget, genesisParentBlockHash, genesisBlockHeader)
import Chainweb.ChainId
import Chainweb.ChainValue
import Chainweb.Difficulty
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Validated BockHeader

-- NOTE: the constructor of this type is intentionally NOT exported. Value of
-- this type must be only created via functions from this module.
--
newtype ValidatedHeader = ValidatedHeader BlockHeader
    deriving (Show, Eq, Ord, Generic)

instance HasChainId ValidatedHeader where
    _chainId = _chainId . _validatedHeader
    {-# INLINE _chainId #-}

instance HasChainwebVersion ValidatedHeader where
    _chainwebVersion = _chainwebVersion . _validatedHeader
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph ValidatedHeader where
    _chainGraph = _chainGraph . _validatedHeader
    {-# INLINE _chainGraph #-}

_validatedHeader :: ValidatedHeader -> BlockHeader
_validatedHeader (ValidatedHeader h) = h
{-# INLINE _validatedHeader #-}

-- | Values of this type witness that a set of BlockHeaders has been validated for
-- the properties defined in this module.
--
-- This includes all intrinsic and inductive block header properties. It does
-- not include pact payload validation and cut validation.
--
-- The validation holds with respect to an implicit block header db, that is
-- represented by a header lookup function in some functions in this module.
--
-- The validation of block headers is witnessed only for the group of headers and
-- may not hold for the members of the set alone. If the headers are inserted
-- in the block header db, they must be inserted all together.
--
-- NOTE: the constructor of this type is intentionally NOT exported. Value of
-- this type must be only created via functions from this module.
--
newtype ValidatedHeaders = ValidatedHeaders (HM.HashMap BlockHash BlockHeader)
    deriving (Show, Eq, Ord, Generic)

_validatedHeaders :: ValidatedHeaders -> HM.HashMap BlockHash BlockHeader
_validatedHeaders (ValidatedHeaders hs) = hs
{-# INLINE _validatedHeaders #-}

-- -------------------------------------------------------------------------- --
-- Input Data Types for Validation Functions

-- $params
-- Values of these types are witnesses for properties of parameters of the
-- validation functions at runtime.
--
-- Usage of values of these types is sound only when they are constructed via
-- the respective smart constructors.
--

data InvalidValidationParameters
    = InvalidChainStepParameters ParentHeader BlockHeader
    | InvalidWebStepParameters (HM.HashMap ChainId ParentHeader) ChainStep
    deriving (Show, Eq, Ord, Generic)

instance Exception InvalidValidationParameters where
    displayException (InvalidChainStepParameters p b) =
        "Invalid block header validation parameters for ChainStep: " <> sshow p <> ", " <> sshow b
    displayException (InvalidWebStepParameters p s) =
        "Invalid block header validation parameters for WebStep: " <> sshow p <> ", " <> sshow s

-- | Witnesses at runtime that
--
-- prop> \(ChainStep p h) -> _blockParent h == _blockHash p
--
-- NOTE: the constructor of this type is intentionally NOT exported.
--
data ChainStep = ChainStep ParentHeader BlockHeader
    deriving (Show, Eq, Ord, Generic)

_chainStepParent :: ChainStep -> ParentHeader
_chainStepParent (ChainStep p _) = p
{-# INLINE _chainStepParent #-}

_chainStepHeader :: ChainStep -> BlockHeader
_chainStepHeader (ChainStep _ h) = h
{-# INLINE _chainStepHeader #-}

chainStep
    :: MonadThrow m
    => ParentHeader
        -- ^ Parent block header. The genesis header is considered its own parent.
    -> BlockHeader
        -- ^ Block header under scrutiny
    -> m ChainStep
chainStep p b
    | _blockParent b == _blockHash (_parentHeader p)
        = return $ ChainStep p b
    | otherwise = throwM $ InvalidChainStepParameters p b

-- | Witnesses at runtime that
--
-- prop> \(WebStep as (ChainStep _ h)) -> and $ HM.zipWith ((==) . _blockHash) as (_blockAdjacentHashes h)
--
-- It doesn't witness that @as@ is of the same size as @_blockAdjacentHashes
-- h@ or that @_blockAdjacentHashes h@ covers all adjacent chains.
--
-- NOTE: the constructor of this type is intentionally NOT exported.
--
data WebStep = WebStep (HM.HashMap ChainId ParentHeader) ChainStep
    deriving (Show, Eq, Ord, Generic)

webStep
    :: MonadThrow m
    => HM.HashMap ChainId ParentHeader
    -> ChainStep
    -> m WebStep
webStep as hp@(ChainStep _ h) = WebStep
    <$> itraverse f hashes
    <*> pure hp
  where
    hashes :: HM.HashMap ChainId BlockHash
    hashes = view (blockAdjacentHashes . getBlockHashRecord) h
    f cid a = case HM.lookup cid as of
        Nothing -> throwM $ InvalidWebStepParameters as hp
        Just x
            | _blockHash (_parentHeader x) == a -> return x
            | otherwise -> throwM $ InvalidWebStepParameters as hp

_webStepAdjs :: WebStep -> HM.HashMap ChainId ParentHeader
_webStepAdjs (WebStep as _) = as
{-# INLINE _webStepAdjs #-}

_webStepChain :: WebStep -> ChainStep
_webStepChain (WebStep _ p) = p
{-# INLINE _webStepChain #-}

_webStepHeader :: WebStep -> BlockHeader
_webStepHeader (WebStep _ p) = _chainStepHeader p
{-# INLINE _webStepHeader #-}

_webStepParent :: WebStep -> ParentHeader
_webStepParent (WebStep _ p) = _chainStepParent p
{-# INLINE _webStepParent #-}

-- -------------------------------------------------------------------------- --
-- BlockHeader Validation Failures

-- | A data type that describes a failure of validating a block header.
--
data ValidationFailure = ValidationFailure
    { _validateFailureParent :: !(Maybe ParentHeader)
    , _validateFailureAdjecents :: !(Maybe (HM.HashMap ChainId ParentHeader))
    , _validateFailureHeader :: !BlockHeader
    , _validationFailureFailures :: ![ValidationFailureType]
    }

chainStepFailure :: ChainStep -> [ValidationFailureType] -> ValidationFailure
chainStepFailure hp = ValidationFailure
    (Just $ _chainStepParent hp)
    Nothing
    (_chainStepHeader hp)

webStepFailure :: WebStep -> [ValidationFailureType] -> ValidationFailure
webStepFailure hp = ValidationFailure
    (Just $ _webStepParent hp)
    (Just $ _webStepAdjs hp)
    (_webStepHeader hp)

instance Show ValidationFailure where
    show (ValidationFailure p as e ts)
        = T.unpack $ "Validation failure"
            <> ". Parent: " <> encodeToText (ObjectEncoded . _parentHeader <$> p)
            <> ". Adjacents: " <> encodeToText ((fmap (ObjectEncoded . _parentHeader)) <$> as)
            <> ". Header: " <> encodeToText (ObjectEncoded e)
            <> ". Description: " <> T.unlines (map description ts)
      where
        description t = case t of
            MissingParent -> "Parent isn't in the database"
            MissingAdjacentParent -> "AdjacentParent isn't in the database"
            CreatedBeforeParent -> "Block claims to have been created before its parent"
            VersionMismatch -> "Block uses a version of chainweb different from its parent"
            AdjacentChainMismatch -> "Block uses the wrong set of adjacent chain ids"
            ChainMismatch -> "Block uses a chaind-id different from its parent"
            AdjacentParentChainMismatch -> "An adjacent parent hash references a block on the wrong chain"
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
            InvalidBraiding -> "The block is not braided correctly into the chainweb"
            InvalidAdjacentVersion -> "An adjancent parent has a chainweb version that does not match the version of the validated header"

-- | An enumeration of possible validation failures for a block header.
--
data ValidationFailureType
    = MissingParent
        -- ^ Parent isn't in the database.
    | MissingAdjacentParent
        -- ^ Adjacent Parent isn't in the database.
    | CreatedBeforeParent
        -- ^ Claims to be created at a time prior to its parent's creation.
    | VersionMismatch
        -- ^ Claims to use a version of chainweb different from that of its
        -- parent.
    | ChainMismatch
        -- ^ Claims to use a chain-id different from that of its parent.
    | AdjacentChainMismatch
        -- ^ The block uses the wrong set of adjacent chain ids.
    | AdjacentParentChainMismatch
        -- ^ An adajacent parent hash references a block on the wrong chain.
    | IncorrectHash
        -- ^ The hash of the header properties as computed by computeBlockHash
        -- does not match the hash given in the header.
    | IncorrectPow
        -- ^ The POW hash of the header does not match the POW target of the
        -- block.
    | IncorrectHeight
        -- ^ The given height is not one more than the parent height.
    | IncorrectWeight
        -- ^ The given weight is not the sum of the target difficulty and the
        -- parent's weight.
    | IncorrectTarget
        -- ^ The given target of the block is not correct. The target of the
        -- first block of an epoch is the adjusted target of the previous epoch.
        -- For all other blocks the target equals the target of the parent
        -- block.
    | IncorrectEpoch
        -- ^ The epoch start time value of the block is incorrect. The epoch
        -- start time of the first block of an epoch equals the block creation
        -- time of the parent block. For all other blocks the epoch start time
        -- equls the epoch start time of the parent block.
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
        -- ^ The block has an invalid feature flag setting.
    | InvalidBraiding
        -- ^ The block is not braided correctly into the chainweb.
    | InvalidAdjacentVersion
        -- ^ An adjacent parent has chainweb version that does not match the
        -- version of the validated header.
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
    , MissingAdjacentParent
    , MissingPayload
    , BlockInTheFuture
    ]

-- | A predicate that checks whether a validation failure is ephemeral.
--
isEphemeral :: [ValidationFailureType] -> Bool
isEphemeral failures
    = not . null $ L.intersect failures ephemeralValidationFailures

-- -------------------------------------------------------------------------- --
-- Top Level Validation Functions

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
    -> (ChainValue BlockHash -> m (Maybe BlockHeader))
        -- ^ Context of Validated BlockHeaders
    -> BlockHeader
    -> m ValidatedHeader
validateBlockHeaderM t lookupHeader h =
    validateAllParentsExist lookupHeader h >>= \case
        Left e -> throwM $ ValidationFailure
            { _validateFailureParent = Nothing
            , _validateFailureAdjecents = Nothing
            , _validateFailureHeader = h
            , _validationFailureFailures = [e]
            }
        Right ws -> do
            let failures = validateBlockHeader t ws
            unless (null failures) $ throwM $ webStepFailure ws failures
            return $ ValidatedHeader h

-- | Validate a set of blocks that may depend on each other.
--
-- This doesn't include checks for
--
-- * MissingPayload
-- * IncorrectPayloadHash
--
validateBlockHeadersM
    :: MonadThrow m
    => Time Micros
        -- ^ The current clock time
    -> (ChainValue BlockHash -> m (Maybe BlockHeader))
        -- ^ Context of Validated BlockHeaders
    -> HM.HashMap BlockHash BlockHeader
    -> m ValidatedHeaders
validateBlockHeadersM t lookupHeader as = do
    traverse_ (validateBlockHeaderM t lookupHeader') as
    return $ ValidatedHeaders as
  where
    lookupHeader' h = case HM.lookup (_chainValueValue h) as of
        Just p -> return (Just p)
        Nothing -> lookupHeader h

-- -------------------------------------------------------------------------- --
-- Partial Top-Level Validation Functions
--
-- These functions don't return ValidatedHeaders, so the result doesn't qualify
-- the validated header to be inserted in the the chain database.

-- | Only validate intrinsic properties of the block header, throwing an
-- exception detailing the failures if any.
--
-- This doesn't return 'ValidatedHeaders' because no complete validation is
-- performed
--
validateIntrinsicM
    :: MonadThrow m
    => Time Micros
        -- ^ The current clock time
    -> BlockHeader
        -- ^ The block header to be checked
    -> m ()
validateIntrinsicM t e = unless (null failures)
    $ throwM (ValidationFailure Nothing Nothing e failures)
  where
    failures = validateIntrinsic t e

-- | Only validate inductive properties of the block header, throwing an
-- exception detailing the failures if any.
--
-- This doesn't return 'ValidatedHeaders' because no complete validation is
-- performed
--
validateInductiveChainM
    :: MonadThrow m
    => (BlockHash -> m (Maybe BlockHeader))
        -- ^ Context of Validated BlockHeaders
    -> BlockHeader
    -> m ()
validateInductiveChainM lookupHeader h =
    validateBlockParentExists lookupHeader h >>= \case
        Left e -> throwM $ ValidationFailure
            { _validateFailureParent = Nothing
            , _validateFailureAdjecents = Nothing
            , _validateFailureHeader = h
            , _validationFailureFailures = [e]
            }
        Right cs -> do
            let failures = validateInductiveChainStep cs
            unless (null failures) $ throwM $ chainStepFailure cs failures

-- -------------------------------------------------------------------------- --
-- Validation of Input Parameters

-- | Validate that the parent exist with the given lookup function.
--
-- Returns the parent if it exists or an validation failure otherwise.
--
validateBlockParentExists
    :: Monad m
    => (BlockHash -> m (Maybe BlockHeader))
    -> BlockHeader
    -> m (Either ValidationFailureType ChainStep)
validateBlockParentExists lookupParent h
    | isGenesisBlockHeader h = return $ Right $ ChainStep (ParentHeader h) h
    | otherwise = lookupParent (_blockParent h) >>= \case
        (Just !p) -> return $ Right $ ChainStep (ParentHeader p) h
        Nothing -> return $ Left MissingParent

-- | Validate that the parent and all adjacent parents exist with the given
-- lookup function.
--
-- Returns the parents if they exist or an validation failure otherwise.
--
validateAllParentsExist
    :: Monad m
    => (ChainValue BlockHash -> m (Maybe BlockHeader))
    -> BlockHeader
    -> m (Either ValidationFailureType WebStep)
validateAllParentsExist lookupParent h = runExceptT $ WebStep
    <$> itraverse f (view (blockAdjacentHashes . getBlockHashRecord) h)
    <*> ExceptT (validateBlockParentExists lookupOnChain h)
  where
    lookupOnChain = lookupParent . ChainValue (_chainId h)
    v = _chainwebVersion h
    f c ph
        | genesisParentBlockHash v c == ph = return
            $ ParentHeader $ genesisBlockHeader v c
        | otherwise = lift (lookupParent $ ChainValue c ph) >>= \case
            (Just !p) -> return $ ParentHeader p
            Nothing -> throwError MissingAdjacentParent

-- -------------------------------------------------------------------------- --
-- Pure Top Level Validation Functions

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
    -> WebStep
    -> Bool
isValidBlockHeader t p = null $ validateBlockHeader t p

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
    -> WebStep
    -> [ValidationFailureType]
        -- ^ A list of ways in which the block header isn't valid
validateBlockHeader t p
    = validateIntrinsic t (_webStepHeader p)
    <> validateInductive p

-- -------------------------------------------------------------------------- --
-- Collections of Validation Properties

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
    , [ AdjacentChainMismatch | not (prop_block_adjacent_chainIds b) ]
    ]

-- | Validate properties of a block with respect to a given parent.
--
validateInductive
    :: WebStep
    -> [ValidationFailureType]
        -- ^ A list of ways in which the block header isn't valid
validateInductive ps
    = validateInductiveChainStep (_webStepChain ps)
    <> validateInductiveWebStep ps

validateInductiveChainStep
    :: ChainStep
        -- ^ parent block header. The genesis header is considered its own parent.
    -> [ValidationFailureType]
        -- ^ A list of ways in which the block header isn't valid
validateInductiveChainStep s = concat
    [ [ IncorrectHeight | not (prop_block_height s) ]
    , [ VersionMismatch | not (prop_block_chainwebVersion s) ]
    , [ IncorrectWeight | not (prop_block_weight s) ]
    , [ ChainMismatch | not (prop_block_chainId s) ]
    ]

validateInductiveWebStep
    :: WebStep
        -- ^ parent block header. The genesis header is considered its own parent.
    -> [ValidationFailureType]
        -- ^ A list of ways in which the block header isn't valid
validateInductiveWebStep s = concat
    [ [ IncorrectEpoch | not (prop_block_epoch s) ]
    , [ IncorrectTarget | not (prop_block_target s) ]
    , [ CreatedBeforeParent | not (prop_block_creationTime s) ]
    , [ AdjacentParentChainMismatch | not (prop_block_adjacent_parents s) ]
    , [ InvalidBraiding | not (prop_block_braiding s) ]
    , [ InvalidAdjacentVersion | not (prop_block_adjacent_parents_version s) ]
    ]

-- -------------------------------------------------------------------------- --
-- Intrinsic BlockHeader properties
-- -------------------------------------------------------------------------- --

powDisabled :: Bool
powDisabled = case unsafeDupablePerformIO $ lookupEnv "DISABLE_POW_VALIDATION" of
  Nothing -> False
  Just{} -> True
{-# NOINLINE powDisabled #-}

prop_block_pow :: BlockHeader -> Bool
prop_block_pow b
    | isGenesisBlockHeader b = True
        -- Genesis block headers are not mined. So there's not need for POW
    | _blockChainwebVersion b == Development && powDisabled = True
    | otherwise = checkTarget (_blockTarget b) (_blockPow b)

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
    ==> _blockTarget b == genesisBlockTarget (_chainwebVersion b) (_chainId b)

prop_block_current :: Time Micros -> BlockHeader -> Bool
prop_block_current t b = BlockCreationTime t >= _blockCreationTime b

prop_block_featureFlags :: BlockHeader -> Bool
prop_block_featureFlags b
    | skipFeatureFlagValidationGuard v h = True
    | otherwise = _blockFlags b == mkFeatureFlags
  where
    v = _chainwebVersion b
    h = _blockHeight b

-- | Verify that the adjacent hashes of the block are for the correct set of
-- chain ids.
--
prop_block_adjacent_chainIds :: BlockHeader -> Bool
prop_block_adjacent_chainIds b
    = isJust $ checkAdjacentChainIds adjGraph b (Expected $ _blockAdjacentChainIds b)
  where
    adjGraph
        | isGenesisBlockHeader b = _chainGraph b
        | otherwise = chainGraphAt (_chainwebVersion b) (_blockHeight b - 1)

-- -------------------------------------------------------------------------- --
-- Inductive BlockHeader Properties
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Single chain inductive properties

prop_block_height :: ChainStep -> Bool
prop_block_height (ChainStep (ParentHeader p) b)
    | isGenesisBlockHeader b = _blockHeight b == _blockHeight p
    | otherwise = _blockHeight b == _blockHeight p + 1

prop_block_chainwebVersion :: ChainStep -> Bool
prop_block_chainwebVersion (ChainStep (ParentHeader p) b) =
    _blockChainwebVersion p == _blockChainwebVersion b

prop_block_weight :: ChainStep -> Bool
prop_block_weight (ChainStep (ParentHeader p) b)
    | isGenesisBlockHeader b = _blockWeight b == _blockWeight p
    | otherwise = _blockWeight b == expectedWeight
  where
    expectedWeight = int (targetToDifficulty (_blockTarget b)) + _blockWeight p

prop_block_chainId :: ChainStep -> Bool
prop_block_chainId (ChainStep (ParentHeader p) b)
    = _blockChainId p == _blockChainId b

-- -------------------------------------------------------------------------- --
-- Multi chain inductive properties

prop_block_target :: WebStep -> Bool
prop_block_target (WebStep as (ChainStep p b))
    = _blockTarget b == powTarget p as (_blockCreationTime b)

prop_block_epoch :: WebStep -> Bool
prop_block_epoch (WebStep as (ChainStep p b))
    | oldDaGuard (_chainwebVersion b) (_blockHeight b)
        = _blockEpochStart b <= EpochStartTime (_bct $ _blockCreationTime b)
        && _blockEpochStart (_parentHeader p) <= _blockEpochStart b
        && _blockEpochStart b == epochStart p as (_blockCreationTime b)
    | otherwise
        = _blockEpochStart b <= EpochStartTime (_bct $ _blockCreationTime b)
        && _blockEpochStart b == epochStart p as (_blockCreationTime b)

prop_block_creationTime :: WebStep -> Bool
prop_block_creationTime (WebStep as (ChainStep (ParentHeader p) b))
    | isGenesisBlockHeader b
        = _blockCreationTime b == _blockCreationTime p
    | oldDaGuard (_chainwebVersion b) (_blockHeight b)
        = _blockCreationTime b > _blockCreationTime p
    | otherwise
        = _blockCreationTime b > _blockCreationTime p
        && all (\x -> _blockCreationTime b > _blockCreationTime (_parentHeader x)) as

-- | The chainId index of the adjacent parents of the header and the blocks
-- in the webstep reference the same hashes and the chain Ids in of the
-- referenced blocks match the chainIds in the index.
--
-- Note that this property is already witnessed by the constructor of WebStep,
-- we include it here again as assertion (to double check during testing) and
-- for documentation purposes.
--
prop_block_adjacent_parents :: WebStep -> Bool
prop_block_adjacent_parents (WebStep as (ChainStep _ b))
    | isGenesisBlockHeader b
        = adjsHashes == imap (\cid _ -> genesisParentBlockHash v cid) as
            -- chainId indexes in web adjadent parent record references the
            -- genesis block parent hashes
    | otherwise
        = adjsHashes == (_blockHash . _parentHeader <$> as)
            -- chainId indexes in web adjadent parent record and web step are
            -- referencing the same hashes
        && iall (\cid h -> cid == _chainId h) as
            -- chainIds of adjancent parent header match the chainId under which
            -- it is indexed
  where
    adjsHashes = _getBlockHashRecord (_blockAdjacentHashes b)
    v = _chainwebVersion b

prop_block_adjacent_parents_version :: WebStep -> Bool
prop_block_adjacent_parents_version (WebStep as (ChainStep _ b))
    = all ((== v) . _blockChainwebVersion . _parentHeader) as
  where
    v = _chainwebVersion b

-- | TODO: we don't current check this here. It is enforced in the cut merge
-- algorithm , namely in 'monotonicCutExtension'. The property that is checked
-- in the cut validation is stronger than the braiding property that we could
-- check here (which is the property that is described in the chainweb paper).
-- So, I check here would add some redundancy, but it's not clear of how much
-- actual value that would be.
--
-- If we want to enforce it here, too, we would have to look back 2 steps in
-- history.
--
prop_block_braiding :: WebStep -> Bool
prop_block_braiding _ = True
