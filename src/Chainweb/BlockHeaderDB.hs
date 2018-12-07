{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.BlockHeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.BlockHeaderDB
(
-- * Chain Database Handle
  Configuration(..)
, BlockHeaderDb
, initBlockHeaderDb
, closeBlockHeaderDb
, copy

-- * Validation
, isValidEntry
, validateEntry
, validateEntryM
, ValidationFailure(..)
, ValidationFailureType(..)
) where

import Control.Arrow
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Lens hiding (children)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans

import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Sequence as Seq

import GHC.Generics

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.TreeDB
import Chainweb.Utils.Paging
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Internal

type K = BlockHash
type E = BlockHeader

-- -------------------------------------------------------------------------- --
-- Internal DB Representation
--
-- Only functions in this section are allowed to modify values of the Db type.

data Db = Db
    { _dbEntries :: !(HM.HashMap K (E, Int))
        -- ^ The map with data base entries along with the index in the insertion
        -- order. This map is add-only, nothing is updated or deleted from it.

    , _dbBranches :: !(HM.HashMap K E)
        -- ^ The set of leaf entries. These are all entries that don't have
        -- children, which are precisely the entries in the children map that
        -- map to an empty set.

    , _dbChildren :: !(HM.HashMap K (HS.HashSet E))
        -- ^ The children relation, which allows to traverse the database
        -- in the direction from the root to the leaves.
        --
        -- Entries are only added to the inner sets of this map. Nothing
        -- is ever deleted.

    , _dbEnumeration :: !(Seq.Seq E)
        -- ^ The insertion order of the database entries.
        --
        -- This sequence is append-only.

        -- An alternative approach would have been to link the entries in
        -- '_dbEntries' via a (mutable) linked list. This approach would have
        -- required an monadic context for upating the map without providing
        -- substantial performance gains
        -- (cf. https://gist.github.com/larskuhtz/5ee9510ad5346f988614f202420ddcc4)
    }
    deriving (Generic)

instance NFData Db

makeLenses ''Db

-- | A a new entry to the database
--
dbAddChecked :: MonadThrow m => E -> Db -> m Db
dbAddChecked e db
    | isMember = return db
    | otherwise = dbAddCheckedInternal
  where
    isMember = HM.member (key e) (_dbEntries db)

    -- Internal helper methods

    -- | Unchecked addition
    --
    -- ASSUMES that
    --
    -- * Item is not yet in database
    --
    -- Guarantees that
    --
    -- * each item without children is included in branches and
    -- * each item is included in children
    -- * each item is included in the enumeration
    --
    dbAdd
        = over dbEnumeration (Seq.:|> e)
        . over dbChildren dbAddChildren
        . over dbBranches dbAddBranch
        . over dbEntries (HM.insert (key e) (e, length (_dbEnumeration db)))
        $ db

    dbAddCheckedInternal :: MonadThrow m => m Db
    dbAddCheckedInternal = case parent e of
        Nothing -> return dbAdd
        Just p -> case HM.lookup p (_dbEntries db) of
            Nothing -> throwM $ TreeDbParentMissing @BlockHeaderDb e
            Just (pe, _) -> do
                unless (rank e == rank pe + 1)
                    $ throwM $ TreeDbInvalidRank @BlockHeaderDb e
                return dbAdd

    dbAddBranch :: HM.HashMap K E -> HM.HashMap K E
    dbAddBranch bs = HM.insert (key e) e
        $ maybe bs (`HM.delete` bs) (parent e)

    dbAddChildren
        :: HM.HashMap K (HS.HashSet E)
        -> HM.HashMap K (HS.HashSet E)
    dbAddChildren cs = HM.insert (key e) mempty $ case parent e of
        Just p -> HM.insertWith (<>) p (HS.singleton e) cs
        _ -> cs

emptyDb :: Db
emptyDb = Db mempty mempty mempty mempty

-- -------------------------------------------------------------------------- --
-- Chain Database Handle

-- | Configuration of the chain DB.
--
data Configuration = Configuration
    { _configRoot :: !BlockHeader
    }

-- | A handle to the database. This is a mutable stateful object.
--
-- The database is guaranteed to never be empty.
--
-- The Constructors and record fields are private to this module in order to
-- guarantee consistency of the database.
--
data BlockHeaderDb = BlockHeaderDb
    { _chainDbId :: !ChainId
    , _chainDbVar :: !(MVar Db)
        -- ^ Database that provides random access the block headers indexed by
        -- their hash. The 'MVar' is used as a lock to sequentialize concurrent
        -- access.

    , _chainDbEnumeration :: !(TVar (Seq.Seq E))
        -- ^ The enumeration of the db wrapped in a 'TVar'. It allows clients to
        -- await additions to the database.
        --
        -- This TVar is updated under the database lock each time a value is
        -- added is added to the sequence. It contains a pointer to the
        -- enumeration in the database and it must not changed anywhere else.
    }

instance HasChainId BlockHeaderDb where
    _chainId = _chainDbId

-- | Initialize a database handle
--
initBlockHeaderDb :: Configuration -> IO BlockHeaderDb
initBlockHeaderDb config = do
    initialDb <- dbAddChecked rootEntry emptyDb
    BlockHeaderDb (_chainId rootEntry)
        <$> newMVar initialDb
        <*> newTVarIO (_dbEnumeration initialDb)
  where
    rootEntry = _configRoot config

-- | Close a database handle and release all resources
--
closeBlockHeaderDb :: BlockHeaderDb -> IO ()
closeBlockHeaderDb = void . takeMVar . _chainDbVar

snapshot :: BlockHeaderDb -> IO Db
snapshot = readMVar . _chainDbVar

enumerationIO :: BlockHeaderDb -> IO (Seq.Seq E)
enumerationIO = readTVarIO . _chainDbEnumeration

enumeration :: BlockHeaderDb -> STM (Seq.Seq E)
enumeration = readTVar . _chainDbEnumeration

copy :: BlockHeaderDb -> IO BlockHeaderDb
copy db = withMVar (_chainDbVar db) $ \var ->
    BlockHeaderDb (_chainDbId db)
        <$> newMVar var
        <*> (newTVarIO =<< readTVarIO (_chainDbEnumeration db))

-- -------------------------------------------------------------------------- --
-- TreeDB instance

instance TreeDb BlockHeaderDb where
    type DbEntry BlockHeaderDb = BlockHeader

    lookup db k = fmap fst . HM.lookup k . _dbEntries <$> snapshot db

    childrenEntries db k =
        HM.lookup k . _dbChildren <$> lift (snapshot db) >>= \case
            Nothing -> lift $ throwM $ TreeDbKeyNotFound @BlockHeaderDb k
            Just c -> S.each c

    leafEntries db n l mir mar
        = _dbBranches <$> lift (snapshot db) >>= \b -> do
            (s :: Maybe (NextItem (HS.HashSet K))) <- lift $ start b n
            S.each b
                & seekStreamSet key s
                & limitLeaves db mir mar
                & limitStream l
      where
        start _ Nothing = return Nothing
        start b (Just (Exclusive x)) = startSet b x Exclusive
        start b (Just (Inclusive x)) = startSet b x Inclusive

        startSet b x clusive = Just . clusive . HS.fromList . fmap key
            <$> S.toList_ (ascendIntersect db (keySet b) =<< liftIO (lookupM db x))

    allKeys db = S.map key . allEntries db

    allEntries db k = do
        sn <- lift $ snapshot db
        case k of
            Nothing -> fromIdx 0
            Just (Exclusive x) -> case HM.lookup x (_dbEntries sn) of
                Nothing -> lift $ throwM $ TreeDbKeyNotFound @BlockHeaderDb x
                Just (_, i) -> fromIdx (i + 1)
            Just (Inclusive x) -> case HM.lookup x (_dbEntries sn) of
                Nothing -> lift $ throwM $ TreeDbKeyNotFound @BlockHeaderDb x
                Just (_, i) -> fromIdx i
      where
        fromIdx i = do
            s <- lift $ atomically $
                Seq.drop i <$> enumeration db >>= \case
                    Seq.Empty -> retry
                    l -> return l
            S.each s
            fromIdx (i + length s)

    entries db k l mir mar = lift (enumerationIO db)
        >>= foldableEntries k l mir mar

    insert db e = liftIO $ insertBlockHeaderDb db [e]

-- -------------------------------------------------------------------------- --
-- Insertions

insertBlockHeaderDb :: BlockHeaderDb -> [E] -> IO ()
insertBlockHeaderDb db es = do

    -- Validate set of additions
    validateAdditionsM db $ HM.fromList $ (key &&& id) <$> es

    -- atomically add set of additions to database
    modifyMVar_ (_chainDbVar db) $ \x -> do
        x' <- foldM (flip dbAddChecked) x rankedAdditions
        atomically $ writeTVar (_chainDbEnumeration db) $ _dbEnumeration x'
        return $!! x'
  where
    rankedAdditions = L.sortOn rank es

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

-- | Validate a set of additions that are supposed to added atomically to
-- the database.
--
validateAdditionsM :: BlockHeaderDb -> HM.HashMap BlockHash BlockHeader -> IO ()
validateAdditionsM db as = traverse_ (validateEntryInternalM lookupParent) as
  where
    lookupParent h = case HM.lookup h as of
        Nothing -> lookup db h
        p -> return p

-- | Validate properties of the block header, throwing an exception detailing
-- the failures if any.
--
validateEntryInternalM
    :: MonadThrow m
    => (BlockHash -> m (Maybe BlockHeader))
    -> BlockHeader
    -> m ()
validateEntryInternalM lookupParent e =
    validateEntryInternal lookupParent e >>= \case
        [] -> return ()
        failures -> throwM (ValidationFailure e failures)

-- | Validate properties of the block header, throwing an exception detailing
-- the failures if any.
--
validateEntryM
    :: BlockHeaderDb
    -> BlockHeader
    -> IO ()
validateEntryM = validateEntryInternalM . lookup

-- | Validate properties of the block header, producing a list of the validation
-- failures
--
validateEntry
    :: BlockHeaderDb
        -- ^ the database
    -> BlockHeader
        -- ^ The block header to be checked
    -> IO [ValidationFailureType]
        -- ^ A list of ways in which the block header isn't valid
validateEntry = validateEntryInternal . lookup

-- | Validate properties of the block header, producing a list of the validation
-- failures
--
validateEntryInternal
    :: Monad m
    => (BlockHash -> m (Maybe BlockHeader))
        -- ^ parent lookup
    -> BlockHeader
        -- ^ The block header to be checked
    -> m [ValidationFailureType]
        -- ^ A list of ways in which the block header isn't valid
validateEntryInternal lookupParent b = (validateBlockHeaderIntrinsic b ++)
    <$> validateBlockHeaderInductiveInternal lookupParent b

-- | Validates properties of a block with respect to its parent.
--
validateBlockHeaderInductiveInternal
    :: Monad m
    => (BlockHash -> m (Maybe BlockHeader))
    -> BlockHeader
    -> m [ValidationFailureType]
validateBlockHeaderInductiveInternal lookupParent b
    | isGenesisBlockHeader b = return (validateParent b b)
    | otherwise = lookupParent (_blockParent b) >>= \case
        Nothing -> return [MissingParent]
        Just p -> return $ validateParent p b

-- | Validates properties of a block which are checkable from the block header
-- without observing the remainder of the database.
--
validateBlockHeaderIntrinsic
    :: BlockHeader -- ^ block header to be validated
    -> [ValidationFailureType]
validateBlockHeaderIntrinsic b = concat
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
    -- TODO:
    -- target of block matches the calculate target for the branch
    ]

-- | Tests if the block header is valid (i.e. 'validateBlockHeader' produces an
-- empty list)
--
isValidEntry :: BlockHeaderDb -> BlockHeader -> IO Bool
isValidEntry s b = null <$> validateEntry s b

-- -------------------------------------------------------------------------- --
-- Inductive BlockHeader Properties

prop_block_height :: BlockHeader -> BlockHeader -> Bool
prop_block_height p b
    | isGenesisBlockHeader b = _blockHeight b == _blockHeight p
    | otherwise = _blockHeight b == _blockHeight p + 1

prop_block_chainwebVersion :: BlockHeader -> BlockHeader -> Bool
prop_block_chainwebVersion p b = _blockChainwebVersion b == _blockChainwebVersion p

prop_block_creationTime :: BlockHeader -> BlockHeader -> Bool
prop_block_creationTime p b
    | isGenesisBlockHeader b = _blockCreationTime b == _blockCreationTime p
    | otherwise = _blockCreationTime b > _blockCreationTime p

prop_block_weight :: BlockHeader -> BlockHeader -> Bool
prop_block_weight p b
    | isGenesisBlockHeader b = _blockWeight b == _blockWeight p
    | otherwise = _blockWeight b == int (targetToDifficulty (_blockTarget b)) + _blockWeight p

