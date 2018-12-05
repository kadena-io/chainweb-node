{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module    : Chainweb.ChainDB
-- Copyright : Copyright © 2018 Kadena LLC.
-- License   : MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability : experimental
--
-- This module provides a database interface for entries in a block chain. It
-- presents a view of a block chain as a:
--
-- * content indexed set of entries
-- * with a parent relation that forms a rooted tree and
-- * with a rank function that provides for each entry it's depth in the
--   tree.
--
-- Additionally, it provides:
--
-- * the set of branches, represented by the leafs of the tree,
-- * a children relation that allow to traverse the tree in the direction from
--   the root to the leafs, and
-- * an awaitable enumeration of the all current and future entries in the tree.
--
-- The tree is monotonically increasing and subtrees are immutable. An entry is
-- never removed from the tree.

-- ## TODO
--
-- Validation of Block Headers is fast enough to perform it synchronously. But
-- payload validation is slower and probably done asynchronously. This means
-- that either:
--
--   * the tree isn't monotonically increasing (we don't want that),
--   * inserting external entries is a blocking operation,
--   * inserting an external entry puts the entry in a db internal staging slot
--     where it is hidden from (normal, sync-related) queries until it's
--     validated.
--   * sync doesn't care, so blocks with invalid payload are distributed and we
--     hope that this doesn't create to much overhead.
--
-- The last option opens an DOS attack vector, but that might be dealt with on a
-- different layer (e.g. reputation management).
--
-- For now we ignore the problem and defer it's solution to a future version
-- when we actually start to deal with block payloads.
--

module Chainweb.ChainDB
(
-- * Chain Database Handle
  Configuration(..)
, ChainDb
, initChainDb
, closeChainDb
, copy

-- * Validation Status
, ValidationStatus(..)

-- * Entry Type
, Key
, Entry
, key
, parent
, rank
, uncheckedKey
, uncheckedEntry
, decideKeyStatus
, decideEntryStatus
, KeySet

-- * Updates
, Updates
, updates
, updatesFrom
, updatesNext

-- * Pure Database Snapshot
, Snapshot
, snapshot
, syncSnapshot

-- * Queries
, branches
, children
, height
, highest
, getEntry
, getEntryIO
, getEntrySync
, lookupEntry

-- * Insertion
, insert

-- * Serialization
, encodeKey
, decodeKey
, encodeEntry
, decodeEntry

-- * Exceptions
, DbException(..)

-- * implementation specific
, entry
, dbKey
, dbEntry

-- * Validation
, isValidEntry
, validateEntry
, validateEntryM
) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens hiding (children)
import Control.Monad
import Control.Monad.Catch

import Data.Aeson
import qualified Data.ByteString as B
import Data.Foldable (toList, maximumBy)
import Data.Function
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Kind
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Numeric.Natural

-- internal imports

import Chainweb.BlockHeader (BlockHeader(..), BlockHeight, prop_block_difficulty, prop_block_hash, prop_block_genesis_parent, prop_block_genesis_target)
import qualified Chainweb.ChainDB.Entry as E
import Chainweb.Utils

-- validation
import Chainweb.Difficulty

-- -------------------------------------------------------------------------- --
-- Internal DB Representation

type ChildrenMap = HM.HashMap E.Key (HS.HashSet E.Key)

data Db = Db
    { _dbEntries :: !(HM.HashMap E.Key E.Entry)
    , _dbBranches :: !(HS.HashSet E.Key)
    , _dbChildren :: !ChildrenMap
    }

makeLenses ''Db

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
--
dbAdd :: E.Entry -> Db -> Db
dbAdd e db = db
    & dbEntries %~ HM.insert (E.key e) e
    & dbBranches %~ dbAddBranch e
    & dbChildren %~ dbAddChildren e

dbAddCheckedInternal :: MonadThrow m => E.Entry -> Db -> m Db
dbAddCheckedInternal e db = case E.parent e of
    Nothing -> return $ dbAdd e db
    Just p -> case HM.lookup p (_dbEntries db) of
        Nothing -> throwM $ ParentMissing (UncheckedEntry e)
        Just pe -> do
            unless (E.rank e == E.rank pe + 1)
                $ throwM $ InvalidRank (UncheckedEntry e)
            return $ dbAdd e db

dbAddChecked_ :: MonadThrow m => E.Entry -> Db -> m (Db, Maybe E.Key)
dbAddChecked_ e db
    | isMember = return (db, Nothing)
    | otherwise = (, Just k) <$> dbAddCheckedInternal e db
  where
    k = E.key e
    isMember = HM.member k (_dbEntries db)

dbAddChecked :: MonadThrow m => E.Entry -> Db -> m Db
dbAddChecked e db = fst <$> dbAddChecked_ e db

dbAddBranch :: E.Entry -> HS.HashSet E.Key -> HS.HashSet E.Key
dbAddBranch e bs = HS.insert (E.key e)
    $ maybe bs (`HS.delete` bs) (E.parent e)

dbAddChildren :: E.Entry -> ChildrenMap -> ChildrenMap
dbAddChildren e cs = HM.insert k mempty $ case E.parent e of
    Just p -> HM.insertWith (<>) p (HS.singleton k) cs
    _ -> cs
  where
    k = E.key e

-- -------------------------------------------------------------------------- --
-- Exceptions

data DbException
    = ValidationFailed (Entry 'Unchecked) SomeException
    | ParentMissing (Entry 'Unchecked)
    | InvalidRank (Entry 'Unchecked)
    | DeserializationFailure SomeException
    | Base64DeserializationFailed
    deriving (Show)

instance Exception DbException

-- -------------------------------------------------------------------------- --
-- Chain Database Handle

-- | Configuration of the chain DB.
--
data Configuration = Configuration
    { _configRoot :: !E.Entry
    }

-- | A handle to the database. This is a mutable stateful object.
--
-- The database is guaranteed to never be empty.
--
data ChainDb = ChainDb
    { _getDb :: MVar Db
    , _dbEnumeration :: !(TVar (Seq.Seq (Key 'Checked)))
    }

-- | Initialize a database handle
--
initChainDb :: Configuration -> IO ChainDb
initChainDb config = ChainDb
    <$> newMVar (dbAdd root emptyDb)
    <*> newTVarIO (Seq.singleton (CheckedKey $ E.key root))
  where
    root = _configRoot config
    emptyDb = Db mempty mempty mempty

-- | Close a database handle and release all resources
--
closeChainDb :: ChainDb -> IO ()
closeChainDb = void . takeMVar . _getDb

-- | Make a copy of a `ChainDb` whose memory is independent of the original.
-- Useful for duplicating chains within a testing environment.
--
copy :: ChainDb -> IO ChainDb
copy chain = do
    tv <- atomically $ readTVar (_dbEnumeration chain) >>= newTVar
    db <- takeMVar mv
    mv' <- newMVar db
    putMVar mv db
    pure $ ChainDb mv' tv
  where
    mv = _getDb chain

-- -------------------------------------------------------------------------- --
-- Validation Status

-- | A valid value is an entry or key of an entry that has been incorporated
-- into the database and has thus been validated to meet all consistency
-- requirements.
--
data ValidationStatus = Unchecked | Checked

-- -------------------------------------------------------------------------- --
-- Entry Type

-- | Key type for Entries. A key uniquly globally identifies an entry but is
-- expected to be much shorted than the entry itself.
--
data Key :: ValidationStatus -> Type where
    UncheckedKey :: E.Key -> Key 'Unchecked
    CheckedKey :: E.Key -> Key 'Checked

deriving instance Show (Key s)
deriving instance Eq (Key s)
deriving instance Ord (Key s)

dbKey :: Key s -> E.Key
dbKey (UncheckedKey k) = k
dbKey (CheckedKey k) = k

instance Hashable (Key s) where
    hashWithSalt s (UncheckedKey k) = hashWithSalt s k
    hashWithSalt s (CheckedKey k) = hashWithSalt s k

-- | Type of a database entry
--
data Entry :: ValidationStatus -> Type where
    UncheckedEntry :: E.Entry -> Entry 'Unchecked
    CheckedEntry :: E.Entry -> Entry 'Checked

deriving instance Show (Entry s)
deriving instance Eq (Entry s)

entry :: E.Entry -> Entry 'Unchecked
entry = UncheckedEntry

dbEntry :: Entry s -> E.Entry
dbEntry (UncheckedEntry e) = e
dbEntry (CheckedEntry e) = e

-- | Compute the 'Key' from an 'Entry'. A key is a globally unique hash of an
-- entry. Two entries have the same key if and only if they are the same.
--
key :: Entry s -> Key s
key (UncheckedEntry e) = UncheckedKey $ E.key e
key (CheckedEntry e) = CheckedKey $ E.key e

-- | Each but exaclty one entry has a parent. The unique entry without a parent
-- is called the root entry.
--
-- The parent relation induces a tree on the set of all entries.
--
parent :: Entry s -> Maybe (Key s)
parent (UncheckedEntry e) = UncheckedKey <$> E.parent e
parent (CheckedEntry e) = CheckedKey <$> E.parent e

-- | The rank of an entry is the depth of the entry in the tree from the root.
--
rank :: Entry s -> Natural
rank = E.rank . dbEntry

-- | It's always possible to declare a key to be unchecked.
--
uncheckedKey :: Key s -> Key 'Unchecked
uncheckedKey = UncheckedKey . dbKey

-- | It's always possible to declare an entry  to be unchecked.
--
uncheckedEntry :: Entry s -> Entry 'Unchecked
uncheckedEntry  = UncheckedEntry . dbEntry

-- | Decides whether a key is checked.
--
decideKeyStatus :: Key s -> Either (Key 'Unchecked) (Key 'Checked)
decideKeyStatus k@UncheckedKey{} = Left k
decideKeyStatus k@CheckedKey{} = Right k

-- | Decides whether an entry is checked.
--
decideEntryStatus :: Entry s -> Either (Entry 'Unchecked) (Entry 'Checked)
decideEntryStatus e@UncheckedEntry{} = Left e
decideEntryStatus e@CheckedEntry{} = Right e

-- | A set of database keys.
--
type KeySet (s :: ValidationStatus) = HS.HashSet (Key s)

-- -------------------------------------------------------------------------- --
-- Updates

-- | This is a mutable object that represents a stateful traversal of all
-- entries in the tree. It has the following properties:
--
-- 1. The traversal is deterministic,
-- 2. the traversed set forms a valid tree,
-- 3. the traversal can be started from any key in the database.
--
-- There is no guarantee that entries are traversed in the same order as they
-- are inserted.
--
data Updates = Updates
    { _updatesCursor :: !(TVar Int)
    , _updatesEnum :: !(TVar (Seq (Key 'Checked)))
    }

-- | Creates a traversal from some recently added key
--
updates :: ChainDb -> IO Updates
updates db = Updates
    <$> newTVarIO 0
    <*> pure (_dbEnumeration db)

-- FIXME improve performance
--
-- | Creates a traversal from a given key
--
updatesFrom :: ChainDb -> Key 'Checked -> IO Updates
updatesFrom db k = do
    enumeration <- readTVarIO enumVar
    idx <- case Seq.elemIndexL k enumeration of
        Just i -> return i
        Nothing -> error "TODO: Internal invariant violation"
    Updates
        <$> newTVarIO idx
        <*> pure enumVar
  where
    enumVar = _dbEnumeration db

-- | Get next entry (represented by it's key) in the traversal
--
updatesNext :: Updates -> STM (Key 'Checked)
updatesNext u = do
    xs <- readTVar (_updatesEnum u)
    c <- readTVar (_updatesCursor u)
    case Seq.lookup c xs of
        Nothing -> retry
        Just x -> do
            writeTVar (_updatesCursor u) (c + 1)
            return x

-- -------------------------------------------------------------------------- --
-- Pure Database Snapshot

-- | An immutable pure snapshot of the database that can be queried and updated
-- efficientl locally in pure code.
--
-- Any changes to the 'Snapshot' are persisted to the backend database and
-- become visible to other users of the backend database only after calling
-- 'syncSnapshot'.
--
-- Any changes to the backend database become visible only after a calling
-- 'syncSnapshot'.
--
data Snapshot = Snapshot
    { _snapshotDb :: !Db
    , _snapshotAdditions :: !(HM.HashMap E.Key E.Entry)
    , _snapshotChainDb :: !ChainDb
    }

makeLenses ''Snapshot

-- | Checkout a pure snapshot of the database.
--
snapshot :: ChainDb -> IO Snapshot
snapshot db@(ChainDb dbVar _) = Snapshot
    <$> readMVar dbVar
    <*> pure mempty
    <*> pure db

-- | Sychronize a database snapshot with the backend database. Because the
-- stored tree is content addressed and monotonically increasing there can't be
-- any conflicts.
--
-- All invariants are checked. If an invariant is violated this throws the
-- respective exception.
--
syncSnapshot :: Snapshot -> IO Snapshot
syncSnapshot s
    | HM.null (_snapshotAdditions s) = snapshot db
    | otherwise = do

        -- insert entries to db and collect new keys
        --
        news <- modifyMVar (_getDb db)
            $ \x -> foldM
                (\(d, ns) e -> fmap (maybe ns (ns |>)) <$> dbAddChecked_ e d)
                (x, mempty)
                rankedAdditions

        -- publish new keys to updates enumeration
        --
        atomically $ modifyTVar' (_dbEnumeration db)
            $ \x -> x <> (CheckedKey <$> news)

        -- return fresh snapshot
        snapshot db
  where
    db = view snapshotChainDb s
    rankedAdditions = L.sortOn E.rank $ HM.elems $ _snapshotAdditions s

-- -------------------------------------------------------------------------- --
-- Queries

-- | The set of all entries (identified by their key) that are not a parent of
-- an entry. These are the leafs of the tree.
--
-- Together with the 'parent' relation this allows to traverse the tree of
-- entries from the leafs to the root.
--
-- This result set is guaranteed to be non-empty. It will at least contain the
-- root of the tree.
--
branches :: Snapshot -> KeySet 'Checked
branches = HS.map CheckedKey . _dbBranches . _snapshotDb

-- | The children relation allows to efficiently traverse the tree of entries
-- from the root to the leafs.
--
children :: Key 'Checked -> Snapshot -> KeySet 'Checked
children k s = case HM.lookup (dbKey k) . _dbChildren $ _snapshotDb s of
    Nothing -> error "TODO internal exception"
    Just c -> HS.map CheckedKey c

-- | Get the entry for a key.
--
-- 'Nothing' is returned only if the snapshot is outdated, i.e. when the
-- snapshot was taken before the key was included in the database. Synchronizing
-- the snapshot with 'syncSnapshot' will resolve this and 'getEntry' is
-- guaranteed to return 'Just' a value.
--
getEntry :: Key 'Checked -> Snapshot -> Maybe (Entry 'Checked)
getEntry = lookupEntry

-- | Looks up an entry in a database snapshot. Returns 'Nothing' if the entry
-- for the given key is not in the snapshot.
--
lookupEntry :: Key t -> Snapshot -> Maybe (Entry 'Checked)
lookupEntry k =
    fmap CheckedEntry . HM.lookup (dbKey k) . _dbEntries . _snapshotDb

-- | Get the entry for key.
--
-- Unlike 'getEntry' this function is guaranteed to return a value at the cost
-- of being in IO and possibly querying the entry from the backend database.
--
getEntryIO :: Key 'Checked -> Snapshot -> IO (Entry 'Checked)
getEntryIO k s = case getEntry k s of
    Just c -> return c
    Nothing -> snapshot db >>= \s' -> case getEntry k s' of
        Just c -> return c
        Nothing -> error "TODO internal exception"
  where
    db = view snapshotChainDb s

-- | Get the entry for a key.
--
-- Similar to `getEntryIO`, except a `Snapshot` is returned as well.
-- If an internal sync needed to occur before yielding an entry,
-- then the `Snapshot` will be new. Otherwise, it will be the same
-- as what was given.
--
-- Fails with an exception if the key was not present in the Snapshot,
-- even after syncing. This would only occur if the checked key was
-- taken from a database /other/ than the one being queried.
--
-- Note: As a side-effect, any non-committed additions will be committed
-- by calling this function.
--
getEntrySync :: Key 'Checked -> Snapshot -> IO (Snapshot, Entry 'Checked)
getEntrySync = f sync
  where
    f g k s  = maybe (g k s) (pure . (s,)) $ getEntry k s
    sync k s = syncSnapshot s >>= f die k
    die _ _  = error "Checked Key from a different database used for Snapshot query"

-- | The current highest `BlockHeight` in the entire chain.
--
height :: Snapshot -> BlockHeight
height = _blockHeight . highest

-- | The `BlockHeader` with the highest block height.
--
highest :: Snapshot -> BlockHeader
highest s = maximumBy (compare `on` _blockHeight)
    . mapMaybe (fmap dbEntry . (`lookupEntry` s))
    . toList $ branches s

-- -------------------------------------------------------------------------- --
-- Insertion

-- | Inserts an 'Entry' in the database snapshots and performs all consistency
-- checks.
--
-- Throws the first 'ValidationFailure' produced by 'validateEntry', if any.
--
-- In addition, there are some redundant checks that presently produce exceptions of
-- other types:
--
-- Raises an 'ParentMissing' exception if the parent of the entry is not in the
-- snapshot.
--
-- Raises an 'InvalidRank' exception if the rank of the entry doesn't equal the
-- rank of the parent plus one.
--
-- Raises an 'ValidationFailed' exception if some other implementation specific
-- consistency check failed.
--
insert :: MonadThrow m => Entry s -> Snapshot -> m Snapshot
insert e s
    | E.key dbe `HM.member` _dbEntries (_snapshotDb s) = return s
    | otherwise = do
        validateEntryM s e
        snapshotDb (dbAddChecked dbe) 
            . over snapshotAdditions (HM.insert (E.key dbe) dbe) 
            $ s
  where
    dbe = dbEntry e

-- -------------------------------------------------------------------------- --
-- Serialization of Entries

-- | Serialize a 'Key'
--
encodeKey :: Key s -> B.ByteString
encodeKey = E.encodeKey . dbKey

-- | Deserialize a 'Key'. The deserialized value is 'Unchecked'
--
-- Raises a 'DeserializationFailure' if decoding fails.
--
decodeKey :: MonadThrow m => B.ByteString -> m (Key 'Unchecked)
decodeKey = fmap UncheckedKey . E.decodeKey

-- | Serialize an 'Entry'
--
encodeEntry :: Entry s -> B.ByteString
encodeEntry = E.encodeEntry . dbEntry

-- | Deserialize an 'Entry'. The deserialized value is 'Unchecked'.
--
-- Raises a 'DeserializationFailure' if decoding fails.
--
decodeEntry :: MonadThrow m => B.ByteString -> m (Entry 'Unchecked)
decodeEntry = fmap UncheckedEntry . E.decodeEntry

-- -------------------------------------------------------------------------- --
-- JSON Serialization

instance ToJSON (Key 'Unchecked) where
    toJSON = toJSON . encodeB64UrlNoPaddingText . encodeKey

instance FromJSON (Key 'Unchecked) where
    parseJSON = withText "key" $ either (fail . show) return
        . (decodeKey <=< decodeB64UrlNoPaddingText)

instance ToJSON (Entry 'Unchecked) where
    toJSON = toJSON . encodeB64UrlNoPaddingText . encodeEntry

instance FromJSON (Entry 'Unchecked) where
    parseJSON = withText "entry" $ either (fail . sshow) return
        . (decodeEntry <=< decodeB64UrlNoPaddingText)

-- -------------------------------------------------------------------------- --
-- BlockHeader Validation

data ValidationFailure = ValidationFailure E.Entry [ValidationFailureType]

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
data ValidationFailureType =
      MissingParent -- ^ Parent isn't in the database
    | CreatedBeforeParent -- ^ Claims to be created at a time prior to its parent's creation
    | VersionMismatch -- ^ Claims to use a version of chainweb different from that of its parent
    | IncorrectHash -- ^ The hash of the header properties as computed by computeBlockHash does not match the hash given in the header
    | IncorrectHeight -- ^ The given height is not one more than the parent height
    | IncorrectWeight -- ^ The given weight is not the sum of the target difficulty and the parent's weight
    | IncorrectTarget -- ^ The given target difficulty for the following block is not correct (TODO: this isn't yet checked, but Chainweb.ChainDB.Difficulty.calculateTarget is relevant.)
    | IncorrectGenesisParent -- ^ The block is a genesis block, but doesn't have its parent set to its own hash.
    | IncorrectGenesisTarget -- ^ The block is a genesis block, but doesn't have the correct difficulty target.
  deriving (Show, Eq, Ord)

instance Exception ValidationFailure

-- | Validate properties of the block header, throwing an exception detailing the failures if any.
validateEntryM
    :: (MonadThrow m)
    => Snapshot
    -> Entry a
    -> m ()
validateEntryM s e = case validateEntry s e of
    [] -> return ()
    failures -> throwM (ValidationFailure (dbEntry e) failures)

-- | Validate properties of the block header, producing a list of the validation failures
validateEntry
    :: Snapshot -- ^ A snapshot of the database
    -> Entry a -- ^ The block header to be checked
    -> [ValidationFailureType] -- ^ A list of ways in which the block header isn't valid
validateEntry s e = case e of
    UncheckedEntry b -> validateBlockHeaderIntrinsic b ++ validateBlockHeaderInductive s b
    CheckedEntry b -> validateBlockHeaderIntrinsic b ++ validateBlockHeaderInductive s b
        -- TODO: Consider returning an empty list immediately, since it's supposed to have already been checked.

-- | Validates properties of a block with respect to its parent.
validateBlockHeaderInductive
    :: Snapshot
    -> E.Entry
    -> [ValidationFailureType]
validateBlockHeaderInductive s b =
    case lookupEntry (UncheckedKey (_blockParent b)) s of
        Nothing -> [MissingParent]
        Just (CheckedEntry p) -> validateParent p b

-- | Validates properties of a block which are checkable from the block header without observing the remainder
-- of the database.
validateBlockHeaderIntrinsic
    :: E.Entry -- ^ block header to be validated
    -> [ValidationFailureType]
validateBlockHeaderIntrinsic b = concat
    [ [ IncorrectTarget | not (prop_block_difficulty b) ]
    , [ IncorrectHash | not (prop_block_hash b) ]
    , [ IncorrectGenesisParent | not (prop_block_genesis_parent b)]
    , [ IncorrectGenesisTarget | not (prop_block_genesis_target b)]
    ]

-- | Validate properties of a block with respect to a given parent.
validateParent
    :: E.Entry -- ^ Parent block header
    -> E.Entry -- ^ Block header under scrutiny
    -> [ValidationFailureType]
validateParent p b = concat
    [ [ IncorrectHeight | not (_blockHeight b == _blockHeight p + 1) ]
    , [ VersionMismatch | not (_blockChainwebVersion b == _blockChainwebVersion p) ]
    , [ CreatedBeforeParent | not (_blockCreationTime b > _blockCreationTime p) ]
    , [ IncorrectWeight | not (_blockWeight b == fromIntegral (targetToDifficulty (_blockTarget b)) + _blockWeight p) ]
    -- TODO:
    -- target of block matches the calculate target for the branch
    ]

-- | Tests if the block header is valid (i.e. 'validateBlockHeader' produces an empty list)
isValidEntry :: Snapshot -> Entry a -> Bool
isValidEntry s b = null (validateEntry s b)
