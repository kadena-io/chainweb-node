{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Store.Git.Internal
-- Copyright: Copyright © 2018 - 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Gregory Collins <greg@kadena.io>, Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Internal machineary for "Chainweb.Store.Git".

module Chainweb.Store.Git.Internal
  ( -- * Types
    -- ** Data
    GitStore(..)
  , GitStoreBlockHeader(..)
  , GitStoreData(..)
  , GitStoreConfig(..)
  , TreeEntry(..)
  , LeafTreeData(..)
  , BlobEntry(..)
  , GitHash(..)
    -- ** Errors
  , GitFailure(..)
    -- ** Utilities
  , NullTerminated
  , terminate

    -- * Queries
  , readLeafTree
  , readHeader
  , readHeader'
  , leaves
  , leaves'
  , highestLeaf
  , lookupByBlockHash
  , lookupTreeEntryByHash
  , readParent
  , leafEntries

    -- * Traversal
  , walk
  , walk'
  , seekHighest

    -- * Insertion
  , InsertResult(..)
  , insertBlock
  , insertBlockHeaderIntoOdb
  , addSelfEntry
  , createBlockHeaderTag
  , tagAsLeaf

    -- * Brackets
  , lockGitStore
  , withOid
  , withObject
  , withTreeBuilder

    -- * Failure
    -- | Convenience functions for handling error codes returned from @libgit2@
    -- functions.
  , throwOnGitError
  , throwGitStoreFailure
  , maybeTGitError

    -- * Utils
  , Spectrum(..)
  , getSpectrum
  , parseLeafTreeFileName
  , oidToByteString
  , getMerkleLogHash
  , mkTreeEntryNameWith
  , mkTagName
  ) where

import qualified Bindings.Libgit2 as G

import Control.Concurrent.MVar (MVar, withMVar)
import Control.DeepSeq (NFData)
import Control.Error.Util (hoistMaybe, hush, nothing)
import Control.Exception (Exception, bracket, bracket_, mask, throwIO)
import Control.Lens.Iso (iso)
import Control.Monad (foldM, unless, void, when, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Bifunctor (first)
import Data.Bits (complement, unsafeShiftL, (.&.))
import Data.ByteArray.Encoding (Base(..), convertFromBase, convertToBase)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.FastBuilder as FB
import qualified Data.ByteString.Unsafe as B
import Data.Char (digitToInt)
import Data.Coerce (coerce)
import Data.Foldable (maximumBy, traverse_)
import Data.Function (on)
import Data.Functor (($>))
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.Semigroup (Max(..), Min(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import Data.Witherable (wither)
import Data.Word (Word64)

import Foreign.C.String (CString)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)

import GHC.Generics (Generic)

import Streaming (Of, Stream)
import qualified Streaming.Prelude as P

import System.Path (Absolute, Path)

-- internal modules

import Chainweb.BlockHash (BlockHash(..))
import Chainweb.BlockHeader
import Chainweb.MerkleLogHash
import Chainweb.TreeDB
import Chainweb.Utils (int)
import Chainweb.Utils.Paging (Limit(..), NextItem(..))

---

--------
-- TYPES
--------

newtype GitStoreBlockHeader = GitStoreBlockHeader BlockHeader
    deriving (Eq, Ord, Show, Generic, Hashable, NFData)

instance TreeDbEntry GitStoreBlockHeader where
    type Key GitStoreBlockHeader = T2 BlockHeight BlockHash

    key (GitStoreBlockHeader bh) = T2 (_blockHeight bh) (_blockHash bh)

    rank (GitStoreBlockHeader bh) = int $ _blockHeight bh

    parent (GitStoreBlockHeader bh)
        | isGenesisBlockHeader bh = Nothing
        | otherwise = Just p
      where
        p = T2 (_blockHeight bh - 1) (_blockParent bh)

instance IsBlockHeader GitStoreBlockHeader where
    isoBH = iso coerce GitStoreBlockHeader

-- | The fundamental git-based storage type. Can be initialized via
-- `Chainweb.Store.Git.withGitStore` and then queried as needed.
--
newtype GitStore = GitStore (MVar GitStoreData)

instance TreeDb GitStore where
    type DbEntry GitStore = GitStoreBlockHeader

    lookup gs (T2 hgt hsh) = fmap GitStoreBlockHeader <$> lookupByBlockHash gs hgt hsh

    entries gs next limit minr0 maxr inner = inner $ do
        ls <- liftIO $ lockGitStore gs leaves'
        let !highest = _te_blockHeight $ maximumBy (compare `on` _te_blockHeight) ls
        counter <- liftIO $ newIORef 0
        countItems counter . postprocess $ work ls highest minr (min (minr + range) highest)
        total <- liftIO $ readIORef counter
        pure (int total, Eos True)
      where
        -- | Best value, found experimentally.
        --
        -- @
        -- | pageSize = 256 | 72.35 |
        -- | pageSize = 128 | 57.47 |
        -- | pageSize = 64  | 49.26 |
        -- | pageSize = 32  | 51.07 |
        -- @
        --
        pageSize :: BlockHeight
        pageSize = 64

        range :: BlockHeight
        range = pageSize - 1

        minr :: BlockHeight
        minr = max (maybe 0 y minr0) (maybe 0 z next)
          where
            y :: MinRank -> BlockHeight
            y (MinRank (Min mh)) = int mh

            z :: NextItem (DbKey GitStore) -> BlockHeight
            z (Inclusive (T2 h _)) = h
            z (Exclusive (T2 h _)) = h + 1

        postprocess :: Stream (Of GitStoreBlockHeader) IO () -> Stream (Of GitStoreBlockHeader) IO ()
        postprocess =
            maybe id filterItems limit
            . maybe id seekToNext next
            . maybe id maxItems maxr

        -- TODO Cache various results of `seekHighest`, so as to speed up subsequent lookups?
        -- | Given a range of `BlockHeight`s, stream all associated
        -- `BlockHeader`s in /ascending/ order.
        --
        work
            :: [TreeEntry]
            -> BlockHeight  -- ^ The height of the heighest leaf in the entire store
            -> BlockHeight  -- ^ Height of the lowest `BlockHeader` to be streamed
            -> BlockHeight  -- ^ Height of the highest `BlockHeader` to be streamed
            -> Stream (Of (DbEntry GitStore)) IO ()
        work ls highest !lower !upper = do
            let !cache = HM.empty
            ls' <- liftIO $ wither (seekHighest gs (lower, upper)) ls
            m <- liftIO $ foldM f cache ls'
            P.mapM g . P.each . sortOn fst $ HM.toList m
            unless (upper >= highest) $
                work ls highest (succ upper) (min (upper + pageSize) highest)
          where
            f :: HM.HashMap (T2 BlockHeight MerkleLogHash) (T2 TreeEntry BlobEntry)
              -> TreeEntry
              -> IO (HM.HashMap (T2 BlockHeight MerkleLogHash) (T2 TreeEntry BlobEntry))
            f cache nxt = lockGitStore gs $ \gsd -> do
                let !start = T2 (_te_blockHeight nxt) (_te_blockHash nxt)
                cachedWalk gsd start lower cache

            g :: (T2 BlockHeight MerkleLogHash, T2 TreeEntry BlobEntry) -> IO GitStoreBlockHeader
            g (_, T2 _ blob) =
                GitStoreBlockHeader <$> lockGitStore gs (\gsd -> readHeader' gsd blob)

    insert gs (GitStoreBlockHeader bh) = void $ insertBlock gs bh

    maxEntry gs = GitStoreBlockHeader <$> maximumBy (compare `on` _blockHeight) <$> leaves gs

leafEntries
    :: Num a
    => GitStore
    -> Maybe (NextItem (T2 BlockHeight BlockHash))
    -> Maybe Limit
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Stream (Of GitStoreBlockHeader) IO (a, Eos)
leafEntries gs next limit minr maxr = do
    ls <- fmap (sortOn _blockHeight) . liftIO $ leaves gs
    counter <- liftIO $ newIORef 0
    countItems counter . postprocess . P.map GitStoreBlockHeader $ P.each ls
    total <- liftIO $ readIORef counter
    pure (int total, Eos True)
    where
    postprocess :: Stream (Of (DbEntry GitStore)) IO () -> Stream (Of (DbEntry GitStore)) IO ()
    postprocess =
        maybe id filterItems limit
        . maybe id seekToNext next
        . maybe id maxItems maxr
        . maybe id minItems minr

    minItems :: MinRank -> Stream (Of (DbEntry GitStore)) IO () -> Stream (Of (DbEntry GitStore)) IO ()
    minItems (MinRank (Min n)) =
        P.dropWhile (\(GitStoreBlockHeader bh) -> int (_blockHeight bh) < n)

filterItems :: Limit -> Stream (Of (DbEntry GitStore)) IO () -> Stream (Of (DbEntry GitStore)) IO ()
filterItems = P.take . int . _getLimit

maxItems :: MaxRank -> Stream (Of (DbEntry GitStore)) IO r -> Stream (Of (DbEntry GitStore)) IO ()
maxItems (MaxRank (Max n)) =
    P.takeWhile (\(GitStoreBlockHeader bh) -> int (_blockHeight bh) <= n)

seekToNext
    :: NextItem (DbKey GitStore)
    -> Stream (Of (DbEntry GitStore)) IO ()
    -> Stream (Of (DbEntry GitStore)) IO ()
seekToNext (Inclusive (T2 _ hash)) =
    P.dropWhile (\(GitStoreBlockHeader bh) -> _blockHash bh /= hash)
seekToNext (Exclusive n) =
    P.drop 1 . seekToNext (Inclusive n)

countItems :: IORef Int -> Stream (Of b) IO r -> Stream (Of b) IO r
countItems counter = P.mapM j
  where
    j i = i <$ atomicModifyIORef' counter (\n -> (n+1, ()))

-- | Many of the functions in this module require this type. The easiest and
-- safest way to get it is via `lockGitStore`. Don't try and manipulate the
-- pointers yourself.
--
data GitStoreData = GitStoreData {
    _gitStore :: {-# UNPACK #-} !(Ptr G.C'git_repository)
  , _gitOdb :: {-# UNPACK #-} !(Ptr G.C'git_odb)
  , _gitConfig :: !GitStoreConfig
}

-- | For opening an existing repository, or for initializing a new one.
--
data GitStoreConfig = GitStoreConfig {
    _gsc_path :: !(Path Absolute)
  , _gsc_genesis :: !BlockHeader
}

-- TODO It's almost certainly possible to give this an instance of `Storable`.
-- Then, the `_ltd_spectrum` field of `LeafTreeData` can become a Storable
-- Vector, from which the rest of the code can benefit.
--
-- See: https://github.com/fosskers/vectortiles/blob/ea1236a84a973e4b0517afeae903986736394a4b/lib/Geography/VectorTile/Geometry.hs#L44-L48
-- | A Haskell-friendly distillation of the `G.C'git_tree_entry` type.
--
data TreeEntry = TreeEntry {
    _te_blockHeight :: {-# UNPACK #-} !BlockHeight
  , _te_blockHash :: {-# UNPACK #-} !MerkleLogHash
  , _te_gitHash :: {-# UNPACK #-} !GitHash
} deriving (Show, Eq, Ord, Generic, NFData)

-- | While `TreeEntry` represents a kind of "pointer" to a stored `BlockHeader`,
-- `LeafTreeData` contains its "spectrum" (points to ancestors in the chain to
-- allow for fast traversal) and a further point to the actual blob data
-- containing the encoded `BlockHeader`.
--
data LeafTreeData = LeafTreeData {
    _ltd_blobEntry :: !BlobEntry
    -- ^ Pointer to the blob data associated with this `TreeEntry`. A
    -- `BlockHeader`.
  , _ltd_spectrum :: !(Vector TreeEntry)
} deriving (Show)

newtype BlobEntry = BlobEntry { _blobEntry :: TreeEntry } deriving (Show)

-- | A reference to a particular git object, corresponding to the type
-- `G.C'git_oid`.
--
newtype GitHash = GitHash ByteString deriving (Eq, Ord, Show, Generic, NFData)

-- | See here for all possible libgit2 errors:
-- https://github.com/libgit2/libgit2/blob/99afd41f1c43c856d39e3b9572d7a2103875a771/include/git2/errors.h#L21
--
data GitFailure = GitFailure {
    gitFailureHFun :: Text  -- ^ The Haskell function the error was thrown from.
  , gitFailureCFun :: Text  -- ^ The C function that originated the error.
  , gitFailureErrorCode :: CInt
} deriving (Show)

instance Exception GitFailure

newtype GitStoreFailure = GitStoreFailure { gitStoreFailureReason :: Text }
  deriving (Show)

instance Exception GitStoreFailure

-- | A `ByteString` whose final byte is a @\\0@. Don't try to be too clever with
-- this.
--
newtype NullTerminated = NullTerminated { _unterminated :: [ByteString] }

nappend :: ByteString -> NullTerminated -> NullTerminated
nappend b (NullTerminated b') = NullTerminated $ b : b'
{-# INLINE nappend #-}

terminate :: NullTerminated -> ByteString
terminate = B.concat . _unterminated
{-# INLINE terminate #-}

----------
-- QUERIES
----------

-- | Follow a (hopefully) established object id (`GitHash`) of a `TreeEntry` to
-- its data, yielding its "spectrum" and a further pointer to its blob data.
--
readLeafTree :: GitStoreData -> GitHash -> IO LeafTreeData
readLeafTree store treeGitHash = withTreeObject store treeGitHash readTree
  where
    readTree :: Ptr G.C'git_tree -> IO LeafTreeData
    readTree pTree = do
        numEntries <- G.c'git_tree_entrycount pTree
        elist <- traverse (readTreeEntry pTree) [0..(numEntries-1)]
        spectrum <- sortSpectrum elist
        when (V.null spectrum) $ throwGitStoreFailure "impossible: empty tree"
        let lastEntry = V.unsafeLast spectrum
        pure $! LeafTreeData (BlobEntry lastEntry) (V.init spectrum)

    readTreeEntry :: Ptr G.C'git_tree -> CSize -> IO TreeEntry
    readTreeEntry pTree idx = G.c'git_tree_entry_byindex pTree idx >>= fromTreeEntryP

    fromTreeEntryP :: Ptr G.C'git_tree_entry -> IO TreeEntry
    fromTreeEntryP entryP = do
        name <- G.c'git_tree_entry_name entryP >>= B.packCString
        oid  <- GitHash <$> (G.c'git_tree_entry_id entryP >>= oidToByteString)
        (h, bh) <- maybe (throwGitStoreFailure "Tree object with incorrect naming scheme!") pure
                         (parseLeafTreeFileName name)
        pure $! TreeEntry h bh oid

    sortSpectrum :: [TreeEntry] -> IO (Vector TreeEntry)
    sortSpectrum l = do
        mv <- V.unsafeThaw (V.fromList l)
        V.sort mv
        V.unsafeFreeze mv

-- TODO Avoid calling the entire `readLeafTree` - use `withTreeObject` and
-- `unsafeReadTree` to fetch the blob hash directly.
-- | Fetch the `BlockHeader` that corresponds to some `TreeEntry`.
--
readHeader :: GitStoreData -> TreeEntry -> IO BlockHeader
readHeader store (TreeEntry _ _ gh) = readLeafTree store gh >>= readHeader' store . _ltd_blobEntry

-- | A short-cut, for when you already have your hands on the inner
-- `BlobEntry`.
--
readHeader' :: GitStoreData -> BlobEntry -> IO BlockHeader
readHeader' store blob = do
    let blobHash = _te_gitHash $ _blobEntry blob
    bs <- getBlob store blobHash
    either (throwGitStoreFailure . T.pack) pure $
        runGetS decodeBlockHeader bs

-- | Fetch the raw byte data of some object in the Git Store.
--
getBlob :: GitStoreData -> GitHash -> IO ByteString
getBlob (GitStoreData repo _ _) gh = bracket lookupBlob destroy readBlob
  where
    lookupBlob :: IO (Ptr G.C'git_blob)
    lookupBlob = mask $ \restore -> alloca $ \pBlob -> withOid gh $ \oid -> do
        throwOnGitError "getBlob" "git_blob_lookup" $
            restore $ G.c'git_blob_lookup pBlob repo oid
        peek pBlob

    destroy :: Ptr G.C'git_blob -> IO ()
    destroy = G.c'git_blob_free

    readBlob :: Ptr G.C'git_blob -> IO ByteString
    readBlob blob = do
        content <- G.c'git_blob_rawcontent blob
        size <- G.c'git_blob_rawsize blob
        B.packCStringLen (castPtr content, fromIntegral size)

-- | The "leaves" - the tips of all branches.
--
leaves :: GitStore -> IO [BlockHeader]
leaves gs = lockGitStore gs $ \gsd -> leaves' gsd >>= traverse (readHeader gsd)

-- | All leaf nodes in their light "pointer" form.
--
-- If we are pruning properly, there should only ever be a few of these, hence a
-- list is appropriate.
--
leaves' :: GitStoreData -> IO [TreeEntry]
leaves' gsd = matchTags gsd (NullTerminated [ "leaf/*\0" ]) 5

highestLeaf :: GitStoreData -> IO TreeEntry
highestLeaf gsd = maximumBy (compare `on` _te_blockHeight) <$> leaves' gsd

matchTags :: GitStoreData -> NullTerminated -> Int -> IO [TreeEntry]
matchTags (GitStoreData repo _ _) nt chop =
    B.unsafeUseAsCString (terminate nt)
        $ \patt -> alloca
        $ \namesP -> do
            throwOnGitError "matchTags" "git_tag_list_match" $
                G.c'git_tag_list_match namesP patt repo
            a <- peek namesP
            names <- peekArray (fromIntegral $ G.c'git_strarray'count a) (G.c'git_strarray'strings a)
            wither getEntry names  -- TODO Report malformed tag names instead of ignoring?
 where
   -- TODO Update this example once a proper fixed-length encoding for `BlockHeight` is chosen.
   -- | Expected argument format:
   --
   -- @
   -- leaf/AAAAAAAAAAA=.7C1XaR2bLUAYKsVlAyBUt9eEupaxi8tb4LtOcOP7BB4=
   -- @
   --
   getEntry :: CString -> IO (Maybe TreeEntry)
   getEntry name = do
       name' <- B.packCString name
       let tagName = B.drop chop name'  -- Slice off "leaf/", etc.
           fullTagPath = B.concat [ "refs/tags/", name', "\0" ]
       B.unsafeUseAsCString fullTagPath
           $ \fullTagPath' -> alloca
           $ \oidP -> runMaybeT $ do
               (bh, bs) <- hoistMaybe $ parseLeafTreeFileName tagName
               maybeTGitError $ G.c'git_reference_name_to_id oidP repo fullTagPath'
               hash <- liftIO $ GitHash <$> oidToByteString oidP
               pure $! TreeEntry bh bs hash

lookupByBlockHash :: GitStore -> BlockHeight -> BlockHash -> IO (Maybe BlockHeader)
lookupByBlockHash gs height bh = lockGitStore gs $ \store -> do
    m <- lookupTreeEntryByHash store (getMerkleLogHash bh) (fromIntegral height)
    traverse (readHeader store) m

-- | Shouldn't throw, in theory.
--
lookupTreeEntryByHash
    :: GitStoreData
    -> MerkleLogHash
    -> BlockHeight
    -> IO (Maybe TreeEntry)
lookupTreeEntryByHash gs bh height =
    fmap (TreeEntry height bh) <$> lookupRefTarget gs tagRef
  where
    tagRef :: NullTerminated
    tagRef = mkTagRef height bh

-- | Shouldn't throw, in theory.
--
lookupRefTarget
    :: GitStoreData
    -> NullTerminated      -- ^ ref path, e.g. tags/foo
    -> IO (Maybe GitHash)
lookupRefTarget (GitStoreData repo _ _) path0 =
    B.unsafeUseAsCString (terminate path)
        $ \cpath -> alloca
        $ \pOid -> runMaybeT $ do
            maybeTGitError $ G.c'git_reference_name_to_id pOid repo cpath
            GitHash <$> liftIO (oidToByteString pOid)
  where
    path :: NullTerminated
    path = nappend "refs/" path0

lookupTreeEntryByHeight
    :: GitStoreData
    -> GitHash         -- ^ starting from this leaf tree
    -> BlockHeight     -- ^ desired blockheight
    -> IO TreeEntry
lookupTreeEntryByHeight gs leafTreeHash height =
    readLeafTree gs leafTreeHash >>=
    lookupTreeEntryByHeight' gs leafTreeHash height

lookupTreeEntryByHeight'
    :: GitStoreData
    -> GitHash
    -> BlockHeight     -- ^ desired blockheight
    -> LeafTreeData
    -> IO TreeEntry
lookupTreeEntryByHeight' gs leafTreeHash height (LeafTreeData (BlobEntry (TreeEntry leafHeight leafBH _)) spectrum)
    | height == leafHeight = pure $! TreeEntry height leafBH leafTreeHash
    | V.null spec' = throwGitStoreFailure "lookup failure"
    | otherwise = search
  where
    spec' :: Vector TreeEntry
    spec' = V.filter (\t -> _te_blockHeight t >= height) spectrum

    search :: IO TreeEntry
    search = do
        let frst = V.unsafeHead spec'
            gh = _te_gitHash frst
        if | _te_blockHeight frst == height -> pure frst
           | otherwise -> lookupTreeEntryByHeight gs gh height

readParent :: GitStoreData -> GitHash -> IO TreeEntry
readParent store treeGitHash = withTreeObject store treeGitHash (unsafeReadTree 1)

-- | Given a `G.C'git_tree` that you've hopefully gotten via the
-- `withTreeObject` bracket, read some @git_tree_entry@ marshalled into a usable
-- Haskell type (`TreeEntry`).
--
-- The offset value (the `CSize`) counts from the /end/ of the array of entries!
-- Therefore, an argument of @0@ will return the /last/ entry.
--
-- *NOTE:* It is up to you to pass a legal `CSize` value. For instance, an
-- out-of-bounds value will result in exceptions thrown from within C code,
-- crashing your program.
--
unsafeReadTree :: CSize -> Ptr G.C'git_tree -> IO TreeEntry
unsafeReadTree offset pTree = do
    numEntries <- G.c'git_tree_entrycount pTree
    let !index = numEntries - (offset + 1)
    gte <- G.c'git_tree_entry_byindex pTree index  -- TODO check for NULL here?
    fromTreeEntryP gte
  where
    fromTreeEntryP :: Ptr G.C'git_tree_entry -> IO TreeEntry
    fromTreeEntryP entryP = do
        name <- G.c'git_tree_entry_name entryP >>= B.packCString
        oid  <- GitHash <$> (G.c'git_tree_entry_id entryP >>= oidToByteString)
        (h, bh) <- maybe (throwGitStoreFailure "Tree object with incorrect naming scheme!") pure
                         (parseLeafTreeFileName name)
        pure $! TreeEntry h bh oid

------------
-- TRAVERSAL
------------

-- | Starting from a node indicated by a given `BlockHeight` and `BlockHash`,
-- traverse the tree from the node to the root, applying some function to each
-- associated `BlockHeader` along the way.
--
walk :: GitStore -> BlockHeight -> BlockHash -> (BlockHeader -> IO ()) -> IO ()
walk gs height (BlockHash bhb) f = lockGitStore gs $ \gsd -> do
    let f' :: BlobEntry -> IO ()
        f' = readHeader' gsd >=> f
    walk' gsd (T2 height bhb) 0 (const $ pure ()) f'

-- | Traverse the tree, as in `Chainweb.Store.Git.walk`. This version is faster,
-- as it does not spend time decoding each `TreeEntry` into a `BlockHeader`
-- (unless you tell it to, of course, say via `readHeader'`).
--
-- Internal usage only (since neither `TreeEntry` nor `LeafTreeData` are
-- exposed).
--
walk'
    :: GitStoreData
    -> T2 BlockHeight MerkleLogHash
    -> BlockHeight
    -> (TreeEntry -> IO ())
    -> (BlobEntry -> IO ())
    -> IO ()
walk' gsd (T2 height hash) target f g =
    lookupTreeEntryByHash gsd hash height >>= \case
        Nothing -> throwGitStoreFailure $ "Lookup failure for block at given height " <> bhText height
        Just te -> do
            f te
            withTreeObject gsd (_te_gitHash te) $ \gt -> do
                blob <- BlobEntry <$> unsafeReadTree 0 gt
                g blob
                unless (height == target) $ do
                    prnt <- unsafeReadTree 1 gt
                    walk' gsd (T2 (_te_blockHeight prnt) (_te_blockHash prnt)) target f g

-- | Walk some length of a git store, accumulating its elements. Stops either
-- when the target height has been reached, or we encounter a node that has
-- already been visited. The second condition should only occur when a
-- prepopulated cache is passed into this function initially, say after walking
-- some other branch first.
--
-- Note: Be careful not to walk too far with this, as it brings all the nodes it
-- visits into memory.
--
cachedWalk
    :: GitStoreData
    -> T2 BlockHeight MerkleLogHash
    -> BlockHeight
    -> HM.HashMap (T2 BlockHeight MerkleLogHash) (T2 TreeEntry BlobEntry)
    -> IO (HM.HashMap (T2 BlockHeight MerkleLogHash) (T2 TreeEntry BlobEntry))
cachedWalk gsd k@(T2 height hash) target cache
    | HM.member k cache = pure cache
    | otherwise = lookupTreeEntryByHash gsd hash height >>= \case
        Nothing -> throwGitStoreFailure $ "Lookup failure for block at given height " <> bhText height
        Just te -> withTreeObject gsd (_te_gitHash te) $ \gt -> do
            blob <- BlobEntry <$> unsafeReadTree 0 gt
            let !cache' = HM.insert k (T2 te blob) cache
            if | height == target -> pure cache'
               | otherwise -> do
                   prnt <- unsafeReadTree 1 gt  -- Fails for the genesis block, hence the `if`.
                   cachedWalk gsd (T2 (_te_blockHeight prnt) (_te_blockHash prnt)) target cache'

-- | Given a range of heights and a `TreeEntry` to start from, seek down its
-- branch to find the highest node which appears in the range.
--
seekHighest :: GitStore -> (BlockHeight, BlockHeight) -> TreeEntry -> IO (Maybe TreeEntry)
seekHighest gs r@(low, high) te
    | _te_blockHeight te < low = pure Nothing
    | _te_blockHeight te <= high = pure $ Just te
    | otherwise = do
        ltd <- lockGitStore gs (\gsd -> readLeafTree gsd (_te_gitHash te))
        seekHighest gs r . nearest $ _ltd_spectrum ltd
  where
    -- TODO `Map.lookupGT`, etc, are great for this kind of lookup. That said,
    -- spectra should never be long enough to make naive `O(n)` lookups
    -- prohibitive. Even an entry at height 1-billion has a spectrum of only
    -- length 51. Binary search on the Vector would also be good for this,
    -- although `vector-algorithms` didn't quite have the right incantation to
    -- be immediately useable.
    -- | The spectrum entry nearest in height to the target `BlockHeight`. Given:
    --
    -- @
    -- >>> getSpectrum 150
    -- [BlockHeight 32,BlockHeight 64,BlockHeight 128,BlockHeight 146,BlockHeight 147,BlockHeight 148]
    -- @
    --
    -- @nearest 100@ should yield the `TreeEntry` at height 128, and @nearest 5@
    -- should yield that at height 32.
    --
    nearest :: Vector TreeEntry -> TreeEntry
    nearest v = case V.findIndex (\i -> _te_blockHeight i >= high) v of
        Nothing -> error "Impossible case"
        Just ix -> V.unsafeIndex v ix

------------
-- INSERTION
------------

data InsertResult = Inserted | AlreadyExists deriving (Eq, Show)

insertBlock :: GitStore -> BlockHeader -> IO InsertResult
insertBlock gs bh = lockGitStore gs $ \store -> do
    let hash = getMerkleLogHash $ _blockHash bh
        height = fromIntegral $ _blockHeight bh
    m <- lookupTreeEntryByHash store hash height
    maybe (go store) (const $ pure AlreadyExists) m
  where
    go :: GitStoreData -> IO InsertResult
    go store = createLeafTree store bh $> Inserted

-- | Given a block header: lookup its parent leaf tree entry, write the block
-- header into the object database, compute a spectrum for the new tree entry,
-- write the @git_tree@ to the repository, tag it under @tags/bh/foo@, and
-- returns the git hash of the new @git_tree@ object.
createLeafTree :: GitStoreData -> BlockHeader -> IO GitHash
createLeafTree store@(GitStoreData repo _ _) bh = withTreeBuilder $ \treeB -> do
    when (height <= 0) $ throwGitStoreFailure "cannot insert genesis block"
    parentTreeEntry <- lookupTreeEntryByHash store parentHash (height - 1) >>=
                       maybe (throwGitStoreFailure "parent hash not found in DB") pure
    let parentTreeGitHash = _te_gitHash parentTreeEntry
    parentTreeData <- readLeafTree store parentTreeGitHash
    treeEntries <- traverse (\h -> lookupTreeEntryByHeight' store parentTreeGitHash h parentTreeData)
                        $ _spectrum spectrum
    newHeaderGitHash <- insertBlockHeaderIntoOdb store bh
    traverse_ (addTreeEntry treeB) treeEntries
    addTreeEntry treeB parentTreeEntry
    addSelfEntry treeB height hash newHeaderGitHash
    treeHash <- alloca $ \oid -> do
        throwOnGitError "createLeafTree" "git_treebuilder_write" $
            G.c'git_treebuilder_write oid repo treeB
        GitHash <$> oidToByteString oid
    createBlockHeaderTag store bh treeHash

    updateLeafTags store parentTreeEntry (TreeEntry height hash treeHash)

    -- TODO:
    --   - compute total difficulty weight vs the winning block, and atomic-replace
    --     the winning ref (e.g. @tags/BEST@) if the new block is better
    pure treeHash

  where
    height :: BlockHeight
    height = _blockHeight bh

    hash :: MerkleLogHash
    hash = getMerkleLogHash $ _blockHash bh

    parentHash :: MerkleLogHash
    parentHash = getMerkleLogHash $ _blockParent bh

    spectrum :: Spectrum
    spectrum = getSpectrum height

    addTreeEntry :: Ptr G.C'git_treebuilder -> TreeEntry -> IO ()
    addTreeEntry tb (TreeEntry h hs gh) = tbInsert tb G.c'GIT_FILEMODE_TREE h hs gh

addSelfEntry :: Ptr G.C'git_treebuilder -> BlockHeight -> MerkleLogHash -> GitHash -> IO ()
addSelfEntry tb h hs gh = tbInsert tb G.c'GIT_FILEMODE_BLOB h hs gh

-- | Insert a tree entry into a @git_treebuilder@.
--
tbInsert
    :: Ptr G.C'git_treebuilder
    -> G.C'git_filemode_t
    -> BlockHeight
    -> MerkleLogHash
    -> GitHash
    -> IO ()
tbInsert tb mode h hs gh =
    withOid gh $ \oid ->
    B.unsafeUseAsCString (terminate name) $ \cname ->
    throwOnGitError "tbInsert" "git_treebuilder_insert" $
        G.c'git_treebuilder_insert nullPtr tb cname oid mode
  where
    name :: NullTerminated
    name = mkTreeEntryNameWith "" h hs

insertBlockHeaderIntoOdb :: GitStoreData -> BlockHeader -> IO GitHash
insertBlockHeaderIntoOdb (GitStoreData _ odb _) bh =
    B.unsafeUseAsCStringLen serializedBlockHeader write
  where
    !serializedBlockHeader = runPutS $! encodeBlockHeader bh

    write :: (Ptr a, Int) -> IO GitHash
    write (cs, ln) = alloca $ \oidPtr -> do
       throwOnGitError "insertBlockHeaderIntoOdb" "git_odb_write" $
           G.c'git_odb_write oidPtr odb (castPtr cs)
                                        (fromIntegral ln)
                                        G.c'GIT_OBJ_BLOB
       GitHash <$> oidToByteString oidPtr

-- | Create a tag within @.git/refs/tags/bh/@ that matches the
-- @blockheight.blockhash@ syntax, as say found in a stored `BlockHeader`'s
-- "spectrum".
--
createBlockHeaderTag :: GitStoreData -> BlockHeader -> GitHash -> IO ()
createBlockHeaderTag gs@(GitStoreData repo _ _) bh leafHash =
    withObject gs leafHash $ \obj ->
    alloca $ \pTagOid ->
    B.unsafeUseAsCString (terminate tagName) $ \cstr ->
    -- @1@ forces libgit to overwrite this tag, should it already exist.
    throwOnGitError "createBlockHeaderTag" "git_tag_create_lightweight" $
        G.c'git_tag_create_lightweight pTagOid repo cstr obj 1
  where
    height :: BlockHeight
    height = _blockHeight bh

    hash :: MerkleLogHash
    hash = getMerkleLogHash $ _blockHash bh

    tagName :: NullTerminated
    tagName = mkTagName height hash

-- | The parent node upon which our new node was written is by definition no
-- longer a leaf, and thus its entry in @.git/refs/leaf/@ must be removed.
--
-- Unless, of course, this new node is part of a fork, and its parent is no
-- longer a leaf as far as the entire store is concerned.
--
updateLeafTags :: GitStoreData -> TreeEntry -> TreeEntry -> IO ()
updateLeafTags store@(GitStoreData repo _ _) oldLeaf newLeaf = do
    tagAsLeaf store newLeaf
    void . B.unsafeUseAsCString nulled $ \cstr ->
        -- throwOnGitError "updateLeafTags" "git_tag_delete" $
        G.c'git_tag_delete repo cstr  -- Ignores failure to work around a bug.
  where
    nulled :: ByteString
    nulled = terminate $ mkName oldLeaf

-- | Tag a `TreeEntry` in @.git/refs/leaf/@.
--
tagAsLeaf :: GitStoreData -> TreeEntry -> IO ()
tagAsLeaf store@(GitStoreData repo _ _) leaf =
    withObject store (_te_gitHash leaf) $ \obj ->
        alloca $ \pTagOid ->
        B.unsafeUseAsCString (terminate $ mkName leaf) $ \cstr ->
        throwOnGitError "tagAsLeaf" "git_tag_create_lightweight" $
            G.c'git_tag_create_lightweight pTagOid repo cstr obj 1

mkName :: TreeEntry -> NullTerminated
mkName (TreeEntry h bh _) = mkLeafTagName h bh

mkLeafTagName :: BlockHeight -> MerkleLogHash -> NullTerminated
mkLeafTagName = mkTreeEntryNameWith "leaf/"

-----------
-- BRACKETS
-----------

-- | Prevents other threads from manipulating the Git Store while we perform
-- some given action.
--
lockGitStore :: GitStore -> (GitStoreData -> IO a) -> IO a
lockGitStore (GitStore m) f = withMVar m f

-- | Bracket pattern around a `G.C'git_tree` struct.
--
withTreeObject
    :: GitStoreData
    -> GitHash
    -> (Ptr G.C'git_tree -> IO a)
    -> IO a
withTreeObject (GitStoreData repo _ _) gitHash f = bracket getTree G.c'git_tree_free f
  where
    getTree :: IO (Ptr G.C'git_tree)
    getTree = mask $ \restore -> alloca $ \ppTree -> withOid gitHash $ \oid -> do
        throwOnGitError "withTreeObject" "git_tree_lookup" $
            restore $ G.c'git_tree_lookup ppTree repo oid
        peek ppTree

withOid :: GitHash -> (Ptr G.C'git_oid -> IO a) -> IO a
withOid (GitHash strOid) f =
    B.unsafeUseAsCStringLen strOid $ \(cstr, clen) -> alloca $ \pOid -> do
        throwOnGitError "withOid" "git_oid_fromstrn" $
            G.c'git_oid_fromstrn pOid cstr (fromIntegral clen)
        f pOid

withObject :: GitStoreData -> GitHash -> (Ptr G.C'git_object -> IO a) -> IO a
withObject (GitStoreData repo _ _) hash f =
    withOid hash $ \oid ->
    alloca $ \pobj -> do
        throwOnGitError "withObject" "git_object_lookup" $
            G.c'git_object_lookup pobj repo oid G.c'GIT_OBJ_ANY
        peek pobj >>= f

withTreeBuilder :: (Ptr G.C'git_treebuilder -> IO a) -> IO a
withTreeBuilder f =
    alloca $ \pTB -> bracket_ (make pTB)
                              (peek pTB >>= G.c'git_treebuilder_free)
                              (peek pTB >>= f)
  where
    make :: Ptr (Ptr G.C'git_treebuilder) -> IO ()
    make p = throwOnGitError "withTreeBuilder" "git_treebuilder_create" $
        G.c'git_treebuilder_create p nullPtr

----------
-- FAILURE
----------

throwOnGitError :: Text -> Text -> IO CInt -> IO ()
throwOnGitError h c m = do
    code <- m
    when (code /= 0) $ throwGitError h c code

throwGitError :: Text -> Text -> CInt -> IO a
throwGitError h c e = throwIO $ GitFailure h c e

maybeTGitError :: IO CInt -> MaybeT IO ()
maybeTGitError m = do
    code <- liftIO m
    when (code /= 0) nothing

throwGitStoreFailure :: Text -> IO a
throwGitStoreFailure = throwIO . GitStoreFailure

--------
-- UTILS
--------

newtype Spectrum = Spectrum { _spectrum :: [BlockHeight] } deriving (Eq, Show)

getSpectrum :: BlockHeight -> Spectrum
getSpectrum (BlockHeight 0) = Spectrum []
getSpectrum (BlockHeight d0) =
    Spectrum . map (BlockHeight . fromIntegral) . dedup $ startSpec ++ rlgs ++ recents
  where
    d0' :: Int64
    d0' = fromIntegral d0

    numRecents = 4
    d = max 0 (d0' - numRecents)
    recents = [d .. (max 0 (d0'-2))]       -- don't include d0 or its parent

    pow2s = [ 1 `unsafeShiftL` x | x <- [5..63] ]

    (startSpec, lastSpec) = fs id 0 pow2s
    diff = d - lastSpec

    -- reverse log spectrum should be quantized on the lower bits
    quantize :: Int64 -> Int64
    quantize !x = let !out = (d - x) .&. complement (x-1) in out

    lgs = map quantize $ takeWhile (< diff) pow2s
    rlgs = reverse lgs

    fs :: ([Int64] -> [Int64]) -> Int64 -> [Int64] -> ([Int64], Int64)
    fs !dl !lst (x:zs) | x < d     = fs (dl . (x:)) x zs
                       | otherwise = (dl [], lst)
    fs !dl !lst [] = (dl [], lst)

dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup o@[_] = o
dedup (x:r@(y:_)) | x == y = dedup r
                  | otherwise = x : dedup r

-- TODO Update this example once a proper fixed-length encoding is chosen.
-- | Parse a git-object filename in the shape of:
--
-- @
-- 1023495.5e4fb6e0605385aee583035ae0db732e485715c8d26888d2a3571a26291fb58e
-- ^       ^
-- |       `-- base64-encoded block hash
-- `-- base64-encoded block height
-- @
parseLeafTreeFileName :: ByteString -> Maybe (BlockHeight, MerkleLogHash)
parseLeafTreeFileName fn = do
    height <- decodeHeight heightStr
    bh <- hush (first show . merkleLogHash =<< convertFromBase Base64URLUnpadded blockHash0)
    pure (height, bh)
  where
    -- TODO if the `rest` is fixed-length, it would be faster to use `splitAt`.
    (heightStr, rest) = B.break (== '.') fn
    blockHash0 = B.drop 1 rest

    decodeHeight :: ByteString -> Maybe BlockHeight
    decodeHeight = Just . BlockHeight . decodeHex

oidToByteString :: Ptr G.C'git_oid -> IO ByteString
oidToByteString pOid = bracket (G.c'git_oid_allocfmt pOid) free B.packCString

-- | Mysteriously missing from the main API of `BlockHash`.
--
getMerkleLogHash :: BlockHash -> MerkleLogHash
getMerkleLogHash (BlockHash bytes) = bytes

bhText :: BlockHeight -> Text
bhText (BlockHeight h) = T.pack $ show h

mkTagRef :: BlockHeight -> MerkleLogHash -> NullTerminated
mkTagRef height hash = nappend "tags/" (mkTagName height hash)

mkTagName :: BlockHeight -> MerkleLogHash -> NullTerminated
mkTagName = mkTreeEntryNameWith "bh/"

-- | Encode a `BlockHeight` and `MerkleLogHash` into the expected format,
-- append some decorator to the front (likely a section of a filepath), and
-- postpend a null-terminator.
--
mkTreeEntryNameWith :: ByteString -> BlockHeight -> MerkleLogHash -> NullTerminated
mkTreeEntryNameWith b (BlockHeight height) (MerkleLogHash hash) =
    NullTerminated [ b, encHeight, ".", encBH, "\0" ]
  where
    encBH :: ByteString
    encBH = convertToBase Base64URLUnpadded hash

    encHeight :: ByteString
    encHeight = FB.toStrictByteString $! FB.word64HexFixed height

decodeHex :: ByteString -> Word64
decodeHex = B.foldl' (\acc c -> (acc * 16) + fromIntegral (digitToInt c)) 0
-- {-# INLINE decodeHex #-}
