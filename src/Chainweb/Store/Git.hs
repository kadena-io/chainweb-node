{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: Chainweb.Store.Git
-- Copyright: Copyright © 2018 - 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Gregory Collins <greg@kadena.io>, Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Git-based `BlockHeader` storage.

module Chainweb.Store.Git
  ( -- * Types
    GitStoreConfig(..)
  , GitStore
  , GitFailure(..)

  -- * Initialization
  , withGitStore

  -- * Insertion
  , InsertResult(..)
  , insertBlock

  -- * Lookup
  , lookupByBlockHash
  , leaves
  , walk
  ) where

import qualified Bindings.Libgit2 as G

import Control.Concurrent.MVar
import Control.Monad (void, when, (>=>))

import Data.Bits ((.|.))
import Data.Bool (bool)
import Data.Bytes.Put (runPutS)
import qualified Data.ByteString.Unsafe as B
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Vector (Vector)
import qualified Data.Vector as V

import Foreign.C.String (withCString)
import Foreign.C.Types (CInt, CUInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)

import Prelude hiding (lookup)

import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)
import System.Path (Absolute, Path, toFilePath)

import UnliftIO.Exception (bracket, bracketOnError, finally, mask)

-- internal modules

import Chainweb.BlockHash (BlockHash(..), BlockHashBytes)
import Chainweb.BlockHeader
    (BlockHeader(..), BlockHeight(..), encodeBlockHeader)
import Chainweb.Store.Git.Internal

---

-- | For opening an existing repository, or for initializing a new one.
--
data GitStoreConfig = GitStoreConfig {
    _gsc_path :: !(Path Absolute)
  , _gsc_genesis :: !BlockHeader
}

-- | The ancient Haskell monks of Mt. Lambda warned me of its power... but I
-- have trained. I have studied. I have reflected on the human condition and my
-- role in the world. I am ready. I dare outstretch my hand and seize The Power
-- for myself - a beautiful global mutable variable.
--
libgitRefCount :: MVar Int
libgitRefCount = unsafePerformIO (newMVar 0)
{-# NOINLINE libgitRefCount #-}

-- | A construction similar to `G.withLibGitDo` from @hlibgit2@, except that the
-- underlying C threads are only initialized when no Haskell threads have
-- reported using this function yet. Likewise, the last Haskell thread to report
-- needing this function will make the appropriate cleanup calls.
--
withLibGit :: IO a -> IO a
withLibGit f = do
    modifyMVar_ libgitRefCount $ \case
        n | n < 0 -> throwGitStoreFailure "Negative ref count to libgit2 bindings!"
          | n == 0 ->
                1 <$ throwOnGitError "withLibGit" "git_threads_init" G.c'git_threads_init
          | otherwise -> pure $! n + 1
    r <- f
    modifyMVar_ libgitRefCount $ \case
        n | n < 1  -> throwGitStoreFailure "More libgit refs were released than were obtained!"
          | n == 1 -> performGC >> G.c'git_threads_shutdown $> 0
          | otherwise -> pure $! n - 1
    pure r

-- | A bracket-pattern that gives access to a `GitStore`, the fundamental
-- git-based storage type. Given a `GitStoreConfig`, this function first assumes
-- an existing repository, and tries to open it. If no such repository exists,
-- it will create a fresh one with the provided genesis `BlockHeader`.
--
-- Underlying pointers to libgit2 are freed automatically.
--
withGitStore :: GitStoreConfig -> (GitStore -> IO a) -> IO a
withGitStore (GitStoreConfig root0 g) f = withLibGit $ bracket open close f
  where
    root :: String
    root = toFilePath root0

    open :: IO GitStore
    open = mask $ \restore ->
        bracketOnError (openRepo restore) G.c'git_repository_free $ \repo -> do
            odb <- openOdb restore repo
            let gsd = GitStoreData repo odb
            insertGenesisBlock g gsd  -- INVARIANT: Should be a no-op for an existing repo.
            GitStore <$> newMVar gsd

    close :: GitStore -> IO ()
    close m = lockGitStore m $ \(GitStoreData p o) ->
        G.c'git_odb_free o `finally` G.c'git_repository_free p

    openRepo :: (IO CInt -> IO CInt) -> IO (Ptr G.C'git_repository)
    openRepo restore = tryOpen restore >>= maybe (initBare restore) pure

    -- | Attempt to open a supposedly existing git store, and fail gracefully if
    -- it doesn't.
    --
    tryOpen :: (IO CInt -> IO CInt) -> IO (Maybe (Ptr G.C'git_repository))
    tryOpen restore = withCString root $ \rootPtr -> alloca $ \repoPtr -> do
        res <- restore $ G.c'git_repository_open_ext repoPtr rootPtr openFlags rootPtr
        bool (pure Nothing) (Just <$> peek repoPtr) $ res == 0

    -- | Initialize an empty git store.
    --
    initBare :: (IO CInt -> IO CInt) -> IO (Ptr G.C'git_repository)
    initBare restore = withCString root $ \rootPtr -> alloca $ \repoPtr -> do
        throwOnGitError "initBare" "git_repository_init" . restore $
            G.c'git_repository_init repoPtr rootPtr 1
        peek repoPtr

    openOdb :: (IO () -> IO a) -> Ptr G.C'git_repository -> IO (Ptr G.C'git_odb)
    openOdb restore repo = alloca $ \podb -> do
        void . restore . throwOnGitError "openOdb" "git_repository_odb" $
            G.c'git_repository_odb podb repo
        peek podb

    -- not sure why this flag is not in gitlab2 bindings
    _FLAG_OPEN_BARE :: CUInt
    _FLAG_OPEN_BARE = 4

    openFlags :: CUInt
    openFlags = G.c'GIT_REPOSITORY_OPEN_NO_SEARCH .|. _FLAG_OPEN_BARE


------------------------------------------------------------------------------
data InsertResult = Inserted | AlreadyExists deriving (Eq, Show)

insertBlock :: GitStore -> BlockHeader -> IO InsertResult
insertBlock gs bh = lockGitStore gs $ \store -> do
    let hash = getBlockHashBytes $ _blockHash bh
        height = fromIntegral $ _blockHeight bh
    m <- lookupTreeEntryByHash store hash height
    maybe (go store) (const $ pure AlreadyExists) m
  where
    go :: GitStoreData -> IO InsertResult
    go store = createLeafTree store bh $> Inserted


------------------------------------------------------------------------------
lookupByBlockHash :: GitStore -> BlockHeight -> BlockHash -> IO (Maybe BlockHeader)
lookupByBlockHash gs height bh = lockGitStore gs $ \store -> do
    m <- lookupTreeEntryByHash store (getBlockHashBytes bh) (fromIntegral height)
    traverse (readHeader store) m
  where


------------------------------------------------------------------------------
insertBlockHeaderIntoOdb :: GitStoreData -> BlockHeader -> IO GitHash
insertBlockHeaderIntoOdb (GitStoreData _ odb) bh =
    B.unsafeUseAsCStringLen serializedBlockHeader write
  where
    !serializedBlockHeader = runPutS $! encodeBlockHeader bh

    write :: (Ptr a, Int) -> IO GitHash
    write (cs, len) = alloca $ \oidPtr -> do
       throwOnGitError "insertBlockHeaderIntoOdb" "git_odb_write" $
           G.c'git_odb_write oidPtr odb (castPtr cs)
                                        (fromIntegral len)
                                        G.c'GIT_OBJ_BLOB
       GitHash <$> oidToByteString oidPtr


------------------------------------------------------------------------------
-- | Insert a tree entry into a @git_treebuilder@.
--
tbInsert
    :: Ptr G.C'git_treebuilder
    -> G.C'git_filemode_t
    -> BlockHeight
    -> BlockHashBytes
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


------------------------------------------------------------------------------
-- | Given a block header: lookup its parent leaf tree entry, write the block
-- header into the object database, compute a spectrum for the new tree entry,
-- write the @git_tree@ to the repository, tag it under @tags/bh/foo@, and
-- returns the git hash of the new @git_tree@ object.
createLeafTree :: GitStoreData -> BlockHeader -> IO GitHash
createLeafTree store@(GitStoreData repo _) bh = withTreeBuilder $ \treeB -> do
    when (height <= 0) $ throwGitStoreFailure "cannot insert genesis block"
    parentTreeEntry <- lookupTreeEntryByHash store parentHash (height - 1) >>=
                       maybe (throwGitStoreFailure "parent hash not found in DB") pure
    let parentTreeGitHash = _te_gitHash parentTreeEntry
    parentTreeData <- readLeafTree store parentTreeGitHash
    treeEntries <- traverse (\h -> lookupTreeEntryByHeight' store parentTreeGitHash h parentTreeData)
                        spectrum
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

    hash :: BlockHashBytes
    hash = getBlockHashBytes $ _blockHash bh

    parentHash :: BlockHashBytes
    parentHash = getBlockHashBytes $ _blockParent bh

    spectrum :: [BlockHeight]
    spectrum = getSpectrum height

    addTreeEntry :: Ptr G.C'git_treebuilder -> TreeEntry -> IO ()
    addTreeEntry tb (TreeEntry h hs gh) = tbInsert tb G.c'GIT_FILEMODE_TREE h hs gh

addSelfEntry :: Ptr G.C'git_treebuilder -> BlockHeight -> BlockHashBytes -> GitHash -> IO ()
addSelfEntry tb h hs gh = tbInsert tb G.c'GIT_FILEMODE_BLOB h hs gh


------------------------------------------------------------------------------
-- | Create a tag within @.git/refs/tags/bh/@ that matches the
-- @blockheight.blockhash@ syntax, as say found in a stored `BlockHeader`'s
-- "spectrum".
--
createBlockHeaderTag :: GitStoreData -> BlockHeader -> GitHash -> IO ()
createBlockHeaderTag gs@(GitStoreData repo _) bh leafHash =
    withObject gs leafHash $ \obj ->
    alloca $ \pTagOid ->
    B.unsafeUseAsCString (terminate tagName) $ \cstr ->
    -- @1@ forces libgit to overwrite this tag, should it already exist.
    throwOnGitError "createBlockHeaderTag" "git_tag_create_lightweight" $
        G.c'git_tag_create_lightweight pTagOid repo cstr obj 1
  where
    height :: BlockHeight
    height = _blockHeight bh

    hash :: BlockHashBytes
    hash = getBlockHashBytes $ _blockHash bh

    tagName :: NullTerminated
    tagName = mkTagName height hash


------------------------------------------------------------------------------
{-
withReference :: GitStoreData
              -> ByteString
              -> (Ptr G.C'git_reference -> IO a)    -- ^ ptr may be null
              -> IO a
withReference (GitStoreData repo _) path0 f = bracket lookup destroy f
  where
    path :: ByteString
    path = B.append "refs/" path0

    destroy :: Ptr G.C'git_reference -> IO ()
    destroy p = when (p /= nullPtr) $ G.c'git_reference_free p

    lookup :: IO (Ptr G.C'git_reference)
    lookup = mask $ \restore ->
             alloca $ \pRef ->
             B.unsafeUseAsCString path $ \cstr -> do
        code <- restore $ G.c'git_reference_lookup pRef repo cstr
        if | code == G.c'GIT_ENOTFOUND -> pure nullPtr
           | code /= 0 -> throwGitError code
           | otherwise -> peek pRef
-}

------------------------------------------------------------------------------
-- | The parent node upon which our new node was written is by definition no
-- longer a leaf, and thus its entry in @.git/refs/leaf/@ must be removed.
--
updateLeafTags :: GitStoreData -> TreeEntry -> TreeEntry -> IO ()
updateLeafTags store@(GitStoreData repo _) oldLeaf newLeaf = do
    tagAsLeaf store newLeaf
    B.unsafeUseAsCString (terminate $ mkName oldLeaf) $ \cstr ->
        throwOnGitError "updateLeafTags" "git_tag_delete" $
            G.c'git_tag_delete repo cstr

-- | Tag a `TreeEntry` in @.git/refs/leaf/@.
--
tagAsLeaf :: GitStoreData -> TreeEntry -> IO ()
tagAsLeaf store@(GitStoreData repo _) leaf =
    withObject store (_te_gitHash leaf) $ \obj ->
        alloca $ \pTagOid ->
        B.unsafeUseAsCString (terminate $ mkName leaf) $ \cstr ->
        throwOnGitError "tagAsLeaf" "git_tag_create_lightweight" $
            G.c'git_tag_create_lightweight pTagOid repo cstr obj 1

mkName :: TreeEntry -> NullTerminated
mkName (TreeEntry h bh _) = mkLeafTagName h bh

mkLeafTagName :: BlockHeight -> BlockHashBytes -> NullTerminated
mkLeafTagName = mkTreeEntryNameWith "leaf/"


------------------------------------------------------------------------------
lookupTreeEntryByHeight
    :: GitStoreData
    -> GitHash         -- ^ starting from this leaf tree
    -> BlockHeight     -- ^ desired blockheight
    -> IO TreeEntry
lookupTreeEntryByHeight gs leafTreeHash height =
    readLeafTree gs leafTreeHash >>=
    lookupTreeEntryByHeight' gs leafTreeHash height


------------------------------------------------------------------------------
lookupTreeEntryByHeight'
    :: GitStoreData
    -> GitHash
    -> BlockHeight     -- ^ desired blockheight
    -> LeafTreeData
    -> IO TreeEntry
lookupTreeEntryByHeight' gs leafTreeHash height (LeafTreeData (TreeEntry leafHeight leafBH _) spectrum)
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

------------------------------------------------------------------------------
_isSorted :: Ord a => [a] -> Bool
_isSorted [] = True
_isSorted [_] = True
_isSorted (x:z@(y:_)) = x < y && _isSorted z


_prop_spectra_sorted :: Bool
_prop_spectra_sorted = all _isSorted $ map getSpectrum [1,10000000 :: BlockHeight]


------------------------------------------------------------------------------
-- | A simplified version of `createLeafTree`, specialized for the Genesis
-- Block.
--
insertGenesisBlock :: BlockHeader -> GitStoreData -> IO ()
insertGenesisBlock g store@(GitStoreData repo _) = withTreeBuilder $ \treeB -> do
    newHeaderGitHash <- insertBlockHeaderIntoOdb store g
    addSelfEntry treeB (_blockHeight g) (getBlockHashBytes $ _blockHash g) newHeaderGitHash
    treeHash <- alloca $ \oid -> do
        throwOnGitError "insertGenesisBlock" "git_treebuilder_write" $
            G.c'git_treebuilder_write oid repo treeB
        GitHash <$> oidToByteString oid
    -- Store a tag in @.git/refs/tags/bh/@
    createBlockHeaderTag store g treeHash
    -- Mark this entry (it's the only entry!) as a "leaf" in @.git/refs/tags/leaf/@.
    tagAsLeaf store (TreeEntry 0 (getBlockHashBytes $ _blockHash g) treeHash)

------------------------------------------------------------------------------
-- | The "leaves" - the tips of all branches.
--
leaves :: GitStore -> IO [BlockHeader]
leaves gs = lockGitStore gs $ \gsd -> leaves' gsd >>= traverse (readHeader gsd)

-- | Starting from a node indicated by a given `BlockHeight` and `BlockHash`,
-- traverse the tree from the node to the root, applying some function to each
-- associated `BlockHeader` along the way.
--
walk :: GitStore -> BlockHeight -> BlockHash -> (BlockHeader -> IO ()) -> IO ()
walk gs height (BlockHash _ bhb) f = lockGitStore gs $ \gsd -> do
    let f' :: LeafTreeData -> IO ()
        f' = readHeader' gsd >=> f
    walk' gsd height bhb (const $ pure ()) f'
