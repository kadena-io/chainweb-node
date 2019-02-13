{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , GitStoreBlockHeader(..)
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

  -- * Maintenance
  , prune
  ) where

import qualified Bindings.Libgit2 as G

import Control.Concurrent.MVar
import Control.Exception (bracket, bracketOnError, finally, mask)
import Control.Monad (void)

import Data.Bits ((.|.))
import Data.Bool (bool)
import Data.Functor (($>))
import qualified Data.Text as T

import Foreign.C.String (withCString)
import Foreign.C.Types (CInt, CUInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)

import Shelly (cd, fromText, run_, shelly)

import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)
import System.Path (toFilePath)

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), BlockHeight(..))
import Chainweb.Store.Git.Internal

---

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
withGitStore gsc@(GitStoreConfig root0 g) f = withLibGit $ bracket open close f
  where
    root :: String
    root = toFilePath root0

    open :: IO GitStore
    open = mask $ \restore ->
        bracketOnError (openRepo restore) G.c'git_repository_free $ \repo -> do
            odb <- openOdb restore repo
            let gsd = GitStoreData repo odb gsc
            insertGenesisBlock g gsd  -- INVARIANT: Should be a no-op for an existing repo.
            GitStore <$> newMVar gsd

    close :: GitStore -> IO ()
    close m = lockGitStore m $ \(GitStoreData p o _) ->
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
-- TODO Manually delete tags in @leaf/@ that this process clears of the header
-- entries of!
-- | Runs the following across a given Git Store:
--
-- @
-- git repack -A
-- git gc
-- @
--
-- This has the effect of drastically reducing the size of the Git repository on
-- disk (~95% reduction from a completely unpacked state). Traversals / lookups
-- also generally get faster.
--
-- *Note:* This operation locks the Git Store.
--
prune :: GitStore -> IO ()
prune gs = lockGitStore gs $ \(GitStoreData _ _ (GitStoreConfig p _)) ->
    shelly $ do
        cd . fromText . T.pack $ toFilePath p
        run_ "git" ["repack", "-A"]
        run_ "git" ["gc"]


------------------------------------------------------------------------------
_isSorted :: Ord a => [a] -> Bool
_isSorted [] = True
_isSorted [_] = True
_isSorted (x:z@(y:_)) = x < y && _isSorted z

_prop_spectra_sorted :: Bool
_prop_spectra_sorted = all (_isSorted . _spectrum) $ map getSpectrum [1,10000000 :: BlockHeight]


------------------------------------------------------------------------------
-- | A simplified version of `createLeafTree`, specialized for the Genesis
-- Block.
--
insertGenesisBlock :: BlockHeader -> GitStoreData -> IO ()
insertGenesisBlock g store@(GitStoreData repo _ _) = withTreeBuilder $ \treeB -> do
    newHeaderGitHash <- insertBlockHeaderIntoOdb store g
    addSelfEntry treeB (_blockHeight g) (getMerkleLogHash $ _blockHash g) newHeaderGitHash
    treeHash <- alloca $ \oid -> do
        throwOnGitError "insertGenesisBlock" "git_treebuilder_write" $
            G.c'git_treebuilder_write oid repo treeB
        GitHash <$> oidToByteString oid
    -- Store a tag in @.git/refs/tags/bh/@
    createBlockHeaderTag store g treeHash
    -- Mark this entry (it's the only entry!) as a "leaf" in @.git/refs/tags/leaf/@.
    tagAsLeaf store (TreeEntry 0 (getMerkleLogHash $ _blockHash g) treeHash)
