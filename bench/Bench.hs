{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main ( main ) where

import Criterion.Main

import Data.Foldable (traverse_)
import qualified Data.Text as T
import Data.Word (Word64)

import Shelly hiding ((</>))

import System.IO (hFlush, stdout)
import System.Path (Absolute, Path, fragment, toFilePath, (</>))
import System.Path.IO (getTemporaryDirectory)
import System.Random (randomIO)

import Text.Printf (printf)

-- internal modules

import Chainweb.BlockHeader
    (BlockHeader(..), genesisBlockHeader, testBlockHeaders)
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Graph (ChainGraph, toChainGraph)
import Chainweb.Store.Git
    (GitStore, GitStoreConfig(..), insertBlock, leaves, lookupByBlockHash,
    walk, withGitStore)
import Chainweb.Store.Git.Internal
    (getBlockHashBytes, leaves', lockGitStore, walk')
import Chainweb.Utils (int)
import Chainweb.Version (ChainwebVersion(..))

import qualified Data.DiGraph as G

---

type Env = (BlockHeader, BlockHeader, BlockHeader)

-- A prepopulated repository fo ~53k block headers.
-- packed53k :: Path Absolute
-- packed53k =
--     fromAbsoluteFilePath "/home/colin/code/haskell/chainweb/chainweb-git-store-test-7226612870463109362"

main :: IO ()
main = do
    tmp <- tempPath
    let !gsc = GitStoreConfig tmp genesis
    withGitStore gsc $ \gs -> do
        defaultMain $ [ gitMain tmp gs ]

gitMain :: Path Absolute -> GitStore -> Benchmark
gitMain tmp gs = env (populate tmp gs) (gitSuite gs)

populate :: Path Absolute -> GitStore -> IO Env
populate tmp gs = do
    ------------------------------------
    -- fake (but legal) chain generation
    ------------------------------------
    let !chainSize = 534
        !nexts = take chainSize $ testBlockHeaders genesis
        !middle = nexts !! (chainSize `div` 2)
    ------------------
    -- block insertion
    ------------------
    putStrLn "INSERTING BLOCKS..." >> hFlush stdout
    traverse_ (insertBlock gs) nexts
    putStrLn "INSERT COMPLETE" >> hFlush stdout
    --------------
    -- git packing
    --------------
    shelly $ do
        let pth = T.pack $ toFilePath tmp
        echo $ "PACKING: " <> pth
        cd $ fromText pth
        run_ "git" ["repack", "-A"]
        echo "PACKING COMPLETE. GCing..."
        run_ "git" ["gc"]
        echo "GCing COMPLETE"
    -----------------------
    -- form the environment
    -----------------------
    pure $! (genesis, middle, last nexts)

gitSuite :: GitStore -> Env -> Benchmark
gitSuite gs ~(frt, mid, lst) =
    bgroup "Git Store"
    [ bench "walk' (just TreeEntry)" $ nfIO (walkB' gs lst)
    , bench "walk (decoded BlockHeaders)" $ nfIO (walkB gs lst)
    , bench "leaves'" $ nfIO (lockGitStore gs leaves')
    , bench "leaves" $ nfIO (leaves gs)
    , bench "lookup (genesis)" $ nfIO (lookupByBlockHash gs (_blockHeight frt) (_blockHash frt))
    , bench "lookup (middle)" $ nfIO (lookupByBlockHash gs (_blockHeight mid) (_blockHash mid))
    , bench "lookup (end)" $ nfIO (lookupByBlockHash gs (_blockHeight lst) (_blockHash lst))
    ]

walkB :: GitStore -> BlockHeader -> IO ()
walkB gs leaf = walk gs (_blockHeight leaf) (_blockHash leaf) (const $ pure ())

walkB' :: GitStore -> BlockHeader -> IO ()
walkB' gs leaf = lockGitStore gs $ \gsd -> do
    walk' gsd (_blockHeight leaf) (getBlockHashBytes $ _blockHash leaf) (const $ pure ()) (const $ pure ())

------------
-- UTILITIES
------------

-- Borrowed from Chainweb.Test.Utils

genesis :: BlockHeader
genesis = toyGenesis chainId0

toyGenesis :: ChainId -> BlockHeader
toyGenesis cid = genesisBlockHeader Test (toChainGraph (const cid) singleton) cid

chainId0 :: ChainId
chainId0 = testChainId 0

singleton :: ChainGraph
singleton = toChainGraph (testChainId . int) G.singleton

-- | Some random path under @/tmp@.
--
-- @
-- Path "/tmp/chainweb-git-store-test-8086816238120523704"
-- @
--
tempPath :: IO (Path Absolute)
tempPath = do
    tmp <- getTemporaryDirectory
    suff <- randomIO @Word64
    pure $ tmp </> fragment (printf "chainweb-git-store-test-%d" suff)
