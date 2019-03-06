{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main ( main ) where

import Control.Monad (void)

import Criterion.Main

import Data.Foldable (traverse_)
import qualified Data.HashSet as HS

import qualified Streaming.Prelude as P

import System.IO (hFlush, stdout)

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), testBlockHeaders)
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Store.Git
import Chainweb.Store.Git.Internal (leaves', lockGitStore)
import Chainweb.TreeDB
import Chainweb.Utils (withTempDir)
import Chainweb.Version (ChainwebVersion(..))

---

type Env = (BlockHeader, BlockHeader, BlockHeader)

main :: IO ()
main = withTempDir "benchmarks" $ \tmp -> do
    let !gsc = GitStoreConfig tmp genesis
    withGitStore gsc $ \gs ->
        defaultMain $ [ gitMain gs ]

gitMain :: GitStore -> Benchmark
gitMain gs = env (populate gs) (gitSuite gs)

populate :: GitStore -> IO Env
populate gs = do
    ------------------------------------
    -- fake (but legal) chain generation
    ------------------------------------
    let !chainSize = 1024
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
    prune gs
    -----------------------
    -- form the environment
    -----------------------
    pure $! (genesis, middle, last nexts)

-- | The lazy pattern match on the tuple is a necessary pattern of Criterion.
--
gitSuite :: GitStore -> Env -> Benchmark
gitSuite gs ~(frt, mid, lst) =
    bgroup "Git Store"
    [ -- bench "walk' (just TreeEntry)" $ nfIO (walkB' gs lst)
      bench "walk (decoded BlockHeaders)" $ nfIO (walkB gs lst)
    , bench "entries (decoded BlockHeaders)" $ nfIO (stream gs)
    , bench "branchEntries" $ nfIO (branches gs lst)
    , bench "leaves'" $ nfIO (lockGitStore gs leaves')
    , bench "leaves" $ nfIO (leaves gs)
    , bench "leafEntries" $ nfIO (leavesS gs)
    , bench "root" $ nfIO (root gs)
    , bench "maxHeader" $ nfIO (maxHeader gs)
    , bench "lookup (genesis)" $ nfIO (lookupByBlockHash gs (_blockHeight frt) (_blockHash frt))
    , bench "lookup (middle)" $ nfIO (lookupByBlockHash gs (_blockHeight mid) (_blockHash mid))
    , bench "lookup (end)" $ nfIO (lookupByBlockHash gs (_blockHeight lst) (_blockHash lst))
    ]

walkB :: GitStore -> BlockHeader -> IO ()
walkB gs leaf = walk gs (_blockHeight leaf) (_blockHash leaf) (const $ pure ())

-- walkB' :: GitStore -> BlockHeader -> IO ()
-- walkB' gs leaf = lockGitStore gs $ \gsd -> do
--     walk' gsd (_blockHeight leaf)
--               (getBlockHashBytes $ _blockHash leaf)
--               0
--               (const $ pure ())
--               (const $ pure ())

stream :: GitStore -> IO ()
stream gs = void . P.length_ $ entries gs Nothing Nothing Nothing Nothing

leavesS :: GitStore -> IO ()
leavesS gs = void . P.effects $ leafEntries gs Nothing Nothing Nothing Nothing

branches :: GitStore -> BlockHeader -> IO ()
branches gs leaf =
    void . P.effects $ branchEntries gs Nothing Nothing Nothing Nothing lows ups
  where
    lows = HS.singleton . LowerBound . key $ GitStoreBlockHeader genesis
    ups  = HS.singleton . UpperBound . key $ GitStoreBlockHeader leaf

------------
-- UTILITIES
------------

-- Borrowed from Chainweb.Test.Utils

genesis :: BlockHeader
genesis = toyGenesis chainId0

toyGenesis :: ChainId -> BlockHeader
toyGenesis cid = genesisBlockHeader (Test singletonChainGraph) cid

chainId0 :: ChainId
chainId0 = testChainId 0
