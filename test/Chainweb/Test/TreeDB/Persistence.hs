{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.TreeDB.Persistence
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.TreeDB.Persistence
( tests
) where

import Control.Monad (void)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

import Data.Align (padZipWith)

import qualified Streaming.Prelude as S

import System.Directory
import System.Path (fromAbsoluteFilePath)

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.Test.Utils (insertN, withToyDB, toyChainId)
import Chainweb.TreeDB
import Chainweb.TreeDB.Persist (fileEntries, persist)

---

tests :: TestTree
tests = testGroup "Persistence"
    [ testGroup "Encoding round-trips"
        [ testCase "Fresh TreeDb (only genesis)" onlyGenesis
        , testCase "Multiple Entries" manyBlocksWritten
        ]
    ]

-- | Persisting a freshly initialized `TreeDb` will successfully read and
-- write its only block, the genesis block.
--
onlyGenesis :: Assertion
onlyGenesis = withToyDB toyChainId $ \g db -> do
    persist p db
    g' <- runResourceT . S.head_ $ fileEntries @(ResourceT IO) p
    g' @?= Just g
    removeFile fp
  where
    fp = "/tmp/only-genesis"
    p = fromAbsoluteFilePath fp

-- | Write a number of block headers to a database, persist that database,
-- reread it, and compare. Guarantees:
--
--  * The DB and its persistence will have the same contents in the same order.
--  * The first block streamed from both the DB and the file will be the genesis.
--
manyBlocksWritten :: Assertion
manyBlocksWritten = withToyDB toyChainId $ \g db -> do
    void $ insertN len g db
    persist p db
    fromDB <- S.toList_ $ entries db Nothing Nothing Nothing Nothing
    fromFi <- runResourceT . S.toList_ $ fileEntries p
    length fromDB @?= len + 1
    length fromFi @?= len + 1
    head fromDB @?= g
    head fromFi @?= g
    let b = and $ padZipWith (==) fromDB fromFi
    assertBool "Couldn't write many blocks" b
    removeFile fp
  where
    fp = "/tmp/many-blocks-written"
    p = fromAbsoluteFilePath fp
    len = 10
