{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Store.Git ( tests ) where

import Data.Word (Word64)

import System.Path (Absolute, Path, fragment, (</>))
import System.Path.IO (getTemporaryDirectory)
import System.Random (randomIO)

import Text.Printf (printf)

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.ChainId
import Chainweb.Store.Git
    (GitStore, GitStoreConfig(..), getSpectrum, withGitStore)
import Chainweb.Test.Utils (toyGenesis)

---

chainId0 :: ChainId
chainId0 = testChainId 0

tests :: TestTree
tests = testGroup "Git Store"
    [ testGroup "Basic Usage"
          [ testCase "New Repository" $ withNewRepo (const $ pure ())
          -- , testCase "Single Insertion" _
          -- , testCase "Repeated Insertion" _
          ]
    , testGroup "Utilities"
          [ testCase "getSpectrum" $ getSpectrum 123 @?= [32, 64, 119, 120, 121]
          ]
    -- TODO Eventually the TreeDb invariant tests will need to be called here.
    ]

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

-- | Initialize a fresh Git store and perform some action over it.
--
withNewRepo :: (GitStore -> IO a) -> IO a
withNewRepo f = tempPath >>= \tmp -> withGitStore (NewStore tmp $ toyGenesis chainId0) f
