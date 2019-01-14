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

import Chainweb.BlockHash (BlockHashBytes(..))
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.ChainId
import Chainweb.Store.Git
import Chainweb.Test.Utils (toyGenesis)

---

chainId0 :: ChainId
chainId0 = testChainId 0

genesis :: BlockHeader
genesis = toyGenesis chainId0

tests :: TestTree
tests = testGroup "Git Store"
    [ testGroup "Basic Usage"
          [ testCase "New Repository" $ withNewRepo legalGenesis
          -- , testCase "Single Insertion" _
          -- , testCase "Repeated Insertion" _
          ]
    , testGroup "Utilities"
          [ testCase "getSpectrum" $ getSpectrum 123 @?= [32, 64, 119, 120, 121]
          , testCase "parseLeafTreeFileName" leafTreeParsing
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
withNewRepo f = tempPath >>= \tmp -> withGitStore (GitStoreConfig tmp genesis) f

legalGenesis :: GitStore -> Assertion
legalGenesis gs = do
    g' <- lookupByBlockHash gs (_blockHeight genesis) (_blockHash genesis)
    g' @?= Just genesis
    ls <- leaves gs
    length ls @?= 1
    head ls @?= genesis

leafTreeParsing :: Assertion
leafTreeParsing = parseLeafTreeFileName fn @?= Just (0, BlockHashBytes bs)
  where
    fn = "AAAAAAAAAAA=.7C1XaR2bLUAYKsVlAyBUt9eEupaxi8tb4LtOcOP7BB4="
    bs = "\236-Wi\GS\155-@\CAN*\197e\ETX T\183\215\132\186\150\177\139\203[\224\187Np\227\251\EOT\RS"
