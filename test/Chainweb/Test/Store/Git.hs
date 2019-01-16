{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Store.Git ( tests ) where

import Data.Foldable (traverse_)
import qualified Data.Vector as V
import Data.Word (Word64)

import System.Path (Absolute, Path, fragment, (</>))
import System.Path.IO (getTemporaryDirectory)
import System.Random (randomIO)

import Text.Printf (printf)

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHash (BlockHashBytes(..))
import Chainweb.BlockHeader (BlockHeader(..), testBlockHeaders)
import Chainweb.ChainId
import Chainweb.Store.Git
import Chainweb.Store.Git.Internal
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
          , testCase "Single Insertion" $ withNewRepo singleInsert
          , testCase "Multiple Insertion" $ withNewRepo multiInsert
          , testCase "Genesis Reinsertion" $ withNewRepo genesisReinsertion
          ]
    , testGroup "Invariants"
          [ testCase "Final entry of spectrum is parent" $ withNewRepo parentIsLastInSpectrum
          ]
    , testGroup "Utilities"
          [ testCase "getSpectrum" $ getSpectrum 123 @?= [32, 64, 119, 120, 121]
          , testCase "getSpectrum" $ getSpectrum 1 @?= [0]
          , testCase "getSpectrum" $ getSpectrum 0 @?= []
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

genesisReinsertion :: GitStore -> Assertion
genesisReinsertion gs = insertBlock gs genesis >>= (@?= AlreadyExists)

singleInsert :: GitStore -> Assertion
singleInsert gs = do
    let next = head $ testBlockHeaders genesis
    r <- insertBlock gs next
    r @?= Inserted
    ls <- leaves gs
    length ls @?= 1
    head ls @?= next

multiInsert :: GitStore -> Assertion
multiInsert gs = do
    let nexts = take 100 $ testBlockHeaders genesis
    r <- traverse (insertBlock gs) nexts
    r @?= replicate 100 Inserted
    ls <- leaves gs
    length ls @?= 1
    head ls @?= last nexts

parentIsLastInSpectrum :: GitStore -> Assertion
parentIsLastInSpectrum gs = do
    let nexts = take 100 $ testBlockHeaders genesis
    traverse_ (insertBlock gs) nexts
    lockGitStore gs $ \gsd -> do
        te@(TreeEntry _ _ gh) <- head <$> leaves' gsd
        ltd <- readLeafTree gsd gh
        bh <- readHeader gsd te
        let parent = V.last $ _ltd_spectrum ltd
        _te_blockHash parent @?= (getBlockHashBytes $ _blockParent bh)
