{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Store.Git ( tests ) where

import Data.Foldable (traverse_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), testBlockHeaders)
import Chainweb.ChainId
import Chainweb.Store.Git
import Chainweb.Store.Git.Internal
import Chainweb.Test.TreeDB
import Chainweb.Test.Utils (toyGenesis)
import Chainweb.Utils (withTempDir)

---

chainId0 :: ChainId
chainId0 = testChainId 0

genesis :: BlockHeader
genesis = toyGenesis chainId0

chainLen :: Int
chainLen = 100

tests :: TestTree
tests = testGroup "Git Store"
    [ testGroup "Basic Usage"
          [ testCase "New Repository" $ withNewRepo legalGenesis
          , testCase "Single Insertion" $ withNewRepo singleInsert
          , testCase "Multiple Insertion" $ withNewRepo multiInsert
          , testCase "Genesis Reinsertion" $ withNewRepo genesisReinsertion
          ]
    , testGroup "Traversal"
          [ testCase "Leaf-to-Genesis" $ withNewRepo basicTrav
          , testCase "seek" $ withNewRepo seeking
          ]
    , testGroup "Invariants"
          [ testCase "Final entry of spectrum is parent" $ withNewRepo parentIsLastInSpectrum
          , testCase "Second-last entry of raw git_tree is parent" $ withNewRepo parentIsSecondLastEntry
          , testCase "Genesis has an empty spectrum Vector" $ withNewRepo genesisEmptySpectrum
          ]
    , testGroup "Utilities"
          [ testCase "getSpectrum" $ getSpectrum 123 @?= Spectrum [32, 64, 119, 120, 121]
          , testCase "getSpectrum" $ getSpectrum 1 @?= Spectrum [0]
          , testCase "getSpectrum" $ getSpectrum 0 @?= Spectrum []
          -- , testCase "parseLeafTreeFileName" leafTreeParsing
          -- , testCase "letsSee" letsSee
          ]
    , treeDbInvariants withNewRepo' Parallel
    ]

-- | Initialize a fresh Git store and perform some action over it.
--
withNewRepo :: (GitStore -> IO a) -> IO a
withNewRepo  = withNewRepo' (GitStoreBlockHeader genesis)

withNewRepo' :: GitStoreBlockHeader -> (GitStore -> IO a) -> IO a
withNewRepo' (GitStoreBlockHeader bh) f = withTempDir "git-store-test"$ \tmp ->
    withGitStore (GitStoreConfig tmp bh) f

legalGenesis :: GitStore -> Assertion
legalGenesis gs = do
    g' <- lookupByBlockHash gs (_blockHeight genesis) (_blockHash genesis)
    g' @?= Just genesis
    ls <- leaves gs
    length ls @?= 1
    head ls @?= genesis

-- TODO Reinstate. The following assumes base64 encoding, which we actually
-- can't use, since it doesn't preserve lexical ordering.
--
-- leafTreeParsing :: Assertion
-- leafTreeParsing = parseLeafTreeFileName fn @?= Just (0, BlockHashBytes bs)
--   where
--     fn = "AAAAAAAAAAA=.7C1XaR2bLUAYKsVlAyBUt9eEupaxi8tb4LtOcOP7BB4="
--     bs = "\236-Wi\GS\155-@\CAN*\197e\ETX T\183\215\132\186\150\177\139\203[\224\187Np\227\251\EOT\RS"

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
    let nexts = take chainLen $ testBlockHeaders genesis
    r <- traverse (insertBlock gs) nexts
    r @?= replicate chainLen Inserted
    ls <- leaves gs
    length ls @?= 1
    head ls @?= last nexts

-- | INVARIANT: The last `TreeEntry` in a node's spectrum should refer to its
-- direct parent.
--
parentIsLastInSpectrum :: GitStore -> Assertion
parentIsLastInSpectrum gs = do
    let nexts = take chainLen $ testBlockHeaders genesis
    traverse_ (insertBlock gs) nexts
    lockGitStore gs $ \gsd -> do
        te@(TreeEntry _ _ gh) <- head <$> leaves' gsd
        bh <- readHeader gsd te
        --------------------------------
        -- Via official, sorted spectrum
        --------------------------------
        ltd <- readLeafTree gsd gh
        let parent = V.last $ _ltd_spectrum ltd
        _te_blockHash parent @?= (getMerkleLogHash $ _blockParent bh)

-- | INVARIANT: The second-last entry of a given @git_tree_entry@ should be the
-- parent of the current node.
--
parentIsSecondLastEntry :: GitStore -> Assertion
parentIsSecondLastEntry gs = do
    let nexts = take chainLen $ testBlockHeaders genesis
    traverse_ (insertBlock gs) nexts
    lockGitStore gs $ \gsd -> do
        te@(TreeEntry _ _ gh) <- head <$> leaves' gsd
        bh <- readHeader gsd te
        --------------------------------
        -- Via raw, undecoded tree entry
        --------------------------------
        prnt <- readParent gsd gh
        _te_blockHash prnt @?= (getMerkleLogHash $ _blockParent bh)

-- | INVARIANT: The `LeafTreeData` of the genesis block has an empty spectrum
-- Vector, since it is the ultimate parent node.
--
genesisEmptySpectrum :: GitStore -> Assertion
genesisEmptySpectrum gs = do
    lockGitStore gs $ \gsd -> do
        leaf <- head <$> leaves' gsd
        ltd <- readLeafTree gsd $ _te_gitHash leaf
        _ltd_spectrum ltd @?= V.empty

basicTrav :: GitStore -> Assertion
basicTrav gs = do
    let nexts = take chainLen $ testBlockHeaders genesis
    traverse_ (insertBlock gs) nexts
    leaf <- head <$> leaves gs
    count <- newIORef (0 :: Int)
    walk gs (_blockHeight leaf) (_blockHash leaf) $ \_ -> do
        atomicModifyIORef' count (\n -> (n + 1, ()))
    final <- readIORef count
    final @?= (chainLen + 1)

seeking :: GitStore -> Assertion
seeking gs = do
    let nexts = take 300 $ testBlockHeaders genesis
    traverse_ (insertBlock gs) nexts
    te <- lockGitStore gs highestLeaf >>= seekHighest gs (0, 256)
    fmap _te_blockHeight te @?= Just 256

-- letsSee :: Assertion
-- letsSee = do
--     t <- generate . tree $ AtMost 550
--     withTreeDb withNewRepo' (GitStoreBlockHeader <$> t) $ \db -> do
--         putStrLn "PRETTY TREE!"
--         -- putStrLn $ prettyTree t
--         void . P.mapM_ f $ entries db Nothing Nothing Nothing Nothing
--   where
--     f (GitStoreBlockHeader bh) = printf "%s %s\n" (show $ _blockHeight bh) (show $ _blockHash bh)
