{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Mempool.Consensus
  ( tests
  ) where

import Control.Monad.IO.Class

import qualified Data.ByteString.Short as SB
import Data.CAS.RocksDB
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Int
import Data.IORef
import Data.Tree
import Data.Vector ((!))
import qualified Data.Vector as V

import GHC.Generics

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic
import Test.Tasty.QuickCheck

-- internal modules
import Pact.Types.Gas

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog hiding (header)
import Chainweb.Difficulty (targetToDifficulty)
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.Mempool
import Chainweb.Test.Utils
import Chainweb.Time
import qualified Chainweb.TreeDB as TreeDB

import Data.LogMessage

----------------------------------------------------------------------------------------------------
tests :: BlockHeaderDb -> BlockHeader -> ScheduledTest
tests db h0 = testGroupSch "mempool-consensus-quickcheck-tests"
    [ testProperty "valid-transactions-source" (prop_validTxSource db h0)
    , testProperty "no-orphaned-txs" (prop_noOrphanedTxs db h0) ]

----------------------------------------------------------------------------------------------------
-- | Poperty: All transactions returned by processFork (for re-introduction to the mempool) come from
--   the old fork and are not represented in the new fork blocks
prop_validTxSource
    :: BlockHeaderDb
    -> BlockHeader
    -> Property
prop_validTxSource db genBlock = monadicIO $ do
    mapRef <- liftIO $ newIORef (HM.empty :: HashMap BlockHeader (HashSet TransactionHash))
    ForkInfo{..} <- genFork db mapRef genBlock

    (reIntroTransV, _) <- run $ processFork' (alogFunction aNoLog) fiBlockHeaderDb
                                    fiNewHeader (Just fiOldHeader) (lookupFunc mapRef)
    let reIntroTrans = HS.fromList $ V.toList reIntroTransV

    assert $ (reIntroTrans `isSubsetOf` fiOldForkTrans)
           && (reIntroTrans `disjoint` fiNewForkTrans)

----------------------------------------------------------------------------------------------------
-- isSubsetOf for HashSet (only used in tests, performance doesn't really matter)
isSubsetOf :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
isSubsetOf x y = null $ x `HS.difference` (x `HS.intersection` y)

----------------------------------------------------------------------------------------------------
-- disjoint for HashSet (only used in tests, performance doesn't really matter)
disjoint :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
disjoint x y = null $ x `HS.intersection` y

----------------------------------------------------------------------------------------------------
-- splitAt for HashSet (only used in tests, performance doesn't really matter)
splitHsAt :: (Eq a, Hashable a) => Int -> HashSet a -> (HashSet a, HashSet a)
splitHsAt n x =
  let firstSet = HS.fromList $ take n (HS.toList x)
  in (firstSet, x `HS.difference` firstSet)

----------------------------------------------------------------------------------------------------
-- | Property: All transactions that were in the old fork (and not also in the new fork) should be
--   marked available to re-entry into the mempool) (i.e., should be found in the Vector returned by
--   processFork)
prop_noOrphanedTxs
    :: BlockHeaderDb
    -> BlockHeader
    -> Property
prop_noOrphanedTxs db genBlock = monadicIO $ do
    mapRef <- liftIO $ newIORef (HM.empty :: HashMap BlockHeader (HashSet TransactionHash))
    ForkInfo{..} <- genFork db mapRef genBlock

    (reIntroTransV, _) <- run $ processFork' (alogFunction aNoLog) fiBlockHeaderDb
                                    fiNewHeader (Just fiOldHeader) (lookupFunc mapRef)
    let reIntroTrans = HS.fromList $ V.toList reIntroTransV
    let expectedTrans = fiOldForkTrans `HS.difference` fiNewForkTrans

    assert $ expectedTrans `isSubsetOf` reIntroTrans

----------------------------------------------------------------------------------------------------
data ForkInfo = ForkInfo
  { fiBlockHeaderDb :: BlockHeaderDb
  , fiOldHeader :: BlockHeader
  , fiNewHeader :: BlockHeader
  , fiOldForkTrans :: HashSet TransactionHash
  , fiNewForkTrans :: HashSet TransactionHash
    -- for printing debug info...
  , fiForkHeight :: Int
  , fiLeftBranchHeight :: Int
  , fiRightBranchHeight :: Int
  , fiPreForkHeaders :: [BlockHeader]
  , fiLeftForkHeaders :: [BlockHeader]
  , fiRightForkHeaders :: [BlockHeader]
  }

data BlockTrans = BlockTrans
    { btBlockHeader :: BlockHeader
    , btTransactions :: HashSet TransactionHash }

data MockPayload = MockPayload
    { _mplHash :: BlockHash
    , _mplTxHashes :: [TransactionHash]
    }
    deriving (Show, Eq, Ord, Generic, Hashable)

----------------------------------------------------------------------------------------------------
lookupFunc
  :: IORef (HashMap BlockHeader (HashSet TransactionHash))
  -> BlockHeader
  -> IO (HashSet TransactionHash)
lookupFunc mapRef h = do
    hm <- readIORef mapRef
    case HM.lookup h hm of
        Nothing -> error "Test/Mempool/Consensus - hashtable lookup failed -- this should not happen"
        Just txs -> return txs

----------------------------------------------------------------------------------------------------
getTransPool :: PropertyM IO (HashSet TransactionHash)
getTransPool =
    HS.fromList <$> sequenceA txHashes
  where
    txHashes = fmap (\n -> do
                        mockTx <- mkMockTx n
                        return $ TransactionHash $ SB.toShort $ mockEncode mockTx )
                    [1..100]

----------------------------------------------------------------------------------------------------
-- Fork generation
----------------------------------------------------------------------------------------------------
-- | Generate a tree containing a fork
genFork
    :: BlockHeaderDb
    -> IORef (HashMap BlockHeader (HashSet TransactionHash))
    -> BlockHeader
    -> PropertyM IO ForkInfo
genFork db mapRef startHeader = do
    allTxs <- getTransPool
    theTree <- genTree db mapRef startHeader allTxs
    return $ buildForkInfo db theTree

----------------------------------------------------------------------------------------------------
mkMockTx :: Int64 -> PropertyM IO MockTx
mkMockTx n = do
    time <- pick arbitrary
    return MockTx
        { mockNonce = n
        , mockGasPrice = GasPrice 0
        , mockGasLimit = mockBlockGasLimit
        , mockMeta = TransactionMetadata time time
        }

----------------------------------------------------------------------------------------------------
takeTrans
    :: HashSet TransactionHash
    -> PropertyM IO (HashSet TransactionHash, HashSet TransactionHash)
takeTrans txs = do
    n <- pick $ choose (1, 3)
    return $ splitHsAt n txs

----------------------------------------------------------------------------------------------------
genTree
  :: BlockHeaderDb
  -> IORef (HashMap BlockHeader (HashSet TransactionHash))
  -> BlockHeader
  -> HashSet TransactionHash
  -> PropertyM IO (Tree BlockTrans)
genTree db mapRef h allTxs = do
    (takenNow, theRest) <- takeTrans allTxs
    next <- header' h
    liftIO $ TreeDB.insert db next
    listOfOne <- preForkTrunk db mapRef next theRest
    newNode mapRef
            BlockTrans { btBlockHeader = h, btTransactions = takenNow }
            listOfOne

----------------------------------------------------------------------------------------------------
-- | Create a new Tree node
newNode
    :: IORef (HashMap BlockHeader (HashSet TransactionHash))
    -> BlockTrans
    -> [Tree BlockTrans]
    -> PropertyM IO (Tree BlockTrans)
newNode mapRef blockTrans children = do
    startMap <- liftIO $ readIORef mapRef
    let h = btBlockHeader blockTrans
    let theNewNode = Node blockTrans children

    -- insert to mock payload store -- key is blockHash, value is list of tx hashes
    let newMap = HM.insert h (btTransactions blockTrans) startMap
    liftIO $ writeIORef mapRef newMap
    return theNewNode

----------------------------------------------------------------------------------------------------
preForkTrunk
    :: BlockHeaderDb
    -> IORef (HashMap BlockHeader (HashSet TransactionHash))
    -> BlockHeader
    -> HashSet TransactionHash
    -> PropertyM IO (Forest BlockTrans)
preForkTrunk db mapRef h avail = do
    next <- header' h
    liftIO $ TreeDB.insert db next
    (takenNow, theRest) <- takeTrans avail
    children <- frequencyM
        [(1, fork db mapRef next theRest), (3, preForkTrunk db mapRef next theRest)]
    theNewNode <- newNode mapRef
                          BlockTrans {btBlockHeader = h, btTransactions = takenNow}
                          children
    return [theNewNode]

----------------------------------------------------------------------------------------------------
-- | Version of frequency where the generators are in IO
frequencyM :: [(Int, PropertyM IO a)] -> PropertyM IO a
frequencyM xs = do
    let indexGens = elements . (:[]) <$> [0..]
    let indexZip = zip (fst <$> xs) indexGens
    -- the original 'frequency' chooses the index of the value:
    n <- pick $ frequency indexZip :: PropertyM IO Int
    snd (V.fromList xs ! n)

----------------------------------------------------------------------------------------------------
fork
    :: BlockHeaderDb
    -> IORef (HashMap BlockHeader (HashSet TransactionHash))
    -> BlockHeader
    -> HashSet TransactionHash
    -> PropertyM IO (Forest BlockTrans)
fork db mapRef h avail = do
    nextLeft <- header' h
    liftIO $ TreeDB.insert db nextLeft
    nextRight <- header' h
    liftIO $ TreeDB.insert db nextRight
    (takenNow, theRest) <- takeTrans avail

    (lenL, lenR) <- genForkLengths

    left <- postForkTrunk db mapRef nextLeft theRest lenL
    right <- postForkTrunk db mapRef nextRight theRest lenR
    theNewNode <- newNode mapRef
                          BlockTrans {btBlockHeader = h, btTransactions = takenNow}
                          (left ++ right)
    return [theNewNode]

----------------------------------------------------------------------------------------------------
genForkLengths :: PropertyM IO (Int, Int)
genForkLengths = do
    left <- pick $ choose (1, 5)
    right <- pick $ choose (1, left)
    return (left, right)

----------------------------------------------------------------------------------------------------
postForkTrunk
    :: BlockHeaderDb
    -> IORef (HashMap BlockHeader (HashSet TransactionHash))
    -> BlockHeader
    -> HashSet TransactionHash
    -> Int
    -> PropertyM IO (Forest BlockTrans)
postForkTrunk db mapRef h avail count = do
    next <- header' h
    (takenNow, theRest) <- takeTrans avail
    children <-
        if count == 0 then return []
        else do
            liftIO $ TreeDB.insert db next
            postForkTrunk db mapRef next theRest (count - 1)
    theNewNode <- newNode mapRef
                          BlockTrans {btBlockHeader = h, btTransactions = takenNow}
                          children
    return [theNewNode]

----------------------------------------------------------------------------------------------------
header' :: BlockHeader -> PropertyM IO BlockHeader
header' h = do
    nonce <- Nonce <$> pick chooseAny
    return
        . fromLog
        . newMerkleLog
        $ nonce
            :+: t'
            :+: _blockHash h
            :+: target
            :+: testBlockPayload h
            :+: _chainId h
            :+: BlockWeight (targetToDifficulty target) + _blockWeight h
            :+: succ (_blockHeight h)
            :+: v
            :+: epochStart h t'
            :+: FeatureFlags 0
            :+: MerkleLogBody mempty
   where
    BlockCreationTime t = _blockCreationTime h
    target = powTarget h t'
    v = _blockChainwebVersion h
    t' = BlockCreationTime (scaleTimeSpan (10 :: Int) second `add` t)

----------------------------------------------------------------------------------------------------
--  Info about generated forks
----------------------------------------------------------------------------------------------------
buildForkInfo
    :: BlockHeaderDb
    -> Tree BlockTrans
    -> ForkInfo
buildForkInfo blockHeaderDb t =
    let (preFork, left, right) = splitNodes t
        forkHeight = length preFork
    in if null preFork || null left || null right
        then error "buildForkInfo -- all of the 3 lists must be non-empty"
        else
            ForkInfo
            { fiBlockHeaderDb = blockHeaderDb
            , fiOldHeader = btBlockHeader (head left)
            , fiNewHeader = btBlockHeader (head right)
            , fiOldForkTrans = HS.unions (btTransactions <$> left)
            , fiNewForkTrans = HS.unions (btTransactions <$> right)
            , fiForkHeight = forkHeight
            , fiLeftBranchHeight = length left + forkHeight
            , fiRightBranchHeight = length right + forkHeight
            , fiPreForkHeaders = btBlockHeader <$> preFork
            , fiLeftForkHeaders = btBlockHeader <$> left
            , fiRightForkHeaders = btBlockHeader <$> right
            }

----------------------------------------------------------------------------------------------------
-- | Split the nodes into a triple of lists (xs, ys, zs) where xs = the nodes on the trunk before
--   the fork, ys = the nodes on the left fork, and zs = the nodes on the right fork
splitNodes :: Tree BlockTrans -> BT3
splitNodes t =
    let (trunk, restOfTree) = takePreFork t
        (leftFork, rightFork) = case restOfTree of
            Node _bt (x : y : _zs) -> (takeFork x [], takeFork y [])
            _someTree -> ([], []) -- should never happen
    in (trunk, leftFork, rightFork)

type BT3 = ([BlockTrans], [BlockTrans], [BlockTrans])

----------------------------------------------------------------------------------------------------
takePreFork :: Tree BlockTrans -> ([BlockTrans], Tree BlockTrans)
takePreFork theTree =
    go theTree []
  where
    go :: Tree BlockTrans -> [BlockTrans] -> ([BlockTrans], Tree BlockTrans) -- remove this
    go (Node bt [x]) xs = go x (bt : xs) -- continue the trunk
    go t@(Node bt [_x, _y]) xs = (bt : xs, t) -- reached the fork
    go someTree xs = (xs, someTree) -- should never happen

----------------------------------------------------------------------------------------------------
takeFork :: Tree BlockTrans -> [BlockTrans] -> [BlockTrans]
takeFork (Node bt [x]) xs = takeFork x (bt : xs) -- continue the fork
takeFork (Node bt []) xs = bt : xs -- done with the fork
takeFork _someTree xs = xs -- should never happen

----------------------------------------------------------------------------------------------------
-- For debuggging
----------------------------------------------------------------------------------------------------
instance Show ForkInfo where
    show ForkInfo{..} =
        "ForkInfo - forkHeight: " ++ show fiForkHeight
        ++ ", leftBranchHeight: " ++ show fiLeftBranchHeight
        ++ ", rightBranchHeight: " ++ show fiRightBranchHeight
        ++ "\n\t"
        ++ ", number of old forkTrans: " ++ show (HS.size fiOldForkTrans)
        ++ debugTrans "oldForkTrans" fiOldForkTrans
        ++ ", number of new forkTrans: " ++ show (HS.size fiNewForkTrans)
        ++ debugTrans "newForkTrans" fiNewForkTrans
        ++ "\n\t"
        ++ "'head' of old fork:"
        ++ "\n\t\tblock height: " ++ show (_blockHeight fiOldHeader)
        ++ "\n\t\tblock hash: " ++ show (_blockHash fiOldHeader)
        ++ "\n\t"
        ++ "'head' of new fork:"
        ++ "\n\t\tblock height: " ++ show (_blockHeight fiNewHeader)
        ++ "\n\t\tblock hash: " ++ show (_blockHash fiNewHeader)
        ++ concatMap (debugHeader "main trunk headers") fiPreForkHeaders
        ++ concatMap (debugHeader "left fork headers") fiLeftForkHeaders
        ++ concatMap (debugHeader "right fork headers") fiRightForkHeaders
        ++ "\n\n"

----------------------------------------------------------------------------------------------------
debugHeader :: String -> BlockHeader -> String
debugHeader context BlockHeader{..} =
    "\nBlockheader from " ++ context ++ ": "
    ++ "\n\t\tblockHeight: " ++ show _blockHeight ++ " (0-based)"
    ++ "\n\t\tblockHash: " ++ show _blockHash
    ++ "\n\t\tparentHash: " ++ show _blockParent
    ++ "\n"

----------------------------------------------------------------------------------------------------
debugTrans :: String -> HashSet TransactionHash -> String
debugTrans context txs = "\n" ++ show (HS.size txs) ++ " TransactionHashes from: " ++ context
                       ++ concatMap (\t -> "\n\t" ++ show t) txs

----------------------------------------------------------------------------------------------------
_runGhci :: IO ()
_runGhci =
    withTempRocksDb "mempool-consensus-test" $ \rdb ->
    withToyDB rdb toyChainId $ \h0 db -> do
        quickCheck (prop_validTxSource db h0)
        quickCheck (prop_noOrphanedTxs db h0)
        return ()
