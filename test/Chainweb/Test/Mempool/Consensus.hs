{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Mempool.Consensus
  ( tests
  ) where

import Control.Monad.IO.Class

import Data.CAS.RocksDB
import Data.Hashable
import Data.Int
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tree
import Data.Vector ((!))
import qualified Data.Vector as V

import GHC.Generics

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic
-- import Test.Tasty

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

import Data.CAS
import qualified Data.CAS.HashMap as C
import Numeric.AffineSpace

data ForkInfo = ForkInfo
  { fiBlockHeaderDb :: BlockHeaderDb
  , fiPayloadStore :: PayloadStore
  , fiOldHeader :: BlockHeader
  , fiNewHeader :: BlockHeader
  , fiOldForkTrans :: Set TransactionHash
  , fiNewForkTrans :: Set TransactionHash
  , fiForkHeight :: Int
  , fiLeftBranchHeight :: Int
  , fiRightBranchHeight :: Int
  , fiPreForkHeaders :: [BlockHeader]
  , fiLeftForkHeaders :: [BlockHeader]
  , fiRightForkHeaders :: [BlockHeader]
  }

instance Show ForkInfo where
    show ForkInfo{..} =
        "ForkInfo - forkHeight: " ++ show fiForkHeight
        ++ ", leftBranchHeight: " ++ show fiLeftBranchHeight
        ++ ", rightBranchHeight: " ++ show fiRightBranchHeight
        ++ "\n\t"
        ++ ", number of old forkTrans: " ++ show (S.size fiOldForkTrans)
        ++ ", number of new forkTrans: " ++ show (S.size fiNewForkTrans)
        ++ "\n\t"
        ++ "'head' of old fork:"
        ++ "\n\t\tblock height: " ++ show (_blockHeight fiOldHeader)
        ++ "\n\t\tblock hash: " ++ show (_blockHash fiOldHeader)
        ++ "\n\t"
        ++ "'head' of new fork:"
        ++ "\n\t\tblock height: " ++ show (_blockHeight fiNewHeader)
        ++ "\n\t\tblock hash: " ++ show (_blockHash fiNewHeader)
        ++ "\n\tmain trunk headers:"
        ++ concatMap debugHeader fiPreForkHeaders
        ++ "\n\tleft fork headers:"
        ++ concatMap debugHeader fiLeftForkHeaders
        ++ "\n\t right fork headers:"
        ++ concatMap debugHeader fiRightForkHeaders
        ++ "\n\n"

data BlockTrans = BlockTrans
    { btBlockHeader :: BlockHeader
    , btTransactions :: Set TransactionHash }

data MockPayload = MockPayload
    { _mplHash :: BlockHash
    , _mplTxHashes :: [TransactionHash]
    }
    deriving (Show, Eq, Ord, Generic, Hashable)

instance IsCasValue MockPayload where
    type CasKeyType MockPayload = BlockHash
    casKey (MockPayload bh _txs) = bh

----------------------------------------------------------------------------------------------------
-- | Poperty: All transactions returned by processFork (for re-introduction to the mempool) come from
--   the old fork and are not represented in the new fork blocks
prop_validTxSource
    :: BlockHeaderDb
    -> C.HashMapCas MockPayload
    -> BlockHeader
    -> Property
prop_validTxSource db payloadStore genBlock = monadicIO $ do
    fi@ForkInfo{..} <- genFork db payloadStore genBlock
    liftIO $ putStrLn "ForkInfo from prop_validTxSource:"
    liftIO $ putStrLn $ show fi

    reIntroTransV <- run $
        processFork fiBlockHeaderDb fiNewHeader (Just fiOldHeader) lookupPayload
    let reIntroTrans = S.fromList $ V.toList reIntroTransV

    assert $ (reIntroTrans `S.isSubsetOf` fiOldForkTrans)
           && (reIntroTrans `S.disjoint` fiNewForkTrans)
  where
    lookupPayload h = casLookup payloadStore (_blockHash h) >>= \case
        Nothing -> error "must not happen"
        Just x -> return $ S.fromList $ _mplTxHashes x

-- | Property: All transactions that were in the old fork (and not also in the new fork) should be
--   marked available to re-entry into the mempool) (i.e., should be found in the Vector returned by
--   processFork)
prop_noOrphanedTxs
    :: BlockHeaderDb
    -> PayloadStore
    -> BlockHeader
    -> Property
prop_noOrphanedTxs db payloadStore genBlock = monadicIO $ do
    fi@ForkInfo{..} <- genFork db payloadStore genBlock
    liftIO $ putStrLn "ForkInfo from prop_noOrphanedTxs:"
    liftIO $ putStrLn $ show fi
    reIntroTransV <- run $
        processFork fiBlockHeaderDb fiNewHeader (Just fiOldHeader) lookupPayload
    let reIntroTrans = S.fromList $ V.toList reIntroTransV
    let expectedTrans = fiOldForkTrans `S.difference` fiNewForkTrans
    assert $ expectedTrans `S.isSubsetOf` reIntroTrans
  where
    lookupPayload h = casLookup payloadStore (_blockHash h) >>= \case
        Nothing -> error "must not happen"
        Just x -> return $ S.fromList $ _mplTxHashes x

-- TODO: revert to [TestTree]
tests :: IO ()
tests = runTests

runTests :: IO ()
runTests =
    withRocksDb "mempool-consensus-test" $ \rdb ->
        withToyDB rdb toyChainId $ \h0 db -> do

            -- is this needed?
            -- TreeDB.insert db h0

            payloadDb <- newMockPayloadDb
            quickCheck (prop_validTxSource db payloadDb h0)
            quickCheck (prop_noOrphanedTxs db payloadDb h0)
            return ()

getTransPool :: PropertyM IO (Set TransactionHash)
getTransPool = do
    S.fromList <$> sequenceA txHashes
  where
    txHashes = fmap (\n -> do
                        mockTx <- mkMockTx n
                        return $ TransactionHash $ mockEncode mockTx )
                    [1..100]

----------------------------------------------------------------------------------------------------
-- Fork generation
----------------------------------------------------------------------------------------------------
genFork
    :: BlockHeaderDb
    -> PayloadStore
    -> BlockHeader
    -> PropertyM IO ForkInfo
genFork db payloadStore startHeader = do
    allTxs <- getTransPool
    theTree <- genTree db payloadStore startHeader allTxs
    return $ buildForkInfo db payloadStore theTree

mkMockTx :: Int64 -> PropertyM IO MockTx
mkMockTx n = do
    time <- pick arbitrary
    return MockTx
        { mockNonce = n
        , mockGasPrice = GasPrice 0
        , mockGasLimit = mockBlockGasLimit
        , mockMeta = TransactionMetadata time time
        }

takeTrans :: Set TransactionHash -> PropertyM IO (Set TransactionHash, Set TransactionHash)
takeTrans txs = do
    n <- pick $ choose (1, 3)
    return $ S.splitAt n txs

debugHeader :: BlockHeader -> String
debugHeader BlockHeader{..} = "\n\t\tblockHeight: " ++ show _blockHeight ++ " (0-based)"
                           ++ "\n\t\tblockHash: " ++ show _blockHash
                           ++ "\n\t\tparentHash: " ++ show _blockParent
                           ++ "\n"

genTree
  :: BlockHeaderDb
  -> PayloadStore
  -> BlockHeader
  -> Set TransactionHash
  -> PropertyM IO (Tree BlockTrans)
genTree db store h allTxs = do
    (takenNow, theRest) <- takeTrans allTxs
    next <- header' h
    listOfOne <- preForkTrunk db store next theRest
    theNewNode <- newNode db store
                          BlockTrans { btBlockHeader = h, btTransactions = takenNow }
                          listOfOne
    return theNewNode

-- | Create a new Tree node and add the BlockHeader to the BlockHeaderDb
newNode
    :: BlockHeaderDb
    -> PayloadStore
    -> BlockTrans
    -> [Tree BlockTrans]
    -> PropertyM IO (Tree BlockTrans)
newNode db store blockTrans children = do
    let h = btBlockHeader blockTrans
    let theNewNode = Node blockTrans children
    liftIO $ TreeDB.insert db h

    -- insert to mock payload store -- key is blockHash, value is list of tx hashes
    liftIO $ insertMockPayload store
        MockPayload {_mplHash = _blockHash h, _mplTxHashes = S.toList (btTransactions blockTrans) }

    return theNewNode

preForkTrunk :: BlockHeaderDb -> PayloadStore -> BlockHeader -> Set TransactionHash -> PropertyM IO (Forest BlockTrans)
preForkTrunk db store h avail = do
    next <- header' h
    (takenNow, theRest) <- takeTrans avail
    children <- frequencyM [(1, fork db store next theRest), (3, preForkTrunk db store next theRest)]
    theNewNode <- newNode db store
                          BlockTrans {btBlockHeader = h, btTransactions = takenNow}
                          children


    -- theNewNode <- newNode db store
    --                       BlockTrans { btBlockHeader = h, btTransactions = takenNow }
    --                       listOfOne

    return [theNewNode]

-- | Version of frequency where the generators are in IO
frequencyM :: [(Int, PropertyM IO a)] -> PropertyM IO a
frequencyM xs = do
    let indexGens = (elements . (:[])) <$> [0..]
    let indexZip = zip (fst <$> xs) indexGens
    n <- pick $ frequency indexZip :: PropertyM IO Int -- the original 'frequency' chooses the index of the value
    snd $ ((V.fromList xs) ! n)

fork
    :: BlockHeaderDb
    -> PayloadStore
    -> BlockHeader
    -> Set TransactionHash
    -> PropertyM IO (Forest BlockTrans)
fork db store h avail = do
    nextLeft <- header' h
    nextRight <- header' h
    (takenNow, theRest) <- takeTrans avail
    left <- postForkTrunk db store nextLeft theRest
    right <- postForkTrunk db store nextRight theRest
    theNewNode <- newNode db store
                          BlockTrans {btBlockHeader = h, btTransactions = takenNow}
                          (left ++ right)
    return [theNewNode]

postForkTrunk :: BlockHeaderDb -> PayloadStore -> BlockHeader -> Set TransactionHash -> PropertyM IO (Forest BlockTrans)
postForkTrunk db store h avail = do
    next <- header' h
    (takenNow, theRest) <- takeTrans avail
    listOf0or1 <- frequencyM [(1, return []), (3, postForkTrunk db store next theRest)]
    theNewNode <- newNode db store
                          BlockTrans {btBlockHeader = h, btTransactions = takenNow}
                          listOf0or1
    return [theNewNode]

header' :: BlockHeader -> PropertyM IO BlockHeader
header' h = do
    nonce <- Nonce <$> pick chooseAny
    miner <- pick arbitrary
    return
        . fromLog
        . newMerkleLog
        $ nonce
            :+: BlockCreationTime (scaleTimeSpan (10 :: Int) second `add` t)
            :+: _blockHash h
            :+: target
            :+: testBlockPayload h
            :+: _chainId h
            :+: BlockWeight (targetToDifficulty target) + _blockWeight h
            :+: succ (_blockHeight h)
            :+: v
            :+: miner
            :+: MerkleLogBody mempty
   where
    BlockCreationTime t = _blockCreationTime h
    target = _blockTarget h -- no difficulty adjustment
    v = _blockChainwebVersion h

type PayloadStore = C.HashMapCas MockPayload

newMockPayloadDb :: IO PayloadStore
newMockPayloadDb = C.emptyCas

insertMockPayload :: PayloadStore -> MockPayload -> IO ()
insertMockPayload db theHash =  casInsert db theHash

_queryMockPayload :: PayloadStore -> BlockHash -> IO (Maybe MockPayload)
_queryMockPayload db theHash = casLookup db theHash

_payloadDbToList :: PayloadStore -> IO [MockPayload]
_payloadDbToList = C.toList

----------------------------------------------------------------------------------------------------
--  Info about generated forks
----------------------------------------------------------------------------------------------------
buildForkInfo :: BlockHeaderDb -> PayloadStore -> Tree BlockTrans -> ForkInfo
buildForkInfo blockHeaderDb store t =
    let (preFork, left, right) = splitNodes t
        forkHeight = length preFork
    in if (null preFork || null left || null right)
        then error "buildForkInfo -- all of the 3 lists must be non-empty"
        else
            ForkInfo
            { fiBlockHeaderDb = blockHeaderDb
            , fiPayloadStore = store
            , fiOldHeader = btBlockHeader (head left)
            , fiNewHeader = btBlockHeader (head right)
            , fiOldForkTrans = S.unions (btTransactions <$> left)
            , fiNewForkTrans = S.unions (btTransactions <$> right)
            , fiForkHeight = forkHeight
            , fiLeftBranchHeight = length left + forkHeight
            , fiRightBranchHeight = length right + forkHeight
            , fiPreForkHeaders = btBlockHeader <$> preFork
            , fiLeftForkHeaders = btBlockHeader <$> left
            , fiRightForkHeaders = btBlockHeader <$> right
            }

type BT3 = ([BlockTrans], [BlockTrans], [BlockTrans])

-- | Split the nodes into a triple of lists (xs, ys, zs) where xs = the nodes on the trunk before
--   the fork, ys = the nodes on the left fork, and zs = the nodes on the right fork
splitNodes :: Tree BlockTrans -> BT3
splitNodes t =
    let (trunk, restOfTree) = takePreFork t
        (leftFork, rightFork) = case restOfTree of
            Node _bt (x : y : _zs) -> (takeFork x [], takeFork y [])
            _someTree -> ([], []) -- should never happen
    -- in (trunk, leftFork, rightFork)
    -- remove this:
    in case (trunk, leftFork, rightFork) of
        ([], [], []) -> error "all 3 empty"
        ([], _y, _z) -> error "trunk is empty (maybe others too)"
        (_x, [], _z) -> error "left is empty (maybe the right as well)"
        (_x, _y, []) -> error "right is empty (others are not"
        (x, y, z) -> (x, y, z)

takePreFork :: Tree BlockTrans -> ([BlockTrans], Tree BlockTrans)
takePreFork theTree =
    go theTree []
  where
    go :: Tree BlockTrans -> [BlockTrans] -> ([BlockTrans], Tree BlockTrans) -- remove this
    go (Node bt (x : [])) xs = go x (bt : xs) -- continue the trunk
    go t@(Node bt (_x : _y : [])) xs = (bt : xs, t) -- reached the fork
    go someTree xs = (xs, someTree) -- should never happen

takeFork :: Tree BlockTrans -> [BlockTrans] -> [BlockTrans]
takeFork (Node bt (x : [])) xs = takeFork x (bt : xs) -- continue the fork
takeFork (Node bt []) xs = bt : xs -- done with the fork
takeFork _someTree xs = xs -- should never happen
