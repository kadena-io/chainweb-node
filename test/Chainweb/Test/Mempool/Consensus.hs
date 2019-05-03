{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Mempool.Consensus
  ( tests
  ) where


import Data.Hashable
import Data.Int
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tree

import GHC.Generics

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen
import Test.Tasty

-- internal modules
import Pact.Types.Gas

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog hiding (header)
import Chainweb.Difficulty (targetToDifficulty)
import Chainweb.Mempool.Mempool
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Version

import Data.CAS
import qualified Data.CAS.HashMap as C
import Numeric.AffineSpace

tests :: [TestTree]
tests = undefined

data ForkInfo = ForkInfo
  { fiOldHeader :: BlockHeader
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

data BlockTrans = BlockTrans
    { btBlockHeader :: BlockHeader
    , btTransactions :: Set TransactionHash }

data FakePayload = FakePayload
    { _fplHash :: BlockPayloadHash
    , _fplTxHashes :: [TransactionHash]
    }
    deriving (Show, Eq, Ord, Generic, Hashable)

instance IsCasValue FakePayload where
    type CasKeyType FakePayload = BlockPayloadHash
    casKey (FakePayload bh txs) = bh

----------------------------------------------------------------------------------------------------
-- | Poperty: All transactions returned by processFork (for re-introduction to the mempool) come from
--   the old fork and are not represented in the new fork blocks
prop_validTxsSource :: ForkInfo -> Bool
prop_validTxsSource = undefined

-- | Property: All transactions that were in the old fork (and not also in the new fork) should be
--   marked available to re-entry into the mempool) (i.e., should be found in the Vector returned by
--   processFork)
prop_noOrhanedTxs :: ForkInfo -> Bool
prop_noOrhanedTxs = undefined

testVersion :: ChainwebVersion
testVersion = Testnet00 -- TODO: what is the right version to use for tests?

run :: IO ()
run = do
    payloadDb <- newFakePayloadDb
    let theGenInfo = genFork payloadDb
    sample theGenInfo
    return ()

----------------------------------------------------------------------------------------------------
--  Info about generated forks
----------------------------------------------------------------------------------------------------
buildForkInfo :: Tree BlockTrans -> ForkInfo
buildForkInfo t =
    let (preFork, left, right) = splitNodes t
        forkHeight = length preFork
    in if (null preFork || null left || null right)
        then error "buildForkInfo -- all of the 3 lists must be non-empty"
        else
            ForkInfo
            { fiOldHeader = btBlockHeader (head left)
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
            Node bt (x : y : zs) -> (takeFork x [], takeFork y [])
            someTree -> ([], []) -- should never happen
    -- in (trunk, leftFork, rightFork)
    -- remove this:
    in case (trunk, leftFork, rightFork) of
        ([], [], []) -> error "all 3 empty"
        ([], y, z) -> error "trunk is empty (maybe others too)"
        (x, [], z) -> error "left is empty (maybe the right as well)"
        (x, y, []) -> error "right is empty (others are not"
        (x, y, z) -> (x, y, z)

takePreFork :: Tree BlockTrans -> ([BlockTrans], Tree BlockTrans)
takePreFork theTree =
    go theTree []
  where
    go :: Tree BlockTrans -> [BlockTrans] -> ([BlockTrans], Tree BlockTrans) -- remove this
    go (Node bt (x : [])) xs = go x (bt : xs) -- continue the trunk
    go t@(Node bt (x : y : [])) xs = (bt : xs, t) -- reached the fork
    go someTree xs = (xs, someTree) -- should never happen

takeFork :: Tree BlockTrans -> [BlockTrans] -> [BlockTrans]
takeFork (Node bt (x : [])) xs = takeFork x (bt : xs) -- continue the fork
takeFork (Node bt []) xs = bt : xs -- done with the fork
takeFork someTree xs = xs -- should never happen

----------------------------------------------------------------------------------------------------
-- Fork generation
----------------------------------------------------------------------------------------------------
-- genFork :: C.HashMapCas FakePayload -> Gen (Tree BlockTrans)
genFork :: C.HashMapCas FakePayload -> Gen ForkInfo
genFork store = do
    allTxs <- getTransPool
    h0 <- genesis testVersion
    theTree <- genTree h0 allTxs
    return $ buildForkInfo theTree

getTransPool :: Gen (Set TransactionHash)
getTransPool = do
    let txHashes = fmap (\n -> do
                         mockTx <- mkMockTx n
                         return $ TransactionHash $ mockEncode mockTx )
                     [1..100]
    S.fromList <$> sequenceA txHashes

mkMockTx :: Int64 -> Gen MockTx
mkMockTx n = do
    time <- arbitrary :: Gen (Time Int64)
    return MockTx
        { mockNonce = n
        , mockGasPrice = GasPrice 0
        , mockGasLimit = mockBlockGasLimit
        , mockMeta = TransactionMetadata time time
        }

takeTrans :: Set TransactionHash -> Gen (Set TransactionHash, Set TransactionHash)
takeTrans txs = do
    n <- choose (1, 3)
    return $ S.splitAt n txs

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

debugHeader :: BlockHeader -> String
debugHeader BlockHeader{..} = "\n\t\tblockHeight: " ++ show _blockHeight ++ " (0-based)"
                           ++ "\n\t\tblockHash: " ++ show _blockHash
                           ++ "\n\t\tparentHash: " ++ show _blockParent
                           ++ "\n"

-- instance Show BlockTrans where
--     show bt =
--       "BlockTrans - someHeaderFields {_blockParent = " ++ show (_blockParent (btBlockHeader bt))
--            ++ ", _blockHash = " ++ show (_blockHash (btBlockHeader bt))
--            ++ ", _blockHeight = " ++ show (_blockHeight (btBlockHeader bt)) ++ "}"
--            ++ "\nNumber of transactions: " ++ show (S.size (btTransactions bt)) ++ "\n\n"


genTree :: BlockHeader -> Set TransactionHash -> Gen (Tree BlockTrans)
genTree h allTxs = do
    (takenNow, theRest) <- takeTrans allTxs
    next <- header' h
    listOfOne <- preForkTrunk next theRest
    return $ Node BlockTrans { btBlockHeader = h, btTransactions = takenNow } listOfOne

preForkTrunk :: BlockHeader -> Set TransactionHash -> Gen (Forest BlockTrans)
preForkTrunk h avail = do
    next <- header' h
    (takenNow, theRest) <- takeTrans avail
    children <- frequency [ (1, fork next theRest)
                          , (3, preForkTrunk next theRest) ]
    return [ Node BlockTrans { btBlockHeader = h, btTransactions = takenNow } children ]

fork :: BlockHeader -> Set TransactionHash -> Gen (Forest BlockTrans)
fork h avail = do
    nextLeft <- header' h
    nextRight <- header' h
    (takenNow, theRest) <- takeTrans avail
    left <- postForkTrunk nextLeft theRest
    right <- postForkTrunk nextRight theRest
    return $ [ Node BlockTrans { btBlockHeader = h, btTransactions = takenNow } (left ++ right) ]

postForkTrunk :: BlockHeader -> Set TransactionHash -> Gen (Forest BlockTrans)
postForkTrunk h avail = do
    next <- header' h
    (takenNow, theRest) <- takeTrans avail
    listOf0or1 <- frequency
        [ (1, return [])
        , (3, postForkTrunk next theRest) ]
    return [ Node BlockTrans { btBlockHeader = h, btTransactions = takenNow } listOf0or1 ]

header' :: BlockHeader -> Gen BlockHeader
header' h = do
    nonce <- Nonce <$> chooseAny
    miner <- arbitrary
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

  -- duplicated, since not exported from Chaineweb.Test.Utils
genesis :: ChainwebVersion -> Gen BlockHeader
genesis v = either (error . show) return $ genesisBlockHeaderForChain v (0 :: Int)

newFakePayloadDb :: IO (C.HashMapCas FakePayload)
newFakePayloadDb = C.emptyCas

insertFakePayload :: C.HashMapCas FakePayload -> FakePayload -> IO ()
insertFakePayload db hash =  casInsert db hash

queryFakePayload :: C.HashMapCas FakePayload -> BlockPayloadHash -> IO (Maybe FakePayload)
queryFakePayload db hash = casLookup db hash

payloadDbToList :: C.HashMapCas FakePayload -> IO [FakePayload]
payloadDbToList = C.toList
