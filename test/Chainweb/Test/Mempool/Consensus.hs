{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  { _fiOldHeader :: BlockHeader
  , _fiNewHeader :: BlockHeader
  , _fiOldForkTrans :: Set TransactionHash
  , _fiNewForkTrans :: Set TransactionHash
  , _fiHeaderTree :: Tree BlockTrans
  } deriving (Show)

type BT3 = ([BlockTrans], [BlockTrans], [BlockTrans])

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

buildForkInfo :: Tree BlockTrans -> ForkInfo
buildForkInfo t =
    let (trunk, left, right) = splitNodes t
    in ForkInfo
      { _fiOldHeader = btBlockHeader (last left)
      , _fiNewHeader = btBlockHeader (last right)
      ,  _fiOldForkTrans = S.unions (btTransactions <$> left)
      ,  _fiNewForkTrans = S.unions (btTransactions <$> right)
      ,  _fiHeaderTree = t
      }


-- | Split the nodes into a triple of lists (xs, ys, zs) where xs = the nodes on the trunk before
--   the fork, ys = the nodes on the left fork, and zs = the nodes on the right fork
splitNodes :: Tree BlockTrans -> BT3
splitNodes t =
    let (trunk, restOfTree) = takeTrunk t
        (leftFork, rightFork) = case restOfTree of
            Node bt (x : y : zs) -> (takeFork x [], takeFork y [])
            someTree -> ([], []) -- should never happen
    in (trunk, leftFork, rightFork)

takeTrunk :: Tree BlockTrans -> ([BlockTrans], Tree BlockTrans)
takeTrunk theTree =
    go theTree []
  where
    go :: Tree BlockTrans -> [BlockTrans] -> ([BlockTrans], Tree BlockTrans) -- remove this
    go (Node bt (x : [])) xs = go x (bt : xs) -- continue the trunk
    go t@(Node bt (x : y : [])) xs = (xs, t) -- reached the fork
    go someTree xs = (xs, someTree) -- should never happen

takeFork :: Tree BlockTrans -> [BlockTrans] -> [BlockTrans]
takeFork (Node bt (x : [])) xs = takeFork x (bt : xs) -- continue the fork
takeFork (Node bt []) xs = xs -- done with the fork
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

instance Show BlockTrans where
    show bt =
      "BlockTrans - someHeaderFields {_blockParent = " ++ show (_blockParent (btBlockHeader bt))
           ++ ", _blockHash = " ++ show (_blockHash (btBlockHeader bt))
           ++ ", _blockHeight = " ++ show (_blockHeight (btBlockHeader bt)) ++ "}"
           ++ "\nNumber of transactions: " ++ show (S.size (btTransactions bt)) ++ "\n\n"


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
