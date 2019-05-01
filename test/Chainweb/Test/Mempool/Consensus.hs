{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Mempool.Consensus
  ( tests
  ) where

import Control.Lens
import Control.Monad.IO.Class

import Data.Hashable
import Data.Int
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tree

import GHC.Generics

import System.Random

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen
import Test.Tasty

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog hiding (header)
import Chainweb.Difficulty (targetToDifficulty)
import Chainweb.Mempool.Mempool
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Test.Utils
import Chainweb.Time (Time(..))
import qualified Chainweb.Time as Time
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
  , _fiHeaderTree :: Tree BlockHeader
  } deriving (Eq, Show)

makeLenses ''ForkInfo

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

----------------------------------------------------------------------------------------------------
-- Fork generation
----------------------------------------------------------------------------------------------------
-- TODO: Remove the maybe
genFork :: Gen (Maybe ForkInfo)
genFork = do
    let allTxs = getTransPool
    store <- liftIO $ newFakePayloadDb
    theTree <- genTree testVersion allTxsh

    return Nothing
-- TODO: fold a list (of BlockHeader -> [TrasnsactionHash]) from the tree & add them to the store
-- foldTree :: (a -> [b] -> b) -> Tree a -> b

-- TODO: get the left and right leaf elements as the 'head' of the old/new forks


getTransPool :: (Set TransactionHash)
getTransPool =
    S.fromList $ map [1 2 .. 100] $ \n ->
        TransactionHash $ mockEncode $ mkMockTx n

mkMockTx :: Int64 -> MockTx
mkMockTx n = MockTx
  { mockNonce = n
  , mockGasPrice = GasPrice 0
  , mockGasLimit = mockBlockGasLimit
  , mockMeta = TransactionMetadata <$> Time.decodeTime <*> Time.decodeTime
  }

taketrans :: set transactionhash -> (set transactionhash, set transactionhash)
taketrans txs = do
  n <- chose (1, 3)
  s.splitat n txs

genTree
    :: ChainwebVersion
    -> C.HashMapCas FakePayload
    -> Set TransactionHash
    -> Gen (Tree (BlockHeader, [TransactionHash]))
genTree v store allTxs = do
    h <- genesis v
    (theForest, _) <- preForkTrunk h allTxs S.empty
    return $ Node h theForest

preForkTrunk
    :: (BlockHeader, Set TransactionHash)
    -> Set TransactionHash
    -> Gen (Forest BlockHeader, [TransactionHash])
preForkTrunk (h, taken) avail = do
    next <- header' h
    let (takenNow, therest) = takeTrans avail
    frequency [ (1, sequenceA [fork (next, takenNow) theRest])
              , (3, sequenceA [preForkTrunk (next, takenNow) theRest])
              ]

fork
    :: (BlockHeader, Set TransactionHash)
    -> Set TransactionHash
    -> Gen (Tree BlockHeader, [TransactionHash])
fork (h, taken) avail = do
    next <- header' h
    let (takenNow, therest) = takeTrans avail
    Node (next, takenNow) <$> [postForkTrunk (next, theRest) ,postForkTrunk (next, theRest)]

postForkTrunk
  :: (BlockHeader, Set TransactionHash)
  -> Set TransactionHash
  -> Gen (Forest BlockHeader, [TransactionHash])
postForkTrunk (h, taken) avail = do
    next <- header' h
    let (takenNow, therest) = takeTrans avail
    frequency [ (1, return [])
              , (3, sequenceA [postForkTrunk (next, takenNow) theRest])
              ]

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
            :+: BlockWeight (targetToDifficulty v target) + _blockWeight h
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

data FakePayload = FakePayload
    { _fplHash :: BlockPayloadHash
    , _fplTxHashes :: [TransactionHash]
    }
    deriving (Show, Eq, Ord, Generic, Hashable)

instance IsCasValue FakePayload where
    type CasKeyType FakePayload = BlockPayloadHash
    casKey (FakePayload fpl) = _fplHash fpl

newFakePayloadDb :: IO (C.HashMapCas FakePayload)
newFakePayloadDb = C.emptyCas

insertFakePayload :: C.HashMapCas FakePayload -> FakePayload -> IO ()
insertFakePayload db hash =  casInsert db hash

queryFakePayload :: C.HashMapCas FakePayload -> BlockPayloadHash -> IO (Maybe FakePayload)
queryFakePayload db hash = casLookup db hash

payloadDbToList :: C.HashMapCas FakePayload -> IO [FakePayload]
payloadDbToList = C.toList
