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
import Pact.Types.Gas

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog hiding (header)
import Chainweb.Difficulty (targetToDifficulty)
import Chainweb.Mempool.Mempool
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Test.Utils
import Chainweb.Time
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
    allTxs <- liftIO $ getTransPool
    store <- liftIO $ newFakePayloadDb
    h0 <- genesis testVersion
    theTree <- genTree h0 allTxs

    return Nothing
-- TODO: fold a list (of BlockHeader -> [TrasnsactionHash]) from the tree & add them to the store
-- foldTree :: (a -> [b] -> b) -> Tree a -> b

-- TODO: get the left and right leaf elements as the 'head' of the old/new forks


getTransPool :: IO (Set TransactionHash)
getTransPool = do
    txHashes <- sequence $ fmap
        (\n -> do
            mockTx <- mkMockTx n
            return $ TransactionHash $ mockEncode mockTx )
        [1 2 .. 100]
    return $ S.fromList txHashes

mkMockTx :: Int64 -> IO MockTx
mkMockTx n = do
    mockMeta <- TransactionMetadata <$> Time.decodeTime <*> Time.decodeTime
    return $ MockTx
        { mockNonce = n
        , mockGasPrice = GasPrice 0
        , mockGasLimit = mockBlockGasLimit
        , mockMeta = mockMeta
        }

takeTrans :: Set TransactionHash -> Gen (Set TransactionHash, Set TransactionHash)
takeTrans txs = do
    n <- choose (1, 3)
    return $ S.splitAt n txs

data BlockTrans = BlockTrans
    { btBlockHeader :: BlockHeader
    , btTransactions :: Set TransactionHash }

genTree
    :: BlockHeader
    -> Set TransactionHash
    -> Gen (Tree BlockTrans)
genTree h allTxs = do
    (takenNow, theRest) <- takeTrans allTxs
    next <- header' h
    listOfOne <- preForkTrunk next theRest
    return $ Node
        BlockTrans { btBlockHeader = h, btTransactions = takenNow }
        listOfOne

preForkTrunk
    :: BlockHeader
    -> Set TransactionHash
    -> Gen (Forest BlockTrans)
preForkTrunk h avail = do
    next <- header' h
    (takenNow, theRest) <- takeTrans avail

    children <- frequency [ (1, fork next theRest)
                          , (3, preForkTrunk next theRest) ]
    return [ Node BlockTrans { btBlockHeader = h, btTransactions = takenNow } children ]


fork
    :: BlockHeader
    -> Set TransactionHash
    -> Gen (Forest BlockTrans)
fork h avail = do
    next <- header' h
    (takenNow, theRest) <- takeTrans avail

    left <- postForkTrunk next theRest
    right <- postForkTrunk next theRest -- TODO: what header to use here?
    return $
        [ Node BlockTrans { btBlockHeader = h, btTransactions = takenNow }
               (left ++ right)
        ]

postForkTrunk
  :: BlockHeader
  -> Set TransactionHash
  -> Gen (Forest BlockTrans)
postForkTrunk h avail = do
    next <- header' h
    (takenNow, theRest) <- takeTrans avail

    listOf0or1 <- frequency
        [ (1, return [])
        , (3, postForkTrunk next theRest)
        ]
    return $
        [ Node BlockTrans { btBlockHeader = h, btTransactions = takenNow }
               listOf0or1
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
