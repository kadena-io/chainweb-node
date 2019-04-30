{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Mempool.Consensus
  ( tests
  ) where

import Data.Hashable
import Data.Set (Set)
-- import qualified Data.Set as S
import Data.Tree
-- import Data.Vector (Vector)
-- import qualified Data.Vector as V

import GHC.Generics

import System.Random

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen
-- import Test.QuickCheck.Monadic
import Test.Tasty
-- import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
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

data ForkInfo = ForkInto
  { fiOldHeader :: BlockHeader
  , fiNewHeader :: BlockHeader
  , fiOldForkTrans :: Set TransactionHash
  , fiNewForkTrans :: Set TransactionHash
  , fiBothForksTrans :: Set TransactionHash
  }

-- processFork :: BlockHeaderDb -> BlockHeader -> Maybe BlockHeader -> IO (Vector TransactionHash)


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

genFork :: IO (Maybe ForkInfo)
genFork = do

    -- Build initial 10 linked headers
    -- let cid = someChainId testVersion
    -- let headers = V.fromList $ getBlockHeaders cid 10
    -- forM_ headers (putStrLn . show)

    -- Add some transactions to each of the initial headers

    -- Add these to the

    -- Choose a common branch point anywhere in the list (including the top for a "no fork" scenario)

    -- Add some new headers based at the common branch point

    -- Add some transactions to each of the new headers

    -- Build the ForkInfo containing everything generated above

    return Nothing
    {-
    return ForkInfo {
      { fioldheader =
      , finewheader =
      , fiOldForkTrans =
      , fiNewForkTrans =
      , fiBothForksTrans =
      }
     -}

getBlockHeaders :: ChainId -> Int -> [BlockHeader]
getBlockHeaders cid n = gbh0 : take (n - 1) (testBlockHeaders gbh0)
  where
    gbh0 = genesisBlockHeader testVersion cid

genTemp :: IO Int
genTemp = do
    z <- getStdRandom (randomR (1,6))
    return z

checkProp :: IO ()
checkProp = quickCheck prop_reverse

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

----------------------------------------------------------------------------------------------------
-- Fork generation
----------------------------------------------------------------------------------------------------
tree' :: ChainwebVersion -> Growth -> Set TransactionHash -> Gen (Tree BlockHeader)
tree' v g txs = do
    h <- genesis v

    -- TODO: create, add the transactions for the Node here
    --       & send the reduced set on...
    let txs' = txs

    Node h <$> forest' g h txs'


forest' :: Growth -> BlockHeader -> Set TransactionHash -> Gen (Forest BlockHeader)
forest' Randomly h txs = randomTrunk' h txs
forest' g@(AtMost n) h txs | n < _blockHeight h = pure []
                           | otherwise = fixedTrunk' g h txs


fixedTrunk' :: Growth -> BlockHeader -> Set TransactionHash -> Gen (Forest BlockHeader)
fixedTrunk' g h txs = frequency [ (1, sequenceA [fork' h txs, trunk' g h txs])
                           , (5, sequenceA [trunk' g h txs]) ]

randomTrunk' :: BlockHeader -> Set TransactionHash -> Gen (Forest BlockHeader)
randomTrunk' h txs = frequency [ (2, pure [])
                          , (4, sequenceA [fork' h txs, trunk' Randomly h txs])
                          , (18, sequenceA [trunk' Randomly h txs]) ]

fork' :: BlockHeader -> Set TransactionHash -> Gen (Tree BlockHeader)
fork' h txs = do
    next <- header' h
    Node next <$> frequency [ (1, pure []), (1, sequenceA [fork' next txs]) ]

trunk' :: Growth -> BlockHeader -> Set TransactionHash -> Gen (Tree BlockHeader)
trunk' g h txs = do
    next <- header' h
    Node next <$> forest' g next txs

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

  -- duplicated, not exported from Chaineweb.Test.Utils
genesis :: ChainwebVersion -> Gen BlockHeader
genesis v = either (error . show) return $ genesisBlockHeaderForChain v (0 :: Int)

newtype FakePayload = FakePayload BlockPayloadHash
    deriving (Show, Eq, Ord, Generic, Hashable)

instance IsCasValue FakePayload where
    type CasKeyType FakePayload = BlockPayloadHash
    casKey (FakePayload blockPlHash) = blockPlHash -- id
    -- casKey :: FakePayload -> CasKeyType FakePayload

newFakePayloadDb :: IO (C.HashMapCas FakePayload)
newFakePayloadDb = C.emptyCas

insertFakePayload :: C.HashMapCas FakePayload -> FakePayload -> IO ()
insertFakePayload db hash =  casInsert db hash

queryFakePayload :: C.HashMapCas FakePayload -> BlockPayloadHash -> IO (Maybe FakePayload)
queryFakePayload db hash = casLookup db hash

payloadDbToList :: C.HashMapCas FakePayload -> IO [FakePayload]
payloadDbToList = C.toList
