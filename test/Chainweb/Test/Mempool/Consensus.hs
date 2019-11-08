{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Mempool.Consensus
  ( tests
  ) where

import Control.Monad.IO.Class

import Data.CAS.RocksDB
import Data.Foldable (toList)
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.IORef
import qualified Data.Vector as V

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic
import Test.Tasty.QuickCheck

-- internal modules
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.Mempool
import Chainweb.Test.ForkGen
import Chainweb.Test.Utils

import Data.LogMessage

----------------------------------------------------------------------------------------------------
tests :: BlockHeaderDb -> BlockHeader -> ScheduledTest
tests db h0 = testGroupSch "mempool-consensus-quickcheck-tests"
    [ testProperty "valid-transactions-source" (prop_validTxSource db h0)
    , testProperty "no-orphaned-txs" (prop_noOrphanedTxs db h0)
    , testProperty "test-processfork-filter" (prop_noOldCrap db h0)
    ]

----------------------------------------------------------------------------------------------------
-- | Property: All transactions returned by processFork (for re-introduction to the mempool) come from
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
                                    (const True)
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
                                    (const True)
    let reIntroTrans = HS.fromList $ V.toList reIntroTransV
    let expectedTrans = fiOldForkTrans `HS.difference` fiNewForkTrans

    assert $ expectedTrans `isSubsetOf` reIntroTrans


----------------------------------------------------------------------------------------------------
-- Tests filtering within processFork'.
prop_noOldCrap
    :: BlockHeaderDb
    -> BlockHeader
    -> Property
prop_noOldCrap db genBlock = monadicIO $ do
    -- tests filtering, in theory on age
    mapRef <- liftIO $ newIORef (HM.empty :: HashMap BlockHeader (HashSet TransactionHash))
    ForkInfo{..} <- genFork db mapRef genBlock
    pre (length fiOldForkTrans > 0)
    let badtx = head $ toList fiOldForkTrans
    let badFilter = (/= badtx)
    (reIntroTransV, _) <- run $ processFork' (alogFunction aNoLog) fiBlockHeaderDb
                                    fiNewHeader (Just fiOldHeader) (lookupFunc mapRef)
                                    badFilter
    let reIntroTrans = HS.fromList $ V.toList reIntroTransV
    assert $ not $ HS.member badtx reIntroTrans



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
_runGhci :: IO ()
_runGhci =
    withTempRocksDb "mempool-consensus-test" $ \rdb ->
    withToyDB rdb toyChainId $ \h0 db -> do
        quickCheck (prop_validTxSource db h0)
        quickCheck (prop_noOrphanedTxs db h0)
        return ()
