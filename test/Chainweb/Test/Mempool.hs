-- | Tests and test infrastructure common to all mempool backends.
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chainweb.Test.Mempool
  ( tests
  , remoteTests
  , MempoolWithFunc(..)
  , InsertCheck
  , mempoolTestCase
  , mempoolProperty
  , lookupIsPending
  , lookupIsMissing
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Decimal (Decimal)
import Data.Function (on)
import Data.Int (Int64)
import Data.IORef
import Data.List (sort, sortBy)
import qualified Data.List.Ordered as OL
import Data.Ord (Down(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (lookup)
import System.Timeout (timeout)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen (Gen, chooseAny)
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))

-- internal modules

import Pact.Parse (ParsedDecimal(..))
import Pact.Types.Gas (GasPrice(..))

import Chainweb.BlockHash
import Chainweb.Mempool.Mempool
import Chainweb.Mempool.InMemTypes (Validators)
import qualified Chainweb.Time as Time

------------------------------------------------------------------------------
-- | Several operations (reintroduce, validate, confirm) can only be performed
-- by the local consensus layer and are not supported for remote mempools. The
-- following test cases are safe to run on remote
remoteTests :: MempoolWithFunc -> [TestTree]
remoteTests withMempool = map ($ withMempool) [
      mempoolTestCase "nil case (startup/destroy)" testStartup
    , mempoolProperty "overlarge transactions are rejected" genTwoSets
                      propOverlarge
    , mempoolProperty "pre-insert checks" genTwoSets propPreInsert
    , mempoolProperty "insert + lookup + getBlock" genNonEmpty propTrivial
    , mempoolProperty "getPending" genNonEmpty propGetPending
    , mempoolProperty "getPending high water marks" hwgen propHighWater
    ]
  where
    hwgen = do
      (xs, ys0) <- genTwoSets
      let ys = take 1000 ys0     -- recency log only has so many entries
      return (xs, ys)

genNonEmpty :: PropertyM IO [MockTx]
genNonEmpty = do
    xs <- pick arbitrary
    pre (not $ null xs)
    return xs

genTwoSets :: PropertyM IO ([MockTx], [MockTx])
genTwoSets = do
    xs <- uniq . sortBy ord <$> genNonEmpty
    ys' <- uniq . sortBy ord <$> genNonEmpty
    let ys = OL.minusBy' ord ys' xs
    pre (not $ null ys)
    return (xs, ys)
  where
    ord = compare `on` onFees
    onFees x = (Down (mockGasPrice x), mockGasLimit x, mockNonce x)

-- TODO: remove "remoteTests" here and fold in the test funcs, since there is
-- no longer a distinction
tests :: MempoolWithFunc -> [TestTree]
tests withMempool = remoteTests withMempool

arbitraryDecimal :: Gen Decimal
arbitraryDecimal = do
    i <- (arbitrary :: Gen Int64)
    return $! fromInteger $ toInteger i

arbitraryGasPrice :: Gen GasPrice
arbitraryGasPrice = GasPrice . ParsedDecimal . abs <$> arbitraryDecimal

instance Arbitrary MockTx where
  arbitrary = MockTx <$> chooseAny
                        <*> arbitraryGasPrice
                        <*> pure mockBlockGasLimit
                        <*> pure emptyMeta
    where
      emptyMeta = TransactionMetadata zero Time.maxTime
      zero = Time.Time (Time.TimeSpan (Time.Micros 0))

type InsertCheck = MVar (Validators MockTx -> Vector MockTx -> IO (Vector Bool))
data MempoolWithFunc =
    MempoolWithFunc (forall a
                     . ((InsertCheck -> MempoolBackend MockTx -> IO a)
                       -> IO a))

mempoolTestCase :: TestName
                -> (InsertCheck -> MempoolBackend MockTx -> IO ())
                -> MempoolWithFunc
                -> TestTree
mempoolTestCase name test (MempoolWithFunc withMempool) =
    testCase name $ tout $ withMempool test
  where
    tout m = timeout 60000000 m >>= maybe (fail "timeout") return


mempoolProperty
    :: TestName
    -> PropertyM IO a
    -> (a -> InsertCheck -> MempoolBackend MockTx -> IO (Either String ()))
    -> MempoolWithFunc
    -> TestTree
mempoolProperty name gen test (MempoolWithFunc withMempool) = testProperty name go
  where
    go = monadicIO (gen >>= run . tout . withMempool . test
                        >>= either fail return)

    tout m = timeout 60000000 m >>= maybe (fail "timeout") return

testStartup :: InsertCheck -> MempoolBackend MockTx -> IO ()
testStartup = const $ const $ return ()


propOverlarge
    :: ([MockTx], [MockTx])
    -> InsertCheck
    -> MempoolBackend MockTx
    -> IO (Either String ())
propOverlarge (txs, overlarge0) _ mempool = runExceptT $ do
    liftIO $ insert $ txs ++ overlarge
    liftIO (lookup txs) >>= V.mapM_ lookupIsPending
    liftIO (lookup overlarge) >>= V.mapM_ lookupIsMissing
  where
    txcfg = mempoolTxConfig mempool
    hash = txHasher txcfg
    insert v = mempoolInsert mempool CheckedInsert $ V.fromList v
    lookup = mempoolLookup mempool . V.fromList . map hash
    overlarge = setOverlarge overlarge0
    setOverlarge = map (\x -> x { mockGasLimit = mockBlockGasLimit + 100 })

propPreInsert
    :: ([MockTx], [MockTx])
    -> InsertCheck
    -> MempoolBackend MockTx
    -> IO (Either String ())
propPreInsert (txs, badTxs) gossipMV mempool =
   bracket (readMVar gossipMV)
           (\old -> modifyMVar_ gossipMV (const $ return old))
           (const go)

  where
    go = runExceptT $ do
        liftIO $ modifyMVar_ gossipMV $ const $ return checkNotBad
        liftIO $ insert $ txs ++ badTxs
        liftIO (lookup txs) >>= V.mapM_ lookupIsPending
        liftIO (lookup badTxs) >>= V.mapM_ lookupIsMissing
    txcfg = mempoolTxConfig mempool
    hash = txHasher txcfg
    insert v = mempoolInsert mempool CheckedInsert $ V.fromList v
    lookup = mempoolLookup mempool . V.fromList . map hash
    checkNotBad _ xs = return $! V.map (not . (`elem` badTxs)) xs


propTrivial
    :: [MockTx]
    -> InsertCheck
    -> MempoolBackend MockTx
    -> IO (Either String ())
propTrivial txs _ mempool = runExceptT $ do
    liftIO $ insert txs
    liftIO (lookup txs) >>= V.mapM_ lookupIsPending
    block <- liftIO getBlock
    when (not $ isSorted block) $
        fail ("getBlock didn't return a sorted block: " ++ show block)

  where
    isSorted xs = let fs = V.map onFees xs
                      ffs = V.zipWith (<=) fs (V.drop 1 fs)
                  in V.and ffs
    txcfg = mempoolTxConfig mempool
    hash = txHasher txcfg
    insert v = mempoolInsert mempool CheckedInsert $ V.fromList v
    lookup = mempoolLookup mempool . V.fromList . map hash

    getBlock = mempoolGetBlock mempool noopMempoolPreBlockCheck 0 nullBlockHash
                               (mempoolBlockGasLimit mempool)
    onFees x = (Down (mockGasPrice x), mockGasLimit x)


propGetPending
    :: [MockTx]
    -> InsertCheck
    -> MempoolBackend MockTx
    -> IO (Either String ())
propGetPending txs0 _ mempool = runExceptT $ do
    liftIO $ insert txs
    pendingOps <- liftIO $ newIORef []
    void $ liftIO $ getPending Nothing $ \v -> modifyIORef' pendingOps (v:)
    allPending <- sort . V.toList . V.concat
                  <$> liftIO (readIORef pendingOps)

    when (txdata /= allPending) $
        let msg = concat [ "getPendingTransactions failure: expected pending "
                         , "list:\n    "
                         , show txdata
                         , "\nbut got:\n    "
                         , show allPending ]
        in fail msg
  where
    txs = uniq $ sortBy (compare `on` onFees) txs0
    txdata = sort $ map hash txs
    onFees x = (Down (mockGasPrice x), mockGasLimit x, mockNonce x)
    hash = txHasher $ mempoolTxConfig mempool
    getPending = mempoolGetPendingTransactions mempool
    insert v = mempoolInsert mempool CheckedInsert $ V.fromList v

propHighWater
    :: ([MockTx], [MockTx])
    -> InsertCheck
    -> MempoolBackend MockTx
    -> IO (Either String ())
propHighWater (txs0, txs1) _ mempool = runExceptT $ do
    liftIO $ insert txs0
    hw <- liftIO $ getPending Nothing $ const (return ())
    liftIO $ insert txs1
    pendingOps <- liftIO $ newIORef []
    void $ liftIO $ getPending (Just hw) $ \v -> modifyIORef' pendingOps (v:)
    allPending <- sort . V.toList . V.concat
                  <$> liftIO (readIORef pendingOps)
    when (txdata /= allPending) $
        let msg = concat [ "getPendingTransactions highwater failure: "
                         , "expected new pending list:\n    "
                         , show txdata
                         , "\nbut got:\n    "
                         , show allPending
                         , "\nhw was: ", show hw
                         , ", txs0 size: ", show (length txs0)
                         ]
        in fail msg

  where
    txdata = sort $ map hash txs1
    hash = txHasher $ mempoolTxConfig mempool
    getPending = mempoolGetPendingTransactions mempool
    insert txs = mempoolInsert mempool CheckedInsert $ V.fromList txs


uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq l@[_] = l
uniq (x:ys@(y:rest)) | x == y    = uniq (x:rest)
                     | otherwise = x : uniq ys


lookupIsPending :: Monad m => LookupResult t -> m ()
lookupIsPending (Pending _) = return ()
lookupIsPending _ = fail "lookup failure: expected pending"

lookupIsMissing :: Monad m => LookupResult t -> m ()
lookupIsMissing Missing = return ()
lookupIsMissing _ = fail "lookup failure: expected missing"
