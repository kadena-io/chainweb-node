{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Tests and test infrastructure common to all mempool backends.

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
import Data.Bifunctor (bimap)
import Data.Decimal (Decimal)
import Data.Function (on)
import qualified Data.HashSet as HashSet
import Data.Int (Int64)
import Data.IORef
import Data.List (sort, sortBy)
import qualified Data.List.Ordered as OL
import Data.Ord (Down(..))
import Data.Tuple.Strict (T2(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Stack
import Prelude hiding (lookup)
import System.Timeout (timeout)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen (chooseAny)
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))

-- internal modules

import Pact.Parse (ParsedDecimal(..))
import Pact.Types.Gas (GasPrice(..))

import Chainweb.BlockHash
import Chainweb.Mempool.Mempool
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
    -- , mempoolProperty "getPending high water marks" hwgen propHighWater
    ]
  where
    _hwgen = do
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

tests :: MempoolWithFunc -> [TestTree]
tests withMempool = remoteTests withMempool ++
                    map ($ withMempool) [
      mempoolProperty "getblock badlist" genTwoSets propBadlist
    ]

arbitraryDecimal :: Gen Decimal
arbitraryDecimal = do
    i <- (arbitrary :: Gen Int64)
    return $! fromInteger $ toInteger i

arbitraryGasPrice :: Gen GasPrice
arbitraryGasPrice = GasPrice . ParsedDecimal . abs <$> arbitraryDecimal

instance Arbitrary MockTx where
  arbitrary = MockTx
      <$> chooseAny
      <*> arbitraryGasPrice
      <*> pure mockBlockGasLimit
      <*> pure emptyMeta
    where
      emptyMeta = TransactionMetadata zero Time.maxTime
      zero = Time.Time (Time.TimeSpan (Time.Micros 0))

type BatchCheck =
    Vector (T2 TransactionHash MockTx)
    -> IO (V.Vector (Either (T2 TransactionHash InsertError)
                            (T2 TransactionHash MockTx)))

-- | We use an `MVar` so that tests can override/modify the instance's insert
-- check for tests. To make quickcheck testing not take forever for remote, we
-- run the server and client mempools in a resource pool -- the test pulls an
-- already running instance from the pool so we need a way to twiddle its check
-- function for the tests.
--
type InsertCheck = MVar BatchCheck

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
    go = monadicIO (gen >>= run . tout . withMempool . test >>= either fail return)

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

propBadlist
    :: ([MockTx], [MockTx])
    -> InsertCheck
    -> MempoolBackend MockTx
    -> IO (Either String ())
propBadlist (txs, badTxs) _ mempool = runExceptT $ do
    liftIO $ insert $ txs ++ badTxs
    liftIO (lookup txs) >>= V.mapM_ lookupIsPending

    -- we will accept the bad txs initially
    liftIO (lookup badTxs) >>= V.mapM_ lookupIsPending

    -- once we call mempoolGetBlock, the bad txs should be badlisted
    liftIO $ void $ mempoolGetBlock mempool preblockCheck 1 nullBlockHash 10000000
    liftIO (lookup txs) >>= V.mapM_ lookupIsPending
    liftIO (lookup badTxs) >>= V.mapM_ lookupIsMissing
    liftIO $ insert badTxs
    liftIO (lookup badTxs) >>= V.mapM_ lookupIsMissing

  where
    badHashes = HashSet.fromList $ map hash badTxs

    preblockCheck _ _ ts =
      let hashes = V.map hash ts
      in return $! V.map (not . flip HashSet.member badHashes) hashes

    txcfg = mempoolTxConfig mempool
    hash = txHasher txcfg
    insert v = mempoolInsert mempool CheckedInsert $ V.fromList v
    lookup = mempoolLookup mempool . V.fromList . map hash

-- TODO Does this need to be updated?
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

    checkOne :: MockTx -> Either InsertError MockTx
    checkOne tx
        | tx `elem` badTxs = Left InsertErrorBadlisted
        | otherwise = Right tx

    checkNotBad
        :: Vector (T2 TransactionHash MockTx)
        -> IO (V.Vector (Either (T2 TransactionHash InsertError)
                                (T2 TransactionHash MockTx)))
    checkNotBad = pure . V.map (\(T2 h tx) -> bimap (T2 h) (T2 h) $ checkOne tx)


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

_propHighWater
    :: ([MockTx], [MockTx])
    -> InsertCheck
    -> MempoolBackend MockTx
    -> IO (Either String ())
_propHighWater (txs0, txs1) _ mempool = runExceptT $ do
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


lookupIsPending :: HasCallStack => MonadIO m => LookupResult t -> m ()
lookupIsPending (Pending _) = return ()
lookupIsPending _ = liftIO $ fail "lookup failure: expected pending"

lookupIsMissing :: HasCallStack => MonadIO m => LookupResult t -> m ()
lookupIsMissing Missing = return ()
lookupIsMissing _ = liftIO $ fail "lookup failure: expected missing"
