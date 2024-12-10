{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Bifunctor (bimap)
import Data.Decimal (Decimal, DecimalRaw(..))
import Data.Function (on)
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.List (sort, sortBy)
import qualified Data.List.Ordered as OL
import Data.Ord (Down(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Stack
import Prelude hiding (lookup)
import System.Timeout (timeout)
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))

-- internal modules

import Pact.Parse (ParsedDecimal(..))

import Chainweb.BlockHash
import Chainweb.Mempool.Mempool
import Chainweb.Test.Utils
import qualified Chainweb.Time as Time
import Chainweb.Utils (T2(..))

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
    hwgen :: PropertyM IO ([MockTx], [MockTx])
    hwgen = do
      (xs, ys0) <- genTwoSets
      let ys = take 1000 ys0     -- recency log only has so many entries
      return (xs, ys)

genNonEmpty :: PropertyM IO [MockTx]
genNonEmpty = do
    now <- liftIO Time.getCurrentTimeIntegral
    xs <- pick arbitrary
    pre (not $ null xs)
    return $ map (updMockMeta now) xs
  where
    updMockMeta now x =
        let meta = mockMeta x
            meta' = meta {
                txMetaCreationTime = now,
                txMetaExpiryTime = Time.add (Time.secondsToTimeSpan (Time.Seconds 600)) now
                }
        in x { mockMeta = meta' }

genTwoSets :: PropertyM IO ([MockTx], [MockTx])
genTwoSets = do
    xs <- uniq . sortBy ord <$> genNonEmpty
    ys' <- uniq . sortBy ord <$> genNonEmpty
    let ys = OL.minusBy' ord ys' xs
    pre (not $ null ys)
    return (xs, ys)
  where
    ord = compare `on` onFees
    onFees x = (Down (mockGasPrice x), mockNonce x, mockGasLimit x)

tests :: MempoolWithFunc -> [TestTree]
tests withMempool = remoteTests withMempool ++
                    map ($ withMempool) [
      mempoolProperty "getblock preblock check badlist" genTwoSets propBadlistPreblock
    , mempoolProperty "mempoolAddToBadList" (pick arbitrary) propAddToBadList
    ]

arbitraryDecimal :: Gen Decimal
arbitraryDecimal = do
    places <- choose (3, 8)
    mantissa <- choose (1, 8)
    return $! Decimal places mantissa

arbitraryGasPrice :: Gen GasPrice
arbitraryGasPrice = GasPrice . ParsedDecimal . abs <$> arbitraryDecimal

instance Arbitrary MockTx where
  arbitrary = MockTx
      <$> chooseAny
      <*> arbitraryGasPrice
      <*> pure (mockBlockGasLimit `div` 50_000)
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

newtype MempoolWithFunc =
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
    tout m = timeout 60_000_000 m >>= maybe (fail "timeout") return


mempoolProperty
    :: TestName
    -> PropertyM IO a
    -> (a -> InsertCheck -> MempoolBackend MockTx -> IO (Either String ()))
    -> MempoolWithFunc
    -> TestTree
mempoolProperty name gen test (MempoolWithFunc withMempool) = testProperty name go
  where
    go = monadicIO (gen >>= run . tout . withMempool . test >>= either fail return)

    tout m = timeout 60_000_000 m >>= maybe (fail "timeout") return

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

propBadlistPreblock
    :: ([MockTx], [MockTx])
    -> InsertCheck
    -> MempoolBackend MockTx
    -> IO (Either String ())
propBadlistPreblock (txs, badTxs) _ mempool = runExceptT $ do
    liftIO $ insert $ txs ++ badTxs
    liftIO (lookup txs) >>= V.mapM_ lookupIsPending

    -- we will accept the bad txs initially
    liftIO (lookup badTxs) >>= V.mapM_ lookupIsPending

    -- once we call mempoolGetBlock, the bad txs should be badlisted
    liftIO $ void $ mempoolGetBlock mempool mockBlockFill preblockCheck 1 nullBlockHash
    liftIO (lookup txs) >>= V.mapM_ lookupIsPending
    liftIO (lookup badTxs) >>= V.mapM_ lookupIsMissing
    liftIO $ insert badTxs
    liftIO (lookup badTxs) >>= V.mapM_ lookupIsMissing

  where
    badHashes = HashSet.fromList $ map hash badTxs

    preblockCheck _ _ ts = return $
      V.map
        (\tx -> if hash tx `HashSet.member` badHashes then Left InsertErrorBadlisted else Right tx)
        ts

    txcfg = mempoolTxConfig mempool
    hash = txHasher txcfg
    insert v = mempoolInsert mempool CheckedInsert $ V.fromList v
    lookup = mempoolLookup mempool . V.fromList . map hash

propAddToBadList
  :: MockTx
  -> InsertCheck
  -> MempoolBackend MockTx
  -> IO (Either String ())
propAddToBadList tx _ mempool = runExceptT $ do
    liftIO (insert [tx])
    liftIO (lookup [tx]) >>= V.mapM_ lookupIsPending

    block <- getBlock
    when (block /= [tx]) $ fail "expected to get our tx back"

    liftIO $ mempoolAddToBadList mempool $ V.singleton $ hash tx
    block' <- getBlock
    when (block' /= []) $ fail "expected to get an empty block"
    liftIO (lookup [tx]) >>= V.mapM_ lookupIsMissing
    liftIO (insert [tx])
    liftIO (lookup [tx]) >>= V.mapM_ lookupIsMissing

  where
    txcfg = mempoolTxConfig mempool
    hash = txHasher txcfg
    insert v = mempoolInsert mempool CheckedInsert $ V.fromList v
    lookup = mempoolLookup mempool . V.fromList . map hash
    getBlock = liftIO
      $ V.toList <$> mempoolGetBlock mempool mockBlockFill noopMempoolPreBlockCheck 1 nullBlockHash

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
    unless (isSorted block) $
        fail ("getBlock didn't return a sorted block: " ++ show block)

  where
    isSorted xs = let fs = V.map onFees xs
                      ffs = V.zipWith (<=) fs (V.drop 1 fs)
                  in V.and ffs
    txcfg = mempoolTxConfig mempool
    hash = txHasher txcfg
    insert v = mempoolInsert mempool CheckedInsert $ V.fromList v
    lookup = mempoolLookup mempool . V.fromList . map hash

    getBlock = mempoolGetBlock mempool mockBlockFill noopMempoolPreBlockCheck 0 nullBlockHash
    onFees x = (Down (mockGasPrice x))


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
    pendingOps0 <- liftIO $ newIORef []
    hw <- liftIO $ getPending Nothing $ \v -> modifyIORef' pendingOps0 (v:)
    p0s <- V.concat <$> (liftIO $ readIORef pendingOps0)
    liftIO $ insert txs1
    pendingOps <- liftIO $ newIORef []
    hw1 <- liftIO $ getPending (Just hw) $ \v -> modifyIORef' pendingOps (v:)
    allPending <- sort . V.toList . V.concat
                  <$> liftIO (readIORef pendingOps)
    when (txdata /= allPending && snd hw1 /= (fromIntegral $ length txs0 + length txdata)) $
        let msg = concat [ "highwater failure"
                         , ", initial batch was ", show (length txs0)
                         , ", retreived ", show (length p0s)
                         , ", with highwater ", show (snd hw)
                         , ". Second batch was ", show (length txdata)
                         , " retreived ", show (length allPending)
                         , ", with highwater ", show (snd hw1)
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
