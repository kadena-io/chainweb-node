-- | Tests and test infrastructure common to all mempool backends.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chainweb.Test.Mempool
  ( tests
  , remoteTests
  , MempoolWithFunc(..)
  , mempoolTestCase
  , mempoolProperty
  , lookupIsPending
  , lookupIsValidated
  , lookupIsConfirmed
  , lookupIsMissing
  ) where

import Control.Applicative
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (Concurrently(..))
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan as TBMChan
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Decimal (Decimal)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.Int (Int64)
import Data.IORef
import Data.List (sort, sortBy)
import qualified Data.List.Ordered as OL
import Data.Ord (Down(..))
import qualified Data.Vector as V
import Prelude hiding (lookup)
import System.Timeout (timeout)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen (Gen, chooseAny, generate, resize)
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))

import Chainweb.Mempool.Mempool
import qualified Chainweb.Time as Time
import qualified Numeric.AffineSpace as AF

------------------------------------------------------------------------------
-- | Several operations (reintroduce, validate, confirm) can only be performed
-- by the local consensus layer and are not supported for remote mempools. The
-- following test cases are safe to run on remote
remoteTests :: MempoolWithFunc -> [TestTree]
remoteTests withMempool = map ($ withMempool) [
      mempoolTestCase "nil case (startup/destroy)" testStartup
    , mempoolProperty "overlarge transactions are rejected" (pick arbitrary)
                      propOverlarge
    , mempoolProperty "insert + lookup + getBlock" (pick arbitrary) propTrivial
    , mempoolProperty "get all pending transactions" (pick arbitrary) propGetPending
    , mempoolProperty "single subscription" (do
          xs <- pick arbitrary
          pre (length xs > 0)
          return xs) propSubscription
    ]

-- | Local-only tests plus all of the tests that are safe for remote.
tests :: MempoolWithFunc -> [TestTree]
tests withMempool = map ($ withMempool) [
      mempoolProperty "validate txs" (pick arbitrary) propValidate
    , mempoolProperty "multiple subscriptions"  (do
          xs <- pick arbitrary
          pre (length xs > 0)
          return xs) propSubscriptions
    , mempoolTestCase "old transactions are reaped" testTooOld
    ] ++ remoteTests withMempool

arbitraryDecimal :: Gen Decimal
arbitraryDecimal = do
    i <- (arbitrary :: Gen Int64)
    return $! fromInteger $ toInteger i


instance Arbitrary MockTx where
  arbitrary = let g x = choose (1, x)
              in MockTx <$> chooseAny
                        <*> arbitraryDecimal
                        <*> g mockBlocksizeLimit
                        <*> pure emptyMeta
    where
      emptyMeta = TransactionMetadata Time.minTime Time.maxTime

data MempoolWithFunc = MempoolWithFunc (forall a . (MempoolBackend MockTx -> IO a) -> IO a)

mempoolTestCase :: TestName
                -> (MempoolBackend MockTx -> IO ())
                -> MempoolWithFunc
                -> TestTree
mempoolTestCase name test (MempoolWithFunc withMempool) =
    testCase name $ tout $ withMempool test
  where
    tout m = timeout 60000000 m >>= maybe (fail "timeout") return


mempoolProperty :: TestName
                -> PropertyM IO a
                -> (a -> MempoolBackend MockTx -> IO (Either String ()))
                -> MempoolWithFunc
                -> TestTree
mempoolProperty name gen test (MempoolWithFunc withMempool) = testProperty name go
  where
    go = monadicIO (gen >>= run . tout . withMempool . test
                        >>= either fail return)

    tout m = timeout 60000000 m >>= maybe (fail "timeout") return

testStartup :: MempoolBackend MockTx -> IO ()
testStartup = const $ return ()

testTooOld :: MempoolBackend MockTx -> IO ()
testTooOld mempool = do
    -- TODO: improve this test. Testing via threadDelay is flaky.
    --
    -- We have to wait for pruning to run, so it's difficult to test this more
    -- correctly without adding a method to trigger pruning manually (and wait
    -- for prune to complete)
    now <- Time.getCurrentTimeIntegral
    txs <- sortTxs <$> generate (resize 1000 $ arbitrary :: Gen [MockTx])
    tooOld <- overrideAge now <$> generate (resize 100 $ arbitrary :: Gen [MockTx])

    insert $ txs ++ tooOld
    threadDelay 1000000   -- 1 second should be enough to trigger pruning,
                          -- expiry is set 75ms in the future and prune runs at
                          -- 100Hz
    runE "existing lookups still exist" $
        (liftIO (lookup txs) >>= V.mapM_ lookupIsPending)
    runE "too old lookups were pruned" $
        (liftIO (lookup tooOld) >>= V.mapM_ lookupIsMissing)
  where
    runE msg m = runExceptT m >>=
                 either (\s -> fail $ "expecting: " ++ msg ++ ": " ++ s) (const $ return ())
    txcfg = mempoolTxConfig mempool
    hash = txHasher txcfg
    insert = mempoolInsert mempool . V.fromList
    lookup = mempoolLookup mempool . V.fromList . map hash
    sortTxs = uniq . sortBy (compare `on` onFees)
    overrideAge now = sortTxs . map (setTooOld now)
    extendTime now = Time.TimeSpan 75000 `AF.add` now
    setTooOld now x = x { mockMeta = (mockMeta x) { txMetaExpiryTime = extendTime now } }
    onFees x = (Down (mockFees x), mockSize x, mockNonce x)

propOverlarge :: ([MockTx], [MockTx]) -> MempoolBackend MockTx -> IO (Either String ())
propOverlarge (txs0, overlarge0) mempool = runExceptT $ do
    liftIO $ insert $ txs ++ overlarge
    liftIO (lookup txs) >>= V.mapM_ lookupIsPending
    liftIO (lookup overlarge) >>= V.mapM_ lookupIsMissing
  where
    txcfg = mempoolTxConfig mempool
    hash = txHasher txcfg
    insert = mempoolInsert mempool . V.fromList
    lookup = mempoolLookup mempool . V.fromList . map hash
    txs = uniq $ sortBy (compare `on` onFees) txs0
    overlarge = setOverlarge $ uniq $ sortBy (compare `on` onFees) overlarge0

    setOverlarge = map (\x -> x { mockSize = mockBlocksizeLimit + 100 })
    onFees x = (Down (mockFees x), mockSize x, mockNonce x)


propTrivial :: [MockTx] -> MempoolBackend MockTx -> IO (Either String ())
propTrivial txs mempool = runExceptT $ do
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
    insert = mempoolInsert mempool . V.fromList
    lookup = mempoolLookup mempool . V.fromList . map hash

    getBlock = mempoolGetBlock mempool (mempoolBlockSizeLimit mempool)
    onFees x = (Down (mockFees x), mockSize x)


propGetPending :: [MockTx] -> MempoolBackend MockTx -> IO (Either String ())
propGetPending txs0 mempool = runExceptT $ do
    let txs = uniq $ sortBy (compare `on` onFees) txs0
    liftIO $ insert txs
    let txdata = sort $ map hash txs
    pendingOps <- liftIO $ newIORef []
    liftIO $ getPending $ \v -> modifyIORef' pendingOps (v:)
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
    onFees x = (Down (mockFees x), mockSize x, mockNonce x)
    hash = txHasher $ mempoolTxConfig mempool
    getPending = mempoolGetPendingTransactions mempool
    insert = mempoolInsert mempool . V.fromList


uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq l@[_] = l
uniq (x:ys@(y:rest)) | x == y    = uniq (x:rest)
                     | otherwise = x : uniq ys

propValidate :: ([MockTx], [MockTx]) -> MempoolBackend MockTx -> IO (Either String ())
propValidate (txs0', txs1') mempool = runExceptT $ do
    let txs0 = uniq $ sortBy ord txs0'
    let txs1 = OL.minusBy' ord (uniq $ sortBy ord txs1') txs0
    insert txs0
    insert txs1
    lookup txs0 >>= V.mapM_ lookupIsPending
    lookup txs1 >>= V.mapM_ lookupIsPending

    markValidated txs1
    lookup txs0 >>= V.mapM_ lookupIsPending
    lookup txs1 >>= V.mapM_ lookupIsValidated

    reintroduce txs1
    lookup txs1 >>= V.mapM_ lookupIsPending

    markConfirmed txs1
    lookup txs1 >>= V.mapM_ lookupIsConfirmed

  where
    ord = compare `on` onFees
    onFees x = (Down (mockFees x), mockSize x, mockNonce x)
    hash = txHasher $ mempoolTxConfig mempool
    insert = liftIO . mempoolInsert mempool . V.fromList
    lookup = liftIO . mempoolLookup mempool . V.fromList . map hash
    markValidated = liftIO . mempoolMarkValidated mempool . V.fromList . map validate
    reintroduce = liftIO . mempoolReintroduce mempool . V.fromList . map hash
    markConfirmed = liftIO . mempoolMarkConfirmed mempool . V.fromList . map hash

    -- TODO: empty forks here
    validate = ValidatedTransaction V.empty

lookupIsPending :: Monad m => LookupResult t -> m ()
lookupIsPending (Pending _) = return ()
lookupIsPending _ = fail "lookup failure: expected pending"

lookupIsConfirmed :: Monad m => LookupResult t -> m ()
lookupIsConfirmed Confirmed = return ()
lookupIsConfirmed _ = fail "lookup failure: expected confirmed"

lookupIsMissing :: Monad m => LookupResult t -> m ()
lookupIsMissing Missing = return ()
lookupIsMissing _ = fail "lookup failure: expected missing"

lookupIsValidated :: Monad m => LookupResult t -> m ()
lookupIsValidated (Validated _) = return ()
lookupIsValidated _ = fail "lookup failure: expected validated"

chunk :: Int -> [a] -> [[a]]
chunk k l = if null l
              then []
              else let a = take k l
                       b = drop k l
                   in a : (if null b then [] else chunk k b)


propSubscription :: [MockTx] -> MempoolBackend MockTx -> IO (Either String ())
propSubscription txs0 mempool = runExceptT $ do
    sub <- subscribe
    mv <- liftIO newEmptyMVar
    result <- liftIO $ Async.runConcurrently (
        insertThread mv *> Concurrently (mockSubscriber (length txChunks) sub mv))
    checkResult result

  where
    txs = uniq $ sortBy ord txs0
    txChunks = chunk 64 txs
    checkResult = checkSubscriptionResult txs
    ord = compare `on` onFees
    onFees x = (Down (mockFees x), mockSize x, mockNonce x)
    insert = liftIO . mempoolInsert mempool . V.fromList
    subscribe = liftIO $ mempoolSubscribe mempool
    insertThread mv = Concurrently $ do
        traverse_ insert txChunks
        takeMVar mv


checkSubscriptionResult :: [MockTx] -> [MockTx] -> ExceptT String IO ()
checkSubscriptionResult txs r =
    when (r /= txs) $
    let msg = concat [ "subscription failed: expected sequence "
                     , show txs
                     , "got "
                     , show r
                     ]
    in fail msg


mockSubscriber :: Int -> IORef (Subscription a) -> MVar () -> IO [a]
mockSubscriber sz subRef mv = do
    sub <- readIORef subRef
    ref <- newIORef []
    go sz ref $ mempoolSubChan sub
    out <- reverse <$> readIORef ref
    -- make sure we touch the ref so it isn't gc'ed
    writeIORef subRef sub
    return out
  where
    go !k ref chan = do
        m <- atomically $ TBMChan.readTBMChan chan
        flip (maybe (return ())) m $ \el -> do
            let l = reverse $ V.toList el
            modifyIORef' ref (l ++)
            if (k <= 1)
              then putMVar mv ()
              else go (k - 1) ref chan

-- In-mem backend supports multiple subscriptions at once, socket-based one
-- doesn't (you'd just connect twice)
propSubscriptions :: [MockTx] -> MempoolBackend MockTx -> IO (Either String ())
propSubscriptions txs0 mempool = runExceptT $ do
    subscriptions <- replicateM numSubscribers subscribe
    mvs <- replicateM numSubscribers (liftIO newEmptyMVar)
    results <- liftIO $ Async.runConcurrently (insertThread mvs *>
                                               runSubscribers mvs subscriptions)
    mapM_ checkResult results

  where
    txs = uniq $ sortBy ord txs0
    txChunks = chunk 64 txs
    checkResult = checkSubscriptionResult txs
    numSubscribers = 7
    ord = compare `on` onFees
    onFees x = (Down (mockFees x), mockSize x, mockNonce x)
    insert = liftIO . mempoolInsert mempool . V.fromList
    subscribe = liftIO $ mempoolSubscribe mempool
    insertThread mvs = Concurrently $ do
        traverse_ insert txChunks
        for_ mvs takeMVar

    runSubscribers :: [MVar ()] -> [IORef (Subscription MockTx)] -> Concurrently [[MockTx]]
    runSubscribers mvs subscriptions =
        Concurrently $ Async.forConcurrently (subscriptions `zip` mvs)
                     $ \(s,mv) -> mockSubscriber (length txChunks) s mv

