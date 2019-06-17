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
  , mempoolTestCase
  , mempoolProperty
  , lookupIsPending
  , lookupIsValidated
  , lookupIsConfirmed
  , lookupIsMissing
  ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Decimal (Decimal)
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

-- internal modules

import Pact.Parse (ParsedDecimal(..))
import Pact.Types.Gas (GasPrice(..))

import Chainweb.BlockHeader
import Chainweb.Mempool.Mempool
import Chainweb.Test.Utils (toyChainId, toyGenesis)
import qualified Chainweb.Time as Time
import qualified Numeric.AffineSpace as AF

------------------------------------------------------------------------------
-- | Several operations (reintroduce, validate, confirm) can only be performed
-- by the local consensus layer and are not supported for remote mempools. The
-- following test cases are safe to run on remote
remoteTests :: MempoolWithFunc -> [TestTree]
remoteTests withMempool = map ($ withMempool) [
      mempoolTestCase "nil case (startup/destroy)" testStartup
    , mempoolProperty "overlarge transactions are rejected" genTwoSets
                      propOverlarge
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

-- | Local-only tests plus all of the tests that are safe for remote.
tests :: MempoolWithFunc -> [TestTree]
tests withMempool = map ($ withMempool) [
      mempoolProperty "validate txs" genTwoSets propValidate
    , mempoolTestCase "old transactions are reaped" testTooOld
    ] ++ remoteTests withMempool

arbitraryDecimal :: Gen Decimal
arbitraryDecimal = do
    i <- (arbitrary :: Gen Int64)
    return $! fromInteger $ toInteger i

arbitraryGasPrice :: Gen GasPrice
arbitraryGasPrice = GasPrice . ParsedDecimal . abs <$> arbitraryDecimal

instance Arbitrary MockTx where
  arbitrary = let g x = choose (1, x)
              in MockTx <$> chooseAny
                        <*> arbitraryGasPrice
                        <*> g mockBlockGasLimit
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
    onFees x = (Down (mockGasPrice x), mockGasLimit x, mockNonce x)

propOverlarge :: ([MockTx], [MockTx]) -> MempoolBackend MockTx -> IO (Either String ())
propOverlarge (txs, overlarge0) mempool = runExceptT $ do
    liftIO $ insert $ txs ++ overlarge
    liftIO (lookup txs) >>= V.mapM_ lookupIsPending
    liftIO (lookup overlarge) >>= V.mapM_ lookupIsMissing
  where
    txcfg = mempoolTxConfig mempool
    hash = txHasher txcfg
    insert = mempoolInsert mempool . V.fromList
    lookup = mempoolLookup mempool . V.fromList . map hash
    overlarge = setOverlarge overlarge0
    setOverlarge = map (\x -> x { mockGasLimit = mockBlockGasLimit + 100 })


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

    getBlock = mempoolGetBlock mempool (mempoolBlockGasLimit mempool)
    onFees x = (Down (mockGasPrice x), mockGasLimit x)


propGetPending :: [MockTx] -> MempoolBackend MockTx -> IO (Either String ())
propGetPending txs0 mempool = runExceptT $ do
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
    insert = mempoolInsert mempool . V.fromList

propHighWater :: ([MockTx], [MockTx]) -> MempoolBackend MockTx -> IO (Either String ())
propHighWater (txs0, txs1) mempool = runExceptT $ do
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
    insert = mempoolInsert mempool . V.fromList


uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq l@[_] = l
uniq (x:ys@(y:rest)) | x == y    = uniq (x:rest)
                     | otherwise = x : uniq ys

propValidate :: ([MockTx], [MockTx]) -> MempoolBackend MockTx -> IO (Either String ())
propValidate (txs0, txs1) mempool = runExceptT $ do
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
    hash = txHasher $ mempoolTxConfig mempool
    insert = liftIO . mempoolInsert mempool . V.fromList
    lookup = liftIO . mempoolLookup mempool . V.fromList . map hash

    markValidated = liftIO . mempoolMarkValidated mempool . V.fromList . map validate

    reintroduce = liftIO . mempoolReintroduce mempool . V.fromList
    markConfirmed = liftIO . mempoolMarkConfirmed mempool . V.fromList . map hash

    -- BlockHash and BlockHeight don't matter for this test...
    validate = ValidatedTransaction (_blockHeight someBlockHeader) (_blockHash someBlockHeader)


someBlockHeader :: BlockHeader
someBlockHeader = head $ testBlockHeaders (toyGenesis toyChainId)

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
