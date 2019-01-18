-- | Tests and test infrastructure common to all mempool backends.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Chainweb.Test.Mempool
  ( tests
  , remoteTests
  , MockTx(..)
  , MempoolWithFunc(..)
  , mockCodec
  , mockBlocksizeLimit
  , mockFeesLimit
  ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently(..))
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan as TBMChan
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Bits (bit, shiftL, shiftR, (.&.))
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString.Char8 (ByteString)
import Data.Decimal (Decimal, DecimalRaw(..))
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Int (Int64)
import Data.IORef
import Data.List (sort, sortBy, unfoldr)
import qualified Data.List.Ordered as OL
import Data.Ord (Down(..))
import qualified Data.Vector as V
import Data.Word (Word64)
import GHC.Generics
import Prelude hiding (lookup)
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
    , mempoolProperty "single subscription" (pick arbitrary) propSubscription
    ]

-- | Local-only tests plus all of the tests that are safe for remote.
tests :: MempoolWithFunc -> [TestTree]
tests withMempool = map ($ withMempool) [
      mempoolProperty "validate txs" (pick arbitrary) propValidate
    , mempoolProperty "multiple subscriptions" (pick arbitrary) propSubscriptions
    , mempoolTestCase "old transactions are reaped" testTooOld
    ] ++ remoteTests withMempool

-- | Mempool only cares about a few projected values from the transaction type
-- (fees, hash, tx size, metadata), so our mock transaction type will only
-- contain these (plus a nonce)
data MockTx = MockTx {
    mockNonce :: {-# UNPACK #-} !Int64
  , mockFees :: {-# UNPACK #-} !Decimal
  , mockSize :: {-# UNPACK #-} !Int64
  , mockMeta :: {-# UNPACK #-} !TransactionMetadata
} deriving (Eq, Ord, Show, Generic)


mockBlocksizeLimit :: Int64
mockBlocksizeLimit = 65535


mockFeesLimit :: Decimal
mockFeesLimit = fromIntegral mockBlocksizeLimit * 4


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

-- | A codec for transactions when sending them over the wire.
mockCodec :: Codec MockTx
mockCodec = Codec mockEncode mockDecode


mockEncode :: MockTx -> ByteString
mockEncode (MockTx sz fees nonce meta) = runPutS $ do
    putWord64le $ fromIntegral sz
    putDecimal fees
    putWord64le $ fromIntegral nonce
    Time.encodeTime $ txMetaCreationTime meta
    Time.encodeTime $ txMetaExpiryTime meta


putDecimal :: MonadPut m => Decimal -> m ()
putDecimal (Decimal places mantissa) = do
    putWord8 places
    putWord8 $ if mantissa >= 0 then 0 else 1
    let ws = toWordList $ if mantissa >= 0 then mantissa else -mantissa
    putWord64le $ fromIntegral $ length ws
    mapM_ putWord64le ws
  where
    toWordList = unfoldr $
                 \d -> if d == 0
                         then Nothing
                         else let !a  = fromIntegral (d .&. (bit 64 - 1))
                                  !d' = d `shiftR` 64
                              in Just (a, d')


getDecimal :: MonadGet m => m Decimal
getDecimal = do
    !places <- fromIntegral <$> getWord8
    !negative <- getWord8
    numWords <- fromIntegral <$> getWord16le
    mantissaWords <- replicateM numWords getWord64le
    let (!mantissa, _) = foldl go (0,0) mantissaWords
    return $! Decimal places (if negative == 0 then mantissa else -mantissa)
  where
    go :: (Integer, Int) -> Word64 -> (Integer, Int)
    go (!soFar, !shiftVal) !next =
        let !i = toInteger next + soFar `shiftL` shiftVal
            !s = shiftVal + 64
        in (i, s)


mockDecode :: ByteString -> Maybe MockTx
mockDecode = either (const Nothing) Just .
             runGetS (MockTx <$> getI64 <*> getDecimal <*> getI64 <*> getMeta)
  where
    getI64 = fromIntegral <$> getWord64le
    getMeta = TransactionMetadata <$> Time.decodeTime <*> Time.decodeTime


data MempoolWithFunc = MempoolWithFunc (forall a . (MempoolBackend MockTx -> IO a) -> IO a)


mempoolTestCase :: TestName
                -> (MempoolBackend MockTx -> IO ())
                -> MempoolWithFunc
                -> TestTree
mempoolTestCase name test (MempoolWithFunc withMempool) = testCase name $ withMempool test


mempoolProperty :: TestName
                -> PropertyM IO a
                -> (a -> MempoolBackend MockTx -> IO (Either String ()))
                -> MempoolWithFunc
                -> TestTree
mempoolProperty name gen test (MempoolWithFunc withMempool) = testProperty name go
  where
    go = monadicIO (gen >>= run . withMempool . test >>= either fail return)


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

propSubscription :: [MockTx] -> MempoolBackend MockTx -> IO (Either String ())
propSubscription txs0 mempool = runExceptT $ do
    sub <- subscribe
    result <- liftIO $ Async.runConcurrently (insertThread *>
                                              Concurrently (mockSubscriber sub))
    checkResult result
  where
    txs = uniq $ sortBy ord txs0
    checkResult = checkSubscriptionResult txs
    ord = compare `on` onFees
    onFees x = (Down (mockFees x), mockSize x, mockNonce x)
    insert = liftIO . mempoolInsert mempool . V.fromList
    subscribe = liftIO $ mempoolSubscribe mempool
    insertThread = Concurrently $ do
        traverse_ insert $ map (:[]) txs
        mempoolShutdown mempool


checkSubscriptionResult :: [MockTx] -> [MockTx] -> ExceptT String IO ()
checkSubscriptionResult txs r =
    when (r /= txs) $
    let msg = concat [ "subscription failed: expected sequence "
                     , show txs
                     , "got "
                     , show r
                     ]
    in fail msg


mockSubscriber :: IORef (Subscription a) -> IO [a]
mockSubscriber subRef = do
    sub <- readIORef subRef
    ref <- newIORef []
    go ref $ mempoolSubChan sub
    out <- reverse <$> readIORef ref
    -- make sure we touch the ref so it isn't gc'ed
    writeIORef subRef sub
    return out
  where
    go ref chan = do
        m <- atomically $ TBMChan.readTBMChan chan
        flip (maybe (return ())) m $ \el -> do
            let l = reverse $ V.toList el
            modifyIORef' ref (l ++)
            go ref chan

-- In-mem backend supports multiple subscriptions at once, socket-based one
-- doesn't (you'd just connect twice)
propSubscriptions :: [MockTx] -> MempoolBackend MockTx -> IO (Either String ())
propSubscriptions txs0 mempool = runExceptT $ do
    subscriptions <- replicateM numSubscribers subscribe
    results <- liftIO $ Async.runConcurrently (insertThread *>
                                               runSubscribers subscriptions)
    mapM_ checkResult results

  where
    txs = uniq $ sortBy ord txs0
    checkResult = checkSubscriptionResult txs
    numSubscribers = 7
    ord = compare `on` onFees
    onFees x = (Down (mockFees x), mockSize x, mockNonce x)
    insert = liftIO . mempoolInsert mempool . V.fromList
    subscribe = liftIO $ mempoolSubscribe mempool
    insertThread = Concurrently $ do
        traverse_ insert $ map (:[]) txs
        mempoolShutdown mempool

    runSubscribers :: [IORef (Subscription MockTx)] -> Concurrently [[MockTx]]
    runSubscribers subscriptions =
        Concurrently $ Async.forConcurrently subscriptions mockSubscriber

