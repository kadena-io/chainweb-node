{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Chainweb.Test.Mempool
  ( tests
  , MockTx(..)
  , MempoolWithFunc(..)
  , mockCodec
  , mockBlocksizeLimit
  , mockFeesLimit
  ) where

import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Bits (bit, shiftL, shiftR, (.&.))
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString.Char8 (ByteString)
import Data.Decimal (Decimal, DecimalRaw(..))
import Data.Int (Int64)
import Data.List (unfoldr)
import Data.Ord (Down(..))
import qualified Data.Vector as V
import Data.Word (Word64)
import GHC.Generics
import Prelude hiding (lookup)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen (chooseAny)
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))

import Chainweb.Mempool.Mempool

------------------------------------------------------------------------------
tests :: MempoolWithFunc -> [TestTree]
tests withMempool = map ($ withMempool) [
      mempoolTestCase "nil case (startup/destroy)" testStartup
    , mempoolProperty "insert + lookup + getBlock" (pick arbitrary) propTrivial
    ]

data MockTx = MockTx {
    mockNonce :: {-# UNPACK #-} !Int64
  , mockFees :: {-# UNPACK #-} !Decimal
  , mockSize :: {-# UNPACK #-} !Int64
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

mockCodec :: Codec MockTx
mockCodec = Codec mockEncode mockDecode

mockEncode :: MockTx -> ByteString
mockEncode (MockTx sz fees nonce) = runPutS $ do
    putWord64le $ fromIntegral sz
    putDecimal fees
    putWord64le $ fromIntegral nonce


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
             runGetS (MockTx <$> getI64 <*> getDecimal <*> getI64)
  where
    getI64 = fromIntegral <$> getWord64le

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

propTrivial :: [MockTx] -> MempoolBackend MockTx -> IO (Either String ())
propTrivial txs mempool = runExceptT $ do
    liftIO $ insert txs
    liftIO (lookup txs) >>= V.mapM_ confirmLookupOK
    block <- liftIO getBlock
    when (not $ isSorted block) $
        fail ("getBlock didn't return a sorted block: " ++ show block)

  where
    isSorted xs = let fs = V.map onFees xs
                      ffs = V.zipWith (<=) fs (V.drop 1 fs)
                  in V.and ffs
    hash = _mempoolHasher mempool
    insert = _mempoolInsert mempool . V.fromList
    lookup = _mempoolLookup mempool . V.fromList . map hash
    confirmLookupOK (Pending _) = return ()
    confirmLookupOK _ = fail "lookup failure"

    getBlock = _mempoolGetBlock mempool (_mempoolBlockSizeLimit mempool)
    onFees x = (Down (mockFees x), mockSize x)
