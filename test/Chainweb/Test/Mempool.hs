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

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Ord (Down(..))
import qualified Data.Vector as V
import GHC.Generics
import Prelude hiding (lookup)
import Test.QuickCheck
import Test.QuickCheck.Gen (chooseAny)
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Chainweb.Mempool.Mempool

------------------------------------------------------------------------------
tests :: MempoolWithFunc -> [TestTree]
tests withMempool = map ($ withMempool) [
      mempoolTestCase "nil case (startup/destroy)" testStartup
    , mempoolProperty "insert + lookup + traversal" (pick arbitrary) propTrivial
    ]

data MockTx = MockTx {
    mockNonce :: {-# UNPACK #-} !Int64
  , mockFees :: {-# UNPACK #-} !Int64
  , mockSize :: {-# UNPACK #-} !Int64
} deriving (Eq, Ord, Show, Generic)

mockBlocksizeLimit :: Int64
mockBlocksizeLimit = 65535

mockFeesLimit :: Int64
mockFeesLimit = mockBlocksizeLimit * 4

instance Arbitrary MockTx where
  arbitrary = let g x = choose (1, x)
              in MockTx <$> chooseAny <*> g mockFeesLimit <*> g mockBlocksizeLimit
  shrink = genericShrink

mockCodec :: Codec MockTx
mockCodec = Codec mockEncode mockDecode

mockEncode :: MockTx -> ByteString
mockEncode (MockTx sz reward nonce) = runPutS $ do
    putWord64le $ fromIntegral sz
    putWord64le $ fromIntegral reward
    putWord64le $ fromIntegral nonce

mockDecode :: ByteString -> Maybe MockTx
mockDecode = either (const Nothing) Just .
             runGetS (MockTx <$> getI64 <*> getI64 <*> getI64)
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
    hash = _mempoolHasher mempool . _codecEncode (_mempoolTxCodec mempool)
    insert = _mempoolInsert mempool . V.fromList
    lookup = _mempoolLookup mempool . V.fromList . map hash
    confirmLookupOK (Pending _) = return ()
    confirmLookupOK _ = fail "lookup failure"

    getBlock = _mempoolGetBlock mempool (_mempoolBlockSizeLimit mempool)
    onFees x = (Down (mockFees x), mockSize x)
