{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chainweb.Test.Mempool.Sync (tests) where

------------------------------------------------------------------------------
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBMChan as TBMChan
import Control.Exception
import Control.Monad (void, when, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as B
import Data.IORef
import qualified Data.Set as Set
import qualified Data.Vector as V
import System.Timeout
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic
import Test.Tasty
------------------------------------------------------------------------------
import Chainweb.Mempool.InMem
import Chainweb.Mempool.Mempool
import Chainweb.Test.Mempool
    (MempoolWithFunc(..), lookupIsPending, mempoolProperty)
import Chainweb.Utils (Codec(..))

tests :: TestTree
tests = mempoolProperty "Mempool.syncMempools" gen propSync withFunc
  where
    withFunc = MempoolWithFunc (withInMemoryMempool testInMemCfg)
    gen = do
      vs@(xs, ys) <- pick arbitrary
      pre (not (null xs || null ys) && length ys < 10000)
      return vs

testInMemCfg :: InMemConfig MockTx
testInMemCfg = InMemConfig txcfg mockBlocksizeLimit (hz 100)
  where
    txcfg = TransactionConfig mockCodec hasher hashmeta mockFees mockSize mockMeta
                              (const $ return True)
    -- run the reaper @100Hz for testing
    hz x = 1000000 `div` x
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec


propSync :: ([MockTx], [MockTx]) -> MempoolBackend MockTx -> IO (Either String ())
propSync (txs0, missing0) localMempool =
    withInMemoryMempool testInMemCfg $ \remoteMempool -> do
        mempoolInsert localMempool txsV
        mempoolInsert remoteMempool txsV
        mempoolInsert remoteMempool missingV

        -- expect remote to deliver us this many transactions during sync.
        -- Timeout to guard against waiting forever
        m <- timeout 20000000 $
             bracket (mempoolSubscribe localMempool)
                     (readIORef >=> mempoolSubFinal) $ \subRef -> do
                 syncThMv <- newEmptyMVar
                 subStarted <- newEmptyMVar
                 tb <- mkTimeBomb (V.length missingV)
                                  (readMVar syncThMv >>=
                                   Async.uninterruptibleCancel)
                 sub <- readIORef subRef
                 subTh <- Async.async (subThread tb sub subStarted)
                 Async.link subTh
                 takeMVar subStarted

                 syncTh <- Async.async $ syncThread remoteMempool
                 putMVar syncThMv syncTh
                 Async.link syncTh

                 Async.wait syncTh `finally` Async.wait subTh
        maybe (fail "timeout") return m

        -- we synced the right number of transactions. verify they're all
        -- there.
        runExceptT $ do
            liftIO (mempoolLookup localMempool missingHashes) >>=
                V.mapM_ lookupIsPending

  where
    -- twiddles the mvar and performs the given action when the given number of
    -- transactions has been observed.
    mkTimeBomb k act = do
        ref <- newIORef k
        return $ \v -> do
            c <- atomicModifyIORef' ref (\x -> let !x' = x - V.length v
                                               in (x', x'))
            when (c == 0) $ do
                void act
                throwIO ThreadKilled

    syncThread remoteMempool = eatExceptions $ syncMempools localMempool remoteMempool

    -- a thread that reads from a mempool subscription and calls a handler for
    -- each element that comes through the channel.
    subThread
        :: (V.Vector MockTx -> IO ()) -> Subscription MockTx -> MVar () -> IO ()
    subThread onElem sub subStarted =
        eatExceptions $ flip finally (mempoolSubFinal sub) $ do
            let chan = mempoolSubChan sub
            putMVar subStarted ()
            let go = atomically (TBMChan.readTBMChan chan) >>=
                     maybe (return ()) (\e -> onElem e >> go)
            go

    hash = txHasher $ mempoolTxConfig localMempool
    txs = Set.fromList txs0
    txsV = V.fromList $ Set.toList txs
    missing = Set.difference (Set.fromList missing0) txs
    missingV = V.fromList $ Set.toList missing
    missingHashes = V.map hash missingV

eatExceptions :: IO () -> IO ()
eatExceptions = handle $ \(e :: SomeException) -> void $ evaluate e
