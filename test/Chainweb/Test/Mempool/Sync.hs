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
import Data.IORef
import Data.Set (Set)
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
      (xs, ys, zs) <- pick arbitrary
      let xss = Set.fromList xs
      let yss = Set.fromList ys `Set.difference` xss
      let zss = Set.fromList zs `Set.difference` (xss `Set.union` yss)
      pre (not (Set.null xss || Set.null yss || Set.null zss) && length ys < 10000 && length zs < 10000)
      return (xss, yss, zss)

testInMemCfg :: InMemConfig MockTx
testInMemCfg = InMemConfig txcfg mockBlocksizeLimit (hz 100)
  where
    txcfg = TransactionConfig mockCodec hasher hashmeta mockFees mockSize mockMeta
                              (const $ return True)
    -- run the reaper @100Hz for testing
    hz x = 1000000 `div` x
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec


propSync :: (Set MockTx, Set MockTx, Set MockTx) -> MempoolBackend MockTx -> IO (Either String ())
propSync (txs, missing, later) localMempool =
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
                 syncFinished <- newEmptyMVar
                 tb1 <- mkTimeBomb (V.length missingV)
                                   (tryPutMVar syncFinished ())
                 tb2 <- mkTimeBomb (V.length missingV + V.length laterV)
                                   (do readMVar syncThMv >>=
                                          Async.uninterruptibleCancel
                                       throwIO ThreadKilled)
                 let tb x = tb1 x `finally` tb2 x
                 sub <- readIORef subRef
                 subTh <- Async.async (subThread tb sub subStarted)
                 Async.link subTh
                 takeMVar subStarted

                 syncTh <- Async.async $ syncThread remoteMempool
                 putMVar syncThMv syncTh
                 Async.link syncTh

                 -- Wait until time bomb 1 goes off
                 takeMVar syncFinished

                 -- We should now be subscribed and waiting for V.length laterV
                 -- more transactions before getting killed. Transactions
                 -- inserted into remote should get synced to us.
                 mempoolInsert remoteMempool laterV

                 Async.wait syncTh `finally` Async.wait subTh

        maybe (fail "timeout") return m

        -- we synced the right number of transactions. verify they're all
        -- there.
        runExceptT $ do
            liftIO (mempoolLookup localMempool missingHashes) >>=
                V.mapM_ lookupIsPending
            liftIO (mempoolLookup localMempool laterHashes) >>=
                V.mapM_ lookupIsPending

  where
    -- twiddles the mvar and performs the given action when the given number of
    -- transactions has been observed.
    mkTimeBomb k act = do
        ref <- newIORef k
        return $ \v -> do
            c <- atomicModifyIORef' ref (\x -> let !x' = x - V.length v
                                               in (x', x'))
            when (c == 0) $ void act

    syncThread remoteMempool = eatExceptions $ syncMempools localMempool remoteMempool

    -- a thread that reads from a mempool subscription and calls a handler for
    -- each element that comes through the channel.
    subThread onElem sub subStarted =
        eatExceptions $ flip finally (mempoolSubFinal sub) $ do
            let chan = mempoolSubChan sub
            putMVar subStarted ()
            let go = atomically (TBMChan.readTBMChan chan) >>=
                     maybe (return ()) (\e -> onElem e >> go)
            go

    hash = txHasher $ mempoolTxConfig localMempool
    txsV = V.fromList $ Set.toList txs
    missingV = V.fromList $ Set.toList missing
    missingHashes = V.map hash missingV

    laterV = V.fromList $ Set.toList later
    laterHashes = V.map hash laterV

eatExceptions :: IO () -> IO ()
eatExceptions = handle $ \(e :: SomeException) -> void $ evaluate e
