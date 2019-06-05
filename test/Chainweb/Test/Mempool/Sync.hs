{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Chainweb.Test.Mempool.Sync (tests) where

------------------------------------------------------------------------------
import Test.Tasty
------------------------------------------------------------------------------
{-
import Chainweb.Mempool.InMem
import Chainweb.Mempool.InMemTypes
import Chainweb.Mempool.Mempool
import Chainweb.Test.Mempool
    (MempoolWithFunc(..), lookupIsPending, mempoolProperty)
-}

{-
tests = mempoolProperty "Mempool.syncMempools" gen
        propSync
        $ MempoolWithFunc
        $ withInMemoryMempool
            testInMemCfg
  where
    gen :: PropertyM IO (Set MockTx, Set MockTx, Set MockTx)
    gen = do
      (xs, ys, zs) <- pick arbitrary
      let xss = Set.fromList xs
      let yss = Set.fromList ys `Set.difference` xss
      let zss = Set.fromList zs `Set.difference` (xss `Set.union` yss)
      pre (not (Set.null xss || Set.null yss || Set.null zss) && length ys < 10000 && length zs < 10000)
      return (xss, yss, zss)

testInMemCfg :: InMemConfig MockTx
testInMemCfg = InMemConfig txcfg mockBlockGasLimit (hz 100) True
  where
    txcfg = TransactionConfig mockCodec hasher hashmeta mockGasPrice
                              mockGasLimit mockMeta (const $ return True)
    -- run the reaper @100Hz for testing
    hz x = 1000000 `div` x
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec

propSync
    :: (Set MockTx, Set MockTx , Set MockTx)
    -> MempoolBackend MockTx
    -> IO (Either String ())
propSync (txs, missing, later) localMempool =
    withInMemoryMempool testInMemCfg $ \remoteMempool -> do
        mempoolInsert localMempool txsV
        mempoolInsert remoteMempool txsV
        mempoolInsert remoteMempool missingV

        -- expect remote to deliver us this many transactions during sync.
        -- Timeout to guard against waiting forever
        m <- timeout 20000000 $ do
                 syncThMv <- newEmptyMVar
                 syncFinished <- newEmptyMVar
                 tb1 <- mkTimeBomb (V.length missingV)
                                   (tryPutMVar syncFinished ())
                 tb2 <- mkTimeBomb (V.length missingV + V.length laterV)
                                   (do readMVar syncThMv >>=
                                          Async.uninterruptibleCancel
                                       throwIO ThreadKilled)
                 let tb x = tb1 x `finally` tb2 x
                 syncTh <- Async.async $ syncThread remoteMempool
                 putMVar syncThMv syncTh
                 Async.link syncTh

                 -- Wait until time bomb 1 goes off
                 takeMVar syncFinished

                 -- We should now be subscribed and waiting for V.length laterV
                 -- more transactions before getting killed. Transactions
                 -- inserted into remote should get synced to us.
                 mempoolInsert remoteMempool laterV
                 Async.wait syncTh

        maybe (fail "timeout") return m

        -- we synced the right number of transactions. verify they're all there.
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

    noLog = const $ const $ return ()
    syncThread remoteMempool =
        eatExceptions $ syncMempools noLog 10 localMempool remoteMempool

    hash = txHasher $ mempoolTxConfig localMempool
    txsV = V.fromList $ Set.toList txs
    missingV = V.fromList $ Set.toList missing
    missingHashes = V.map hash missingV

    laterV = V.fromList $ Set.toList later
    laterHashes = V.map hash laterV

eatExceptions :: IO () -> IO ()
eatExceptions = handle $ \(e :: SomeException) -> void $ evaluate e
-}

tests :: TestTree
tests = testGroup "Chainweb.Mempool.sync" []
