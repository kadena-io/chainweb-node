{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Chainweb.Test.Mempool.Sync (tests) where

------------------------------------------------------------------------------
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (void, when)
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
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Mempool.InMem
import Chainweb.Mempool.InMemTypes
import Chainweb.Mempool.Mempool
import Chainweb.Test.Mempool
    (InsertCheck, MempoolWithFunc(..), lookupIsPending, mempoolProperty)
import Chainweb.Utils (Codec(..))
import Chainweb.Version (ChainwebVersion(..))
------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Chainweb.Mempool.sync"
    [ mempoolProperty "Mempool.syncMempools" gen propSync $ MempoolWithFunc wf
    ]
  where
    wf :: (InsertCheck -> MempoolBackend MockTx -> IO a) -> IO a
    wf f = do
        mv <- newMVar (pure . V.map Right)
        let cfg = InMemConfig txcfg mockBlockGasLimit 2048 Right (checkMv mv)
        withInMemoryMempool cfg (Test singletonChainGraph) $ f mv

    checkMv :: MVar (t -> IO b) -> t -> IO b
    checkMv mv xs = do
        f <- readMVar mv
        f xs

    gen :: PropertyM IO (Set MockTx, Set MockTx, Set MockTx)
    gen = do
      (xs, ys, zs) <- pick arbitrary
      let xss = Set.fromList xs
      let yss = Set.fromList ys `Set.difference` xss
      let zss = Set.fromList zs `Set.difference` (xss `Set.union` yss)
      pre (not (Set.null xss || Set.null yss || Set.null zss)
           && length ys < 10000
           && length zs < 10000)
      return (xss, yss, zss)

txcfg :: TransactionConfig MockTx
txcfg = TransactionConfig mockCodec hasher hashmeta mockGasPrice
                          mockGasLimit mockMeta
  where
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec

testInMemCfg :: InMemConfig MockTx
testInMemCfg =
    InMemConfig txcfg mockBlockGasLimit 2048 Right (pure . V.map Right)

propSync
    :: (Set MockTx, Set MockTx , Set MockTx)
    -> InsertCheck
    -> MempoolBackend MockTx
    -> IO (Either String ())
propSync (txs, missing, later) _ localMempool' =
    withInMemoryMempool testInMemCfg (Test singletonChainGraph) $ \remoteMempool -> do
        mempoolInsert localMempool' CheckedInsert txsV
        mempoolInsert remoteMempool CheckedInsert txsV
        mempoolInsert remoteMempool CheckedInsert missingV

        syncThMv <- newEmptyMVar
        syncFinished <- newEmptyMVar

        let nmissing = V.length missingV
        let nlater = V.length laterV
        let onInitialSyncFinished = tryPutMVar syncFinished ()
        let onFinalSyncFinished = do
                readMVar syncThMv >>= Async.uninterruptibleCancel
                throwIO ThreadKilled
        localMempool <-
              timebomb nmissing onInitialSyncFinished =<<
              timebomb (nmissing + nlater) onFinalSyncFinished localMempool'
        let syncThread = eatExceptions $
                         syncMempools noLog 10 localMempool remoteMempool


        -- expect remote to deliver transactions during sync.
        -- Timeout to guard against waiting forever
        m <- timeout 20000000 $ do
            syncTh <- Async.async syncThread
            putMVar syncThMv syncTh
            Async.link syncTh

            -- Wait until time bomb 1 goes off
            takeMVar syncFinished

            -- We should now be subscribed and waiting for V.length laterV
            -- more transactions before getting killed. Transactions
            -- inserted into remote should get synced to us.
            mempoolInsert remoteMempool CheckedInsert laterV
            Async.wait syncTh

        maybe (fail "timeout") return m

        -- we synced the right number of transactions. verify they're all there.
        runExceptT $ do
            liftIO (mempoolLookup localMempool missingHashes) >>=
                V.mapM_ lookupIsPending
            liftIO (mempoolLookup localMempool laterHashes) >>=
                V.mapM_ lookupIsPending

  where
    noLog = const $ const $ return ()

    hash = txHasher $ mempoolTxConfig localMempool'
    txsV = V.fromList $ Set.toList txs
    missingV = V.fromList $ Set.toList missing
    missingHashes = V.map hash missingV

    laterV = V.fromList $ Set.toList later
    laterHashes = V.map hash laterV

eatExceptions :: IO () -> IO ()
eatExceptions = handle $ \(e :: SomeException) -> void $ evaluate e

timebomb :: Int -> IO a -> MempoolBackend t -> IO (MempoolBackend t)
timebomb k act mp = do
    ref <- newIORef k
    return $! mp { mempoolInsert = ins ref }
  where
    ins ref t v = do
        mempoolInsert mp t v
        c <- atomicModifyIORef' ref (\x -> let !x' = x - V.length v
                                           in (x', x'))
        when (c == 0) $ void act     -- so that the bomb only triggers once
