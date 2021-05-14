{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Chainweb.Test.Mempool.Sync (tests) where

------------------------------------------------------------------------------
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.IORef
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Strict
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Timeout
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
------------------------------------------------------------------------------
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Mempool.InMem
import Chainweb.Mempool.InMemTypes
import Chainweb.Mempool.Mempool
import Chainweb.Test.Mempool
    (InsertCheck, MempoolWithFunc(..), lookupIsPending, mempoolProperty)
import Chainweb.Utils (Codec(..))
import Chainweb.Version (ChainwebVersion(..))
import P2P.Peer
import P2P.Test.Orphans
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
                              (1024 * 10) (const $ return ())
        withInMemoryMempool cfg (Test singletonChainGraph) $ f mv

    checkMv :: MVar (t -> IO b) -> t -> IO b
    checkMv mv xs = do
        f <- readMVar mv
        f xs

    gen :: PropertyM IO (PeerInfo, Set MockTx, Set MockTx, Set MockTx)
    gen = do
      (xs, ys, zs) <- pick arbitrary
      let xss = Set.fromList xs
      let yss = Set.fromList ys `Set.difference` xss
      let zss = Set.fromList zs `Set.difference` (xss `Set.union` yss)
      pre (not (Set.null xss || Set.null yss || Set.null zss)
           && length ys < 10_000
           && length zs < 10_000)
      peer <- pick arbitraryPeerInfo
      return (peer, xss, yss, zss)

txcfg :: TransactionConfig MockTx
txcfg = TransactionConfig mockCodec hasher hashmeta mockGasPrice
                          mockGasLimit mockMeta
  where
    hashmeta = chainwebTestHashMeta
    hasher = chainwebTestHasher . codecEncode mockCodec

testInMemCfg :: InMemConfig MockTx
testInMemCfg =
    InMemConfig txcfg mockBlockGasLimit 2048 Right (pure . V.map Right) (1024 * 10)
                (const $ return ())

withHops :: Vector t -> Vector (t, HopCount)
withHops v = v `V.zip` V.replicate (V.length v) 0

propSync
    :: (PeerInfo, Set MockTx, Set MockTx , Set MockTx)
    -> InsertCheck
    -> MempoolBackend MockTx
    -> IO (Either String ())
propSync (peer, txs, missing, later) _ localMempool' = do
    newTxs <- newMVar []
    let onNew v = modifyMVar_ newTxs (return . (v:))
    let mpcfg = testInMemCfg { _inmemOnNewTransactions = onNew }
    withInMemoryMempool mpcfg (Test singletonChainGraph) $ \remoteMempool -> do
        mempoolInsert localMempool' CheckedInsert txsVHops
        mempoolInsert remoteMempool CheckedInsert txsVHops

        -- test that the on-insert callback works.
        testSet1 <- modifyMVar newTxs $ \xs -> return ([], V.concat xs)
        let sortedTestSet1 = V.fromList (sort (map sfst $ V.toList testSet1))
        let sortedTxsHashes = V.fromList (sort (V.toList txsHashes))
        assertEqual "onNewTransactions callback works" sortedTxsHashes sortedTestSet1

        mempoolInsert remoteMempool CheckedInsert missingVHops

        doneVar <- newEmptyMVar
        syncFinished <- newEmptyMVar

        let nmissing = V.length missingV
        let nlater = V.length laterV
        let onInitialSyncFinished = tryPutMVar syncFinished ()
        let onFinalSyncFinished = putMVar doneVar ()
        localMempool <-
              timebomb nmissing onInitialSyncFinished =<<
              timebomb (nmissing + nlater) onFinalSyncFinished localMempool'
        let syncThread = syncMempools noLog peer 10 localMempool remoteMempool

        -- expect remote to deliver transactions during sync.
        -- Timeout to guard against waiting forever
        m <- timeout 20_000_000 $ do
            Async.withAsync syncThread $ \_ -> do
                -- Wait until time bomb 1 goes off
                takeMVar syncFinished

                -- We should now be subscribed and waiting for V.length laterV
                -- more transactions before getting killed. Transactions
                -- inserted into remote should get synced to us.
                mempoolInsert remoteMempool CheckedInsert laterV

                -- wait until time bomb 2 goes off
                takeMVar doneVar

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
    txsHashes = V.map hash txsV
    missingV = V.fromList $ Set.toList missing
    txsVHops = withHops txsV
    missingVHops = withHops missingV
    missingHashes = V.map hash missingV

    laterV = withHops (V.fromList $ Set.toList later)
    laterHashes = V.map (hash . fst) laterV

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
