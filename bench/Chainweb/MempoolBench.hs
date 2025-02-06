{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Chainweb.MempoolBench (bench) where

import Control.Lens
import Control.Monad
import Data.Vector qualified as V

import Criterion.Main qualified as C
import PropertyMatchers qualified as P
import PropertyMatchers ((?))

import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Mempool.Mempool qualified as Mempool
import Chainweb.Mempool.InMem qualified as InMem
import Chainweb.Mempool.InMemTypes qualified as InMem
import Chainweb.Pact4.Transaction
import Chainweb.Test.Pact4.Utils
import Chainweb.Test.TestVersions
import Chainweb.Utils
import Chainweb.Utils.Bench
import Chainweb.Version
import Pact.Types.ChainMeta (getCurrentCreationTime)
import Pact.Types.Command
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Set as S
import Data.IORef
import Data.List (unfoldr)

txCfg :: Mempool.TransactionConfig UnparsedTransaction
txCfg = Mempool.pact4TransactionConfig

inmemCfg :: InMem.InMemConfig UnparsedTransaction
inmemCfg = InMem.InMemConfig
    { InMem._inmemTxCfg = txCfg
    , InMem._inmemTxBlockSizeLimit = Mempool.GasLimit 150_000
    , InMem._inmemTxMinGasPrice = Mempool.GasPrice (0.0000000001)
    , InMem._inmemMaxRecentItems = 2048
    , InMem._inmemPreInsertPureChecks = return
    , InMem._inmemPreInsertBatchChecks = return . fmap return
    , InMem._inmemCurrentTxsSize = 1024 * 1024
    }

v :: ChainwebVersion
v = instantCpmTestVersion singletonChainGraph

setup :: V.Vector UnparsedTransaction -> IO (NoopNFData (Mempool.MempoolBackend UnparsedTransaction))
setup txs = do
    mp <- InMem.startInMemoryMempoolTest inmemCfg
    Mempool.mempoolInsert mp Mempool.UncheckedInsert txs
    return (NoopNFData mp)

cmds :: V.Vector UnparsedTransaction
cmds = unsafePerformIO $ V.generateM 4096 $ \i -> do
    now <- getCurrentCreationTime
    fmap unparseTransaction
        $ buildCwCmd (sshow i) v
        $ set cbCreationTime now
        $ set cbGasLimit (Mempool.GasLimit 1)
        $ defaultCmd

txHash :: UnparsedTransaction -> Mempool.TransactionHash
txHash = Mempool.txHasher txCfg

expiredCmds :: V.Vector UnparsedTransaction
expiredCmds = unsafePerformIO $ V.generateM 4096 $ \i -> do
    fmap unparseTransaction
        $ buildCwCmd (sshow i) v
        $ set cbGasLimit (Mempool.GasLimit 1)
        $ defaultCmd

setupMakeTxs :: V.Vector UnparsedTransaction -> IO (NoopNFData (Mempool.MempoolBackend UnparsedTransaction, V.Vector UnparsedTransaction))
setupMakeTxs txs = do
    mp <- InMem.startInMemoryMempoolTest inmemCfg
    return $ NoopNFData (mp, txs)

bfEmpty :: Mempool.BlockFill
bfEmpty = Mempool.BlockFill
    { Mempool._bfGasLimit = Mempool.GasLimit 150_000
        -- ^ Fetch pending transactions up to this limit.
    , Mempool._bfTxHashes = mempty
        -- ^ Fetch only transactions not in set.
    , Mempool._bfCount = 0
    }

bfWithNHashes :: Int -> Mempool.BlockFill
bfWithNHashes n = bfEmpty
    { Mempool._bfTxHashes = S.fromList (txHash <$> V.toList (V.take n cmds))
    }

mempoolGetBlockBench :: _ => _
mempoolGetBlockBench name p bf n = C.bench name
    $ C.perRunEnv (setup (V.take n cmds)) $ \(NoopNFData mp) -> do
    Mempool.mempoolGetBlock mp bf Mempool.noopMempoolPreBlockCheck (BlockHeight 1) nullBlockHash
        >>= p

allPendingTxHashes :: Mempool.MempoolBackend t -> IO [Mempool.TransactionHash]
allPendingTxHashes mp = do
    allRef <- newIORef []
    void $ Mempool.mempoolGetPendingTransactions mp Nothing (\pending -> modifyIORef' allRef (pending :))
    concatMap V.toList <$> readIORef allRef

chunksOf :: Int -> V.Vector a -> [V.Vector a]
chunksOf i = unfoldr go
    where
    go vec
        | V.null vec = Nothing
        | otherwise = Just (V.splitAt i vec)

overlappingChunksOf :: Int -> V.Vector a -> [V.Vector a]
overlappingChunksOf i = unfoldr go
    where
    go vec
        | V.null vec = Nothing
        | otherwise = Just (V.take i vec, V.drop (i `div` 2) vec)

bench :: C.Benchmark
bench = C.bgroup "mempool" $ concat
    [
        [ mempoolGetBlockBench
            ("mempoolGetBlock " <> show n)
            (P.fun V.length ? P.equals n) bfEmpty n
        | n <- [1,16,64,256,1024,4096]
        ],
        [ mempoolGetBlockBench
            ("mempoolGetBlockHalfExcludedHashes " <> show n)
            (P.fun V.length ? P.equals (max 1 (n `div` 2))) (bfWithNHashes (n `div` 2)) n
        | n <- [1,16,64,256,1024,4096]
        ],
        [ C.bench "mempoolInsertChecked" $ C.perRunEnvWithCleanup
            (setupMakeTxs (V.take 2000 cmds))
            (\(NoopNFData (mp, _)) ->
                allPendingTxHashes mp >>= P.fun length ? P.equals 2000
            )
            $ \(NoopNFData ~(mp, txs)) -> do
                Mempool.mempoolInsert mp Mempool.CheckedInsert txs
        , C.bench "mempoolInsert" $ C.perRunEnvWithCleanup
            (setupMakeTxs (V.take 2000 cmds))
            (\(NoopNFData (mp, _)) ->
                allPendingTxHashes mp >>= P.fun length ? P.equals 2000
            )
            $ \(NoopNFData ~(mp, txs)) -> do
                Mempool.mempoolInsert mp Mempool.UncheckedInsert txs
        , C.bench "mempoolInsertMultipleBatches" $ C.perRunEnvWithCleanup
            (setupMakeTxs (V.take 2000 cmds))
            (\(NoopNFData (mp, _)) ->
                allPendingTxHashes mp >>= P.fun length ? P.equals 2000
            )
            $ \(NoopNFData ~(mp, txs)) -> do
                forM_ (chunksOf 100 txs) $ \chunk ->
                    Mempool.mempoolInsert mp Mempool.UncheckedInsert chunk
        , C.bench "mempoolInsertOverlappingBatches" $ C.perRunEnvWithCleanup
            (setupMakeTxs (V.take 2000 cmds))
            (\(NoopNFData (mp, _)) ->
                allPendingTxHashes mp >>= P.fun length ? P.equals 2000
            )
            $ \(NoopNFData ~(mp, txs)) -> do
                forM_ (overlappingChunksOf 100 txs) $ \chunk ->
                    Mempool.mempoolInsert mp Mempool.UncheckedInsert chunk
        , C.bench "mempoolAddToBadList" $ C.perRunEnvWithCleanup
            (setupMakeTxs (V.take 2000 cmds))
            (\(NoopNFData (mp, _)) ->
                Mempool.mempoolCheckBadList mp (txHash <$> V.take 2000 cmds)
                    >>= P.alignExact (V.replicate 2000 (P.equals True))
            )
            $ \(NoopNFData ~(mp, txs)) -> do
                Mempool.mempoolAddToBadList mp (Mempool.pact4RequestKeyToTransactionHash . cmdToRequestKey <$> txs)
        , C.bench "mempoolPrune" $ C.perRunEnvWithCleanup
            (setup (V.take 2000 cmds))
            (\(NoopNFData mp) ->
                allPendingTxHashes mp >>= P.fun length ? P.equals 2000)
            $ \(NoopNFData mp) -> do
                Mempool.mempoolPrune mp
        , C.bench "mempoolPruneExpired" $ C.perRunEnvWithCleanup
            (setup (V.take 2000 expiredCmds))
            (\(NoopNFData mp) ->
                allPendingTxHashes mp >>= P.equals [])
            $ \(NoopNFData mp) -> do
                Mempool.mempoolPrune mp
        ]
    ]

    -- TODO: benchmark what happens when we have a bunch of txs that are too big for the gas limit,
    -- interleaved with txs that are smaller, in gas price order
