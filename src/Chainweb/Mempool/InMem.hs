{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | A mock in-memory mempool backend that does not persist to disk.
module Chainweb.Mempool.InMem
  (
   -- * Initialization functions
    withInMemoryMempool
  , withInMemoryMempool_

    -- * Low-level create/destroy functions
  , makeInMemPool
  , newInMemMempoolData
  ) where

------------------------------------------------------------------------------
import Control.Applicative (pure, (<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar, withMVarMasked)
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception (bracket, mask_, throw)
import Control.Monad (unless, void, (<$!>))

import Data.Aeson
import Data.Foldable (foldl', foldlM)
import qualified Data.HashPSQ as PSQ
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Ord (Down(..))
import Data.Tuple.Strict
import Data.Vector (Vector)
import qualified Data.Vector as V

import Pact.Types.Gas (GasPrice(..))

import Prelude hiding (init, lookup, pred)

import System.LogLevel
import System.Random

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Logger
import Chainweb.Mempool.InMemTypes
import Chainweb.Mempool.Mempool
import Chainweb.Utils

------------------------------------------------------------------------------
toPriority :: GasPrice -> GasLimit -> Priority
toPriority r s = (Down r, s)


------------------------------------------------------------------------------
makeInMemPool :: ToJSON t
              => FromJSON t
              => InMemConfig t
              -> IO (InMemoryMempool t)
makeInMemPool cfg = mask_ $ do
    nonce <- randomIO
    dataLock <- newInMemMempoolData >>= newMVar
    return $! InMemoryMempool cfg dataLock nonce

destroyInMemPool :: InMemoryMempool t -> IO ()
destroyInMemPool = const $ return ()


------------------------------------------------------------------------------
newInMemMempoolData :: ToJSON t => FromJSON t => IO (InMemoryMempoolData t)
newInMemMempoolData =
    InMemoryMempoolData <$!> newIORef 0
                        <*> newIORef PSQ.empty
                        <*> newIORef emptyRecentLog


------------------------------------------------------------------------------
toMempoolBackend
    :: InMemoryMempool t
    -> IO (MempoolBackend t)
toMempoolBackend mempool = do
    return $! MempoolBackend
      { mempoolTxConfig = tcfg
      , mempoolBlockGasLimit = blockSizeLimit
      , mempoolMember = member
      , mempoolLookup = lookup
      , mempoolInsert = insert
      , mempoolMarkValidated = markValidated
      , mempoolGetBlock = getBlock
      , mempoolGetPendingTransactions = getPending
      , mempoolClear = clear
      }
  where
    cfg = _inmemCfg mempool
    nonce = _inmemNonce mempool
    lockMVar = _inmemDataLock mempool

    InMemConfig tcfg blockSizeLimit _ = cfg
    member = memberInMem lockMVar
    lookup = lookupInMem lockMVar
    insert = insertInMem cfg lockMVar
    markValidated = markValidatedInMem lockMVar
    getBlock = getBlockInMem cfg lockMVar
    getPending = getPendingInMem cfg nonce lockMVar
    clear = clearInMem lockMVar


------------------------------------------------------------------------------
-- | A 'bracket' function for in-memory mempools.
withInMemoryMempool :: ToJSON t
                    => FromJSON t
                    => InMemConfig t
                    -> (MempoolBackend t -> IO a)
                    -> IO a
withInMemoryMempool cfg f = do
    let action inMem = do
          back <- toMempoolBackend inMem
          f $! back
    bracket (makeInMemPool cfg) destroyInMemPool action

withInMemoryMempool_ :: ToJSON t
                     => FromJSON t
                     => Logger logger
                     => logger
                     -> InMemConfig t
                     -> (MempoolBackend t -> IO a)
                     -> IO a
withInMemoryMempool_ l cfg f = do
    let action inMem = do
          r <- race (monitor inMem) $ do
            back <- toMempoolBackend inMem
            f $! back
          case r of
            Left () -> throw $ InternalInvariantViolation "mempool monitor exited unexpectedly"
            Right result -> return result
    bracket (makeInMemPool cfg) destroyInMemPool action
  where
    monitor m = do
        let lf = logFunction l
        logFunctionText l Info $ "Initialized Mempool Monitor"
        runForeverThrottled lf "Chainweb.Mempool.InMem.withInMemoryMempool_.monitor" 10 (10 * mega) $ do
            stats <- getMempoolStats m
            logFunctionText l Debug $ "got stats"
            logFunctionJson l Info stats
            logFunctionText l Debug $ "logged stats"
            threadDelay 60000000 {- 1 minute -}

------------------------------------------------------------------------------
memberInMem :: MVar (InMemoryMempoolData t)
            -> Vector TransactionHash
            -> IO (Vector Bool)
memberInMem lock txs = do
    q <- withMVarMasked lock (readIORef . _inmemPending)
    V.mapM (memberOne q) txs

  where
    memberOne q txHash = return $! PSQ.member txHash q

------------------------------------------------------------------------------
lookupInMem :: MVar (InMemoryMempoolData t)
            -> Vector TransactionHash
            -> IO (Vector (LookupResult t))
lookupInMem lock txs = do
    q <- withMVarMasked lock (readIORef . _inmemPending)
    V.mapM (fmap fromJuste . lookupOne q) txs
  where
    lookupOne q txHash = return $! (lookupQ q txHash <|>
                                    pure Missing)
    lookupQ q txHash = Pending . snd <$> PSQ.lookup txHash q


------------------------------------------------------------------------------
markValidatedInMem :: MVar (InMemoryMempoolData t)
                   -> Vector TransactionHash
                   -> IO ()
markValidatedInMem lock txs = withMVarMasked lock $ \mdata -> do
    let pref = _inmemPending mdata
    modifyIORef' pref $ \psq -> foldl' (flip PSQ.delete) psq txs

------------------------------------------------------------------------------
insertInMem :: InMemConfig t    -- ^ in-memory config
            -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
            -> Vector t  -- ^ new transactions
            -> IO ()
insertInMem cfg lock txs0 = do
    txs <- V.filterM preGossipCheck txs0
    i <- withMVarMasked lock $ \mdata -> do
        newHashes <- V.mapM (insOne mdata) txs
        modifyIORef' (_inmemInserted mdata) (+ (length txs))
        modifyIORef' (_inmemRecentLog mdata) $
            recordRecentTransactions maxRecent newHashes
        readIORef (_inmemInserted mdata)

    unless (i < 5000) $ do
        withMVarMasked lock $ \mdata -> do
            modifyIORef' (_inmemPending mdata) $ \ps -> do
                if (PSQ.size ps > 10000)
                  then PSQ.fromList $ take 5000 $ PSQ.toList ps
                  else ps

  where
    preGossipCheck tx = do
        -- TODO: other well-formedness checks go here (e.g. ttl check)
        return $! sizeOK tx

    txcfg = _inmemTxCfg cfg
    getSize = txGasLimit txcfg
    maxSize = _inmemTxBlockSizeLimit cfg
    sizeOK tx = getSize tx <= maxSize
    maxRecent = _inmemMaxRecentItems cfg
    hasher = txHasher txcfg

    getPriority x = let r = txGasPrice txcfg x
                        s = txGasLimit txcfg x
                    in toPriority r s

    insOne mdata tx = do
        let !txhash = hasher tx
        modifyIORef' (_inmemPending mdata) $
           PSQ.insert txhash (getPriority tx) tx
        return txhash


------------------------------------------------------------------------------
getBlockInMem :: InMemConfig t
              -> MVar (InMemoryMempoolData t)
              -> MempoolPreBlockCheck t
              -> BlockHeight
              -> BlockHash
              -> GasLimit
              -> IO (Vector t)
getBlockInMem cfg lock txValidate bheight phash size0 = do
    withMVar lock $ \mdata -> do
        psq <- readIORef $ _inmemPending mdata
        go mdata psq size0 []

  where
    del !psq tx = let h = hasher tx
                  in PSQ.delete h psq

    hasher = txHasher txcfg
    txcfg = _inmemTxCfg cfg
    getSize = txGasLimit txcfg
    maxSize = _inmemTxBlockSizeLimit cfg
    sizeOK tx = getSize tx <= maxSize

    validateBatch mdata q = do
        oks1 <- txValidate bheight phash q
        let oks2 = V.map sizeOK q
        let oks = V.zipWith (&&) oks1 oks2
        let (good0, bad0) = V.partition snd $ V.zip q oks
        let good = V.map fst good0
        let bad = V.map fst bad0
        modifyIORef' (_inmemPending mdata) $ \psq ->
            let !psq' = V.foldl' del psq bad
            in psq'
        return good

    maxInARow :: Int
    maxInARow = 200

    nextBatch !psq !remainingGas = getBatch psq remainingGas [] 0
    getBatch !psq !sz !soFar !inARow
        -- we'll keep looking for transactions until we hit maxInARow that are
        -- too large
      | inARow >= maxInARow || sz <= 0 = (psq, soFar)
      | otherwise =
            case PSQ.minView psq of
              Nothing -> (psq, soFar)
              (Just (_, _, tx, !psq')) ->
                  let txSz = getSize tx
                  in if txSz <= sz
                       then getBatch psq' (sz - txSz) (tx:soFar) 0
                       else getBatch psq' sz soFar (inARow + 1)

    go !mdata !psq !remainingGas !soFar = do
        let (psq', nb) = nextBatch psq remainingGas
        if null nb
          then return $! V.concat soFar
          else do
            good <- validateBatch mdata (V.fromList nb)
            let newGas = V.foldl' (\s t -> s + getSize t) 0 good
            go mdata psq' (remainingGas - newGas) (good : soFar)


------------------------------------------------------------------------------
getPendingInMem :: InMemConfig t
                -> ServerNonce
                -> MVar (InMemoryMempoolData t)
                -> Maybe (ServerNonce, MempoolTxId)
                -> (Vector TransactionHash -> IO ())
                -> IO (ServerNonce, MempoolTxId)
getPendingInMem cfg nonce lock since callback = do
    (psq, !rlog) <- readLock
    maybe (sendAll psq) (sendSome psq rlog) since
    return $! (nonce, _rlNext rlog)

  where
    sendAll psq = do
        (dl, sz) <- foldlM go initState psq
        void $ sendChunk dl sz

    sendSome psq rlog (rNonce, oHw) = do
        if rNonce /= nonce
          then sendAll psq
          else sendSince psq rlog oHw

    sendSince psq rlog oHw = do
        let mbTxs = getRecentTxs maxNumRecent oHw rlog
        case mbTxs of
          Nothing -> sendAll psq
          Just txs -> do
              let isPending = flip PSQ.member psq
              callback $! V.fromList $ filter isPending txs

    readLock = withMVar lock $ \mdata -> do
        !psq <- readIORef $ _inmemPending mdata
        rlog <- readIORef $ _inmemRecentLog mdata
        return $! (psq, rlog)

    initState = (id, 0)    -- difference list
    hash = txHasher $ _inmemTxCfg cfg
    maxNumRecent = _inmemMaxRecentItems cfg

    go (dl, !sz) tx = do
        let txhash = hash tx
        let dl' = dl . (txhash:)
        let !sz' = sz + 1
        if sz' >= chunkSize
          then do sendChunk dl' sz'
                  return initState
          else return $! (dl', sz')

    chunkSize = 1024 :: Int

    sendChunk _ 0 = return ()
    sendChunk dl _ = callback $! V.fromList $ dl []

------------------------------------------------------------------------------
clearInMem :: MVar (InMemoryMempoolData t) -> IO ()
clearInMem lock = do
    withMVarMasked lock $ \mdata -> do
        writeIORef (_inmemPending mdata) PSQ.empty
        writeIORef (_inmemRecentLog mdata) emptyRecentLog


------------------------------------------------------------------------------
emptyRecentLog :: RecentLog
emptyRecentLog = RecentLog 0 mempty

recordRecentTransactions :: Int -> Vector TransactionHash -> RecentLog -> RecentLog
recordRecentTransactions maxNumRecent newTxs rlog = rlog'
  where
    !rlog' = RecentLog { _rlNext = newNext
                       , _rlRecent = newL
                       }

    numNewItems = V.length newTxs
    oldNext = _rlNext rlog
    newNext = oldNext + fromIntegral numNewItems
    newTxs' = V.reverse (V.map (T2 oldNext) newTxs)
    newL' = newTxs' <> _rlRecent rlog
    newL = force $ V.take maxNumRecent newL'


-- | Get the recent transactions from the transaction log. Returns Nothing if
-- the old high water mark is too out of date.
getRecentTxs :: Int -> MempoolTxId -> RecentLog -> Maybe [TransactionHash]
getRecentTxs maxNumRecent oldHw rlog
    | oldHw <= oldestHw || oldHw > oldNext = Nothing
    | oldHw == oldNext = Just mempty
    | otherwise = Just $! V.toList txs

  where
    oldNext = _rlNext rlog
    oldestHw = oldNext - fromIntegral maxNumRecent
    txs = V.map ssnd $ V.takeWhile pred $ _rlRecent rlog
    pred (T2 x _) = x >= oldHw

------------------------------------------------------------------------------
getMempoolStats :: InMemoryMempool t -> IO MempoolStats
getMempoolStats m = do
    withMVar (_inmemDataLock m) $ \d -> MempoolStats
        <$> (length <$> readIORef (_inmemPending d))
        <*> (length . _rlRecent <$> readIORef (_inmemRecentLog d))
