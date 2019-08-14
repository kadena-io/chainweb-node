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

    -- * Low-level create/destroy functions
  , makeInMemPool
  , newInMemMempoolData
  ) where

------------------------------------------------------------------------------
import Control.Applicative (pure, (<|>))
import Control.Concurrent.MVar (MVar, newMVar, withMVar, withMVarMasked)
import Control.DeepSeq
import Control.Exception (bracket, mask_)
import Control.Monad (void, when, (<$!>))

import Data.Aeson
import Data.Foldable (foldl', foldlM)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashPSQ as PSQ
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Ord (Down(..))
import Data.Tuple.Strict
import Data.Vector (Vector)
import qualified Data.Vector as V

import Pact.Types.Gas (GasPrice(..))

import Prelude hiding (init, lookup, pred)

import System.Random

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
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
    InMemoryMempoolData <$!> newIORef PSQ.empty
                        <*> newIORef emptyRecentLog
                        <*> newIORef mempty


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
      , mempoolQuarantine = quarantine
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
    quarantine = quarantineInMem cfg lockMVar
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
    let qref = _inmemQuarantine mdata
    modifyIORef' pref $ \psq -> foldl' (flip PSQ.delete) psq txs
    modifyIORef' qref $ \q -> foldl' (flip HashMap.delete) q txs


------------------------------------------------------------------------------
quarantineInMem :: InMemConfig t    -- ^ in-memory config
                -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
                -> Vector t  -- ^ new transactions
                -> IO ()
quarantineInMem cfg lock txs =
    withMVarMasked lock $ \mdata -> V.mapM_ (insOne mdata) txs
  where
    txcfg = _inmemTxCfg cfg
    hasher = txHasher txcfg
    getSize = txGasLimit txcfg
    maxSize = _inmemTxBlockSizeLimit cfg
    sizeOK tx = getSize tx <= maxSize

    insOne mdata tx = do
        let !txhash = hasher tx
        if sizeOK tx
          then void $ modifyIORef' (_inmemQuarantine mdata) $ HashMap.insert txhash tx
          else return ()


------------------------------------------------------------------------------
insertInMem :: InMemConfig t    -- ^ in-memory config
            -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
            -> Vector t  -- ^ new transactions
            -> IO ()
insertInMem cfg lock txs = do
    withMVarMasked lock $ \mdata -> do
        newHashes <- V.mapM (insOne mdata) txs
        modifyIORef' (_inmemRecentLog mdata) $
            recordRecentTransactions maxRecent newHashes

  where
    txcfg = _inmemTxCfg cfg
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
              -> BlockHeight
              -> BlockHash
              -> GasLimit
              -> IO (Vector t)
getBlockInMem cfg lock pheight phash size0 = do
    -- validate quarantined instructions.
    withMVar lock $ \mdata -> do
        let qref = _inmemQuarantine mdata
        q <- V.fromList . HashMap.elems <$> readIORef qref
        void $ validateBatch mdata q True
        writeIORef qref mempty
        psq <- readIORef $ _inmemPending mdata
        go mdata psq size0 []

  where
    getPriority x = let r = txGasPrice txcfg x
                        s = txGasLimit txcfg x
                    in toPriority r s

    ins !psq !tx = let h = hasher tx
                       p = getPriority tx
                   in PSQ.insert h p tx psq

    del !psq tx = let h = hasher tx
                  in PSQ.delete h psq

    hasher = txHasher txcfg
    txcfg = _inmemTxCfg cfg
    maxRecent = _inmemMaxRecentItems cfg
    getSize = txGasLimit txcfg
    maxSize = _inmemTxBlockSizeLimit cfg
    sizeOK tx = getSize tx <= maxSize

    validateBatch mdata q doInsert = do
        oks1 <- txValidate txcfg pheight phash q
        let oks2 = V.map sizeOK q
        let oks = V.zipWith (&&) oks1 oks2
        let (good0, bad0) = V.partition snd $ V.zip q oks
        let good = V.map fst good0
        let bad = V.map fst bad0
        modifyIORef' (_inmemPending mdata) $ \psq ->
            let !psq' = if doInsert
                          then V.foldl' ins psq good
                          else psq
                !psq'' = V.foldl' del psq' bad
            in psq''
        when doInsert $ modifyIORef' (_inmemRecentLog mdata) $
            recordRecentTransactions maxRecent (V.map hasher good)
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
            good <- validateBatch mdata (V.fromList nb) False
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

