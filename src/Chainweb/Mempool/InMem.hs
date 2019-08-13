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
  , makeSelfFinalizingInMemPool

    -- * Low-level create/destroy functions
  , makeInMemPool
  , newInMemMempoolData
  ) where

------------------------------------------------------------------------------
import Control.Applicative (pure, (<|>))
import Control.Concurrent.MVar
    (MVar, newMVar, readMVar, withMVar, withMVarMasked)
import Control.DeepSeq
import Control.Exception (bracket, bracketOnError, mask_)
import Control.Monad (void, (<$!>))

import Data.Aeson
import Data.Foldable (foldlM)
import qualified Data.HashPSQ as PSQ
import Data.IORef
    (IORef, mkWeakIORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Ord (Down(..))
import Data.Tuple.Strict
import Data.Vector (Vector)
import qualified Data.Vector as V

import Pact.Types.Gas (GasPrice(..))

import Prelude hiding (init, lookup, pred)

import System.Random

-- internal imports

import Chainweb.Mempool.InMemTypes
import Chainweb.Mempool.Mempool
import Chainweb.Utils (fromJuste, ssnd)

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
    InMemoryMempoolData <$!> newIORef PSQ.empty <*> newIORef emptyRecentLog


------------------------------------------------------------------------------
makeSelfFinalizingInMemPool :: FromJSON t
                            => ToJSON t
                            => InMemConfig t
                            -> IO (MempoolBackend t)
makeSelfFinalizingInMemPool cfg =
    mask_ $ bracketOnError (makeInMemPool cfg) destroyInMemPool $ \mpool -> do
        ref <- newIORef mpool
        wk <- mkWeakIORef ref (destroyInMemPool mpool)
        back <- toMempoolBackend mpool
        let txcfg = mempoolTxConfig back
        let bsl = mempoolBlockGasLimit back
        return $ wrapBackend txcfg bsl (ref, wk)

------------------------------------------------------------------------------
wrapBackend :: TransactionConfig t
            -> GasLimit
            -> (IORef (InMemoryMempool t), b)
            -> MempoolBackend t
wrapBackend txcfg bsl mp =
      MempoolBackend
      { mempoolTxConfig = txcfg
      , mempoolBlockGasLimit = bsl
      , mempoolMember = withRef mp . flip mempoolMember
      , mempoolLookup = withRef mp . flip mempoolLookup
      , mempoolInsert = withRef mp . flip mempoolInsert
      , mempoolGetBlock = withRef mp . flip mempoolGetBlock
      , mempoolGetPendingTransactions = getPnd mp
      , mempoolClear = withRef mp mempoolClear
      }
    where
      getPnd :: (IORef (InMemoryMempool t), a)
             -> Maybe HighwaterMark
             -> (Vector TransactionHash -> IO ())
             -> IO HighwaterMark
      getPnd (ref, _wk) a b = do
          mpl <- readIORef ref
          mb <- toMempoolBackend mpl
          x <- mempoolGetPendingTransactions mb a b
          writeIORef ref mpl
          return x

      withRef :: (IORef (InMemoryMempool t), a)
              -> (MempoolBackend t -> IO z)
              -> IO z
      withRef (ref, _wk) f = do
            mpl <- readIORef ref
            mb <- toMempoolBackend mpl
            x <- f mb
            writeIORef ref mpl
            return x

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
insertInMem :: InMemConfig t    -- ^ in-memory config
            -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
            -> Vector t  -- ^ new transactions
            -> IO ()
insertInMem cfg lock txs = do
    withMVarMasked lock $ \mdata -> do
        newHashes <- (V.map fst . V.filter snd) <$> V.mapM (insOne mdata) txs
        modifyIORef' (_inmemRecentLog mdata) $
            recordRecentTransactions maxRecent newHashes

  where
    txcfg = _inmemTxCfg cfg
    validateTx = txValidate txcfg
    getSize = txGasLimit txcfg
    maxSize = _inmemTxBlockSizeLimit cfg
    maxRecent = _inmemMaxRecentItems cfg
    hasher = txHasher txcfg

    sizeOK tx = getSize tx <= maxSize
    getPriority x = let r = txGasPrice txcfg x
                        s = txGasLimit txcfg x
                    in toPriority r s

    exists _mdata _txhash = do
        -- TODO: here's the hook into the validation oracle
        error "TODO: call validation oracle here"

    insOne mdata tx = do
        let !txhash = hasher tx
        let good = (txhash, True)
        let bad = (txhash, False)

        b <- exists mdata txhash
        if b
          then return $! bad
          else do
            v <- validateTx tx
            -- TODO: return error on unsuccessful validation?
            if v && sizeOK tx
              then do
                -- TODO: is it any better to build up a PSQ in pure code and then
                -- union? Union is only one modifyIORef
                --
                -- or, this should be a fold inside modifyIORef'
                modifyIORef' (_inmemPending mdata) $
                   PSQ.insert txhash (getPriority tx) tx
                return $! good
              else return $! bad


------------------------------------------------------------------------------
getBlockInMem :: InMemConfig t
              -> MVar (InMemoryMempoolData t)
              -> GasLimit
              -> IO (Vector t)
getBlockInMem cfg lock size0 = do
    psq <- readMVar lock >>= (readIORef . _inmemPending)
    return $! V.unfoldr go (psq, size0)

  where
    -- as the block is getting full, we'll skip ahead this many transactions to
    -- try to find a smaller tx to fit in the block. DECIDE: exhaustive search
    -- of pending instead?
    maxSkip = 30 :: Int
    getSize = txGasLimit $ _inmemTxCfg cfg

    go (psq, sz) = lookahead sz maxSkip psq

    lookahead _ 0 _ = Nothing
    lookahead !sz !skipsLeft !psq = do
        (_, _, tx, psq') <- PSQ.minView psq
        let txSz = getSize tx
        if txSz <= sz
          then return (tx, (psq', sz - txSz))
          else lookahead sz (skipsLeft - 1) psq'


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

