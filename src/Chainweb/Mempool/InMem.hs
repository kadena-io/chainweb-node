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
  , makeSelfFinalizingInMemPool

    -- * Low-level create/destroy functions
  , makeInMemPool
  , newInMemMempoolData

    -- * statistics
  , getMempoolStats
  ) where

------------------------------------------------------------------------------
import Control.Applicative (pure, (<|>))
import Control.Concurrent (forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
    (MVar, newMVar, readMVar, withMVar, withMVarMasked)
import Control.DeepSeq
import Control.Exception (bracket, bracketOnError, mask_, throw)
import Control.Monad (forever, void, (<$!>))

import Data.Foldable (foldl', foldlM)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashPSQ as PSQ
import qualified Data.HashSet as HashSet
import Data.IORef
    (IORef, mkWeakIORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Ord (Down(..))
import Data.Tuple.Strict
import Data.Vector (Vector)
import qualified Data.Vector as V

import Pact.Types.Gas (GasPrice(..))

import Prelude hiding (init, lookup, pred)

import System.LogLevel
import System.Random

-- internal imports

import Chainweb.Logger
import Chainweb.Mempool.InMemTypes
import Chainweb.Mempool.Mempool
import qualified Chainweb.Time as Time
import Chainweb.Utils (InternalInvariantViolation(..), fromJuste, runForeverThrottled, mega)

------------------------------------------------------------------------------
toPriority :: GasPrice -> GasLimit -> Priority
toPriority r s = (Down r, s)


------------------------------------------------------------------------------
makeInMemPool :: InMemConfig t
              -> IO (InMemoryMempool t)
makeInMemPool cfg = mask_ $ do
    nonce <- randomIO
    dataLock <- newInMemMempoolData >>= newMVar
    tid <- forkIOWithUnmask (reaperThread cfg dataLock)
    return $! InMemoryMempool cfg dataLock tid nonce

destroyInMemPool :: InMemoryMempool t -> IO ()
destroyInMemPool = mask_ . killThread . _inmemReaper

getMempoolStats :: InMemoryMempool t -> IO MempoolStats
getMempoolStats m = do
    d <- readMVar $ _inmemDataLock m
    MempoolStats
        <$> (length <$> readIORef (_inmemPending d))
        <*> (length <$> readIORef (_inmemValidated d))
        <*> (length <$> readIORef (_inmemConfirmed d))
        <*> (length . _rlRecent <$> readIORef (_inmemRecentLog d))

------------------------------------------------------------------------------
newInMemMempoolData :: IO (InMemoryMempoolData t)
newInMemMempoolData = InMemoryMempoolData <$!> newIORef PSQ.empty
                           <*> newIORef HashMap.empty
                           <*> newIORef HashSet.empty
                           <*> newIORef Nothing
                           <*> newIORef emptyRecentLog


------------------------------------------------------------------------------
makeSelfFinalizingInMemPool :: InMemConfig t
                            -> IO (MempoolBackend t)
makeSelfFinalizingInMemPool cfg =
    mask_ $ bracketOnError (makeInMemPool cfg) destroyInMemPool $ \mpool -> do
        ref <- newIORef mpool
        wk <- mkWeakIORef ref (destroyInMemPool mpool)
        back <- toMempoolBackend mpool
        let txcfg = mempoolTxConfig back
        let bsl = mempoolBlockGasLimit back
        return $ wrapBackend txcfg bsl (ref, wk)

----------------------------------------------------------------------------------------------------
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
      , mempoolMarkValidated = withRef mp . flip mempoolMarkValidated
      , mempoolMarkConfirmed = withRef mp . flip mempoolMarkConfirmed
      , mempoolReintroduce = withRef mp . flip mempoolReintroduce
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
reaperThread :: InMemConfig t
             -> MVar (InMemoryMempoolData t)
             -> (forall a . IO a -> IO a)
             -> IO b
reaperThread cfg dataLock restore = forever $ do
    restore $ threadDelay interval   -- TODO: randomize wait time slightly to
                                     -- avoid thundering herd on wakeup
    withMVar dataLock $ \mdata -> reap mdata
  where
    txcfg = _inmemTxCfg cfg
    expiryTime = txMetaExpiryTime . (txMetadata txcfg)
    interval = _inmemReaperIntervalMicros cfg
    reap (InMemoryMempoolData pendingRef _ _ _ _) = do
        now <- Time.getCurrentTimeIntegral
        modifyIORef' pendingRef $ reapPending now

    reapPending !now !pending =
        let agg k _ !tx !txs = if expiryTime tx <= now
                               then (k:txs) else txs
            tooOld = PSQ.fold' agg [] pending
        in foldl' (flip PSQ.delete) pending tooOld

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
      , mempoolMarkValidated = markValidated
      , mempoolMarkConfirmed = markConfirmed
      , mempoolReintroduce = reintroduce
      , mempoolGetPendingTransactions = getPending
      , mempoolClear = clear
      }
  where
    cfg = _inmemCfg mempool
    nonce = _inmemNonce mempool
    lockMVar = _inmemDataLock mempool

    InMemConfig tcfg blockSizeLimit _ _ _ = cfg
    member = memberInMem lockMVar
    lookup = lookupInMem lockMVar
    insert = insertInMem cfg lockMVar
    getBlock = getBlockInMem cfg lockMVar
    markValidated = markValidatedInMem cfg lockMVar
    markConfirmed = markConfirmedInMem lockMVar
    reintroduce = reintroduceInMem cfg lockMVar
    getPending = getPendingInMem cfg nonce lockMVar
    clear = clearInMem lockMVar


------------------------------------------------------------------------------
-- | A 'bracket' function for in-memory mempools.
withInMemoryMempool :: InMemConfig t
                    -> (MempoolBackend t -> IO a)
                    -> IO a
withInMemoryMempool cfg f = do
    let action inMem = do
          back <- toMempoolBackend inMem
          f $! back
    bracket (makeInMemPool cfg) destroyInMemPool action

withInMemoryMempool_ :: Logger logger
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
    (q, validated, confirmed) <- withMVarMasked lock $ \mdata -> do
        q <- readIORef $ _inmemPending mdata
        validated <- readIORef $ _inmemValidated mdata
        confirmed <- readIORef $ _inmemConfirmed mdata
        return $! (q, validated, confirmed)
    return $! V.map (memberOne q validated confirmed) txs

  where
    memberOne q validated confirmed txHash =
        PSQ.member txHash q ||
        HashMap.member txHash validated ||
        HashSet.member txHash confirmed

------------------------------------------------------------------------------
lookupInMem :: MVar (InMemoryMempoolData t)
            -> Vector TransactionHash
            -> IO (Vector (LookupResult t))
lookupInMem lock txs = do
    (q, validated, confirmed) <- withMVarMasked lock $ \mdata -> do
        q <- readIORef $ _inmemPending mdata
        validated <- readIORef $ _inmemValidated mdata
        confirmed <- readIORef $ _inmemConfirmed mdata
        return $! (q, validated, confirmed)
    return $! V.map (fromJuste . lookupOne q validated confirmed) txs
  where
    lookupOne q validated confirmed txHash =
        lookupQ q txHash <|>
        lookupVal validated txHash <|>
        lookupConfirmed confirmed txHash <|>
        pure Missing

    lookupQ q txHash = Pending . snd <$> PSQ.lookup txHash q
    lookupVal val txHash = Validated <$> HashMap.lookup txHash val
    lookupConfirmed confirmed txHash =
        if HashSet.member txHash confirmed
          then Just Confirmed
          else Nothing


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
    exists mdata txhash = do
        valMap <- readIORef $ _inmemValidated mdata
        confMap <- readIORef $ _inmemConfirmed mdata
        return $! (HashMap.member txhash valMap || HashSet.member txhash confMap)
    insOne mdata tx = do
        b <- exists mdata txhash
        v <- validateTx tx
        -- TODO: return error on unsuccessful validation?
        if v && not b && sizeOK tx
          then do
            -- TODO: is it any better to build up a PSQ in pure code and then
            -- union? Union is only one modifyIORef
            modifyIORef' (_inmemPending mdata) $
               PSQ.insert txhash (getPriority tx) tx
            return (txhash, True)
          else return (txhash, False)
      where
        txhash = hasher tx


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
markValidatedInMem :: InMemConfig t
                   -> MVar (InMemoryMempoolData t)
                   -> Vector (ValidatedTransaction t)
                   -> IO ()
markValidatedInMem cfg lock txs = withMVarMasked lock $ \mdata ->
    V.mapM_ (validateOne mdata) txs
  where
    hash = txHasher $ _inmemTxCfg cfg

    validateOne mdata tx = do
        let txhash = hash $ validatedTransaction tx
        modifyIORef' (_inmemPending mdata) $ PSQ.delete txhash
        modifyIORef' (_inmemValidated mdata) $ HashMap.insert txhash tx


------------------------------------------------------------------------------
markConfirmedInMem :: MVar (InMemoryMempoolData t)
                   -> Vector TransactionHash
                   -> IO ()
markConfirmedInMem lock txhashes =
    withMVarMasked lock $ \mdata -> V.mapM_ (confirmOne mdata) txhashes
  where
    confirmOne mdata txhash = do
        modifyIORef' (_inmemPending mdata) $ PSQ.delete txhash
        modifyIORef' (_inmemValidated mdata) $ HashMap.delete txhash
        modifyIORef' (_inmemConfirmed mdata) $ HashSet.insert txhash


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
reintroduceInMem' :: InMemConfig t
                  -> MVar (InMemoryMempoolData t)
                  -> Vector TransactionHash
                  -> IO ()
reintroduceInMem' cfg lock txhashes = do
    withMVarMasked lock $ \mdata ->
        V.mapM_ (reintroduceOne mdata) txhashes

  where
    txcfg = _inmemTxCfg cfg
    price = txGasPrice txcfg
    limit = txGasLimit txcfg
    getPriority x = let r = price x
                        s = limit x
                    in toPriority r s
    reintroduceOne mdata txhash = do
        m <- HashMap.lookup txhash <$> readIORef (_inmemValidated mdata)
        maybe (return ()) (reintroduceIt mdata txhash) m
    reintroduceIt mdata txhash (ValidatedTransaction _ _ tx) = do
        modifyIORef' (_inmemValidated mdata) $ HashMap.delete txhash
        modifyIORef' (_inmemPending mdata) $ PSQ.insert txhash (getPriority tx) tx

------------------------------------------------------------------------------
reintroduceInMem :: InMemConfig t
                 -> MVar (InMemoryMempoolData t)
                 -> Vector t
                 -> IO ()
reintroduceInMem cfg lock txs =
    reintroduceInMem' cfg lock (V.map hashIt txs)
  where
    hashIt = txHasher $ _inmemTxCfg cfg

------------------------------------------------------------------------------
clearInMem :: MVar (InMemoryMempoolData t) -> IO ()
clearInMem lock = do
    withMVarMasked lock $ \mdata -> do
        writeIORef (_inmemPending mdata) PSQ.empty
        writeIORef (_inmemValidated mdata) HashMap.empty
        writeIORef (_inmemConfirmed mdata) HashSet.empty
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
    txs = V.map (\(T2 _ b) -> b) $ V.takeWhile pred $ _rlRecent rlog
    pred (T2 x _) = x >= oldHw

