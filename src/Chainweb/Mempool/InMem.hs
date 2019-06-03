{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Concurrent (forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.MVar
    (MVar, newMVar, readMVar, withMVar, withMVarMasked)
import Control.Exception (bracket, bracketOnError, mask_)
import Control.Monad (forever, void, (<$!>))

import Data.Foldable (foldl', foldlM)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashPSQ as PSQ
import qualified Data.HashSet as HashSet
import Data.Int (Int64)
import Data.IORef
    (IORef, mkWeakIORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Ord (Down(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.ForeignPtr

import Pact.Types.Gas (GasPrice(..))

import Prelude hiding (init, lookup)

-- internal imports

import Chainweb.Mempool.InMemTypes
import Chainweb.Mempool.Mempool
import qualified Chainweb.Time as Time
import Chainweb.Utils (fromJuste)


------------------------------------------------------------------------------
toPriority :: GasPrice -> Int64 -> Priority
toPriority r s = (Down r, s)


------------------------------------------------------------------------------
makeInMemPool :: InMemConfig t
              -> IO (InMemoryMempool t)
makeInMemPool cfg = mask_ $ do
    dataLock <- newInMemMempoolData >>= newMVar
    tid <- forkIOWithUnmask (reaperThread cfg dataLock)
    return $! InMemoryMempool cfg dataLock tid

destroyInMemPool :: InMemoryMempool t -> IO ()
destroyInMemPool = mask_ . killThread . _inmemReaper


------------------------------------------------------------------------------
newInMemMempoolData :: IO (InMemoryMempoolData t)
newInMemMempoolData = InMemoryMempoolData <$!> newIORef PSQ.empty
                           <*> newIORef HashMap.empty
                           <*> newIORef HashSet.empty
                           <*> newIORef Nothing
                           <*> newZeroCounter
  where
    newZeroCounter = do
        let !val = 0 :: Word64
        fp <- mallocPlainForeignPtrAlignedBytes 8 (alignment val)
        withForeignPtr fp (flip poke val)
        return fp


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
            -> Int64
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
      getPnd (ref, _wk) a b = do
          mpl <- readIORef ref
          mb <- toMempoolBackend mpl
          x <- mempoolGetPendingTransactions mb a b
          writeIORef ref mpl
          return x

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
    lockMVar = _inmemDataLock mempool

    InMemConfig tcfg blockSizeLimit _ = cfg
    member = memberInMem lockMVar
    lookup = lookupInMem lockMVar
    insert = insertInMem cfg lockMVar
    getBlock = getBlockInMem cfg lockMVar
    markValidated = markValidatedInMem cfg lockMVar
    markConfirmed = markConfirmedInMem lockMVar
    reintroduce = reintroduceInMem cfg lockMVar
    getPending = getPendingInMem cfg lockMVar
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
    withMVarMasked lock $ \mdata -> V.mapM_ (insOne mdata) txs

  where
    txcfg = _inmemTxCfg cfg
    validateTx = txValidate txcfg
    getSize = txGasLimit txcfg
    maxSize = _inmemTxBlockSizeLimit cfg
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
            modifyIORef' (_inmemPending mdata) $
               PSQ.insert txhash (getPriority tx) tx
            return (tx, True)
          else return (tx, False)
      where
        txhash = hasher tx


------------------------------------------------------------------------------
getBlockInMem :: InMemConfig t
              -> MVar (InMemoryMempoolData t)
              -> Int64
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
                -> MVar (InMemoryMempoolData t)
                -> Maybe MempoolTxId
                -> (Vector TransactionHash -> IO ())
                -> IO MempoolTxId
getPendingInMem cfg lock _first callback = do
    (psq, hw) <- readLock
    -- TODO: either:
    --  1) add a tx log, if _first is set then try to read off the tx log
    --  2) (easier, slower) add tx id to maps and filter by it here
    (dl, sz) <- foldlM go initState psq
    void $ sendChunk dl sz
    return hw

  where
    readLock = withMVar lock $ \mdata -> do
        !psq <- readIORef $ _inmemPending mdata
        !hw <- withForeignPtr (_inmemNextTxId mdata) peek
        return $! (psq, hw)

    initState = (id, 0)    -- difference list
    hash = txHasher $ _inmemTxCfg cfg

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
    sendChunk dl _ = callback $ V.fromList $ dl []

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
        -- we won't reset the broadcaster but that's ok, the same one can be
        -- re-used
