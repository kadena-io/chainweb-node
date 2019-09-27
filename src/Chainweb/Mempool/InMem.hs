{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar, newMVar, withMVar, withMVarMasked)
import Control.DeepSeq
import Control.Exception (bracket, mask_, throw)
import Control.Monad (void, (<$!>))

import Data.Aeson
import qualified Data.ByteString.Short as SB
import Data.Foldable (foldl', foldlM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tuple.Strict
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Tim as TimSort

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
import Chainweb.Time
import Chainweb.Utils

------------------------------------------------------------------------------
toPriority :: GasPrice -> GasLimit -> T2 (Down GasPrice) GasLimit
toPriority r s = T2 (Down r) s
{-# INLINE toPriority #-}

------------------------------------------------------------------------------
compareOnGasPrice :: TransactionConfig t -> t -> t -> Ordering
compareOnGasPrice txcfg a b = compare aa bb
  where
    getGL = txGasLimit txcfg
    getGP = txGasPrice txcfg
    !aa = toPriority (getGP a) (getGL a)
    !bb = toPriority (getGP b) (getGL b)
{-# INLINE compareOnGasPrice #-}

------------------------------------------------------------------------------
makeInMemPool :: InMemConfig t
              -> IO (InMemoryMempool t)
makeInMemPool cfg = mask_ $ do
    nonce <- randomIO
    dataLock <- newInMemMempoolData >>= newMVar
    return $! InMemoryMempool cfg dataLock nonce

destroyInMemPool :: InMemoryMempool t -> IO ()
destroyInMemPool = const $ return ()


------------------------------------------------------------------------------
newInMemMempoolData :: IO (InMemoryMempoolData t)
newInMemMempoolData =
    InMemoryMempoolData <$!> newIORef 0
                        <*> newIORef mempty
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
      , mempoolMarkValidated = markValidated
      , mempoolGetBlock = getBlock
      , mempoolGetPendingTransactions = getPending
      , mempoolClear = clear
      }
  where
    cfg = _inmemCfg mempool
    nonce = _inmemNonce mempool
    lockMVar = _inmemDataLock mempool

    InMemConfig tcfg blockSizeLimit _ _ = cfg
    member = memberInMem lockMVar
    lookup = lookupInMem tcfg lockMVar
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
            approximateThreadDelay 60000000 {- 1 minute -}

------------------------------------------------------------------------------
memberInMem :: MVar (InMemoryMempoolData t)
            -> Vector TransactionHash
            -> IO (Vector Bool)
memberInMem lock txs = do
    q <- withMVarMasked lock (readIORef . _inmemPending)
    V.mapM (memberOne q) txs

  where
    memberOne q txHash = return $! HashMap.member txHash q

------------------------------------------------------------------------------
lookupInMem :: TransactionConfig t
            -> MVar (InMemoryMempoolData t)
            -> Vector TransactionHash
            -> IO (Vector (LookupResult t))
lookupInMem txcfg lock txs = do
    q <- withMVarMasked lock (readIORef . _inmemPending)
    V.mapM (fmap fromJuste . lookupOne q) txs
  where
    lookupOne q txHash = return $! (lookupQ q txHash <|> pure Missing)
    codec = txCodec txcfg
    fixup !bs = either (const Missing) Pending
                    $! codecDecode codec
                    $! SB.fromShort bs
    lookupQ q txHash = fixup <$!> HashMap.lookup txHash q


------------------------------------------------------------------------------
markValidatedInMem :: MVar (InMemoryMempoolData t)
                   -> Vector TransactionHash
                   -> IO ()
markValidatedInMem lock txs = withMVarMasked lock $ \mdata -> do
    let pref = _inmemPending mdata
    modifyIORef' pref $ \psq -> foldl' (flip HashMap.delete) psq txs

maxNumPending :: Int
maxNumPending = 10000

------------------------------------------------------------------------------
insertInMem :: InMemConfig t    -- ^ in-memory config
            -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
            -> InsertType
            -> Vector t  -- ^ new transactions
            -> IO ()
insertInMem cfg lock runCheck txs0 = do
    badmap <- withMVarMasked lock $ readIORef . _inmemBadMap
    -- release the lock before we call the user filter -- in case it recurses
    txhashes <- preInsertFilter badmap txs0
    withMVarMasked lock $ \mdata -> do
        let countRef = _inmemCountPending mdata
        cnt <- readIORef countRef
        let txs = V.take (max 0 (maxNumPending - cnt)) txhashes
        let numTxs = V.length txs
        let newCnt = cnt + numTxs
        writeIORef countRef $! newCnt
        pending <- readIORef (_inmemPending mdata)
        let (!pending', newHashesDL) = V.foldl' insOne (pending, id) txs
        let !newHashes = V.fromList $ newHashesDL []
        writeIORef (_inmemPending mdata) $! force pending'
        modifyIORef' (_inmemRecentLog mdata) $
            recordRecentTransactions maxRecent newHashes

  where
    preInsertFilter b txs =
        let hashes = V.map hasher txs
            txhashes = V.zip hashes txs
        in if runCheck == CheckedInsert
             then preInsertFilterReal b txhashes
             else return txhashes

    preInsertFilterReal badmap txs = do
        now <- getCurrentTimeIntegral
        let txs1 = prevalidate [sizeOK, ttlCheck now, notInBadMap badmap] txs
        oks <- _inmemPreInsertCheck cfg $ V.map snd txs1
        return $! V.map fst $ V.filter snd $ V.zip txs1 oks

    notInBadMap badmap (h, _) = not (HashMap.member h badmap)

    prevalidate validations = V.map fst .
                              V.filter snd .
                              V.map (\tx -> (tx, all ($ tx) validations))
    txcfg = _inmemTxCfg cfg
    encodeTx = codecEncode (txCodec txcfg)
    getSize = txGasLimit txcfg
    maxSize = _inmemTxBlockSizeLimit cfg
    sizeOK (_, tx) = getSize tx <= maxSize
    maxRecent = _inmemMaxRecentItems cfg
    hasher = txHasher txcfg
    ttlCheck (Time (TimeSpan now)) (_, tx) =
      case txMetadata txcfg tx of
        TransactionMetadata (Time (TimeSpan creationTime)) (Time (TimeSpan expiryTime)) ->
            creationTime < now && now < expiryTime && creationTime < expiryTime

    insOne (!pending, !soFar) !(txhash, tx) =
        let !bytes = SB.toShort $! encodeTx tx
        in (HashMap.insert txhash bytes pending, soFar . (txhash:))


------------------------------------------------------------------------------
getBlockInMem :: forall t .
                 InMemConfig t
              -> MVar (InMemoryMempoolData t)
              -> MempoolPreBlockCheck t
              -> BlockHeight
              -> BlockHash
              -> GasLimit
              -> IO (Vector t)
getBlockInMem cfg lock txValidate bheight phash size0 = do
    withMVar lock $ \mdata -> do
        !psq0 <- readIORef $ _inmemPending mdata
        now <- getCurrentTimeIntegral
        !badmap <- pruneBadMap now <$!> readIORef (_inmemBadMap mdata)
        let !psq = HashMap.map decodeTx psq0
        T3 psq' badmap' out <- go psq badmap size0 []

        -- put the txs chosen for the block back into the map -- they don't get
        -- expunged until they are mined and validated by consensus.
        let !psq'' = HashMap.map (SB.toShort . encodeTx) $! V.foldl' ins psq' out
        writeIORef (_inmemPending mdata) $! force psq''
        writeIORef (_inmemCountPending mdata) $! HashMap.size psq''
        writeIORef (_inmemBadMap mdata) $! force badmap'
        return $! out

  where
    pruneBadMap now = HashMap.filter (> now)

    ins !h !t = HashMap.insert (hasher t) t h
    insBadMap !h !t = let endTime = txMetaExpiryTime (txMetadata txcfg t)
                      in HashMap.insert (hasher t) endTime h

    del !psq tx = let h = hasher tx
                  in HashMap.delete h psq

    hasher = txHasher txcfg
    txcfg = _inmemTxCfg cfg
    codec = txCodec txcfg
    decodeTx tx0 = either err id $! codecDecode codec tx
      where
        !tx = SB.fromShort tx0
        err s = error $
                mconcat [ "Error decoding tx (\""
                        , s
                        , "\"): tx was: "
                        , T.unpack (T.decodeUtf8 tx)
                        ]
    encodeTx = codecEncode codec
    getSize = txGasLimit txcfg
    maxSize = _inmemTxBlockSizeLimit cfg
    sizeOK tx = getSize tx <= maxSize

    validateBatch
        :: HashMap TransactionHash t
        -> BadMap
        -> Vector t
        -> IO (T3 (Vector t) (HashMap TransactionHash t) BadMap)
    validateBatch !psq0 !badmap q = do
        oks1 <- txValidate bheight phash q
        let oks2 = V.map sizeOK q
        let !oks = V.zipWith (&&) oks1 oks2
        let (good1, bad1) = V.partition snd $! V.zip q oks
        let !good = V.map fst good1

        -- remove considered txs -- successful ones will be re-added at the end
        let !psq' = V.foldl' del psq0 q
        -- txs that fail pre-block validation get sent to the naughty list.
        let !badmap' = V.foldl' insBadMap badmap (V.map fst bad1)
        return $! T3 good psq' badmap'

    maxInARow :: Int
    maxInARow = 200

    unconsV v = T2 (V.unsafeHead v) (V.unsafeTail v)

    nextBatch
        :: HashMap TransactionHash t
        -> GasLimit
        -> IO [t]
    nextBatch !psq !remainingGas = do
        let !pendingTxs0 = V.fromList $ HashMap.elems psq
        mPendingTxs <- V.unsafeThaw pendingTxs0
        TimSort.sortBy (compareOnGasPrice txcfg) mPendingTxs
        !pendingTxs <- V.unsafeFreeze mPendingTxs
        return $! getBatch pendingTxs remainingGas [] 0

    getBatch
        :: Vector t
        -> GasLimit
        -> [t]
        -> Int
        -> [t]
    getBatch !pendingTxs !sz !soFar !inARow
        -- we'll keep looking for transactions until we hit maxInARow that are
        -- too large
      | V.null pendingTxs = soFar
      | inARow >= maxInARow || sz <= 0 = soFar
      | otherwise = do
            let (T2 !tx !pendingTxs') = unconsV pendingTxs
            let txSz = getSize tx
            if txSz <= sz
              then do
                  getBatch pendingTxs' (sz - txSz) (tx:soFar) 0
              else getBatch pendingTxs' sz soFar (inARow + 1)

    go :: HashMap TransactionHash t
       -> BadMap
       -> GasLimit
       -> [Vector t]
       -> IO (T3 (HashMap TransactionHash t) BadMap (Vector t))
    go !psq !badmap !remainingGas !soFar = do
        nb <- nextBatch psq remainingGas
        if null nb
          then return $! T3 psq badmap (V.concat soFar)
          else do
            T3 good psq' badmap' <- validateBatch psq badmap $! V.fromList nb
            let newGas = V.foldl' (\s t -> s + getSize t) 0 good
            go psq' badmap' (remainingGas - newGas) (good : soFar)


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
        let keys = HashMap.keys psq
        (dl, sz) <- foldlM go initState keys
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
              let isPending = flip HashMap.member psq
              callback $! V.fromList $ filter isPending txs

    readLock = withMVar lock $ \mdata -> do
        !psq <- readIORef $ _inmemPending mdata
        rlog <- readIORef $ _inmemRecentLog mdata
        return $! (psq, rlog)

    initState = (id, 0)    -- difference list
    maxNumRecent = _inmemMaxRecentItems cfg

    go (dl, !sz) txhash = do
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
        writeIORef (_inmemPending mdata) mempty
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
        <$!> (HashMap.size <$!> readIORef (_inmemPending d))
        <*> (length . _rlRecent <$!> readIORef (_inmemRecentLog d))
        <*> (HashMap.size <$!> readIORef (_inmemBadMap d))
