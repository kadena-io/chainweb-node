{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
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

  , validateOne
  , txTTLCheck
  ) where

------------------------------------------------------------------------------
import Control.Applicative ((<|>))
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Error.Util (hush)
import Control.Exception (bracket, evaluate, mask_, throw)
import Control.Monad

import Data.Aeson
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Short as SB
import Data.Decimal
import Data.Foldable (foldl', foldlM)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Tim as TimSort

import Pact.Parse

import Prelude hiding (init, lookup, pred)

import System.LogLevel
import System.Random

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Mempool.CurrentTxs
import Chainweb.Mempool.InMemTypes
import Chainweb.Mempool.Mempool
import Chainweb.Pact.Utils (maxTTL)
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion)

------------------------------------------------------------------------------
compareOnGasPrice :: TransactionConfig t -> t -> t -> Ordering
compareOnGasPrice txcfg a b = compare aa bb
  where
    getGP = txGasPrice txcfg
    !aa = Down $ getGP a
    !bb = Down $ getGP b
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
    InMemoryMempoolData <$!> newIORef mempty
                        <*> newIORef emptyRecentLog
                        <*> newIORef mempty
                        <*> newIORef newCurrentTxs


------------------------------------------------------------------------------
toMempoolBackend
    :: NFData t
    => Logger logger
    => logger
    -> InMemoryMempool t
    -> IO (MempoolBackend t)
toMempoolBackend logger mempool = do
    return $! MempoolBackend
      { mempoolTxConfig = tcfg
      , mempoolMember = member
      , mempoolLookup = lookup
      , mempoolInsert = insert
      , mempoolInsertCheck = insertCheck
      , mempoolMarkValidated = markValidated
      , mempoolAddToBadList = addToBadList
      , mempoolCheckBadList = checkBadList
      , mempoolGetBlock = getBlock
      , mempoolPrune = prune
      , mempoolGetPendingTransactions = getPending
      , mempoolClear = clear
      }
  where
    cfg = _inmemCfg mempool
    nonce = _inmemNonce mempool
    lockMVar = _inmemDataLock mempool

    InMemConfig tcfg _ _ _ _ _ _ = cfg
    member = memberInMem lockMVar
    lookup = lookupInMem tcfg lockMVar
    insert = insertInMem cfg lockMVar
    insertCheck = insertCheckInMem cfg lockMVar
    markValidated = markValidatedInMem logger tcfg lockMVar
    addToBadList = addToBadListInMem lockMVar
    checkBadList = checkBadListInMem lockMVar
    getBlock = getBlockInMem logger cfg lockMVar
    getPending = getPendingInMem cfg nonce lockMVar
    prune = pruneInMem lockMVar
    clear = clearInMem lockMVar


------------------------------------------------------------------------------
-- | A 'bracket' function for in-memory mempools.
--
-- This function is only used in testing. Use 'withInMemoryMempool_' for
-- production.
--
withInMemoryMempool :: ToJSON t
                    => FromJSON t
                    => NFData t
                    => InMemConfig t
                    -> ChainwebVersion
                    -> (MempoolBackend t -> IO a)
                    -> IO a
withInMemoryMempool cfg _v f = do
    let action inMem = do
          back <- toMempoolBackend l inMem
          f $! back
    bracket (makeInMemPool cfg) destroyInMemPool action
  where
    l = genericLogger Debug (\ _ -> return ())

-- | A 'bracket' function for in-memory mempools.
--
withInMemoryMempool_ :: Logger logger
                     => NFData t
                     => logger
                     -> InMemConfig t
                     -> ChainwebVersion
                     -> (MempoolBackend t -> IO a)
                     -> IO a
withInMemoryMempool_ l cfg _v f = do
    let action inMem = do
          r <- race (monitor inMem) $ do
            back <- toMempoolBackend l inMem
            f $! back
          case r of
            Left () -> throw $ InternalInvariantViolation "mempool monitor exited unexpectedly"
            Right result -> return result
    bracket (makeInMemPool cfg) destroyInMemPool action
  where
    monitor m = do
        let lf = logFunction l
        logFunctionText l Info "Initialized Mempool Monitor"
        runForeverThrottled lf "Chainweb.Mempool.InMem.withInMemoryMempool_.monitor" 10 (10 * mega) $ do
            stats <- getMempoolStats m
            logFunctionText l Debug "got stats"
            logFunctionJson l Info stats
            logFunctionText l Debug "logged stats"
            approximateThreadDelay 60_000_000 {- 1 minute -}

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
lookupInMem :: NFData t
            => TransactionConfig t
            -> MVar (InMemoryMempoolData t)
            -> Vector TransactionHash
            -> IO (Vector (LookupResult t))
lookupInMem txcfg lock txs = do
    q <- withMVarMasked lock (readIORef . _inmemPending)
    v <- V.mapM (evaluate . force . fromJuste . lookupOne q) txs
    return $! v
  where
    lookupOne q txHash = lookupQ q txHash <|> pure Missing
    codec = txCodec txcfg
    fixup pe =
        let bs = _inmemPeBytes pe
        in either (const Missing) Pending
               $! codecDecode codec
               $! SB.fromShort bs
    lookupQ q txHash = fixup <$!> HashMap.lookup txHash q


------------------------------------------------------------------------------
markValidatedInMem
    :: Logger logger
    => logger
    -> TransactionConfig t
    -> MVar (InMemoryMempoolData t)
    -> Vector t
    -> IO ()
markValidatedInMem logger tcfg lock txs = withMVarMasked lock $ \mdata -> do
    modifyIORef' (_inmemPending mdata) $ \psq ->
        foldl' (flip HashMap.delete) psq hashes

    -- This isn't atomic, which is fine. If something goes wrong we may end up
    -- with some false negatives, which means that the mempool would use more
    -- resources for pending txs.
    --
    let curTxIdxRef = _inmemCurrentTxs mdata
    let validatedCount = length (V.zip expiries hashes)
    logg Debug $ "mark " <> sshow validatedCount <> " txs as validated"
    x <- readIORef curTxIdxRef
    !x' <- currentTxsInsertBatch x (V.zip expiries hashes)
    when (currentTxsSize x /= currentTxsSize x') $ do
      logg Info $ "previous current tx index size: " <> sshow (currentTxsSize x)
      logg Info $ "new current tx index size: " <> sshow (currentTxsSize x')
    writeIORef curTxIdxRef x'
  where
    hashes = txHasher tcfg <$> txs
    expiries = txMetaExpiryTime . txMetadata tcfg <$> txs

    logg = logFunctionText logger

------------------------------------------------------------------------------
addToBadListInMem :: MVar (InMemoryMempoolData t)
                  -> Vector TransactionHash
                  -> IO ()
addToBadListInMem lock txs = withMVarMasked lock $ \mdata -> do
    !pnd <- readIORef $ _inmemPending mdata
    !bad <- readIORef $ _inmemBadMap mdata
    let !pnd' = foldl' (flip HashMap.delete) pnd txs
    -- we don't have the expiry time here, so just use maxTTL
    now <- getCurrentTimeIntegral
    let (ParsedInteger mt) = maxTTL
    let !endTime = add (secondsToTimeSpan $ fromIntegral mt) now
    let !bad' = foldl' (\h tx -> HashMap.insert tx endTime h) bad txs
    writeIORef (_inmemPending mdata) pnd'
    writeIORef (_inmemBadMap mdata) bad'


------------------------------------------------------------------------------
checkBadListInMem
    :: MVar (InMemoryMempoolData t)
    -> Vector TransactionHash
    -> IO (Vector Bool)
checkBadListInMem lock hashes = withMVarMasked lock $ \mdata -> do
    !bad <- readIORef $ _inmemBadMap mdata
    return $! V.map (`HashMap.member` bad) hashes


maxNumPending :: Int
maxNumPending = 10000

------------------------------------------------------------------------------

-- | Validation: A short-circuiting variant of this check that fails outright at
-- the first detection of any validation failure on any Transaction.
--
insertCheckInMem
    :: forall t
    .  NFData t
    => InMemConfig t    -- ^ in-memory config
    -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
    -> Vector t  -- ^ new transactions
    -> IO (Either (T2 TransactionHash InsertError) ())
insertCheckInMem cfg lock txs
  | V.null txs = pure $ Right ()
  | otherwise = do
    now <- getCurrentTimeIntegral
    badmap <- withMVarMasked lock $ readIORef . _inmemBadMap
    curTxIdx <- withMVarMasked lock $ readIORef . _inmemCurrentTxs

    -- We hash the tx here and pass it around around to avoid needing to repeat
    -- the hashing effort.
    let withHashes :: Either (T2 TransactionHash InsertError) (Vector (T2 TransactionHash t))
        withHashes = for txs $ \tx ->
          let !h = hasher tx
          in bimap (T2 h) (T2 h) $ validateOne cfg badmap curTxIdx now tx h

    case withHashes of
        Left _ -> pure $! void withHashes
        Right r -> void . sequenceA <$!> _inmemPreInsertBatchChecks cfg r
  where
    hasher :: t -> TransactionHash
    hasher = txHasher (_inmemTxCfg cfg)

-- | Validation: Confirm the validity of some single transaction @t@.
-- Note that this function is not called during block validation. This
-- merely exists to validate a transaction entering the mempool.
validateOne
    :: forall t a
    .  NFData t
    => InMemConfig t
    -> HashMap TransactionHash a
    -> CurrentTxs
    -> Time Micros
    -> t
    -> TransactionHash
    -> Either InsertError t
validateOne cfg badmap curTxIdx now t h =
    sizeOK
    >> gasPriceRoundingCheck
    >> gasPriceMinCheck
    >> ttlCheck
    >> notDuplicate
    >> notInBadMap
    >> _inmemPreInsertPureChecks cfg t
  where
    txcfg :: TransactionConfig t
    txcfg = _inmemTxCfg cfg

    expiry :: Time Micros
    expiry = txMetaExpiryTime $ txMetadata txcfg t

    sizeOK :: Either InsertError ()
    sizeOK = ebool_ (InsertErrorOversized maxSize) (getSize t <= maxSize)
      where
        getSize = txGasLimit txcfg
        maxSize = _inmemTxBlockSizeLimit cfg

    -- prop_tx_gas_min
    gasPriceMinCheck :: Either InsertError ()
    gasPriceMinCheck = ebool_ (InsertErrorUndersized (getPrice t) minGasPrice) (getPrice t >= minGasPrice)
      where
        minGasPrice = _inmemTxMinGasPrice cfg
        getPrice = txGasPrice txcfg

    -- prop_tx_gas_rounding
    gasPriceRoundingCheck :: Either InsertError ()
    gasPriceRoundingCheck =
        ebool_ (InsertErrorOther msg) (f (txGasPrice txcfg t))
      where
        f (GasPrice (ParsedDecimal d)) = decimalPlaces d <= 12
        msg = mconcat
            [ "This  transaction's gas price: "
            , sshow (txGasPrice txcfg t)
            , " is not correctly rounded."
            , "It should be rounded to at most 12 decimal places."
            ]

    -- prop_tx_ttl_arrival
    ttlCheck :: Either InsertError ()
    ttlCheck = txTTLCheck txcfg now t

    notInBadMap :: Either InsertError ()
    notInBadMap = maybe (Right ()) (const $ Left InsertErrorBadlisted) $ HashMap.lookup h badmap

    notDuplicate :: Either InsertError ()
    notDuplicate
        | currentTxsMember curTxIdx expiry h = Left InsertErrorDuplicate
        | otherwise = Right ()

-- | Check the TTL of a transaction.
txTTLCheck :: TransactionConfig t -> Time Micros -> t -> Either InsertError ()
txTTLCheck txcfg (Time (TimeSpan now)) t =
    ebool_ InsertErrorInvalidTime (ct < now && now < et && ct < et)
  where
    TransactionMetadata (Time (TimeSpan ct)) (Time (TimeSpan et)) = txMetadata txcfg t


-- | Validation: Similar to `insertCheckInMem`, but does not short circuit.
-- Instead, bad transactions are filtered out and the successful ones are kept.
--
insertCheckInMem'
    :: forall t
    .  NFData t
    => InMemConfig t    -- ^ in-memory config
    -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
    -> Vector t  -- ^ new transactions
    -> IO (Vector (T2 TransactionHash t))
insertCheckInMem' cfg lock txs
  | V.null txs = pure V.empty
  | otherwise = do
    now <- getCurrentTimeIntegral
    badmap <- withMVarMasked lock $ readIORef . _inmemBadMap
    curTxIdx <- withMVarMasked lock $ readIORef . _inmemCurrentTxs

    let withHashes :: Vector (T2 TransactionHash t)
        withHashes = flip V.mapMaybe txs $ \tx ->
          let !h = hasher tx
          in (T2 h) <$> hush (validateOne cfg badmap curTxIdx now tx h)

    V.mapMaybe hush <$!> _inmemPreInsertBatchChecks cfg withHashes
  where
    txcfg = _inmemTxCfg cfg
    hasher = txHasher txcfg

insertInMem
    :: forall t
    .  NFData t
    => InMemConfig t    -- ^ in-memory config
    -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
    -> InsertType
    -> Vector t  -- ^ new transactions
    -> IO ()
insertInMem cfg lock runCheck txs0 = do
    txhashes <- insertCheck
    withMVarMasked lock $ \mdata -> do
        pending <- readIORef (_inmemPending mdata)
        let cnt = HashMap.size pending
        let txs = V.take (max 0 (maxNumPending - cnt)) txhashes
        let T2 !pending' !newHashesDL = V.foldl' insOne (T2 pending id) txs
        let !newHashes = V.fromList $ newHashesDL []
        writeIORef (_inmemPending mdata) $! force pending'
        modifyIORef' (_inmemRecentLog mdata) $
            recordRecentTransactions maxRecent newHashes
  where
    insertCheck :: IO (Vector (T2 TransactionHash t))
    insertCheck = if runCheck == CheckedInsert
                  then insertCheckInMem' cfg lock txs0
                  else return $! V.map (\tx -> T2 (hasher tx) tx) txs0

    txcfg = _inmemTxCfg cfg
    encodeTx = codecEncode (txCodec txcfg)
    maxRecent = _inmemMaxRecentItems cfg
    hasher = txHasher txcfg

    insOne (T2 pending soFar) (T2 txhash tx) =
        let !gp = txGasPrice txcfg tx
            !gl = txGasLimit txcfg tx
            !bytes = SB.toShort $! encodeTx tx
            !expTime = txMetaExpiryTime $ txMetadata txcfg tx
            !x = PendingEntry gp gl bytes expTime
        in T2 (HashMap.insert txhash x pending) (soFar . (txhash:))


------------------------------------------------------------------------------
getBlockInMem
    :: forall t . NFData t
    => forall l . Logger l
    => l
    -> InMemConfig t
    -> MVar (InMemoryMempoolData t)
    -> BlockFill
    -> MempoolPreBlockCheck t
    -> BlockHeight
    -> BlockHash
    -> IO (Vector t)
getBlockInMem logg cfg lock (BlockFill gasLimit txHashes _)  txValidate bheight phash = do
    logFunctionText logg Info $ "getBlockInMem: " <> sshow (gasLimit,bheight,phash)
    withMVar lock $ \mdata -> do
        now <- getCurrentTimeIntegral

        -- drop any expired transactions.
        pruneInternal mdata now
        !(T2 psq seen) <- filterSeen <$> readIORef (_inmemPending mdata)
        !badmap <- readIORef (_inmemBadMap mdata)
        let size0 = gasLimit

        -- get our batch of output transactions, along with a new pending map
        -- and badmap
        T3 psq' badmap' out <- go psq badmap size0 []

        -- put the txs chosen for the block back into the map -- they don't get
        -- expunged until they are mined and validated by consensus.
        let !psq'' = V.foldl' ins (HashMap.union seen psq') out
        writeIORef (_inmemPending mdata) $! force psq''
        writeIORef (_inmemBadMap mdata) $! force badmap'
        mout <- V.thaw $ V.map (snd . snd) out
        TimSort.sortBy (compareOnGasPrice txcfg) mout
        V.unsafeFreeze mout

  where

    filterSeen = (`HashMap.foldlWithKey'` (T2 mempty mempty)) $ \(T2 unseens seens) k v ->
      if S.member k txHashes then (T2 unseens (HashMap.insert k v seens))
      else (T2 (HashMap.insert k v unseens) seens)

    ins !m (!h,(!b,!t)) =
        let !pe = PendingEntry (txGasPrice txcfg t)
                               (txGasLimit txcfg t)
                               b
                               (txMetaExpiryTime $ txMetadata txcfg t)
        in HashMap.insert h pe m

    insBadMap !m (!h,(_,!t)) = let endTime = txMetaExpiryTime (txMetadata txcfg t)
                              in HashMap.insert h endTime m

    del !psq (h, _) = HashMap.delete h psq

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
    getSize = txGasLimit txcfg
    maxSize = _inmemTxBlockSizeLimit cfg
    sizeOK tx = getSize tx <= maxSize

    validateBatch
        :: PendingMap
        -> BadMap
        -> Vector (TransactionHash, (SB.ShortByteString, t))
        -> IO (T3 (Vector (TransactionHash, (SB.ShortByteString, t)))
                  PendingMap
                  BadMap)
    validateBatch !psq0 !badmap q = do
        let txs = V.map (snd . snd) q
        oks1 <- txValidate bheight phash txs
        let oks2 = V.map sizeOK txs
        let !oks = V.zipWith (&&) oks1 oks2
        let (good, bad1) = V.partition snd $! V.zip q oks

        -- remove considered txs -- successful ones will be re-added at the end
        let !psq' = V.foldl' del psq0 q
        -- txs that fail pre-block validation get sent to the naughty list.
        let !badmap' = V.foldl' insBadMap badmap (V.map fst bad1)
        return $! T3 (V.map fst good) psq' badmap'

    maxInARow :: Int
    maxInARow = 200

    unconsV v = T2 (V.unsafeHead v) (V.unsafeTail v)

    nextBatch
        :: PendingMap
        -> GasLimit
        -> IO [(TransactionHash, (SB.ShortByteString, t))]
    nextBatch !psq !remainingGas = do
        let !pendingTxs0 = HashMap.toList psq
        mPendingTxs <- mutableVectorFromList pendingTxs0
        TimSort.sortBy (compare `on` snd) mPendingTxs
        !pendingTxs <- V.unsafeFreeze mPendingTxs
        return $! getBatch pendingTxs remainingGas [] 0

    getBatch
        :: Vector (TransactionHash, PendingEntry)
        -> GasLimit
        -> [(TransactionHash, (SB.ShortByteString, t))]
        -> Int
        -> [(TransactionHash, (SB.ShortByteString, t))]
    getBatch !pendingTxs !sz !soFar !inARow
        -- we'll keep looking for transactions until we hit maxInARow that are
        -- too large
      | V.null pendingTxs = soFar
      | inARow >= maxInARow || sz <= 0 = soFar
      | otherwise = do
            let (T2 (h, pe) !pendingTxs') = unconsV pendingTxs
            let !txbytes = _inmemPeBytes pe
            let !tx = decodeTx txbytes
            let !txSz = getSize tx
            if txSz <= sz
              then getBatch pendingTxs' (sz - txSz) ((h,(txbytes, tx)):soFar) 0
              else getBatch pendingTxs' sz soFar (inARow + 1)

    go :: PendingMap
       -> BadMap
       -> GasLimit
       -> [Vector (TransactionHash, (SB.ShortByteString, t))]
       -> IO (T3 PendingMap BadMap
                 (Vector (TransactionHash, (SB.ShortByteString, t))))
    go !psq !badmap !remainingGas !soFar = do
        nb <- nextBatch psq remainingGas
        if null nb
          then return $! T3 psq badmap (V.concat soFar)
          else do
            T3 good psq' badmap' <- validateBatch psq badmap $! V.fromList nb
            let !newGas = V.foldl' (\s (_, (_, t)) -> s + getSize t) 0 good
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
    return (nonce, _rlNext rlog)

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
        return (psq, rlog)

    initState = (id, 0)    -- difference list
    maxNumRecent = _inmemMaxRecentItems cfg

    go (dl, !sz) txhash = do
        let dl' = dl . (txhash:)
        let !sz' = sz + 1
        if sz' >= chunkSize
          then do sendChunk dl' sz'
                  return initState
          else return (dl', sz')

    chunkSize = 1024 :: Int

    sendChunk _ 0 = return ()
    sendChunk dl _ = callback $! V.fromList $ dl []

------------------------------------------------------------------------------
clearInMem :: MVar (InMemoryMempoolData t) -> IO ()
clearInMem lock = newInMemMempoolData >>= void . swapMVar lock

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
        <*> (currentTxsSize <$!> readIORef (_inmemCurrentTxs d))

------------------------------------------------------------------------------
-- | Prune the mempool's pending map and badmap.
--
-- Complexity is linear in the size of the mempool, which is fine if it isn't
-- applied to often and at a constant rate.
--
pruneInMem
    :: forall t . NFData t
    => MVar (InMemoryMempoolData t)
    -> IO ()
pruneInMem lock = do
    now <- getCurrentTimeIntegral
    withMVar lock $ \mdata -> pruneInternal mdata now


------------------------------------------------------------------------------
pruneInternal
    :: forall t . NFData t
    => InMemoryMempoolData t
    -> Time Micros
    -> IO ()
pruneInternal mdata now = do
    let pref = _inmemPending mdata
    !pending <- readIORef pref
    !pending' <- evaluate $ force $ HashMap.filter flt pending
    writeIORef pref pending'

    let bref = _inmemBadMap mdata
    !badmap <- (force . pruneBadMap) <$!> readIORef bref
    writeIORef bref badmap

  where
    -- keep transactions that expire in the future.
    flt pe = _inmemPeExpires pe > now
    pruneBadMap = HashMap.filter (> now)
