{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | A mock in-memory mempool backend that does not persist to disk.
module Chainweb.Mempool.InMem
  (
   -- * Initialization functions
    startInMemoryMempoolTest
  , withInMemoryMempool

    -- * Low-level create/destroy functions
  , makeInMemPool
  , newInMemMempoolData

  , txTTLCheck
  ) where

#if MIN_VERSION_base(4,20,0)
import Data.Foldable (foldlM)
#else
import Data.Foldable (foldl', foldlM)
#endif

import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Mempool.CurrentTxs
import Chainweb.Mempool.InMemTypes
import Chainweb.Mempool.Mempool
import Chainweb.Pact4.Validations (defaultMaxTTL, defaultMaxCoinDecimalPlaces)
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion)
import Control.Applicative ((<|>))
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Error.Util (hush)
import Control.Exception (evaluate, mask_, throw)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (runExceptT, throwError)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SB
import Data.Decimal
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List qualified as List
import Data.Maybe
import Data.Ord
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Tim qualified as TimSort
import Numeric.AffineSpace
import Pact.Parse
import Pact.Types.ChainMeta qualified as P
import Prelude hiding (init, lookup, pred)
import System.LogLevel
import System.Random

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

------------------------------------------------------------------------------

newInMemMempoolData :: IO (InMemoryMempoolData t)
newInMemMempoolData =
    InMemoryMempoolData <$!> newIORef mempty
                        <*> newIORef emptyRecentLog
                        <*> newIORef mempty
                        <*> newIORef newCurrentTxs

------------------------------------------------------------------------------
toMempoolBackend
    :: forall t logger
    . NFData t
    => Logger logger
    => logger
    -> InMemoryMempool t
    -> IO (MempoolBackend t)
toMempoolBackend logger mempool = do
    return $! MempoolBackend
      { mempoolTxConfig = tcfg
      , mempoolMember = memberInMem lockMVar
      , mempoolLookup = lookupInMem tcfg lockMVar
      , mempoolLookupEncoded = lookupEncodedInMem lockMVar
      , mempoolInsert = insertInMem logger cfg lockMVar
      , mempoolInsertCheck = insertCheckInMem logger cfg lockMVar
      , mempoolInsertCheckVerbose = insertCheckVerboseInMem logger cfg lockMVar
      , mempoolMarkValidated = markValidatedInMem logger tcfg lockMVar
      , mempoolAddToBadList = addToBadListInMem lockMVar
      , mempoolCheckBadList = checkBadListInMem lockMVar
      , mempoolGetBlock = getBlockInMem logger cfg lockMVar
      , mempoolPrune = pruneInMem logger lockMVar
      , mempoolGetPendingTransactions = getPendingInMem cfg nonce lockMVar
      , mempoolClear = clearInMem lockMVar
      }
  where
    cfg = _inmemCfg mempool
    nonce = _inmemNonce mempool
    lockMVar = _inmemDataLock mempool

    InMemConfig tcfg _ _ _ _ _ _ = cfg

------------------------------------------------------------------------------
-- | A 'bracket' function for in-memory mempools.
--
-- This function is only used in testing. Use 'withInMemoryMempool' for
-- production.
--
startInMemoryMempoolTest
  :: NFData t
  => InMemConfig t
  -> IO (MempoolBackend t)
startInMemoryMempoolTest cfg = do
    toMempoolBackend l =<< makeInMemPool cfg
  where
    l = genericLogger Debug (\ _ -> return ())

-- | A 'bracket' function for in-memory mempools.
--
withInMemoryMempool
  :: Logger logger
  => NFData t
  => logger
  -> InMemConfig t
  -> ChainwebVersion
  -> (MempoolBackend t -> IO a)
  -> IO a
withInMemoryMempool l cfg _v f = do
    let action inMem = do
          r <- race (monitor inMem) $ do
            back <- toMempoolBackend l inMem
            f $! back
          case r of
            Left () -> throw $ InternalInvariantViolation "mempool monitor exited unexpectedly"
            Right result -> return result
    action =<< makeInMemPool cfg
  where
    monitor m = do
        let lf = logFunction l
        logFunctionText l Debug "Initialized Mempool Monitor"
        runForeverThrottled lf "Chainweb.Mempool.InMem.withInMemoryMempool.monitor" 10 (10 * mega) $ do
            stats <- getMempoolStats m
            logFunctionJson l Info stats
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
lookupEncodedInMem :: NFData t
            => MVar (InMemoryMempoolData t)
            -> Vector TransactionHash
            -> IO (Vector (LookupResult ByteString))
lookupEncodedInMem lock txs = do
    q <- withMVarMasked lock (readIORef . _inmemPending)
    V.mapM (evaluate . fromJuste . lookupOne q) txs
  where
    lookupOne q txHash = lookupQ q txHash <|> pure Missing
    fixup pe =
        let bs = _inmemPeBytes pe
        in Pending $! SB.fromShort bs
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
      logg Debug $ "previous current tx index size: " <> sshow (currentTxsSize x)
      logg Debug $ "new current tx index size: " <> sshow (currentTxsSize x')
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
    let P.TTLSeconds (ParsedInteger mt) = defaultMaxTTL
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
-- This function is used when a transaction is inserted into the mempool. It is
-- NOT used when a new block is created. For the latter more strict validation
-- methods are used. In particular TTL validation is uses the current time as
-- reference in the former case and the creation time of the parent header in
-- the latter case.
--
insertCheckInMem
    :: forall logger t. (NFData t, Logger logger)
    => logger
    -> InMemConfig t    -- ^ in-memory config
    -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
    -> Vector t  -- ^ new transactions
    -> IO (Either (T2 TransactionHash InsertError) ())
insertCheckInMem logger cfg lock txs
  | V.null txs = pure $ Right ()
  | otherwise = do
    now <- getCurrentTimeIntegral
    badmap <- withMVarMasked lock $ readIORef . _inmemBadMap
    curTxIdx <- withMVarMasked lock $ readIORef . _inmemCurrentTxs

    -- We hash the tx here and pass it around around to avoid needing to repeat
    -- the hashing effort.
    withHashes :: Either (T2 TransactionHash InsertError) (Vector (T2 TransactionHash t)) <- runExceptT $ do
      forM txs $ \tx -> do
        let !h = hasher tx
        case validateOne cfg badmap curTxIdx now tx h of
          Right t -> pure (T2 h t)
          Left insertErr -> do
            liftIO $ logValidateOneFailure logger "insertCheckInMem" h insertErr
            throwError (T2 h insertErr)

    case withHashes of
        Left _ -> pure $! void withHashes
        Right r -> void . sequenceA <$!> _inmemPreInsertBatchChecks cfg r
  where
    hasher :: t -> TransactionHash
    hasher = txHasher (_inmemTxCfg cfg)

-- | This function is used when a transaction(s) is inserted into the mempool via
--   the service API. It is NOT used when a new block is created.
--   For the latter, more strict validation methods are used. In particular, TTL validation
--   uses the current time as reference in the former case (mempool insertion)
--   and the creation time of the parent header in the latter case (new block creation).
--
insertCheckVerboseInMem
    :: forall t logger. (NFData t, Logger logger)
    => logger
    -> InMemConfig t    -- ^ in-memory config
    -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
    -> Vector t  -- ^ new transactions
    -> IO (Vector (T2 TransactionHash (Either InsertError t)))
insertCheckVerboseInMem logger cfg lock txs
  | V.null txs = return V.empty
  | otherwise = do
      now <- getCurrentTimeIntegral
      badmap <- withMVarMasked lock $ readIORef . _inmemBadMap
      curTxIdx <- withMVarMasked lock $ readIORef . _inmemCurrentTxs
      
      withHashesAndPositions :: (HashMap TransactionHash (Int, InsertError), HashMap TransactionHash (Int, t)) <- do
        pos <- flip V.imapM txs $ \i tx -> do
          let !h = hasher tx
          case validateOne cfg badmap curTxIdx now tx h of
            Right t -> pure (i, h, Right t)
            Left insertErr -> do
              logValidateOneFailure logger "insertCheckVerboseInMem" h insertErr
              pure (i, h, Left insertErr)

        pure
          $ over _1 (HashMap.fromList . V.toList)
          $ over _2 (HashMap.fromList . V.toList)
          $ V.partitionWith (\(i, h, e) -> bimap (\err -> (h, (i, err))) (\err -> (h, (i, err))) e)
          $ pos

      let (prevFailures, prevSuccesses) = withHashesAndPositions

      preInsertBatchChecks <- _inmemPreInsertBatchChecks cfg (V.fromList $ List.map (\(h, (_, t)) -> T2 h t) $ HashMap.toList prevSuccesses)

      let update (failures, successes) result = case result of
              Left (T2 txHash insertError) ->
                case HashMap.lookup txHash successes of
                  Just (i, _) ->
                    -- add to failures and remove from successes
                    ( HashMap.insert txHash (i, insertError) failures
                    , HashMap.delete txHash successes
                    )
                  Nothing -> error "insertCheckInMem: impossible"
              -- nothing to do; the successes already contains this value.
              Right _ -> (failures, successes)
      let (failures, successes) = V.foldl' update (prevFailures, prevSuccesses) preInsertBatchChecks

      let allEntries =
            [ (i, T2 txHash (Left insertError))
            | (txHash, (i, insertError)) <- HashMap.toList failures
            ] ++
            [ (i, T2 txHash (Right val))
            | (txHash, (i, val)) <- HashMap.toList successes
            ]
      let sortedEntries = V.fromList $ List.map snd $ List.sortBy (compare `on` fst) allEntries

      return sortedEntries
  where
    hasher :: t -> TransactionHash
    hasher = txHasher (_inmemTxCfg cfg)

-- | Validation: Confirm the validity of some single transaction @t@.
--
-- This function is only used during insert checks. TTL validation is done in
-- the context of the current time (now).
--
-- This function is NOT used during the pre validation when creating a new
-- block.
--
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
        f (GasPrice (ParsedDecimal d)) = decimalPlaces d <= defaultMaxCoinDecimalPlaces
        msg = T.unwords
            [ "This transaction's gas price:"
            , sshow (txGasPrice txcfg t)
            , "is not correctly rounded."
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
--
-- A grace period of 10 seconds is applied for the tx creation time to account for
-- clock shifts between client and server. While this can slightly increase the
-- chance that the transaction is rejected later during validation, this is fine
-- because this value is still much smaller than the grace period in the final
-- validation where 95 seconds are allowed (cf.
-- "Chainweb.Pact.Utils.lenientTimeSlop").
--
-- This check is used when a TX is inserted into the mempool. The reference time
-- for the check is the current time. The validation for inclusion into a block
-- is the creation time of the parent block. Therefor success in this function
-- doesn't guarantee succesfull validation in the context of a block.
--
txTTLCheck :: TransactionConfig t -> Time Micros -> t -> Either InsertError ()
txTTLCheck txcfg now t = do
    ebool_ InsertErrorTimeInFuture (ct < now .+^ gracePeriod)
    ebool_ InsertErrorTTLExpired (now < et && ct < et)
  where
    TransactionMetadata ct et = txMetadata txcfg t
    gracePeriod = scaleTimeSpan @Int 10 second

-- | Validation: Similar to `insertCheckInMem`, but does not short circuit.
-- Instead, bad transactions are filtered out and the successful ones are kept.
--
-- This function is used when a transaction is inserted into the mempool. It is
-- NOT used when a new block is created. For the latter more strict validation
-- methods are used. In particular TTL validation is uses the current time as
-- reference in the former case and the creation time of the parent header in
-- the latter case.
--
insertCheckInMem'
    :: forall t logger. (NFData t, Logger logger)
    => logger
    -> InMemConfig t    -- ^ in-memory config
    -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
    -> Vector t  -- ^ new transactions
    -> IO (Vector (T2 TransactionHash t))
insertCheckInMem' logger cfg lock txs
  | V.null txs = pure V.empty
  | otherwise = do
    now <- getCurrentTimeIntegral
    badmap <- withMVarMasked lock $ readIORef . _inmemBadMap
    curTxIdx <- withMVarMasked lock $ readIORef . _inmemCurrentTxs

    withHashes :: Vector (T2 TransactionHash t) <- do
      flip V.mapMaybeM txs $ \tx -> do
        let !h = hasher tx
        case validateOne cfg badmap curTxIdx now tx h of
          Right t -> pure (Just (T2 h t))
          Left insertErr -> do
            logValidateOneFailure logger "insertCheckInMem'" h insertErr
            pure Nothing

    V.mapMaybe hush <$!> _inmemPreInsertBatchChecks cfg withHashes
  where
    txcfg = _inmemTxCfg cfg
    hasher = txHasher txcfg

insertInMem
    :: forall t logger. (NFData t, Logger logger)
    => logger
    -> InMemConfig t    -- ^ in-memory config
    -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
    -> InsertType
    -> Vector t  -- ^ new transactions
    -> IO ()
insertInMem logger cfg lock runCheck txs0 = do
    logFunctionText logger Debug $ "insertInMem: " <> sshow (runCheck, V.length txs0)
    txhashes <- insertCheck
    withMVarMasked lock $ \mdata -> do
        pending <- readIORef (_inmemPending mdata)
        logFunctionText logger Debug $ "insertInMem: pending txs: " <> sshow (HashMap.keys pending)
        let cnt = HashMap.size pending
        let txs = V.take (max 0 (maxNumPending - cnt)) txhashes
        let T2 !pending' !newHashesDL = V.foldl' insOne (T2 pending id) txs
        logFunctionText logger Debug $ "insertInMem: updated pending txs: " <> sshow (HashMap.keys pending')
        let !newHashes = V.fromList $ newHashesDL []
        writeIORef (_inmemPending mdata) $! force pending'
        modifyIORef' (_inmemRecentLog mdata) $
            recordRecentTransactions maxRecent newHashes
  where
    insertCheck :: IO (Vector (T2 TransactionHash t))
    insertCheck = case runCheck of
      CheckedInsert -> insertCheckInMem' logger cfg lock txs0
      UncheckedInsert -> return $! V.map (\tx -> T2 (hasher tx) tx) txs0

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
    :: forall t l to.
    (NFData t, Logger l)
    => l
    -> InMemConfig t
    -> MVar (InMemoryMempoolData t)
    -> BlockFill
    -> MempoolPreBlockCheck t to
    -> BlockHeight
    -> BlockHash
    -> IO (Vector to)
getBlockInMem logg cfg lock (BlockFill gasLimit txHashes _) txValidate bheight phash = do
    logFunctionText logg Debug $ "getBlockInMem: " <> sshow (gasLimit,bheight,phash)
    withMVar lock $ \mdata -> do
        now <- getCurrentTimeIntegral

        pendingDataBeforePrune <- readIORef (_inmemPending mdata)
        logFunctionText logg Debug $ "getBlockInMem: data before prune" <> sshow (HashMap.keys pendingDataBeforePrune)

        -- drop any expired transactions.
        pruneInternal logg mdata now

        pendingData <- readIORef (_inmemPending mdata)
        logFunctionText logg Debug $ "getBlockInMem: pending txs (pre filter-seen): " <> sshow (HashMap.keys pendingData)

        !(T2 psq seen) <- filterSeen <$> readIORef (_inmemPending mdata)
        logFunctionText logg Debug $ "getBlockInMem: pending txs (post filter-seen): " <> sshow (HashMap.keys psq)
        logFunctionText logg Debug $ "getBlockInMem: seen txs: " <> sshow (HashMap.keys seen)
        !badmap <- readIORef (_inmemBadMap mdata)
        logFunctionText logg Debug $ "getBlockInMem: bad txs: " <> sshow (HashMap.keys badmap)
        let size0 = gasLimit

        -- get our batch of output transactions, along with a new pending map
        -- and badmap
        T3 psq' badmap' out <- go psq badmap size0 []

        -- put the txs chosen for the block back into the map -- they don't get
        -- expunged until they are mined and validated by consensus.
        let !psq'' = V.foldl' ins (HashMap.union seen psq') out
        writeIORef (_inmemPending mdata) $! force psq''
        writeIORef (_inmemBadMap mdata) $! force badmap'
        mout <- V.thaw $ V.map (\(_, (_, t, tOut)) -> (t, tOut)) out
        TimSort.sortBy (compareOnGasPrice txcfg `on` fst) mout
        fmap snd <$> V.unsafeFreeze mout

  where

    filterSeen :: PendingMap -> T2 PendingMap (HashMap TransactionHash PendingEntry)
    filterSeen p = HashMap.foldlWithKey' loop (T2 mempty mempty) p
      where
        loop (T2 unseens seens) k v =
          if S.member k txHashes
          then T2 unseens (HashMap.insert k v seens)
          else T2 (HashMap.insert k v unseens) seens

    ins !m (!h,(!b,!t,_)) =
        let !pe = PendingEntry (txGasPrice txcfg t)
                               (txGasLimit txcfg t)
                               b
                               (txMetaExpiryTime $ txMetadata txcfg t)
        in HashMap.insert h pe m

    insBadMap !m (!h,!t) = let endTime = txMetaExpiryTime (txMetadata txcfg t)
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
    sizeOK tx = when (getSize tx > maxSize) (Left $ InsertErrorOversized maxSize)

    validateBatch
        :: PendingMap
        -> BadMap
        -> Vector (TransactionHash, (SB.ShortByteString, t))
        -> IO (T3 [(TransactionHash, (SB.ShortByteString, t, to))]
                  PendingMap
                  BadMap)
    validateBatch !psq0 !badmap q = do
        let txs = V.map (snd . snd) q
        oks1 <- txValidate bheight phash txs
        let oks2 = V.map sizeOK txs
        let !oks = V.zipWith (\ok1 ok2 -> ok1 <* ok2) oks1 oks2
        let (bad1, good) =
              partitionEithers
                [ either (\_err -> Left (txHash, t)) (\tOut -> Right (txHash, (bytes, t, tOut))) r
                | ((txHash, (bytes, t)), r) <- V.toList (V.zip q oks)
                ]
        logFunctionText logg Debug $ "validateBatch badlisting: " <> sshow (map fst bad1)

        -- remove considered txs -- successful ones will be re-added at the end
        let !psq' = V.foldl' del psq0 q
        -- txs that fail pre-block validation get sent to the naughty list.
        let !badmap' = foldl' insBadMap badmap bad1
        return $! T3 good psq' badmap'

    maxInARow :: Int
    maxInARow = 200

    unconsV v = T2 (V.unsafeHead v) (V.unsafeTail v)

    nextBatch
        :: PendingMap
        -> GasLimit
        -> IO [(TransactionHash, (SB.ShortByteString, t))]
    nextBatch !psq !remainingGas = do
        let !pendingTxs0 = HashMap.toList psq
        logFunctionText logg Debug $ "nextBatch pendingTxs: " <> sshow (map fst pendingTxs0)
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
      -> [[(TransactionHash, (SB.ShortByteString, t, to))]]
      -> IO (T3 PendingMap BadMap (Vector (TransactionHash, (SB.ShortByteString, t, to))))
    go !psq !badmap !remainingGas !soFar = do
        nb <- nextBatch psq remainingGas
        if null nb
          then do
            logFunctionText logg Debug "getBlockInMem: Batch empty"
            return $! T3 psq badmap (V.fromList $ concat soFar)
          else do
            logFunctionText logg Debug "validating batch..."
            T3 good psq' badmap' <- validateBatch psq badmap $! V.fromList nb
            let !newGas = foldl' (\s (_, (_, t, _)) -> s + getSize t) 0 good
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
-- applied too often and at a constant rate.
--
pruneInMem
    :: forall t logger. (NFData t, Logger logger)
    => logger
    -> MVar (InMemoryMempoolData t)
    -> IO ()
pruneInMem logger lock = do
    now <- getCurrentTimeIntegral
    withMVar lock $ \mdata -> pruneInternal logger mdata now


------------------------------------------------------------------------------
pruneInternal
    :: forall t logger. (NFData t, Logger logger)
    => logger
    -> InMemoryMempoolData t
    -> Time Micros
    -> IO ()
pruneInternal logger mdata now = do
    let pref = _inmemPending mdata
    !pending <- readIORef pref
    logFunctionText logger Debug $ "pruneInternal: pending txs pre-filter: " <> sshow (HashMap.keys pending)
    logFunctionText logger Debug $ "pruneInternal: current time (micros): " <> sshow now
    forM_ (HashMap.toList pending) $ \(h, pe) ->
        logFunctionText logger Debug $ "pruneInternal: (pending tx, expiration): " <> sshow (h, _inmemPeExpires pe)
    !pending' <- evaluate $ force $ HashMap.filter flt pending
    logFunctionText logger Debug $ "pruneInternal: pending txs post-filter: " <> sshow (HashMap.keys pending')
    writeIORef pref pending'

    let bref = _inmemBadMap mdata
    badMapBefore <- readIORef bref
    logFunctionText logger Debug $ "pruneInternal: bad txs before prune: " <> sshow (HashMap.keys badMapBefore)
    !badmap <- (force . pruneBadMap) <$!> readIORef bref
    logFunctionText logger Debug $ "pruneInternal: bad txs after prune: " <> sshow (HashMap.keys badMapBefore)
    writeIORef bref badmap

  where
    -- keep transactions that expire in the future.
    flt pe = _inmemPeExpires pe > now
    pruneBadMap = HashMap.filter (> now)

logValidateOneFailure :: (Logger logger)
  => logger
  -> T.Text -- ^ location
  -> TransactionHash
  -> InsertError
  -> IO ()
logValidateOneFailure logger loc (TransactionHash hsb) insertErr = do
  let abbrevReqKey = T.decodeUtf8 (BS.take 6 (SB.fromShort hsb))
  logFunctionText logger Info $ loc <> ": " <> abbrevReqKey <> "... failed mempool check. " <> sshow insertErr
