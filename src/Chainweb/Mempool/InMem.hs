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
    -- * Types
    InMemoryMempool
  , InMemConfig(..)

    -- * Initialization functions
  , withInMemoryMempool
  , withTestInMemoryMempool
  , withTxBroadcaster

    -- * Low-level create/destroy functions
  , makeSelfFinalizingInMemPool
  , makeInMemPool
  , createTxBroadcaster
  , destroyTxBroadcaster
  ) where

------------------------------------------------------------------------------
import Control.Applicative (pure, (<|>))
import Control.Concurrent
    (ThreadId, forkIOWithUnmask, killThread, threadDelay, withMVar)
import Control.Concurrent.MVar
    (MVar, modifyMVarMasked_, newEmptyMVar, newMVar, putMVar, readMVar,
    takeMVar, withMVarMasked)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan (TBMChan)
import qualified Control.Concurrent.STM.TBMChan as TBMChan
import Control.Exception
    (AsyncException(ThreadKilled), SomeException, bracket, bracketOnError,
    evaluate, finally, handle, mask_, throwIO)
import Control.Monad (forever, join, void, (>=>))

import Data.Foldable (foldl', foldlM, traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as PSQ
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int (Int64)
import Data.IORef
    (IORef, atomicModifyIORef', mkWeakIORef, modifyIORef', newIORef, readIORef,
    writeIORef)
import Data.Maybe (isJust)
import Data.Ord (Down(..))
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)

import Pact.Types.Gas (GasPrice(..))

import Prelude hiding (init, lookup)

import System.Mem.Weak (Weak)
import qualified System.Mem.Weak as Weak
import System.Timeout (timeout)

-- internal imports

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import qualified Chainweb.Mempool.Consensus as MPCon
import Chainweb.Mempool.Mempool
import Chainweb.Payload.PayloadStore
import qualified Chainweb.Time as Time
import Chainweb.Transaction
import Chainweb.Utils (fromJuste)

import Data.CAS
import Data.CAS.RocksDB
import Data.LogMessage (LogFunction)

------------------------------------------------------------------------------
-- | Priority for the search queue
type Priority = (Down GasPrice, Int64)

toPriority :: GasPrice -> Int64 -> Priority
toPriority r s = (Down r, s)

------------------------------------------------------------------------------
-- | Priority search queue -- search by transaction hash in /O(log n)/ like a
-- tree, find-min in /O(1)/ like a heap
type PSQ t = HashPSQ TransactionHash Priority t

type SubscriptionId = Word64

------------------------------------------------------------------------------
-- | Transaction edits -- these commands will be sent to the broadcast thread
-- over an STM channel.
data TxEdit t = Subscribe !SubscriptionId (Subscription t) (MVar (IORef (Subscription t)))
              | Unsubscribe !SubscriptionId
              | Transactions (Vector t)
              | Close
type TxSubscriberMap t = HashMap SubscriptionId (Weak (IORef (Subscription t)))

-- | The 'TxBroadcaster' is responsible for broadcasting new transactions out
-- to any readers. Commands are posted to the channel and are executed by a
-- helper thread.
data TxBroadcaster t = TxBroadcaster {
    _txbSubIdgen :: {-# UNPACK #-} !(IORef SubscriptionId)
  , _txbThread :: {-# UNPACK #-} !(MVar ThreadId)
  , _txbQueue :: {-# UNPACK #-} !(TBMChan (TxEdit t))
  , _txbThreadDone :: {-# UNPACK #-} !(MVar ())
}

-- | Runs a user computation with a newly-created transaction broadcaster. The
-- broadcaster is destroyed after the user function runs.
withTxBroadcaster :: (TxBroadcaster t -> IO a) -> IO a
withTxBroadcaster = bracket createTxBroadcaster destroyTxBroadcaster

_defaultTxQueueLen :: Int
_defaultTxQueueLen = 64

-- | Creates a 'TxBroadcaster' object. Normally 'withTxBroadcaster' should be
-- used instead. Care should be taken in 'bracket'-style initialization
-- functions to get the exception handling right; if 'destroyTxBroadcaster' is
-- not called the broadcaster thread will leak.
createTxBroadcaster :: IO (TxBroadcaster t)
createTxBroadcaster = mask_ $ do
    idgen <- newIORef 0
    threadMV <- newEmptyMVar
    doneMV <- newEmptyMVar
    q <- atomically $ TBMChan.newTBMChan _defaultTxQueueLen
    let !tx = TxBroadcaster idgen threadMV q doneMV
    forkIOWithUnmask (broadcasterThread tx) >>= putMVar threadMV
    return tx

-- | Destroys a 'TxBroadcaster' object.
destroyTxBroadcaster :: TxBroadcaster t -> IO ()
destroyTxBroadcaster (TxBroadcaster _ _ q doneMV) = do
    atomically $ TBMChan.writeTBMChan q Close
    readMVar doneMV


------------------------------------------------------------------------------
broadcastTxs :: Vector t -> TxBroadcaster t -> IO ()
broadcastTxs txs (TxBroadcaster _ _ q _) =
    -- TODO: timeout here?
    atomically $ void $ TBMChan.writeTBMChan q (Transactions txs)

-- FIXME: read this from config
tout :: TxBroadcaster t -> IO a -> IO (Maybe a)
tout _ m = timeout 2000000 m


------------------------------------------------------------------------------
-- | Subscribe to a 'TxBroadcaster'.
subscribeInMem :: TxBroadcaster t -> IO (IORef (Subscription t))
subscribeInMem broadcaster = do
    let q = _txbQueue broadcaster
    subQ <- atomically $ TBMChan.newTBMChan defaultQueueLen
    subId <- nextTxId $! _txbSubIdgen broadcaster
    let final = atomically $ TBMChan.writeTBMChan q (Unsubscribe subId)
    let !sub = Subscription subQ final
    done <- newEmptyMVar
    let !item = Subscribe subId sub done
    -- TODO: timeout here
    atomically $ TBMChan.writeTBMChan q item
    takeMVar done
  where
    defaultQueueLen = 64


------------------------------------------------------------------------------
-- | The transcription broadcaster thread reads commands from its channel and
-- dispatches them. Once a 'Close' command comes through, the broadcaster
-- thread closes up shop and returns.
broadcasterThread :: TxBroadcaster t -> (forall a . IO a -> IO a) -> IO ()
broadcasterThread broadcaster@(TxBroadcaster _ _ q doneMV) restore =
    eatExceptions (restore $ bracket init cleanup go)
      `finally` putMVar doneMV ()
  where
    -- Initially we start with no subscribers.
    init :: IO (MVar (TxSubscriberMap t))
    init = newMVar HashMap.empty

    -- Our cleanup action is to traverse the subscriber map and call
    -- TBMChan.closeTBMChan on any valid subscriber references. Subscribers
    -- should then be notified that the broadcaster is finished.
    cleanup mapMV = do
        readMVar mapMV >>= traverse_ closeWeakChan

    -- The main loop. Reads a command from the channel and dispatches it,
    -- forever.
    go !mapMV = forever . void $ do
        cmd <- atomically $ TBMChan.readTBMChan q
        maybe goodbyeCruelWorld (processCmd mapMV) cmd

    processCmd mv x = modifyMVarMasked_ mv $ flip processCmd' x

    -- new subscriber: add it to the map
    processCmd' hm (Subscribe sid s done) = do
        ref <- newIORef s
        w <- mkWeakIORef ref (mempoolSubFinal s)
        putMVar done ref
        return $! HashMap.insert sid w hm
    -- unsubscribe: call finalizer and delete from subscriber map
    processCmd' hm (Unsubscribe sid) = do
        (join <$> mapM Weak.deRefWeak (HashMap.lookup sid hm)) >>=
            mapM_ (readIORef >=> closeChan)
        return $! HashMap.delete sid hm
    -- throw 'ThreadKilled' on receiving 'Close' -- cleanup will be called
    processCmd' hm Close = do
        goodbyeCruelWorld >> return hm
    processCmd' hm (Transactions v) = broadcast v hm

    -- ignore any exceptions received
    eatExceptions = handle $ \(e :: SomeException) -> void $ evaluate e

    closeWeakChan w = Weak.deRefWeak w >>=
                      maybe (return ()) (readIORef >=> closeChan)
    closeChan m = do
        atomically . TBMChan.closeTBMChan $ mempoolSubChan m

    -- TODO: write to subscribers in parallel
    broadcast txV hm = do
        let subs = HashMap.toList hm
        foldlM (write txV) hm subs

    write txV hm (sid, w) = do
        ms <- Weak.deRefWeak w >>= mapM readIORef
        case ms of
          -- delete mapping if weak ref expires
          Nothing -> do
              return $! HashMap.delete sid hm
          (Just s) -> do
              m <- tout broadcaster $ atomically $ void $
                   TBMChan.writeTBMChan (mempoolSubChan s) txV
              -- close chan and delete mapping on timeout.
              maybe (do atomically $ TBMChan.closeTBMChan (mempoolSubChan s)
                        return $! HashMap.delete sid hm)
                    (const $ return hm)
                    m

    goodbyeCruelWorld = throwIO ThreadKilled


------------------------------------------------------------------------------
-- | Configuration for in-memory mempool.
data InMemConfig t = InMemConfig {
    _inmemTxCfg :: {-# UNPACK #-} !(TransactionConfig t)
  , _inmemTxBlockSizeLimit :: {-# UNPACK #-} !Int64
  , _inmemReaperIntervalMicros :: {-# UNPACK #-} !Int
}


------------------------------------------------------------------------------
data InMemoryMempool t cas = InMemoryMempool {
    _inmemCfg :: InMemConfig t
  , _inmemDataLock :: MVar (InMemoryMempoolData t)
  , _inmemBroadcaster :: TxBroadcaster t
  , _inmemReaper :: ThreadId
  , _inmemBlockHeaderDb :: BlockHeaderDb
  , _inmemPayloadStore :: Maybe (PayloadDb cas)
  -- TODO: reap expired transactions
}

------------------------------------------------------------------------------
data InMemoryMempoolData t = InMemoryMempoolData {
    _inmemPending :: IORef (PSQ t)
    -- | We've seen this in a valid block, but if it gets forked and loses
    -- we'll have to replay it.
    --
    -- N.B. atomic access to these IORefs is not necessary -- we hold the lock
    -- here.
  , _inmemValidated :: IORef (HashMap TransactionHash (ValidatedTransaction t))
  , _inmemConfirmed :: IORef (HashSet TransactionHash)
  , _inmemLastNewBlockParent :: Maybe (IORef BlockHeader)
}


------------------------------------------------------------------------------
makeInMemPool
    :: InMemConfig t
    -> TxBroadcaster t
    -> BlockHeaderDb
    -> Maybe (PayloadDb cas)
    -> IO (InMemoryMempool t cas)
makeInMemPool cfg txB blockHeaderDb payloadDb = mask_ $ do
    dataLock <- newData  >>= newMVar
    tid <- forkIOWithUnmask (reaperThread cfg dataLock)
    return $! InMemoryMempool cfg dataLock txB tid blockHeaderDb payloadDb
  where
    newData = InMemoryMempoolData <$> newIORef PSQ.empty
                                  <*> newIORef HashMap.empty
                                  <*> newIORef HashSet.empty
                                  <*> return Nothing

------------------------------------------------------------------------------
makeSelfFinalizingInMemPool :: PayloadCas cas
                            => InMemConfig t
                            -> BlockHeaderDb
                            -> Maybe (PayloadDb cas)
                            -> IO (MempoolBackend t)
makeSelfFinalizingInMemPool cfg blockHeaderDb payloadStore =
    mask_ $ bracketOnError createTxBroadcaster destroyTxBroadcaster $ \txb -> do
        mpool <- makeInMemPool cfg txb blockHeaderDb payloadStore
        ref <- newIORef mpool
        wk <- mkWeakIORef ref (destroyTxBroadcaster txb)
        back <- toMempoolBackend mpool
        let txcfg = mempoolTxConfig back
        let bsl = mempoolBlockGasLimit back
        let lastPar = mempoolLastNewBlockParent back

        -- let h0 = back `asTypeOf` _  -- :: MempoolBackend t l

        -- h :: l -> BlockHeader -> IO (Vector ChainwebTransaction)
        -- let h = mempoolProcessFork back `asTypeOf` _ --

        let procFork = mempoolProcessFork back
        return $! wrapBackend txcfg bsl (ref, wk) lastPar procFork
      where
        withRef (ref, _wk) f = do
            mp <- readIORef ref
            mb <- toMempoolBackend mp
            x <- f mb

            writeIORef ref mp
            return x
        wrapBackend :: PayloadCas cas
                    => TransactionConfig t
                    -> Int64
                    -> (IORef (InMemoryMempool t cas), b)
                    -> Maybe (IORef BlockHeader)
                    -> (LogFunction -> BlockHeader -> IO (Vector ChainwebTransaction))
                    -> MempoolBackend t
        wrapBackend txcfg bsl mp lastPar pFork =
            MempoolBackend txcfg bsl lastPar pFork f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11
          where
            f1 = withRef mp . flip mempoolMember
            f2 = withRef mp . flip mempoolLookup
            f3 = withRef mp . flip mempoolInsert
            f4 = withRef mp . flip mempoolGetBlock
            f5 = withRef mp . flip mempoolMarkValidated
            f6 = withRef mp . flip mempoolMarkConfirmed
            f7 = withRef mp . flip mempoolReintroduce
            f8 = withRef mp . flip mempoolGetPendingTransactions
            f9 = withRef mp mempoolSubscribe
            f10 = withRef mp mempoolShutdown
            f11 = withRef mp mempoolClear


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
    reap (InMemoryMempoolData pendingRef _ _ _) = do
        now <- Time.getCurrentTimeIntegral
        modifyIORef' pendingRef $ reapPending now

    reapPending now pending =
        let agg k _ !tx !txs = if expiryTime tx <= now
                               then (k:txs) else txs
            tooOld = PSQ.fold' agg [] pending
        in foldl' (flip PSQ.delete) pending tooOld

------------------------------------------------------------------------------
toMempoolBackend
    :: PayloadCas cas
    => InMemoryMempool t cas
    -> IO (MempoolBackend t)
toMempoolBackend (InMemoryMempool cfg@(InMemConfig tcfg blockSizeLimit _)
                                  lockMVar
                                  broadcaster _ blockHeaderDb payloadStore) = do
    lock <- readMVar lockMVar
    let lastParentRef = _inmemLastNewBlockParent lock

    return $ MempoolBackend tcfg blockSizeLimit lastParentRef processFork member lookup insert getBlock
        markValidated markConfirmed reintroduce getPending subscribe shutdown clear
  where
    member = memberInMem lockMVar
    lookup = lookupInMem lockMVar
    insert = insertInMem broadcaster cfg lockMVar
    getBlock = getBlockInMem cfg lockMVar
    markValidated = markValidatedInMem cfg lockMVar
    markConfirmed = markConfirmedInMem lockMVar
    processFork = processForkInMem lockMVar blockHeaderDb payloadStore
    reintroduce = reintroduceInMem broadcaster cfg lockMVar
    getPending = getPendingInMem cfg lockMVar
    subscribe = subscribeInMem broadcaster
    shutdown = shutdownInMem broadcaster
    clear = clearInMem lockMVar

------------------------------------------------------------------------------
inMemPayloadLookup
    :: forall cas . PayloadCas cas
    => Maybe (PayloadDb cas)
    -> BlockHeader
    -> IO (Set ChainwebTransaction)
inMemPayloadLookup payloadStore bh =
    case payloadStore of
        Nothing -> return mempty
        Just s -> do
            pwo <- casLookupM s (_blockPayloadHash bh)
            MPCon.chainwebTxsFromPWO pwo
  where
    casLookupM s h = do
        x <- casLookup s h
        case x of
            Nothing -> throwIO $ PayloadNotFoundException h
            Just pwo -> return pwo

------------------------------------------------------------------------------
processForkInMem :: PayloadCas cas
                 => MVar (InMemoryMempoolData t)
                 -> BlockHeaderDb
                 -> Maybe (PayloadDb cas)
                 -> LogFunction
                 -> BlockHeader
                 -> IO (Vector ChainwebTransaction)
processForkInMem lock blockHeaderDb payloadStore logFun newHeader = do
    theData <- readMVar lock
    -- convert: Maybe (IORef BlockHeader) -> Maybe BlockHeader
    lastHeader <- traverse readIORef (_inmemLastNewBlockParent theData)

    MPCon.processFork logFun blockHeaderDb newHeader lastHeader
        (inMemPayloadLookup payloadStore)

-- | A 'bracket' function for in-memory mempools.
withInMemoryMempool :: PayloadCas cas
                    => InMemConfig t
                    -> BlockHeaderDb
                    -> Maybe (PayloadDb cas)
                    -> (MempoolBackend t -> IO a)
                    -> IO a
withInMemoryMempool cfg blockHeaderDb payloadDb f =
    withTxBroadcaster $ \txB -> do
        let inMemIO = makeInMemPool cfg txB blockHeaderDb payloadDb
        let action inMem = do
              back <- toMempoolBackend inMem
              f back
        bracket inMemIO destroyInMemPool action

------------------------------------------------------------------------------
withTestInMemoryMempool
    :: forall a t . InMemConfig t
    -> ((RocksDb -> IO a) -> IO a)
    -> (RocksDb -> (BlockHeaderDb -> IO a) -> IO a)
    -> (MempoolBackend t -> IO a) -> IO a
withTestInMemoryMempool cfg withRocks withBlocks withMempool =
    withRocks $ \rocksDb ->
    withBlocks rocksDb $ \blockHeaderDb ->
    withTxBroadcaster $ \txB -> do
        let inMemIO = makeInMemPool cfg txB blockHeaderDb (Nothing :: Maybe (PayloadDb RocksDbCas))
            action inMem = do
              back <- toMempoolBackend inMem
              withMempool back
        bracket inMemIO destroyInMemPool action

------------------------------------------------------------------------------
destroyInMemPool :: InMemoryMempool t cas -> IO ()
destroyInMemPool (InMemoryMempool _ _ _ tid _ _) = killThread tid

-------------------------------------------
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
shutdownInMem :: TxBroadcaster t -> IO ()
shutdownInMem broadcaster = atomically $ TBMChan.writeTBMChan q Close
  where
    q = _txbQueue broadcaster


------------------------------------------------------------------------------
insertInMem :: TxBroadcaster t  -- ^ transaction broadcaster
            -> InMemConfig t    -- ^ in-memory config
            -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
            -> Vector t  -- ^ new transactions
            -> IO ()
insertInMem broadcaster cfg lock txs = do
    newTxs <- withMVarMasked lock $ \mdata ->
                  V.map fst . V.filter ((==True) . snd) <$>
                   V.mapM (insOne mdata) txs
    broadcastTxs newTxs broadcaster

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
                -> (Vector TransactionHash -> IO ())
                -> IO ()
getPendingInMem cfg lock callback = do
    psq <- readMVar lock >>= readIORef . _inmemPending
    (dl, sz) <- foldlM go initState psq
    void $ sendChunk dl sz

  where
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
reintroduceInMem' :: TxBroadcaster t
                 -> InMemConfig t
                 -> MVar (InMemoryMempoolData t)
                 -> Vector TransactionHash
                 -> IO ()
reintroduceInMem' broadcaster cfg lock txhashes = do
    newOnes <- withMVarMasked lock $ \mdata ->
                   V.map fromJuste . V.filter isJust <$>
                   V.mapM (reintroduceOne mdata) txhashes
    -- we'll rebroadcast reintroduced transactions, clients can filter.
    broadcastTxs newOnes broadcaster

  where
    txcfg = _inmemTxCfg cfg
    price = txGasPrice txcfg
    limit = txGasLimit txcfg
    getPriority x = let r = price x
                        s = limit x
                    in toPriority r s
    reintroduceOne mdata txhash = do
        m <- HashMap.lookup txhash <$> readIORef (_inmemValidated mdata)
        maybe (return Nothing) (reintroduceIt mdata txhash) m
    reintroduceIt mdata txhash (ValidatedTransaction _ _ tx) = do
        modifyIORef' (_inmemValidated mdata) $ HashMap.delete txhash
        modifyIORef' (_inmemPending mdata) $ PSQ.insert txhash (getPriority tx) tx
        return $! Just tx

------------------------------------------------------------------------------
reintroduceInMem :: TxBroadcaster t
                 -> InMemConfig t
                 -> MVar (InMemoryMempoolData t)
                 -> Vector t
                 -> IO ()
reintroduceInMem broadcaster cfg lock txs =
    reintroduceInMem' broadcaster cfg lock (V.map hashIt txs)
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


------------------------------------------------------------------------------
nextTxId :: IORef SubscriptionId -> IO SubscriptionId
nextTxId = flip atomicModifyIORef' (dup . (+1))
  where
    dup a = (a, a)
