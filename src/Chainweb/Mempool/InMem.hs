{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A mock in-memory mempool backend that does not persist to disk.
module Chainweb.Mempool.InMem
  (
    -- * Types
    InMemoryMempool
  , InMemConfig(..)

    -- * Initialization functions
  , withInMemoryMempool
  , withTxBroadcaster

    -- * Low-level create/destroy functions
  , makeInMemPool
  , createTxBroadcaster
  , destroyTxBroadcaster
  ) where

------------------------------------------------------------------------------
import Control.Applicative (pure, (<|>))
import Control.Concurrent (ThreadId, forkIOWithUnmask)
import Control.Concurrent.MVar
    (MVar, modifyMVarMasked_, newEmptyMVar, newMVar, putMVar, readMVar,
    withMVarMasked)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan (TBMChan)
import qualified Control.Concurrent.STM.TBMChan as TBMChan
import Control.Exception
    (AsyncException(ThreadKilled), SomeException, bracket, evaluate, finally,
    handle, mask_, throwIO)
import Control.Monad (forever, join, void)
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (foldlM, for_, traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as PSQ
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust, isJust)
import Data.Ord (Down(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import Prelude
    (Bool(..), Eq, IO, Int, Maybe(..), Monad(..), Num(..), Ord, flip, fmap,
    fst, id, mapM, mapM_, maybe, not, snd, ($), ($!), (&&), (.), (<$>), (<*>),
    (<=), (==), (>=), (||))
import System.Mem.Weak (Weak)
import qualified System.Mem.Weak as Weak
------------------------------------------------------------------------------
import Chainweb.Mempool.Mempool


------------------------------------------------------------------------------
-- | Priority for the search queue
type Priority = (Down TransactionReward, Int64)

toPriority :: TransactionReward -> Int64 -> Priority
toPriority r s = (Down r, s)


------------------------------------------------------------------------------
-- | Priority search queue -- search by transaction hash in /O(log n)/ like a
-- tree, find-min in /O(1)/ like a heap
type PSQ t = HashPSQ TransactionHash Priority t

type SubscriptionId = Word64

------------------------------------------------------------------------------
-- | Transaction edits -- these commands will be sent to the broadcast thread
-- over an STM channel.
data TxEdit t = Subscribe !SubscriptionId (Subscription t)
              | Unsubscribe !SubscriptionId
              | Transactions (Vector t)
              | Close
type TxSubscriberMap t = HashMap SubscriptionId (Weak (Subscription t))

-- | The 'TxBroadcaster' is responsible for broadcasting new transactions out
-- to any readers. Commands are posted to the channel and are executed by a
-- helper thread.
data TxBroadcaster t = TxBroadcaster {
    _txbSubIdgen :: {-# UNPACK #-} !(IORef SubscriptionId)
  , _txbThread :: MVar ThreadId
  , _txbQueue :: TBMChan (TxEdit t)
  , _txbThreadDone :: MVar ()
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
-- | Broadcast a group of new transactions via 'TxBroadcaster'.
broadcastTxs :: Vector t -> TxBroadcaster t -> IO ()
broadcastTxs txs (TxBroadcaster _ _ q _) =
    -- DECIDE: should this be "tryWrite"?
    atomically $ TBMChan.writeTBMChan q (Transactions txs)


------------------------------------------------------------------------------
-- | Subscribe to a 'TxBroadcaster'.
subscribeInMem :: TxBroadcaster t -> IO (Subscription t)
subscribeInMem broadcaster = do
    let q = _txbQueue broadcaster
    subQ <- atomically $ TBMChan.newTBMChan defaultQueueLen
    subId <- nextTxId $! _txbSubIdgen broadcaster
    let final = atomically $ TBMChan.writeTBMChan q (Unsubscribe subId)
    let !sub = Subscription subQ final
    let !item = Subscribe subId sub
    atomically $ TBMChan.writeTBMChan q item
    return sub
  where
    defaultQueueLen = 64


------------------------------------------------------------------------------
-- | The transcription broadcaster thread reads commands from its channel and
-- dispatches them. Once a 'Close' command comes through, the broadcaster
-- thread closes up shop and returns.
broadcasterThread :: TxBroadcaster t -> (forall a . IO a -> IO a) -> IO ()
broadcasterThread (TxBroadcaster _ _ q doneMV) restore =
    eatExceptions (restore $ bracket init cleanup go)
      `finally` putMVar doneMV ()
  where
    -- Initially we start with no subscribers.
    init :: IO (MVar (TxSubscriberMap t))
    init = newMVar HashMap.empty

    -- Our cleanup action is to traverse the subscriber map and call
    -- TBMChan.closeTBMChan on any valid subscriber references. Subscribers
    -- should then be notified that the broadcaster is finished.
    cleanup mapMV = readMVar mapMV >>= traverse_ closeWeakChan

    -- The main loop. Reads a command from the channel and dispatches it,
    -- forever.
    go !mapMV = forever . void $ do
        cmd <- atomically $ TBMChan.readTBMChan q
        maybe goodbyeCruelWorld (processCmd mapMV) cmd

    processCmd mv x = modifyMVarMasked_ mv $ flip processCmd' x

    -- new subscriber: add it to the map
    processCmd' hm (Subscribe sid s) = do
        w <- Weak.mkWeakPtr s $! Just (_mempoolSubFinal s)
        return $! HashMap.insert sid w hm
    -- unsubscribe: call finalizer and delete from subscriber map
    processCmd' hm (Unsubscribe sid) = do
        (join <$> mapM Weak.deRefWeak (HashMap.lookup sid hm)) >>= mapM_ closeChan
        return $! HashMap.delete sid hm
    -- throw 'ThreadKilled' on receiving 'Close' -- cleanup will be called
    processCmd' hm Close = goodbyeCruelWorld >> return hm
    processCmd' hm (Transactions v) = do
        broadcast v hm
        return hm

    -- ignore any exceptions received
    eatExceptions = handle $ \(e :: SomeException) -> void $ evaluate e

    closeWeakChan w = Weak.deRefWeak w >>= maybe (return ()) closeChan
    closeChan = atomically . TBMChan.closeTBMChan . _mempoolSubChan

    broadcast txV = traverse_ (write txV)

    write txV w = do
        ms <- Weak.deRefWeak w
        for_ ms $ \s -> atomically $ void $
                        TBMChan.tryWriteTBMChan (_mempoolSubChan s) txV

    goodbyeCruelWorld = throwIO ThreadKilled


------------------------------------------------------------------------------
-- | Configuration for in-memory mempool.
data InMemConfig t = InMemConfig {
    _inmemCodec :: Codec t
  , _inmemHasher :: ByteString -> TransactionHash
  , _inmemHashMeta :: HashMeta
  , _inmemTxReward :: t -> TransactionReward
  , _inmemTxSize :: t -> Int64
  , _inmemTxBlockSizeLimit :: Int64
}


------------------------------------------------------------------------------
data InMemoryMempool t = InMemoryMempool {
    _inmemCfg :: InMemConfig t
  , _inmemDataLock :: MVar (InMemoryMempoolData t)
  , _inmemBroadcaster :: TxBroadcaster t
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
}


------------------------------------------------------------------------------
makeInMemPool :: InMemConfig t -> TxBroadcaster t -> IO (InMemoryMempool t)
makeInMemPool cfg txB = do
    dataLock <- newData >>= newMVar
    return $! InMemoryMempool cfg dataLock txB

  where
    newData = InMemoryMempoolData <$> newIORef PSQ.empty
                                  <*> newIORef HashMap.empty
                                  <*> newIORef HashSet.empty


------------------------------------------------------------------------------
toMempoolBackend :: InMemoryMempool t -> MempoolBackend t
toMempoolBackend (InMemoryMempool cfg@(InMemConfig codec hasher hashMeta
                                       txRewardFunc txSizeFunc blockSizeLimit)
                                  lock broadcaster) =
    MempoolBackend codec hasher hashMeta txRewardFunc txSizeFunc
                   blockSizeLimit lookup insert getBlock markValidated
                   markConfirmed reintroduce getPending subscribe
  where
    lookup = lookupInMem lock
    insert = insertInMem broadcaster cfg lock
    getBlock = getBlockInMem cfg lock
    markValidated = markValidatedInMem cfg lock
    markConfirmed = markConfirmedInMem lock
    reintroduce = reintroduceInMem broadcaster cfg lock
    getPending = getPendingInMem cfg lock
    subscribe = subscribeInMem broadcaster


------------------------------------------------------------------------------
-- | A 'bracket' function for in-memory mempools.
withInMemoryMempool :: InMemConfig t
                    -> (MempoolBackend t -> IO a)
                    -> IO a
withInMemoryMempool cfg f = withTxBroadcaster $ \txB ->
                            makeInMemPool cfg txB >>= (f . toMempoolBackend)


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
    return $! V.map (fromJust . lookupOne q validated confirmed) txs
  where
    lookupOne q validated confirmed txHash =
        lookupQ q txHash <|>
        lookupVal validated txHash <|>
        lookupConfirmed confirmed txHash <|>
        pure Missing

    lookupQ q txHash = fmap (Pending . snd) $ PSQ.lookup txHash q
    lookupVal val txHash = fmap Validated $ HashMap.lookup txHash val
    lookupConfirmed confirmed txHash =
        if HashSet.member txHash confirmed
          then Just Confirmed
          else Nothing


------------------------------------------------------------------------------
insertInMem :: TxBroadcaster t  -- ^ transaction broadcaster
            -> InMemConfig t    -- ^ in-memory config
            -> MVar (InMemoryMempoolData t)  -- ^ in-memory state
            -> Vector t  -- ^ new transactions
            -> IO ()
insertInMem broadcaster cfg lock txs = do
    newTxs <- withMVarMasked lock $ \mdata ->
                  ((V.map fst . V.filter ((==True) . snd)) <$>
                   V.mapM (insOne mdata) txs)
    broadcastTxs newTxs broadcaster

  where
    encode = _codecEncode $ _inmemCodec cfg
    hasher = _inmemHasher cfg

    -- TODO: validate transaction; transaction size and gas limit has to be
    -- below maximums
    isValid _ = True
    getPriority x = let r = _inmemTxReward cfg x
                        s = _inmemTxSize cfg x
                    in toPriority r s
    exists mdata txhash = do
        valMap <- readIORef $ _inmemValidated mdata
        confMap <- readIORef $ _inmemConfirmed mdata
        return $! (HashMap.member txhash valMap || HashSet.member txhash confMap)
    insOne mdata tx = do
        b <- exists mdata txhash
        if not b && isValid tx
          then do
            modifyIORef' (_inmemPending mdata) $
               PSQ.insert txhash (getPriority tx) tx
            return (tx, True)
          else return (tx, False)
      where
        txhash = hasher $ encode tx


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
    getSize = _inmemTxSize cfg

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
markValidatedInMem cfg lock txs = withMVarMasked lock $ \mdata -> do
    V.mapM_ (validateOne mdata) txs
  where
    encode = _codecEncode $ _inmemCodec cfg
    hash = _inmemHasher cfg . encode

    validateOne mdata tx = do
        let txhash = hash $ _validatedTransaction tx
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
    encode = _codecEncode $ _inmemCodec cfg
    hash = _inmemHasher cfg . encode

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
reintroduceInMem :: TxBroadcaster t
                 -> InMemConfig t
                 -> MVar (InMemoryMempoolData t)
                 -> Vector TransactionHash
                 -> IO ()
reintroduceInMem broadcaster cfg lock txhashes = do
    newOnes <- withMVarMasked lock $ \mdata ->
                   (V.map fromJust . V.filter isJust) <$>
                   V.mapM (reintroduceOne mdata) txhashes
    -- we'll rebroadcast reintroduced transactions, clients can filter.
    broadcastTxs newOnes broadcaster

  where
    reward = _inmemTxReward cfg
    size = _inmemTxSize cfg
    getPriority x = let r = reward x
                        s = size x
                    in toPriority r s
    reintroduceOne mdata txhash = do
        m <- HashMap.lookup txhash <$> readIORef (_inmemValidated mdata)
        maybe (return Nothing) (reintroduceIt mdata txhash) m
    reintroduceIt mdata txhash (ValidatedTransaction _ tx) = do
        modifyIORef' (_inmemValidated mdata) $ HashMap.delete txhash
        modifyIORef' (_inmemPending mdata) $ PSQ.insert txhash (getPriority tx) tx
        return $! Just tx


------------------------------------------------------------------------------
nextTxId :: IORef SubscriptionId -> IO SubscriptionId
nextTxId = flip atomicModifyIORef' (dup . (+1))
  where
    dup a = (a, a)

