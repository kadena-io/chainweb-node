{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A mock in-memory mempool backend that does not persist to disk.

module Chainweb.Mempool.InMem
  ( InMemoryMempool
  , InMemConfig(..)
  , withInMemoryMempool
  , makeInMemPool
  , toMempoolBackend
  ) where

------------------------------------------------------------------------------
import Control.Applicative (pure, (<|>))
import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread, myThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan (TBMChan)
import qualified Control.Concurrent.STM.TBMChan as TBMChan
import Control.Exception
    (AsyncException(ThreadKilled), SomeException, bracket, catch, evaluate,
    finally, handle, mask, mask_, throwIO)
import Control.Monad (forever, join, void, when)
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (for_, traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as PSQ
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef, newIORef, readIORef)
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import Prelude
    (Bool(..), Eq, IO, Maybe(..), Monad(..), Num(..), Ord, flip, fmap, mapM,
    mapM_, maybe, not, snd, undefined, ($), ($!), (&&), (.), (<$>), (<*>),
    (||))
import System.Mem.Weak (Weak)
import qualified System.Mem.Weak as Weak
------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Mempool.Mempool


------------------------------------------------------------------------------
-- | PSQ is a min-heap, this is a quick and dirty way of negating the priority
-- calculation
newtype NegatedReward = NegatedReward TransactionReward
  deriving (Ord, Eq)


------------------------------------------------------------------------------
_toNegated :: TransactionReward -> NegatedReward
_toNegated t = NegatedReward (0 - t)
_fromNegated :: NegatedReward -> TransactionReward
_fromNegated (NegatedReward t) = 0 - t


------------------------------------------------------------------------------
type PSQ t = HashPSQ TransactionHash NegatedReward t


------------------------------------------------------------------------------
type SubscriptionId = Word64
data TxEdit t = Subscribe !SubscriptionId (Subscription t)
              | Unsubscribe !SubscriptionId
              | Transactions (Vector t)
              | Close


------------------------------------------------------------------------------
type TxSubscriberMap t = HashMap SubscriptionId (Weak (Subscription t))
data TxBroadcaster t = TxBroadcaster {
    _txbSubIdgen :: {-# UNPACK #-} !(IORef SubscriptionId)
  , _txbThread :: MVar ThreadId
  , _txbQueue :: TBMChan (TxEdit t)
  , _txbThreadDone :: MVar ()
}


------------------------------------------------------------------------------
withTxBroadcaster :: (TxBroadcaster t -> IO a) -> IO a
withTxBroadcaster = bracket create destroy
  where
    defaultQueueLen = 32

    create = do
        idgen <- newIORef 0
        threadMV <- newEmptyMVar
        doneMV <- newEmptyMVar
        q <- atomically $ TBMChan.newTBMChan defaultQueueLen
        let !tx = TxBroadcaster idgen threadMV q doneMV
        forkIOWithUnmask (broadcastThread tx) >>= putMVar threadMV
        return tx

    destroy (TxBroadcaster _ _ q doneMV) = do
        atomically $ TBMChan.writeTBMChan q Close
        readMVar doneMV


------------------------------------------------------------------------------
broadcastThread :: TxBroadcaster t -> (forall a . IO a -> IO a) -> IO ()
broadcastThread (TxBroadcaster _ _ q doneMV) restore =
    eatExceptions (restore $ bracket init cleanup go)
      `finally` putMVar doneMV ()
  where
    --------------------------------------------------------------------------
    init :: IO (MVar (TxSubscriberMap t))
    init = newMVar HashMap.empty
    cleanup mapMV = readMVar mapMV >>= traverse_ closeWeakChan
    go !mapMV = forever . void $ do
        cmd <- atomically $ TBMChan.readTBMChan q
        maybe goodbyeCruelWorld (processCmd mapMV) cmd

    --------------------------------------------------------------------------
    eatExceptions = handle $ \(e :: SomeException) -> void $ evaluate e

    --------------------------------------------------------------------------
    closeWeakChan w = Weak.deRefWeak w >>= maybe (return ()) closeChan
    closeChan = atomically . TBMChan.closeTBMChan . _mempoolSubChan

    --------------------------------------------------------------------------
    broadcast txV = traverse_ (write txV)
    write txV w = do
        ms <- Weak.deRefWeak w
        for_ ms $ \s -> atomically $ void $
                        TBMChan.tryWriteTBMChan (_mempoolSubChan s) txV

    --------------------------------------------------------------------------
    goodbyeCruelWorld = throwIO ThreadKilled

    --------------------------------------------------------------------------
    processCmd mv x = modifyMVarMasked_ mv $ flip processCmd' x

    processCmd' hm (Subscribe sid s) = do
        w <- Weak.mkWeakPtr s $ Just (atomically . void . TBMChan.tryWriteTBMChan q
                                          $ Unsubscribe sid)
        return $! HashMap.insert sid w hm

    processCmd' hm (Unsubscribe sid) = do
        (join <$> mapM Weak.deRefWeak (HashMap.lookup sid hm)) >>= mapM_ closeChan
        return $! HashMap.delete sid hm

    processCmd' hm Close = goodbyeCruelWorld >> return hm

    processCmd' hm (Transactions v) = do
        broadcast v hm
        return hm


------------------------------------------------------------------------------
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
toMempoolBackend (InMemoryMempool cfg@(InMemConfig codec hasher hashMeta txRewardFunc
                                       txSizeFunc blockSizeLimit)
                                  lock broadcaster) = do
    MempoolBackend codec hasher hashMeta txRewardFunc txSizeFunc
                   blockSizeLimit lookup insert getBlock markValidated
                   markConfirmed reintroduce getPending subscribe
  where
    lookup = lookupInMem lock
    insert = insertInMem cfg lock
    getBlock = undefined
    markValidated = undefined
    markConfirmed = undefined
    reintroduce = undefined
    getPending = undefined
    subscribe = undefined


------------------------------------------------------------------------------
withInMemoryMempool :: InMemConfig t
                    -> (MempoolBackend t -> IO a)
                    -> IO a
withInMemoryMempool cfg f = withTxBroadcaster $ \txB ->
                            makeInMemPool cfg txB >>= (f . toMempoolBackend)


------------------------------------------------------------------------------
lookupInMem :: MVar (InMemoryMempoolData t)
            -> Vector TransactionHash
            -> IO (Vector (LookupResult t))
lookupInMem lock txs = withMVar lock $ \mdata -> do
    q <- readIORef $ _inmemPending mdata
    validated <- readIORef $ _inmemValidated mdata
    confirmed <- readIORef $ _inmemConfirmed mdata
    return $! V.map (fromJust . lookupOne q validated confirmed) txs
  where
    lookupOne q validated confirmed txHash =
        lookupQ q txHash <|>
        lookupVal validated txHash <|>
        lookupConfirmed confirmed txHash <|>
        pure Missing

    lookupQ q txHash = fmap (Pending . snd) $ PSQ.lookup txHash q
    lookupVal validated txHash = fmap Validated $ HashMap.lookup txHash validated
    lookupConfirmed confirmed txHash =
        if HashSet.member txHash confirmed
          then Just Confirmed
          else Nothing


------------------------------------------------------------------------------
insertInMem cfg lock txs = withMVar lock $ \mdata -> V.mapM_ (insOne mdata) txs
  where
    encode = _codecEncode $ _inmemCodec cfg
    hasher = _inmemHasher cfg

    -- TODO: validate transaction; transaction size and gas limit has to be
    -- below maximums
    isValid _ = True
    getReward = NegatedReward . _inmemTxReward cfg
    exists mdata txhash = do
        valMap <- readIORef $ _inmemValidated mdata
        confMap <- readIORef $ _inmemConfirmed mdata
        return $! (HashMap.member txhash valMap || HashSet.member txhash confMap)
    insOne mdata tx = do
        b <- exists mdata txhash
        when (not b && isValid tx) $ modifyIORef (_inmemPending mdata) $
            PSQ.insert txhash (getReward tx) tx
      where
        txhash = hasher $ encode tx


------------------------------------------------------------------------------
nextTxId :: IORef SubscriptionId -> IO SubscriptionId
nextTxId = flip atomicModifyIORef' (dup . (+1))
  where
    dup a = (a, a)

