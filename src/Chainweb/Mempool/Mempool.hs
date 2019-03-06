{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Chainweb.Mempool.Mempool
  ( MempoolBackend(..)
  , TransactionConfig(..)
  , TransactionHash(..)
  , TransactionFees
  , TransactionMetadata(..)
  , HashMeta(..)
  , Subscription(..)
  , ValidationInfo(..)
  , ValidatedTransaction(..)
  , LookupResult(..)
  , MockTx(..)
  , mockCodec
  , mockBlocksizeLimit
  , mockFeesLimit
  , finalizeSubscriptionImmediately
  , chainwebTestHasher
  , chainwebTestHashMeta
  , noopMempool
  , syncMempools
  ) where
------------------------------------------------------------------------------
import Control.Concurrent (myThreadId)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan (TBMChan)
import qualified Control.Concurrent.STM.TBMChan as TBMChan
import Control.DeepSeq (NFData)
import Control.Exception
import Control.Monad (replicateM, unless, when)
import Crypto.Hash (hash)
import Crypto.Hash.Algorithms (SHA512t_256)
import Data.Aeson
import Data.Bits (bit, shiftL, shiftR, (.&.))
import Data.ByteArray (convert)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Decimal (Decimal)
import Data.Decimal (DecimalRaw(..))
import Data.Foldable (traverse_)
import Data.Hashable (Hashable(hashWithSalt))
import Data.Int (Int64)
import Data.IORef
import Data.List (unfoldr)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import Foreign.StablePtr
import GHC.Generics

------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Time (Time(..))
import qualified Chainweb.Time as Time
import Chainweb.Utils
    (Codec(..), decodeB64UrlNoPaddingText, encodeB64UrlNoPaddingText)


------------------------------------------------------------------------------
data LookupResult t = Missing
                    | Validated (ValidatedTransaction t)
                    | Confirmed
                    | Pending t
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)     -- TODO: a handwritten instance

------------------------------------------------------------------------------
data TransactionConfig t = TransactionConfig {
    -- | converting transactions to/from bytestring.
    txCodec :: {-# UNPACK #-} !(Codec t)

    -- | hash function to use when making transaction hashes.
  , txHasher :: t -> TransactionHash

    -- | hash function metadata.
  , txHashMeta :: {-# UNPACK #-} !HashMeta

    -- | getter for the transaction fees.
  , txFees :: t -> TransactionFees

    -- | getter for transaction size.
  , txSize :: t -> Int64
  , txMetadata :: t -> TransactionMetadata
  , txValidate :: t -> IO Bool
  }

------------------------------------------------------------------------------
-- | Mempool backend API. Here @t@ is the transaction payload type.
data MempoolBackend t = MempoolBackend {
    mempoolTxConfig :: {-# UNPACK #-} !(TransactionConfig t)

    -- TODO: move this inside TransactionConfig or new MempoolConfig ?
  , mempoolBlockSizeLimit :: Int64

    -- | Returns true if the given transaction hash is known to this mempool.
  , mempoolMember :: Vector TransactionHash -> IO (Vector Bool)

    -- | Lookup transactions in the pending queue by hash.
  , mempoolLookup :: Vector TransactionHash -> IO (Vector (LookupResult t))

    -- | Insert the given transactions into the mempool.
  , mempoolInsert :: Vector t -> IO ()

    -- | given maximum block size, produce a candidate block of transactions
    -- for mining.
  , mempoolGetBlock :: Int64 -> IO (Vector t)

    -- | mark the given transactions as being mined and validated.
  , mempoolMarkValidated :: Vector (ValidatedTransaction t) -> IO ()

    -- | mark the given hashes as being past confirmation depth.
  , mempoolMarkConfirmed :: Vector TransactionHash -> IO ()

    -- | These transactions were on a losing fork. Reintroduce them.
  , mempoolReintroduce :: Vector TransactionHash -> IO ()

    -- | given a callback function, loops through the pending candidate
    -- transactions and supplies the hashes to the callback in chunks. No
    -- ordering of hashes is presupposed.
  , mempoolGetPendingTransactions
      :: (Vector TransactionHash -> IO ()) -> IO ()

  , mempoolSubscribe :: IO (IORef (Subscription t))
  , mempoolShutdown :: IO ()

  -- | A hook to clear the mempool. Intended only for the in-mem backend and
  -- only for testing.
  , mempoolClear :: IO ()
}


noopMempool :: MempoolBackend t
noopMempool = MempoolBackend txcfg 1000 noopMember noopLookup noopInsert noopGetBlock
                             noopMarkValidated noopMarkConfirmed noopReintroduce
                             noopGetPending noopSubscribe noopShutdown noopClear
  where
    unimplemented = fail "unimplemented"
    noopCodec = Codec (const "") (const $ Left "unimplemented")
    noopHasher = const $ chainwebTestHasher "noopMempool"
    noopHashMeta = chainwebTestHashMeta
    noopFees = const 0
    noopSize = const 1
    noopMeta = const $ TransactionMetadata Time.minTime Time.maxTime
    txcfg = TransactionConfig noopCodec noopHasher noopHashMeta noopFees noopSize
                              noopMeta (const $ return True)

    noopMember v = return $ V.replicate (V.length v) False
    noopLookup v = return $ V.replicate (V.length v) Missing
    noopInsert = const $ return ()
    noopGetBlock = const $ return V.empty
    noopMarkValidated = const $ return ()
    noopMarkConfirmed = const $ return ()
    noopReintroduce = const $ return ()
    noopGetPending = const $ return ()
    noopSubscribe = unimplemented
    noopShutdown = return ()
    noopClear = return ()


------------------------------------------------------------------------------
data SyncState = SyncState {
    _syncCount :: {-# UNPACK #-} !Int64
  , _syncMissing :: ![Vector TransactionHash]
  , _syncTooMany :: !Bool
  }

-- | Pulls any missing pending transactions from a remote mempool.
--
-- The sync procedure:
--
--    1. begin subscription with remote's mempool
--    2. get the list of pending transaction hashes from remote.
--    3. lookup any hashes that are missing, and insert them into local
--    4. block forever, waiting to be killed (subscription will still be active.)
--
-- Blocks until killed, or until the underlying mempool connection throws an
-- exception.

syncMempools
    :: Show t
    => MempoolBackend t     -- ^ local mempool
    -> MempoolBackend t     -- ^ remote mempool
    -> IO ()
syncMempools localMempool remoteMempool =
    bracket startSubscription cleanupSubscription sync

  where
    startSubscription = do
        sub <- mempoolSubscribe remoteMempool
        th <- readIORef sub >>= Async.async . subThread
        Async.link th
        p <- myThreadId >>= newStablePtr  -- otherwise we will get "blocked
                                          -- indefinitely on mvar" during sync
        return (sub, th, p)

    cleanupSubscription (sub, th, p) = do
      Async.uninterruptibleCancel th
      freeStablePtr p
      mempoolSubFinal <$> readIORef sub

    -- TODO: update a counter and bug out if too many new txs read
    subThread sub =
        let chan = mempoolSubChan sub
            go = do
                m <- atomically (TBMChan.readTBMChan chan)
                maybe (return ()) (\v -> mempoolInsert localMempool v >> go) m
        in go

    maxCnt = 10000   -- don't pull more than this many new transactions from a
                     -- single peer in a session.

    syncChunk missing hashes = do
        res <- (`V.zip` hashes) <$> mempoolMember localMempool hashes
        let !newMissing = V.map snd $ V.filter (not . fst) res
        let !newMissingCnt = V.length newMissing
        when (newMissingCnt > 0) $ do
            (SyncState cnt chunks tooMany) <- readIORef missing
            when (not tooMany) $ do
                let !newCnt = cnt + fromIntegral newMissingCnt
                let !tooMany' = (newCnt >= maxCnt)

                writeIORef missing $!
                    if tooMany'
                        then SyncState cnt chunks True
                        else SyncState newCnt (newMissing : chunks) False

    fromPending (Pending t) = t
    fromPending _ = error "impossible"

    isPending (Pending _) = True
    isPending _ = False

    fetchMissing chunk = do
        res <- mempoolLookup remoteMempool chunk
        let !newTxs = V.map fromPending $ V.filter isPending res
        mempoolInsert localMempool newTxs

    sync _ = do
        missing <- newIORef $ SyncState 0 [] False
        mempoolGetPendingTransactions remoteMempool $ syncChunk missing
        (SyncState _ missingChunks tooMany) <- readIORef missing

        -- Go fetch missing transactions from remote
        traverse_ fetchMissing missingChunks
        -- We're done syncing. If we cut off the sync session because of too
        -- many remote txs, we are free to go now, but otherwise we'll block
        -- indefinitely waiting for the p2p session to kill us (and slurping
        -- from the remote subscription queue).
        unless tooMany $ atomically retry


------------------------------------------------------------------------------
-- | Raw/unencoded transaction hashes.
--
-- TODO: production versions of this kind of DB should salt with a
-- runtime-generated constant to avoid collision attacks; see the \"hashing and
-- security\" section of the hashable docs.
newtype TransactionHash = TransactionHash ByteString
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Hashable TransactionHash where
  hashWithSalt s (TransactionHash h) = hashWithSalt s (hashCode :: Int)
    where
      hashCode = either error id $ runGetS (fromIntegral <$> getWord64host) (B.take 8 h)
  {-# INLINE hashWithSalt #-}

instance ToJSON TransactionHash where
  toJSON (TransactionHash x) = toJSON $! encodeB64UrlNoPaddingText x
instance FromJSON TransactionHash where
  parseJSON = withText "TransactionHash" (either (fail . show) return . p)
    where
      p :: Text -> Either SomeException TransactionHash
      p = (TransactionHash <$>) . decodeB64UrlNoPaddingText

------------------------------------------------------------------------------
-- | Fees to be awarded to the miner for processing a transaction. Higher is better
type TransactionFees = Decimal


------------------------------------------------------------------------------
data TransactionMetadata = TransactionMetadata {
    txMetaCreationTime :: {-# UNPACK #-} !(Time Int64)
  , txMetaExpiryTime :: {-# UNPACK #-} !(Time Int64)
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


------------------------------------------------------------------------------
-- | Mempools will check these values match in APIs
data HashMeta = HashMeta {
    hashmetaName :: {-# UNPACK #-} !Text
  , hashmetaLenBytes :: {-# UNPACK #-} !Int
}

chainwebTestHasher :: ByteString -> TransactionHash
chainwebTestHasher s = let b = convert $ hash @_ @SHA512t_256 $ "TEST" <> s
                       in TransactionHash b

chainwebTestHashMeta :: HashMeta
chainwebTestHashMeta = HashMeta "chainweb-sha512-256" 32

------------------------------------------------------------------------------
-- | Clients can subscribe to a mempool to receive new transactions as soon as
-- they appear.
data Subscription t = Subscription {
    mempoolSubChan :: TBMChan (Vector t)
  , mempoolSubFinal :: IO ()
  -- TODO: activity timer
}


------------------------------------------------------------------------------
data ValidationInfo = ValidationInfo {
    validatedHeight :: {-# UNPACK #-} !BlockHeight
  , validatedHash :: {-# UNPACK #-} !BlockHash
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)     -- TODO: a handwritten instance

data ValidatedTransaction t = ValidatedTransaction {
    validatedForks :: Vector ValidationInfo
  , validatedTransaction :: t
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)     -- TODO: a handwritten instance


------------------------------------------------------------------------------
finalizeSubscriptionImmediately :: Subscription t -> IO ()
finalizeSubscriptionImmediately = mempoolSubFinal


-- | Mempool only cares about a few projected values from the transaction type
-- (fees, hash, tx size, metadata), so our mock transaction type will only
-- contain these (plus a nonce)
data MockTx = MockTx {
    mockNonce :: {-# UNPACK #-} !Int64
  , mockFees :: {-# UNPACK #-} !Decimal
  , mockSize :: {-# UNPACK #-} !Int64
  , mockMeta :: {-# UNPACK #-} !TransactionMetadata
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


instance (Show i, Integral i) => ToJSON (DecimalRaw i) where
    toJSON d = let s = T.pack $ show d
               in toJSON s

instance (Read i, Integral i) => FromJSON (DecimalRaw i) where
    parseJSON v = do
        s <- T.unpack <$> parseJSON v
        return $! read s


mockBlocksizeLimit :: Int64
mockBlocksizeLimit = 65535


mockFeesLimit :: Decimal
mockFeesLimit = fromIntegral mockBlocksizeLimit * 4


-- | A codec for transactions when sending them over the wire.
mockCodec :: Codec MockTx
mockCodec = Codec mockEncode mockDecode


mockEncode :: MockTx -> ByteString
mockEncode (MockTx sz fees nonce meta) = runPutS $ do
    putWord64le $ fromIntegral sz
    putDecimal fees
    putWord64le $ fromIntegral nonce
    Time.encodeTime $ txMetaCreationTime meta
    Time.encodeTime $ txMetaExpiryTime meta


putDecimal :: MonadPut m => Decimal -> m ()
putDecimal (Decimal places mantissa) = do
    putWord8 places
    putWord8 $ if mantissa >= 0 then 0 else 1
    let ws = toWordList $ if mantissa >= 0 then mantissa else -mantissa
    putWord16le $ fromIntegral $ length ws
    mapM_ putWord64le ws
  where
    toWordList = unfoldr $
                 \d -> if d == 0
                         then Nothing
                         else let !a  = fromIntegral (d .&. (bit 64 - 1))
                                  !d' = d `shiftR` 64
                              in Just (a, d')


getDecimal :: MonadGet m => m Decimal
getDecimal = do
    !places <- fromIntegral <$> getWord8
    !negative <- getWord8
    numWords <- fromIntegral <$> getWord16le
    mantissaWords <- replicateM numWords getWord64le
    let (!mantissa, _) = foldl go (0,0) mantissaWords
    return $! Decimal places (if negative == 0 then mantissa else -mantissa)
  where
    go :: (Integer, Int) -> Word64 -> (Integer, Int)
    go (!soFar, !shiftVal) !next =
        let !i = toInteger next + soFar `shiftL` shiftVal
            !s = shiftVal + 64
        in (i, s)


mockDecode :: ByteString -> Either String MockTx
mockDecode = runGetS (MockTx <$> getI64 <*> getDecimal <*> getI64 <*> getMeta)
  where
    getI64 = fromIntegral <$> getWord64le
    getMeta = TransactionMetadata <$> Time.decodeTime <*> Time.decodeTime

