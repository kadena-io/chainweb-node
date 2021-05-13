{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

------------------------------------------------------------------------------
-- | Mempool
--
-- The mempool contains an in-memory store of all of the pending transactions
-- that have been submitted by users for inclusion into blocks, and is
-- responsible for collecting the candidate transaction set when we prepare a
-- new block for mining.
--
-- The in-memory mempool implementation (see @InMem.hs@ and @InMemTypes.hs@)
-- contains three datasets:
--
-- 1. a priority search queue of pending transactions, keyed by pact
-- transaction hash, and (currently) ordered by gas price (descending) with
-- ties broken by gas limit (ascending). Basically -- we prefer highest bidder
-- first, and all else being equal, smaller transactions.
--
-- 2. a recent-modifications log, storing the last @k@ updates to the mempool.
-- This is in here so peers can sync with us by fetching starting with a
-- high-water mark, rather than having to go through a complete re-sync of all
-- transaction hashes.
--
-- Transaction lifecycle:
--
--   - a transaction is created and signed by a user, and sent (via the pact
--     @/send@ endpoint) to the mempool, using 'mempoolInsert'
--
--   - miner calls 'mempoolGetBlock', at which point the top transactions (up
--     to the gas limit) are run through the MempoolPreBlockCheck and discarded
--     if they fail
--
--   - the transaction makes it into a mined block
--
--   - consensus calls pact's @newBlock@ to create the next block; at this
--     point, the Consensus module processes the fork and: a) removes any
--     transactions that made it onto the winning fork so that they are no
--     longer considered for inclusion in blocks or gossiped, b) reintroduces
--     any transactions into the mempool that were on the losing fork but
--     didn't make it onto the winning one.
--
-- The mempool API is defined as a record-of-functions in 'MempoolBackend'.

module Chainweb.Mempool.Mempool
  ( MempoolBackend(..)
  , MempoolPreBlockCheck
  , TransactionConfig(..)
  , TransactionHash(..)
  , TransactionMetadata(..)
  , MempoolTxId
  , HashMeta(..)
  , ValidatedTransaction(..)
  , LookupResult(..)
  , MockTx(..)
  , ServerNonce
  , HighwaterMark(..)
  , InsertType(..)
  , InsertError(..)

  , chainwebTransactionConfig
  , mockCodec
  , mockEncode
  , mockBlockGasLimit
  , chainwebTestHasher
  , chainwebTestHashMeta
  , noopMempool
  , noopMempoolPreBlockCheck
  , syncMempools
  , syncMempools'
  , mkHighwaterMark
  , GasLimit(..)
  ) where
------------------------------------------------------------------------------
import Control.Concurrent.MVar
import Control.DeepSeq (NFData)
import Control.Exception
import Control.Lens
import Control.Monad (replicateM, unless)

import Crypto.Hash (hash)
import Crypto.Hash.Algorithms (SHA512t_256)

import Data.Aeson
import Data.Bits (bit, shiftL, shiftR, (.&.))
import Data.ByteArray (convert)
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString.Base64.URL as B64
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as SB
import Data.Decimal (Decimal, DecimalRaw(..))
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Hashable (Hashable(hashWithSalt))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int (Int64)
import Data.IORef
import Data.List (unfoldr)
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Tim as TimSort
import Data.Word (Word64)
import GHC.Generics
import Prelude hiding (log)
import System.IO.Unsafe (unsafePerformIO)

-- internal modules

import Pact.Parse (ParsedDecimal(..), ParsedInteger(..))
import Pact.Types.ChainMeta (TTLSeconds(..), TxCreationTime(..))
import Pact.Types.Command
import Pact.Types.Gas (GasLimit(..), GasPrice(..))
import qualified Pact.Types.Hash as H
import System.LogLevel

import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.Time
    (Micros(..), Time(..), TimeSpan(..), getCurrentTimeIntegral)
import qualified Chainweb.Time as Time
import Chainweb.Transaction
import Chainweb.Utils
import Data.LogMessage (LogFunctionText)
import P2P.Peer (PeerInfo)

------------------------------------------------------------------------------
data LookupResult t = Missing
                    | Pending t
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData) -- TODO: a handwritten instance

instance Functor LookupResult where
    fmap _ Missing = Missing
    fmap f (Pending x) = Pending $! f x

instance Foldable LookupResult where
    foldr f seed t = case t of
                       Missing -> seed
                       (Pending x) -> f x seed

instance Traversable LookupResult where
    traverse f t = case t of
                     Missing -> pure Missing
                     Pending x -> Pending <$> f x

------------------------------------------------------------------------------
type MempoolPreBlockCheck t = BlockHeight -> BlockHash -> Vector t -> IO (Vector Bool)

------------------------------------------------------------------------------
-- | Mempool operates over a transaction type @t@. Mempool needs several
-- functions on @t@, e.g. \"how do you encode and decode @t@ over the wire?\"
-- and \"how is @t@ hashed?\". This information is passed to mempool in a
-- 'TransactionConfig'.
data TransactionConfig t = TransactionConfig {
    -- | converting transactions to/from bytestring. Note: the generated
    -- bytestring is currently expected to be valid utf8 so that it can be
    -- safely JSON-encoded by servant (we should revisit this)
    txCodec :: {-# UNPACK #-} !(Codec t)

    -- | hash function to use when making transaction hashes.
  , txHasher :: t -> TransactionHash

    -- | hash function metadata. Currently informational only -- for future use.
  , txHashMeta :: {-# UNPACK #-} !HashMeta

    -- | getter for the transaction gas price.
  , txGasPrice :: t -> GasPrice

    -- | getter for transaction gas limit.
  , txGasLimit :: t -> GasLimit

    -- | getter for transaction metadata (creation and expiry timestamps)
  , txMetadata :: t -> TransactionMetadata
  }

------------------------------------------------------------------------------
type MempoolTxId = Int64
type ServerNonce = Int
newtype HighwaterMark = HighwaterMark { _unHwMark :: T2 ServerNonce MempoolTxId }
  deriving (Show, Eq, Ord, Typeable, Generic)

mkHighwaterMark :: ServerNonce -> MempoolTxId -> HighwaterMark
mkHighwaterMark a b = HighwaterMark $! T2 a b

instance FromJSON HighwaterMark where
    parseJSON v = do
        (a, b) <- parseJSON v
        return $! mkHighwaterMark a b

instance ToJSON HighwaterMark where
    toEncoding (HighwaterMark (T2 a b)) = toEncoding (a, b)
    toJSON (HighwaterMark (T2 a b)) = toJSON (a, b)

data InsertType = CheckedInsert | UncheckedInsert
  deriving (Show, Eq)

data PeerSyncHistory = PeerSyncHistory {
    _histOurMark :: HighwaterMark
  , _histTheirMark :: HighwaterMark
  , _histTime :: Time Micros
}

type SyncHistoryTable = HashMap PeerInfo PeerSyncHistory
data SyncHistory = SyncHistory {
    _histTable :: SyncHistoryTable
  , _histOperationCount :: IORef Int
}

syncGlobalHistoryTable :: MVar SyncHistory
syncGlobalHistoryTable = unsafePerformIO $ do
    ref <- newIORef 0
    newMVar $ SyncHistory mempty ref
{-# NOINLINE syncGlobalHistoryTable #-}

fetchGlobalSyncHistory :: PeerInfo -> IO (Maybe PeerSyncHistory)
fetchGlobalSyncHistory peer = withMVar syncGlobalHistoryTable $ \tab -> do
    let !hm = _histTable tab
    return $ HashMap.lookup peer hm

-- | How frequently to purge the peers table (in # of update operations).
-- TODO: make configurable?
syncGlobalHistoryPurgeCount :: Int
syncGlobalHistoryPurgeCount = 2047

-- | How many peers to remember? Note that we may retain more peers than this
-- in the local table since they are only cleared every
-- syncGlobalHistoryPurgeCount times a peer entry is updated.
syncGlobalPeerHistoryMemory :: Int
syncGlobalPeerHistoryMemory = 4096


purgeOldGlobalHistory :: SyncHistoryTable -> IO SyncHistoryTable
purgeOldGlobalHistory tab = do
    mvec <- V.unsafeThaw $ V.fromList $ HashMap.toList tab
    TimSort.sortBy comparator mvec
    v <- V.take syncGlobalPeerHistoryMemory <$> V.unsafeFreeze mvec
    return $! HashMap.fromList $ V.toList v
  where
    -- reverse-comparison on time, i.e. newest-first
    comparator = flip (compare `on` (_histTime  . snd))


updateGlobalSyncHistory :: PeerInfo -> HighwaterMark -> HighwaterMark -> IO ()
updateGlobalSyncHistory peer ourHwMark theirHwMark =
    modifyMVar syncGlobalHistoryTable $ \ght -> do
        let !countRef = _histOperationCount ght
        !count <- readIORef countRef
        -- update count; every K iterations we'll purge the table
        tab <- if (count >= syncGlobalHistoryPurgeCount)
                 then do
                   writeIORef countRef 0
                   purgeOldGlobalHistory $ _histTable ght
                 else do
                   writeIORef countRef $! count + 1
                   return $! _histTable ght
        !now <- getCurrentTimeIntegral
        let !entry = PeerSyncHistory ourHwMark theirHwMark now
        let !tab' = HashMap.insert peer entry tab
        let !newGHT = SyncHistory tab' countRef
        return (newGHT, ())


data InsertError = InsertErrorDuplicate
                 | InsertErrorInvalidTime
                 | InsertErrorOversized GasLimit
                 | InsertErrorBadlisted
                 | InsertErrorMetadataMismatch
                 | InsertErrorTransactionsDisabled
                 | InsertErrorBuyGas Text
                 | InsertErrorCompilationFailed Text
                 | InsertErrorOther Text
  deriving (Generic, Eq, NFData)

instance Show InsertError
  where
    show InsertErrorDuplicate = "Transaction already exists on chain"
    show InsertErrorInvalidTime = "Transaction time is invalid or TTL is expired"
    show (InsertErrorOversized (GasLimit l)) = "Transaction gas limit exceeds block gas limit (" <> show l <> ")"
    show InsertErrorBadlisted =
        "Transaction is badlisted because it previously failed to validate."
    show InsertErrorMetadataMismatch =
        "Transaction metadata (chain id, chainweb version) conflicts with this \
        \endpoint"
    show InsertErrorTransactionsDisabled = "Transactions are disabled until 2019 Dec 5"
    show (InsertErrorBuyGas msg) = "Attempt to buy gas failed with: " <> T.unpack msg
    show (InsertErrorCompilationFailed msg) = "Transaction compilation failed: " <> T.unpack msg
    show (InsertErrorOther m) = "insert error: " <> T.unpack m

instance Exception InsertError

------------------------------------------------------------------------------
-- | Mempool backend API. Here @t@ is the transaction payload type.
data MempoolBackend t = MempoolBackend {
    mempoolTxConfig :: {-# UNPACK #-} !(TransactionConfig t)

    -- | Returns true if the given transaction hash is known to this mempool.
  , mempoolMember :: Vector TransactionHash -> IO (Vector Bool)

    -- | Lookup transactions in the pending queue by hash.
  , mempoolLookup :: Vector TransactionHash -> IO (Vector (LookupResult t))

    -- | Insert the given transactions into the mempool.
  , mempoolInsert :: InsertType      -- run pre-gossip check? Ignored at remote pools.
                  -> Vector t
                  -> IO ()

    -- | Perform the pre-insert check for the given transactions. Short-circuits
    -- on the first Transaction that fails.
  , mempoolInsertCheck :: Vector t -> IO (Either (T2 TransactionHash InsertError) ())

    -- | Remove the given hashes from the pending set.
  , mempoolMarkValidated :: Vector t -> IO ()

    -- | Mark a transaction as bad.
  , mempoolAddToBadList :: TransactionHash -> IO ()

    -- | Returns 'True' if the transaction is badlisted.
  , mempoolCheckBadList :: Vector TransactionHash -> IO (Vector Bool)

    -- | given maximum block size, produce a candidate block of transactions
    -- for mining.
    --
  , mempoolGetBlock
      :: MempoolPreBlockCheck t -> BlockHeight -> BlockHash -> IO (Vector t)

    -- | Discard any expired transactions.
  , mempoolPrune :: IO ()

    -- | given a previous high-water mark and a chunk callback function, loops
    -- through the pending candidate transactions and supplies the hashes to
    -- the callback in chunks. No ordering of hashes is presupposed. Returns
    -- the remote high-water mark.
  , mempoolGetPendingTransactions
      :: Maybe HighwaterMark                -- previous high-water mark, if any
      -> (Vector TransactionHash -> IO ())  -- chunk callback
      -> IO HighwaterMark                   -- returns remote high water mark

  -- | A hook to clear the mempool. Intended only for the in-mem backend and
  -- only for testing.
  , mempoolClear :: IO ()
}

noopMempoolPreBlockCheck :: MempoolPreBlockCheck t
noopMempoolPreBlockCheck _ _ v = return $! V.replicate (V.length v) True

noopMempool :: IO (MempoolBackend t)
noopMempool = do
  return $ MempoolBackend
    { mempoolTxConfig = txcfg
    , mempoolMember = noopMember
    , mempoolLookup = noopLookup
    , mempoolInsert = noopInsert
    , mempoolInsertCheck = noopInsertCheck
    , mempoolMarkValidated = noopMV
    , mempoolAddToBadList = noopAddToBadList
    , mempoolCheckBadList = noopCheckBadList
    , mempoolGetBlock = noopGetBlock
    , mempoolPrune = return ()
    , mempoolGetPendingTransactions = noopGetPending
    , mempoolClear = noopClear
    }
  where
    noopCodec = Codec (const "") (const $ Left "unimplemented")
    noopHasher = const $ chainwebTestHasher "noopMempool"
    noopHashMeta = chainwebTestHashMeta
    noopGasPrice = const 0
    noopSize = const 1
    noopMeta = const $ TransactionMetadata Time.minTime Time.maxTime
    txcfg = TransactionConfig noopCodec noopHasher noopHashMeta noopGasPrice noopSize
                              noopMeta
    noopMember v = return $ V.replicate (V.length v) False
    noopLookup v = return $ V.replicate (V.length v) Missing
    noopInsert = const $ const $ return ()
    noopInsertCheck _ = fail "unsupported"
    noopMV = const $ return ()
    noopAddToBadList = const $ return ()
    noopCheckBadList v = return $ V.replicate (V.length v) False
    noopGetBlock _ _ _ = return V.empty
    noopGetPending = const $ const $ return (mkHighwaterMark 0 0)
    noopClear = return ()


------------------------------------------------------------------------------
chainwebTransactionConfig :: TransactionConfig ChainwebTransaction
chainwebTransactionConfig = TransactionConfig chainwebPayloadCodec
    commandHash
    chainwebTestHashMeta
    getGasPrice
    getGasLimit
    txmeta

  where
    getGasPrice = gasPriceOf . fmap payloadObj
    getGasLimit = gasLimitOf . fmap payloadObj
    getTimeToLive = timeToLiveOf . fmap payloadObj
    getCreationTime = creationTimeOf . fmap payloadObj
    commandHash c = let (H.Hash !h) = H.toUntypedHash $ _cmdHash c
                    in TransactionHash $! SB.toShort $ h
    txmeta t =
        TransactionMetadata
        (toMicros ct)
        (toMicros $ ct + min maxDuration ttl)
      where
        (TxCreationTime ct) = getCreationTime t
        toMicros = Time . TimeSpan . Micros . fromIntegral . (1000000 *)
        (TTLSeconds ttl) = getTimeToLive t
        maxDuration = 2 * 24 * 60 * 60 + 1

------------------------------------------------------------------------------
data SyncState = SyncState {
    _syncCount :: {-# UNPACK #-} !Int64
  , _syncMissing :: ![Vector TransactionHash]
  , _syncPresent :: !(HashSet TransactionHash)
  , _syncTooMany :: !Bool
  }

-- | Pulls any missing pending transactions from a remote mempool.
--
-- The sync procedure:
--
--   1. get the list of pending transactions from their side since the last
--   time we synced (or all of them, on first contact)
--
--   2. lookup any hashes that are missing, and insert them into local
--
--   3. get the list of transactions from our side since the last time we
--   synced (or all of them, if never) and send them if we're not sure if
--   remote has them.
--
-- Loops until killed, or until the underlying mempool connection throws an
-- exception.
--
syncMempools'
    :: Show t
    => LogFunctionText
    -> PeerInfo
    -> Int
        -- ^ polling interval in microseconds
    -> MempoolBackend t
        -- ^ local mempool
    -> MempoolBackend t
        -- ^ remote mempool
    -> IO ()
syncMempools' log0 peer us localMempool remoteMempool = sync

  where
    maxCnt = 5000
        -- don't pull more than this many new transactions from a single peer in
        -- a session.
        --
        -- TODO: replace this with a token bucket scheme

    -- This function is called for each chunk of pending hashes in the the remote mempool.
    --
    -- The 'SyncState' collects
    -- * the total count of hashes that are missing from the local pool,
    -- * chunks of hashes that are missing from the local pool,
    -- * the set of hashes that is available locally but missing from the remote pool, and
    -- * whether there are @tooMany@ missing hashes.
    --
    -- When we already collected @tooMany@ missing hashes we stop updating the sync state
    --
    -- TODO: would it help if 'mempoolGetPendingTransactions' would provide an option for early
    -- termination in case we got @tooMany@ missing hashes?
    --
    syncChunk syncState hashes = do
        (SyncState cnt chunks remoteHashes tooMany) <- readIORef syncState

        -- If there are too many missing hashes we stop collecting
        --
        unless tooMany $ do

            -- Collect remote hashes that are missing from the local pool
            res <- (`V.zip` hashes) <$> mempoolMember localMempool hashes
            let !newMissing = V.map snd $ V.filter (not . fst) res
            let !newMissingCnt = V.length newMissing

            -- Collect set of all remote hashes
            let !remoteHashes' = V.foldl' (flip HashSet.insert) remoteHashes hashes

            -- Count number of missing hashes and decide if there @tooMany@
            let !newCnt = cnt + fromIntegral newMissingCnt
            let !tooMany' = newCnt >= maxCnt

            -- Update the SyncState
            writeIORef syncState $!
                if tooMany'
                    then SyncState cnt chunks remoteHashes True
                    else SyncState newCnt (newMissing : chunks) remoteHashes' False

    fromPending (Pending t) = t
    fromPending _ = error "impossible"

    isPending (Pending _) = True
    isPending _ = False

    fetchMissing chunk = do
        res <- mempoolLookup remoteMempool chunk
        let !newTxs = V.map fromPending $ V.filter isPending res
        mempoolInsert localMempool CheckedInsert newTxs

    deb :: Text -> IO ()
    deb = log0 Debug

    sync = finally initialSync (deb "sync exiting")

    initialSync = do
        mHistory <- fetchGlobalSyncHistory peer
        let ourHwMark = fmap _histOurMark mHistory
        let theirHwMark = fmap _histOurMark mHistory
        goSync ourHwMark theirHwMark

    goSync !localHw !remoteHw = do
        deb "Get new pending hashes from remote"
        (numMissingFromLocal, missingChunks, remoteHashes, remoteHw') <-
            fetchSince remoteHw
        deb $ T.concat
            [ "sync: "
            , sshow numMissingFromLocal
            , " new remote hashes to fetch"
            ]
        traverse_ fetchMissing missingChunks
        (numPushed, !localHw') <- push localHw remoteHashes
        deb $ "pushed " <> sshow numPushed <> " transactions to remote."
        updateGlobalSyncHistory peer localHw' remoteHw'

        -- TODO: do we really need to loop here?
        approximateThreadDelay us
        goSync (Just localHw') (Just remoteHw')

    -- get pending hashes from remote since the given (optional) high water mark
    fetchSince oldRemoteHw = do
        -- Intialize and collect SyncState
        let emptySyncState = SyncState 0 [] HashSet.empty False
        syncState <- newIORef emptySyncState
        remoteHw <- mempoolGetPendingTransactions remoteMempool oldRemoteHw $
                    syncChunk syncState
        (SyncState numMissingFromLocal missingChunks remoteHashes _) <-
            readIORef syncState
        -- immediately destroy ioref contents to assist GC
        writeIORef syncState emptySyncState
        return (numMissingFromLocal, missingChunks, remoteHashes, remoteHw)

    -- Push transactions that are available locally but are possibly missing
    -- to the remote pool.
    --
    push ourHw0 remoteHashes = do
        ref <- newIORef 0
        ourHw <- mempoolGetPendingTransactions localMempool ourHw0 $ \chunk -> do
            let chunk' = V.filter (not . flip HashSet.member remoteHashes) chunk
            unless (V.null chunk') $ do
                sendChunk chunk'
                modifyIORef' ref (+ V.length chunk')
        numPushed <- readIORef ref
        return (numPushed, ourHw)

    -- Send a chunk of tranactions to the remote pool.
    --
    sendChunk chunk = do
        v <- (V.map fromPending . V.filter isPending) <$> mempoolLookup localMempool chunk
        unless (V.null v) $ mempoolInsert remoteMempool CheckedInsert v


syncMempools
    :: Show t
    => LogFunctionText
    -> PeerInfo
    -> Int                  -- ^ polling interval in microseconds
    -> MempoolBackend t     -- ^ local mempool
    -> MempoolBackend t     -- ^ remote mempool
    -> IO ()
syncMempools log peer us localMempool remoteMempool =
    syncMempools' log peer us localMempool remoteMempool

------------------------------------------------------------------------------
-- | Raw/unencoded transaction hashes.
--
-- TODO: production versions of this kind of DB should salt with a
-- runtime-generated constant to avoid collision attacks; see the \"hashing and
-- security\" section of the hashable docs.
newtype TransactionHash = TransactionHash { unTransactionHash :: SB.ShortByteString }
  deriving stock (Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Show TransactionHash where
    show = T.unpack . encodeToText

instance Hashable TransactionHash where
  hashWithSalt s (TransactionHash h) = hashWithSalt s (hashCode :: Int)
    where
      hashCode = either error id $ runGetS (fromIntegral <$> getWord64host) (B.take 8 $ SB.fromShort h)
  {-# INLINE hashWithSalt #-}

instance ToJSON TransactionHash where
  toJSON (TransactionHash x) = toJSON $! encodeB64UrlNoPaddingText $ SB.fromShort x
instance FromJSON TransactionHash where
  parseJSON = withText "TransactionHash" (either (fail . show) return . p)
    where
      p :: Text -> Either SomeException TransactionHash
      !p = (TransactionHash . SB.toShort <$>) . decodeB64UrlNoPaddingText

instance HasTextRepresentation TransactionHash where
  toText (TransactionHash th) = encodeB64UrlNoPaddingText $ SB.fromShort th
  fromText = (TransactionHash . SB.toShort <$>) . decodeB64UrlNoPaddingText

------------------------------------------------------------------------------
data TransactionMetadata = TransactionMetadata {
    txMetaCreationTime :: {-# UNPACK #-} !(Time Micros)
  , txMetaExpiryTime :: {-# UNPACK #-} !(Time Micros)
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, NFData)


------------------------------------------------------------------------------
-- | Mempools will check these values match in APIs
data HashMeta = HashMeta {
    hashmetaName :: {-# UNPACK #-} !Text
  , hashmetaLenBytes :: {-# UNPACK #-} !Int
}

chainwebTestHasher :: ByteString -> TransactionHash
chainwebTestHasher s = let !b = SB.toShort $ convert $ hash @_ @SHA512t_256 $ "TEST" <> s
                       in TransactionHash b

chainwebTestHashMeta :: HashMeta
chainwebTestHashMeta = HashMeta "chainweb-sha512-256" 32

data ValidatedTransaction t = ValidatedTransaction
    { validatedHeight :: {-# UNPACK #-} !BlockHeight
    , validatedHash :: {-# UNPACK #-} !BlockHash
    , validatedTransaction :: !t
    }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData) -- TODO: a handwritten instance


------------------------------------------------------------------------------
-- | Mempool only cares about a few projected values from the transaction type
-- (gas price, gas limit, hash, metadata), so our mock transaction type will only
-- contain these (plus a nonce)
data MockTx = MockTx {
    mockNonce :: {-# UNPACK #-} !Int64
  , mockGasPrice :: {-# UNPACK #-} !GasPrice
  , mockGasLimit :: !GasLimit
  , mockMeta :: {-# UNPACK #-} !TransactionMetadata
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, NFData)


mockBlockGasLimit :: GasLimit
mockBlockGasLimit = 100_000_000


-- | A codec for transactions when sending them over the wire.
mockCodec :: Codec MockTx
mockCodec = Codec mockEncode mockDecode


mockEncode :: MockTx -> ByteString
mockEncode (MockTx nonce (GasPrice (ParsedDecimal price)) limit meta) =
  B64.encode $
  runPutS $ do
    putWord64le $ fromIntegral nonce
    putDecimal price
    putWord64le $ fromIntegral limit
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
    !places <- getWord8
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
mockDecode s = do
    s' <- B64.decode s
    runGetS (MockTx <$> getI64 <*> getPrice <*> getGL <*> getMeta) s'
  where
    getPrice = GasPrice . ParsedDecimal <$> getDecimal
    getGL = GasLimit . ParsedInteger . fromIntegral <$> getWord64le
    getI64 = fromIntegral <$> getWord64le
    getMeta = TransactionMetadata <$> Time.decodeTime <*> Time.decodeTime
