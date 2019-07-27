{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.Mempool.Mempool
  ( MempoolBackend(..)
  , TransactionConfig(..)
  , TransactionHash(..)
  , TransactionMetadata(..)
  , MempoolTxId
  , HashMeta(..)
  , ValidatedTransaction(..)
  , LookupResult(..)
  , MockTx(..)
  , ServerNonce
  , HighwaterMark

  , chainwebTransactionConfig
  , mockCodec
  , mockEncode
  , mockBlockGasLimit
  , chainwebTestHasher
  , chainwebTestHashMeta
  , noopMempool
  , syncMempools
  , syncMempools'
  , GasLimit(..)
  ) where
------------------------------------------------------------------------------
import Control.Concurrent (threadDelay)
import Control.DeepSeq (NFData)
import Control.Exception
import Control.Monad (replicateM, when)

import Crypto.Hash (hash)
import Crypto.Hash.Algorithms (SHA512t_256)

import Data.Aeson
import Data.Bits (bit, shiftL, shiftR, (.&.))
import Data.ByteArray (convert)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Decimal (Decimal, DecimalRaw(..))
import Data.Foldable (traverse_)
import Data.Hashable (Hashable(hashWithSalt))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int (Int64)
import Data.IORef
import Data.List (unfoldr)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)

import GHC.Generics

import Prelude hiding (log)

import System.LogLevel

-- internal modules

import Pact.Parse (ParsedDecimal(..))
import Pact.Types.Command
import Pact.Types.Gas (GasPrice(..),GasLimit(..))
import qualified Pact.Types.Hash as H

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Time (Micros(..), Time(..))
import qualified Chainweb.Time as Time
import Chainweb.Transaction
import Chainweb.Utils
    (Codec(..), decodeB64UrlNoPaddingText, encodeB64UrlNoPaddingText,
    encodeToText, sshow)
import Data.LogMessage (LogFunctionText)

------------------------------------------------------------------------------
data LookupResult t = Missing
                    | Validated (ValidatedTransaction t)
                    | Confirmed
                    | Pending t
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData) -- TODO: a handwritten instance

------------------------------------------------------------------------------
data TransactionConfig t = TransactionConfig {
    -- | converting transactions to/from bytestring.
    txCodec :: {-# UNPACK #-} !(Codec t)

    -- | hash function to use when making transaction hashes.
  , txHasher :: t -> TransactionHash

    -- | hash function metadata.
  , txHashMeta :: {-# UNPACK #-} !HashMeta

    -- | getter for the transaction gas price.
  , txGasPrice :: t -> GasPrice

    -- | getter for transaction gas limit.
  , txGasLimit :: t -> GasLimit
  , txMetadata :: t -> TransactionMetadata
  , txValidate :: t -> IO Bool
  }

------------------------------------------------------------------------------
type MempoolTxId = Int64
type ServerNonce = Int
type HighwaterMark = (ServerNonce, MempoolTxId)

------------------------------------------------------------------------------
-- | Mempool backend API. Here @t@ is the transaction payload type.
data MempoolBackend t = MempoolBackend {
    mempoolTxConfig :: {-# UNPACK #-} !(TransactionConfig t)

    -- TODO: move this inside TransactionConfig or new MempoolConfig ?
    -- TODO this can potentially change at runtime
  , mempoolBlockGasLimit :: GasLimit

    -- | Returns true if the given transaction hash is known to this mempool.
  , mempoolMember :: Vector TransactionHash -> IO (Vector Bool)

    -- | Lookup transactions in the pending queue by hash.
  , mempoolLookup :: Vector TransactionHash -> IO (Vector (LookupResult t))

    -- | Insert the given transactions into the mempool.
  , mempoolInsert :: Vector t -> IO ()

    -- | given maximum block size, produce a candidate block of transactions
    -- for mining.
    -- TODO what is the relationship of this GasLimit to the configured one?
    -- Not sure this is something an external client should be dictating.
  , mempoolGetBlock :: GasLimit -> IO (Vector t)

    -- | mark the given transactions as being mined and validated.
  , mempoolMarkValidated :: Vector (ValidatedTransaction t) -> IO ()

    -- | mark the given hashes as being past confirmation depth.
  , mempoolMarkConfirmed :: Vector TransactionHash -> IO ()

    -- | These transactions were on a losing fork. Reintroduce them.

  , mempoolReintroduce :: Vector t -> IO ()

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


noopMempool :: IO (MempoolBackend t)
noopMempool = do
  return $ MempoolBackend
    { mempoolTxConfig = txcfg
    , mempoolBlockGasLimit = 1000
    , mempoolMember = noopMember
    , mempoolLookup = noopLookup
    , mempoolInsert = noopInsert
    , mempoolGetBlock = noopGetBlock
    , mempoolMarkValidated = noopMarkValidated
    , mempoolMarkConfirmed = noopMarkConfirmed
    , mempoolReintroduce = noopReintroduce
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
                              noopMeta (const $ return True)
    noopMember v = return $ V.replicate (V.length v) False
    noopLookup v = return $ V.replicate (V.length v) Missing
    noopInsert = const $ return ()
    noopGetBlock = const $ return V.empty
    noopMarkValidated = const $ return ()
    noopMarkConfirmed = const $ return ()
    noopReintroduce = const $ return ()
    noopGetPending = const $ const $ return (0,0)
    noopClear = return ()



------------------------------------------------------------------------------
chainwebTransactionConfig :: TransactionConfig ChainwebTransaction
chainwebTransactionConfig = TransactionConfig chainwebPayloadCodec
    commandHash
    chainwebTestHashMeta
    getGasPrice
    getGasLimit
    (const txmeta)
    (const $ return True)       -- TODO: insert extra transaction validation here

  where
    getGasPrice = gasPriceOf . fmap payloadObj
    getGasLimit = fromIntegral . gasLimitOf . fmap payloadObj
    commandHash c = let (H.Hash h) = H.toUntypedHash $ _cmdHash c
                    in TransactionHash h

    -- TODO: plumb through origination + expiry time from pact once it makes it
    -- into PublicMeta
    txmeta = TransactionMetadata Time.minTime Time.maxTime

------------------------------------------------------------------------------
data SyncState = SyncState {
    _syncCount :: {-# UNPACK #-} !Int64
  , _syncMissing :: ![Vector TransactionHash]
  , _syncPresent :: !(HashSet TransactionHash)
  , _syncTooMany :: !Bool
  }

-- | Pulls any missing pending transactions from a remote mempool.
--
-- The initial sync procedure:
--
--    1. get the list of pending transaction hashes from remote,
--    2. lookup any hashes that are missing, and insert them into local,
--    3. push any hashes remote is missing
--
-- After initial sync, will loop subscribing to updates from remote.
--
-- Loops until killed, or until the underlying mempool connection throws an
-- exception.
--
syncMempools'
    :: Show t
    => LogFunctionText
    -> Int
        -- ^ polling interval in microseconds
    -> MempoolBackend t
        -- ^ local mempool
    -> MempoolBackend t
        -- ^ remote mempool
    -> IO ()
syncMempools' log0 us localMempool remoteMempool = sync

  where
    maxCnt = 10000
        -- don't pull more than this many new transactions from a single peer in
        -- a session.

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
        when (not tooMany) $ do

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
        mempoolInsert localMempool newTxs

    deb :: Text -> IO ()
    deb = log0 Debug

    sync = finally (initialSync >>= subsequentSync) (deb "sync exiting")

    initialSync = do
        deb "Get full list of pending hashes from remote"

        (numMissingFromLocal, missingChunks, remoteHashes, remoteHw) <-
            fetchSince Nothing

        deb $ T.concat
            [ sshow (HashSet.size remoteHashes)
            , " hashes at remote ("
            , sshow numMissingFromLocal
            , " need to be fetched)"
            ]

        -- todo: should we do subsequent sync of pushed txs also?
        (numPushed, _) <- do
            -- Go fetch missing transactions from remote
            traverse_ fetchMissing missingChunks

            -- Push our missing txs to remote.
            push remoteHashes

        deb $ "pushed " <> sshow numPushed <> " new transactions to remote."
        return remoteHw

    subsequentSync !remoteHw = do
        deb "Get new pending hashes from remote"
        (numMissingFromLocal, missingChunks, _, remoteHw') <-
            fetchSince $! Just remoteHw
        deb $ T.concat
            [ "sync: "
            , sshow numMissingFromLocal
            , " new remote hashes need to be fetched"
            ]
        traverse_ fetchMissing missingChunks
        threadDelay us
        subsequentSync remoteHw'

    -- get pending hashes from remote since the given (optional) high water mark
    fetchSince oldRemoteHw = do
        -- Intialize and collect SyncState
        let emptySyncState = SyncState 0 [] HashSet.empty False
        syncState <- newIORef emptySyncState
        remoteHw <- mempoolGetPendingTransactions remoteMempool oldRemoteHw $ syncChunk syncState
        (SyncState numMissingFromLocal missingChunks remoteHashes _) <- readIORef syncState
        -- immediately destroy ioref contents to assist GC
        writeIORef syncState emptySyncState
        return (numMissingFromLocal, missingChunks, remoteHashes, remoteHw)

    -- Push transactions that are available locally but are missing from the
    -- remote pool to the remote pool.
    --
    push remoteHashes = do
        ref <- newIORef 0
        ourHw <- mempoolGetPendingTransactions localMempool Nothing $ \chunk -> do
            let chunk' = V.filter (not . flip HashSet.member remoteHashes) chunk
            when (not $ V.null chunk') $ do
                sendChunk chunk'
                modifyIORef' ref (+ V.length chunk')
        numPushed <- readIORef ref
        return (numPushed, ourHw)

    -- Send a chunk of tranactions to the remote pool.
    --
    sendChunk chunk = do
        v <- (V.map fromPending . V.filter isPending) <$> mempoolLookup localMempool chunk
        when (not $ V.null v) $ mempoolInsert remoteMempool v


syncMempools
    :: Show t
    => LogFunctionText
    -> Int                  -- ^ polling interval in microseconds
    -> MempoolBackend t     -- ^ local mempool
    -> MempoolBackend t     -- ^ remote mempool
    -> IO ()
syncMempools log us localMempool remoteMempool =
    syncMempools' log us localMempool remoteMempool

------------------------------------------------------------------------------
-- | Raw/unencoded transaction hashes.
--
-- TODO: production versions of this kind of DB should salt with a
-- runtime-generated constant to avoid collision attacks; see the \"hashing and
-- security\" section of the hashable docs.
newtype TransactionHash = TransactionHash ByteString
  deriving stock (Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Show TransactionHash where
    show = T.unpack . encodeToText

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
chainwebTestHasher s = let b = convert $ hash @_ @SHA512t_256 $ "TEST" <> s
                       in TransactionHash b

chainwebTestHashMeta :: HashMeta
chainwebTestHashMeta = HashMeta "chainweb-sha512-256" 32

data ValidatedTransaction t = ValidatedTransaction
    { validatedHeight :: {-# UNPACK #-} !BlockHeight
    , validatedHash :: {-# UNPACK #-} !BlockHash
    , validatedTransaction :: t
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
  , mockGasLimit :: {-# UNPACK #-} !Int64
  , mockMeta :: {-# UNPACK #-} !TransactionMetadata
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, NFData)


mockBlockGasLimit :: Int64
mockBlockGasLimit = 65535


-- | A codec for transactions when sending them over the wire.
mockCodec :: Codec MockTx
mockCodec = Codec mockEncode mockDecode


mockEncode :: MockTx -> ByteString
mockEncode (MockTx nonce (GasPrice (ParsedDecimal price)) limit meta) = runPutS $ do
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
mockDecode = runGetS (MockTx <$> getI64 <*> getPrice <*> getI64 <*> getMeta)
  where
    getPrice = GasPrice . ParsedDecimal <$> getDecimal
    getI64 = fromIntegral <$> getWord64le
    getMeta = TransactionMetadata <$> Time.decodeTime <*> Time.decodeTime
