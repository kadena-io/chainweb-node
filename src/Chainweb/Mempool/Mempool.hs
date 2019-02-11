{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ) where
------------------------------------------------------------------------------
import Control.Concurrent.STM.TBMChan (TBMChan)
import Control.DeepSeq (NFData)
import Control.Monad (replicateM)
import Data.Bits (bit, shiftL, shiftR, (.&.))
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Decimal (Decimal, DecimalRaw(..))
import Data.Hashable (Hashable(..))
import Data.Int (Int64)
import Data.IORef (IORef)
import Data.List (unfoldr)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import GHC.Generics (Generic)


------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Time (Time(..))
import qualified Chainweb.Time as Time
import Chainweb.Utils (Codec(..))
import Chainweb.Version


------------------------------------------------------------------------------
data LookupResult t = Missing
                    | Validated (ValidatedTransaction t)
                    | Confirmed
                    | Pending t
  deriving (Show)

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
  }

------------------------------------------------------------------------------
-- | Mempool backend API. Here @t@ is the transaction payload type.
data MempoolBackend t = MempoolBackend {
    mempoolTxConfig :: {-# UNPACK #-} !(TransactionConfig t)

    -- TODO: move this inside TransactionConfig ?
  , mempoolBlockSizeLimit :: Int64

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
}


noopMempool :: MempoolBackend t
noopMempool = MempoolBackend txcfg 1000 noopLookup noopInsert noopGetBlock
                             noopMarkValidated noopMarkConfirmed noopReintroduce
                             noopGetPending noopSubscribe noopShutdown
  where
    unimplemented = fail "unimplemented"
    noopCodec = Codec (const "") (const $ Left "unimplemented")
    noopHasher = const $ chainwebTestHasher "noopMempool"
    noopHashMeta = chainwebTestHashMeta
    noopFees = const 0
    noopSize = const 1
    noopMeta = const $ TransactionMetadata Time.minTime Time.maxTime
    txcfg = TransactionConfig noopCodec noopHasher noopHashMeta noopFees noopSize
                              noopMeta

    noopLookup v = return $ V.replicate (V.length v) Missing
    noopInsert = const $ return ()
    noopGetBlock = const $ return V.empty
    noopMarkValidated = const $ return ()
    noopMarkConfirmed = const $ return ()
    noopReintroduce = const $ return ()
    noopGetPending = const $ return ()
    noopSubscribe = unimplemented
    noopShutdown = return ()


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
      hashCode = either error id $ runGetS (fromIntegral <$> getWord64host) (S.take 8 h)
  {-# INLINE hashWithSalt #-}


------------------------------------------------------------------------------
-- | Fees to be awarded to the miner for processing a transaction. Higher is better
type TransactionFees = Decimal


------------------------------------------------------------------------------
data TransactionMetadata = TransactionMetadata {
    txMetaCreationTime :: {-# UNPACK #-} !(Time Int64)
  , txMetaExpiryTime :: {-# UNPACK #-} !(Time Int64)
} deriving (Eq, Ord, Show)


------------------------------------------------------------------------------
-- | Mempools will check these values match in APIs
data HashMeta = HashMeta {
    hashmetaName :: {-# UNPACK #-} !Text
  , hashmetaLenBytes :: {-# UNPACK #-} !Int
}

chainwebTestHasher :: ByteString -> TransactionHash
chainwebTestHasher s = let (BlockHashBytes b) = cryptoHash Test s
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
} deriving (Show)

data ValidatedTransaction t = ValidatedTransaction {
    validatedForks :: Vector ValidationInfo
  , validatedTransaction :: t
} deriving (Show)


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

