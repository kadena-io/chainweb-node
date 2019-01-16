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
  , Codec(..)
  , HashMeta(..)
  , Subscription(..)
  , ValidationInfo(..)
  , ValidatedTransaction(..)
  , LookupResult(..)
  , finalizeSubscriptionImmediately
  , chainwebTestHasher
  , chainwebTestHashMeta
  ) where
------------------------------------------------------------------------------
import Control.Concurrent.STM.TBMChan (TBMChan)
import Control.DeepSeq (NFData)
import Data.Bytes.Get (getWord64host, runGetS)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Decimal (Decimal)
import Data.Hashable (Hashable(..))
import Data.Int (Int64)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)


------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Time (Time(..))
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
-- | TODO: maybe use Put/Get ?
data Codec t = Codec {
    codecEncode :: t -> ByteString
  , codecDecode :: ByteString -> Maybe t
}


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
