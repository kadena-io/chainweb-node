{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module Chainweb.Mempool.Mempool
  ( MempoolBackend(..)
  , TransactionHash
  , TransactionReward
  , TransactionMetadata(..)
  , Codec(..)
  , HashMeta(..)
  , Subscription(..)
  , ValidationInfo(..)
  , ValidatedTransaction(..)
  , LookupResult(..)
  , finalizeSubscriptionImmediately
  ) where
------------------------------------------------------------------------------
import Control.Concurrent.STM.TBMChan (TBMChan)
import Control.DeepSeq (NFData)
import Data.Bytes.Get (getWord64host, runGetS)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Hashable (Hashable(..))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)


------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Time (Time(..))


------------------------------------------------------------------------------
data LookupResult t = Missing
                    | Validated (ValidatedTransaction t)
                    | Confirmed
                    | Pending t

------------------------------------------------------------------------------
-- | Mempool backend API. Here @t@ is the transaction payload type.
data MempoolBackend t = MempoolBackend {
    -- | converting transactions to/from bytestring.
    _mempoolTxCodec :: {-# UNPACK #-} !(Codec t)

    -- | hash function to use when making transaction hashes.
  , _mempoolHasher :: ByteString -> TransactionHash

    -- | hash function metadata.
  , _mempoolHashMeta :: {-# UNPACK #-} !HashMeta

    -- | getter for the transaction reward.
  , _mempoolTxReward :: t -> TransactionReward

    -- | getter for transaction size.
  , _mempoolTxSize :: t -> Int64
  , _mempoolBlockSizeLimit :: Int64

    -- | Lookup transactions in the pending queue by hash.
  , _mempoolLookup :: Vector TransactionHash -> IO (Vector (LookupResult t))

    -- | Insert the given transactions into the mempool.
  , _mempoolInsert :: Vector t -> IO ()

    -- | given maximum block size, produce a candidate block of transactions
    -- for mining.
  , _mempoolGetBlock :: Int64 -> IO (Vector t)

    -- | mark the given transactions as being mined and validated.
  , _mempoolMarkValidated :: Vector (ValidatedTransaction t) -> IO ()

    -- | mark the given hashes as being past confirmation depth.
  , _mempoolMarkConfirmed :: Vector TransactionHash -> IO ()

    -- | These transactions were on a losing fork. Reintroduce them.
  , _mempoolReintroduce :: Vector TransactionHash -> IO ()

    -- | given a callback function, loops through the pending candidate
    -- transactions and supplies the hashes to the callback in chunks.
  , _mempoolGetPendingTransactions
      :: (Vector TransactionHash -> IO ()) -> IO ()

  , _mempoolSubscribe :: IO (Subscription t)
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
-- | For now, just use int64 as a metric for how much reward is to be gained
-- from a block. Higher is better
type TransactionReward = Int64


------------------------------------------------------------------------------
-- | TODO: maybe use Put/Get ?
data Codec t = Codec {
    _codecEncode :: t -> ByteString
  , _codecDecode :: ByteString -> Maybe t
}


------------------------------------------------------------------------------
data TransactionMetadata = TransactionMetadata {
    _metaIngestTime :: {-# UNPACK #-} !(Time Int64)
  , _metaExpiryTime :: {-# UNPACK #-} !(Time Int64)
}


------------------------------------------------------------------------------
-- | Mempools will check these values match in APIs
data HashMeta = HashMeta {
    _hashmetaName :: {-# UNPACK #-} !Text
  , _hashmetaLenBytes :: {-# UNPACK #-} !Int
}


------------------------------------------------------------------------------
-- | Clients can subscribe to a mempool to receive new transactions as soon as
-- they appear.
data Subscription t = Subscription {
    _mempoolSubChan :: TBMChan (Vector t)
  , _mempoolSubFinal :: IO ()
  -- TODO: activity timer
}


------------------------------------------------------------------------------
data ValidationInfo = ValidationInfo {
    _validatedHeight :: !BlockHeight
  , _validatedHash :: !BlockHash
}

data ValidatedTransaction t = ValidatedTransaction {
    _validatedForks :: Vector ValidationInfo
  , _validatedTransaction :: t
}


------------------------------------------------------------------------------
finalizeSubscriptionImmediately :: Subscription t -> IO ()
finalizeSubscriptionImmediately = _mempoolSubFinal

