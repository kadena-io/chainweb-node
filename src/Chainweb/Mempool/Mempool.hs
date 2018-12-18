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
  , finalizeSubscriptionImmediately
  ) where
------------------------------------------------------------------------------
import Control.Concurrent.MVar (MVar)
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
import System.Mem.Weak (Weak)
import qualified System.Mem.Weak as Weak


------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Time (Time(..))

------------------------------------------------------------------------------
-- | Mempool backend API. Here @t@ is the transaction payload type.
data MempoolBackend t = MempoolBackend {
    -- | converting transactions to/from bytestring.
    _mempool_tx_codec :: {-# UNPACK #-} !(Codec t)

    -- | hash function to use when making transaction hashes.
  , _mempool_hasher :: ByteString -> TransactionHash

    -- | hash function metadata.
  , _mempool_hash_meta :: {-# UNPACK #-} !HashMeta

    -- | getter for the transaction reward.
  , _mempool_tx_reward :: t -> TransactionReward

    -- | getter for transaction size.
  , _mempool_tx_size :: t -> Int64

    -- | Lookup transactions by hash.
  , _mempool_lookup :: Vector TransactionHash -> IO (Vector (Maybe t))

    -- | Insert the given transactions into the mempool.
  , _mempool_insert :: Vector t -> IO ()

    -- | given maximum block size, produce a candidate block of transactions
    -- for mining.
  , _mempool_get_block :: Int64 -> IO (Vector t)

    -- | mark the given hashes as being mined and validated.
  , _mempool_mark_validated :: Vector TransactionHash -> IO ()

    -- | mark the given hashes as being past confirmation depth.
  , _mempool_mark_confirmed :: Vector TransactionHash -> IO ()

    -- | These transactions were on a losing fork. Reintroduce them.
  , _mempool_reintroduce :: Vector TransactionHash -> IO ()

    -- | given a callback function, loops through the pending candidate
    -- transactions and supplies the hashes to the callback in chunks.
  , _mempool_get_pending_transactions
      :: (Vector TransactionHash -> IO ()) -> IO ()

  , _mempool_subscribe :: IO (Subscription t)
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
    _encode :: t -> ByteString
  , _decode :: ByteString -> Maybe t
}


------------------------------------------------------------------------------
data TransactionMetadata = TransactionMetadata {
    _meta_ingest_time :: {-# UNPACK #-} !(Time Int64)
  , _meta_expiry_time :: {-# UNPACK #-} !(Time Int64)
}


------------------------------------------------------------------------------
-- | Mempools will check these values match in APIs
data HashMeta = HashMeta {
    _hashmeta_name :: {-# UNPACK #-} !Text
  , _hashmeta_len_bytes :: {-# UNPACK #-} !Int
}


------------------------------------------------------------------------------
-- | Clients can subscribe to a mempool to receive new transactions as soon as
-- they appear.
data Subscription t = Subscription {
    _mempool_sub_chan :: MVar (TBMChan t)
  , _mempool_sub_final :: Weak (MVar (TBMChan t))
}


------------------------------------------------------------------------------
data ValidationInfo = ValidationInfo {
    _validated_height :: !BlockHeight
  , _validated_hash :: !BlockHash
}

data ValidatedTransaction t = ValidatedTransaction {
    _validated_forks :: Vector ValidationInfo
  , _validated_transaction :: t
}


------------------------------------------------------------------------------
finalizeSubscriptionImmediately :: Subscription t -> IO ()
finalizeSubscriptionImmediately = Weak.finalize . _mempool_sub_final

