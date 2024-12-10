{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Mempool.CurrentTxs
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A probabilistic index for non-expired transactions.
--
-- The false positive rate is very low. False negatives rate is 0 when the
-- number of entries is smaller than 'maxCurrentTxsSize' and grows with each
-- entry beyond that size.
--
-- The purpose of this data structure is to allow the mempool to keep track of
-- transactions that haven't yet expired and have already been included in a
-- block. A false positive causes the mempool to ignore a valid transactions. A
-- false negative causes the mempool to mark a transactions as pending which has
-- already been included in a block. The transaction would only be rejected by
-- pact service during new block validation once it has been included in another
-- block.
--
module Chainweb.Mempool.CurrentTxs
( CurrentTxs(..)
, newCurrentTxs
, currentTxsSize
, currentTxsInsert
, currentTxsInsertBatch
, currentTxsMember
, currentTxsRemove
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
#if !MIN_VERSION_base(4,20,0)
import Data.Foldable
#endif
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V

import Numeric.Natural

import System.Random

-- internal imports

-- import Chainweb.Mempool.InMemTypes
import Chainweb.Mempool.Mempool
import Chainweb.Time
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Configuration

-- | Limits the size of the set of current txs. If the size is reached elements are
-- deleted, and queries may return false negatives. No guarantee is made about
-- which elements are deleted.
--
-- 16 MB ~ 1000000 entries
--
maxCurrentTxsSize :: Natural
maxCurrentTxsSize = 1024 * 1024

-- -------------------------------------------------------------------------- --
-- Keys

-- | The first 8 bytes are the expiration time of the key, the remaining 8 bytes
-- are the first 8 bytes of tx hash. These two number provide plenty of entropy
-- to make collisions very unlikely. In the rare event of an collision, the
-- mempool may ignore some legitimate txs.
--
data CurrentTxsKey = CurrentTxsKey
    { _currentTxsExpiration :: {-# UNPACK #-} !(Time Micros)
    , _currentTxsKey :: {-# UNPACK #-} !BS.ShortByteString
    }
    deriving (Show, Eq, Ord)

getCurrentTxsKey :: Time Micros -> TransactionHash -> CurrentTxsKey
getCurrentTxsKey expiry h = CurrentTxsKey
    { _currentTxsExpiration = expiry
    , _currentTxsKey = BS.toShort $ B.take 8 $ BS.fromShort $ unTransactionHash h
    }

-- -------------------------------------------------------------------------- --
-- Index

newtype CurrentTxs = CurrentTxs { _currentTxs :: S.Set CurrentTxsKey }

newCurrentTxs :: CurrentTxs
newCurrentTxs = CurrentTxs mempty

currentTxsSize :: CurrentTxs -> Int
currentTxsSize (CurrentTxs s) = S.size s
{-# INLINE currentTxsSize #-}

-- Membership

currentTxsMember :: CurrentTxs -> Time Micros -> TransactionHash -> Bool
currentTxsMember s expiry h = S.member (getCurrentTxsKey expiry h) (_currentTxs s)
{-# INLINE currentTxsMember #-}

-- Insertion

-- | If the set is full random elements are deleted.
--
currentTxsInsert :: CurrentTxs -> Time Micros -> TransactionHash -> IO CurrentTxs
currentTxsInsert s expiry h =
    CurrentTxs . S.insert (getCurrentTxsKey expiry h) . _currentTxs
    <$> pruneCurrentTxs s

currentTxsInsertBatch
    :: CurrentTxs
    -> V.Vector (Time Micros, TransactionHash)
    -> IO CurrentTxs
currentTxsInsertBatch s txs = do
    s0 <- pruneCurrentTxs s
    pruneCurrentTxs $ CurrentTxs $ foldl' ins (_currentTxs s0) txs
  where
    ins x (e, h) = S.insert (getCurrentTxsKey e h) x

-- Deletion

currentTxsRemove :: CurrentTxs -> Time Micros -> TransactionHash -> CurrentTxs
currentTxsRemove (CurrentTxs s) expiry h = CurrentTxs $
    S.delete (getCurrentTxsKey expiry h) s
{-# INLINE currentTxsRemove #-}

-- Pruning

pruneCurrentTxs :: CurrentTxs -> IO CurrentTxs
pruneCurrentTxs (CurrentTxs s) = do
    now <- getCurrentTimeIntegral
    let s0 = S.dropWhileAntitone (\x -> _currentTxsExpiration x < now) s
        l = S.size s0

    -- a random set of distinct set indexes that are going to be deleted to make
    -- room for the new entry. For performance reasons we delete elements in
    -- chunks of 1000.
    --
    indexesToBeDeleted <- if l >= int maxCurrentTxsSize
      then
        -- The following terminates because n <= l
        --
        -- @maxCurrentTxsSize@ should be much larger than 1000. The worst case
        -- expected running time is for @1000 == maxCurrentTxsSize@, which still
        -- is very fast.
        --
        let n = min l (max 1000 (l - int maxCurrentTxsSize))
        in take n . L.nub . randomRs (0, l-1) <$> newStdGen
      else return []

    -- Not sure if foldl' would be faster here
    return $ CurrentTxs $! foldl' (flip S.deleteAt) s0 indexesToBeDeleted
