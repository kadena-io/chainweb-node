{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Mempool.CurrentTxIndex
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A probabilistic index for non-expired transactions.
--
-- The false positive rate is very low. False negatives rate is 0 when the
-- number of entries is smaller than 'maxCurrentTxIdxSize' and grows with each
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
module Chainweb.Mempool.CurrentTxIndex
( CurrentTxIdx
, newCurrentTxIdx
, currentTxIdxSize
, currentTxIdxInsert
, currentTxIdxInsertBatch
, currentTxIdxMember
, currentTxIdxRemove
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
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

-- | Limits the size of the commit log. If the size is reached elements are
-- deleted, and queries may return false negatives. Not guarantee is made about
-- what elements are deleted.
--
-- 16 MB ~ 1000000 entries
--
maxCurrentTxIdxSize :: Natural
maxCurrentTxIdxSize = 1024 * 1024

-- -------------------------------------------------------------------------- --
-- Keys

-- | The first 8 bytes are the expiration time of the key, the remaining 8 bytes
-- are the first 8 bytes of tx hash. These two number provide plenty of entropy
-- to make collisions very unlikely. In the rare event of an collision, the
-- mempool may ignore some legitimate txs.
--
data CurrentTxIdxKey = CurrentTxIdxKey
    { _currentTxIdxExpiration :: {-# UNPACK #-} !(Time Micros)
    , _currentTxIdxKey :: {-# UNPACK #-} !BS.ShortByteString
    }
    deriving (Show, Eq, Ord)

getCurrentTxIdxKey :: Time Micros -> TransactionHash -> CurrentTxIdxKey
getCurrentTxIdxKey expiry h = CurrentTxIdxKey
    { _currentTxIdxExpiration = expiry
    , _currentTxIdxKey = BS.toShort $ B.take 8 $ BS.fromShort $ unTransactionHash h
    }

-- -------------------------------------------------------------------------- --
-- Index

newtype CurrentTxIdx = CurrentTxIdx { _currentTxIdx :: S.Set CurrentTxIdxKey }

newCurrentTxIdx :: CurrentTxIdx
newCurrentTxIdx = CurrentTxIdx mempty

currentTxIdxSize :: CurrentTxIdx -> Int
currentTxIdxSize (CurrentTxIdx s) = S.size s
{-# INLINE currentTxIdxSize #-}

-- Membership

currentTxIdxMember :: CurrentTxIdx -> Time Micros -> TransactionHash -> Bool
currentTxIdxMember s expiry h = S.member (getCurrentTxIdxKey expiry h) (_currentTxIdx s)
{-# INLINE currentTxIdxMember #-}

-- Insertion

-- | If the set is full random elements are deleted.
--
currentTxIdxInsert :: CurrentTxIdx -> Time Micros -> TransactionHash -> IO CurrentTxIdx
currentTxIdxInsert s expiry h =
    CurrentTxIdx . S.insert (getCurrentTxIdxKey expiry h) . _currentTxIdx
    <$> pruneCurrentTxIdx s

currentTxIdxInsertBatch
    :: CurrentTxIdx
    -> V.Vector (Time Micros, TransactionHash)
    -> IO CurrentTxIdx
currentTxIdxInsertBatch s txs = do
    s0 <- pruneCurrentTxIdx s
    let s1 = CurrentTxIdx $ foldr ins (_currentTxIdx s0) txs
    pruneCurrentTxIdx s1
  where
    ins (e, h) = S.insert (getCurrentTxIdxKey e h)

-- Deletion

currentTxIdxRemove :: CurrentTxIdx -> Time Micros -> TransactionHash -> CurrentTxIdx
currentTxIdxRemove (CurrentTxIdx s) expiry h = CurrentTxIdx $
    S.delete (getCurrentTxIdxKey expiry h) s
{-# INLINE currentTxIdxRemove #-}

-- Pruning

pruneCurrentTxIdx :: CurrentTxIdx -> IO CurrentTxIdx
pruneCurrentTxIdx (CurrentTxIdx s) = do
    now <- getCurrentTimeIntegral
    let s0 = S.dropWhileAntitone (\x -> _currentTxIdxExpiration x < now) s
        l = S.size s0

    -- a random set of distinct set indexes that are going to be deleted to make
    -- room for the new entry. For performance reasons we delete elements in
    -- chunks of 1000.
    --
    indexesToBeDeleted <- if l >= int maxCurrentTxIdxSize
      then
        -- The following terminates because n <= l
        --
        -- @maxCurrentTxIdxSize@ should be much larger than 1000. The worst case
        -- expected running time is for @1000 == maxCurrentTxIdxSize@, which still
        -- is very fast.
        --
        let n = min l (max 1000 (l - int maxCurrentTxIdxSize))
        in take n . L.nub . randomRs (0, l-1) <$> newStdGen
      else return []

    -- Not sure if foldl' would be faster here
    return $ CurrentTxIdx $! foldr S.deleteAt s0 indexesToBeDeleted

