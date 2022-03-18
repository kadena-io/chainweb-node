{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.BlockHeaderDB.PruneForks
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Prune old forks from BlockHeader DB
--
module Chainweb.BlockHeaderDB.PruneForks
( pruneForksLogg
, pruneForks
, pruneForks_
) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Catch

import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.Semigroup
import qualified Data.Text as T

import GHC.Generics
import GHC.Stack

import Numeric.Natural

import Prelude hiding (lookup)

import qualified Streaming.Prelude as S

import System.LogLevel

-- internal imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Internal
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.TreeDB
import Chainweb.Utils hiding (Codec)
import Chainweb.Version

import Data.CAS
import Data.CAS.RocksDB
import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- Chain Database Pruning

pruneForksLogg
    :: Logger logger
    => logger
    -> BlockHeaderDb
    -> Natural
        -- ^ The depth at which pruning starts. Block at this depth are used as
        -- pivots and actual deletion starts at (depth - 1).
        --
        -- Note, that the max rank isn't necessarly included in the current best
        -- cut. So one, should choose a depth for which one is confident that
        -- all forks are resolved.

    -> (Bool -> BlockHeader -> IO ())
        -- ^ Deletion call back. This hook is called /after/ the entry is
        -- deleted from the database. It's main purpose is to delete any
        -- resources that were related to the deleted header and that are not
        -- needed any more. The first parameter indicates whether the block
        -- header got deleted from the chain database.

    -> IO Int
pruneForksLogg = pruneForks . logFunctionText

-- | Prunes most block headers from forks that are older than the given number
-- of blocks.
--
-- This function guarantees that the predecessors of all remaining nodes also
-- remain in the database and that there are are no tangling references.
--
-- This function doesn't guarantee to delete all blocks on forks. A small number
-- of fork blocks may not get deleted.
--
-- The function takes a callback that are invoked on each block header in the
-- database starting from the given depth. The callback takes a parameter that
-- indicates whether the related block is pruned or not.
--
pruneForks
    :: LogFunctionText
    -> BlockHeaderDb
    -> Natural
        -- ^ The depth at which pruning starts. Block at this depth are used as
        -- pivots and actual deletion starts at (depth - 1).
        --
        -- Note, that the max rank isn't necessarly included in the current best
        -- cut. So one, should choose a depth for which one is confident that
        -- all forks are resolved.

    -> (Bool -> BlockHeader -> IO ())
        -- ^ Deletion call back. This hook is called /after/ the entry is
        -- deleted from the database. It's main purpose is to delete any
        -- resources that were related to the deleted header and that are not
        -- needed any more. The first parameter indicates whether the block
        -- header got deleted from the chain database.

    -> IO Int
pruneForks logg cdb depth callback = do
    hdr <- maxEntry cdb
    if
        | int (_blockHeight hdr) <= depth -> do
            logg Info
                $ "Skipping database pruning because the maximum block height "
                <> sshow (_blockHeight hdr) <> " is not larger than then requested depth "
                <> sshow depth
            return 0
        | int (_blockHeight hdr) <= int genHeight + depth -> do
            logg Info $ "Skipping database pruning because there are not yet"
                <> " enough block headers on the chain"
            return 0
        | otherwise -> do
            let mar = MaxRank $ Max $ int (_blockHeight hdr) - depth
            pruneForks_ logg cdb mar (MinRank $ Min $ int genHeight) callback
  where
    v = _chainwebVersion cdb
    cid = _chainId cdb
    genHeight = genesisHeight v cid

data PruneForksException
    = PruneForksDbInvariantViolation BlockHeight [BlockHeight] T.Text
    deriving (Show, Eq, Ord, Generic)

instance Exception PruneForksException

-- | Prune forks between the given min rank and max rank.
--
-- Only block headers that are ancestors of a block header that has block height
-- max rank are kept.
--
-- This function is mostly for internal usage. For details about the arguments
-- and behavior see the documentation of 'pruneForks'.
--
-- TODO add option to also validate the block headers
--
pruneForks_
    :: HasCallStack
    => LogFunctionText
    -> BlockHeaderDb
    -> MaxRank
    -> MinRank
    -> (Bool -> BlockHeader -> IO ())
    -> IO Int
pruneForks_ logg _ (MaxRank (Max mar)) _  _
    | mar <= 1 = 0 <$ logg Warn ("Skipping database pruning for max bound of " <> sshow mar)
pruneForks_ logg cdb mar mir callback = do
    logg Debug $ "Pruning block header database for chain " <> sshow (_chainId cdb)
        <> " with upper bound " <> sshow (_getMaxRank mar)
        <> " and lower bound " <> sshow (_getMinRank mir)

    -- parent hashes of all blocks at height max rank @mar@.
    --
    !pivots <- entries cdb Nothing Nothing (Just $ MinRank $ Min $ _getMaxRank mar) (Just mar)
        $ fmap (force . L.nub) . S.toList_ . S.map _blockParent
            -- the set of pivots is expected to be very small. In fact it is
            -- almost always a singleton set.

    if null pivots
      then do
        logg Warn
            $ "Skipping database pruning because of an empty set of block headers at upper pruning bound " <> sshow mar
        return 0
      else
        withReverseHeaderStream cdb (mar - 1) mir
            $ S.foldM_ go (return (pivots, int (_getMaxRank mar), 0)) (\(_,_,!n) -> evaluate n)
            . progress 200000 reportProgress

  where
    reportProgress i a = logg Info
        $ "inspected " <> sshow i
        <> " block headers. Current height "
        <> sshow (_blockHeight a)

    go :: ([BlockHash], BlockHeight, Int) -> BlockHeader -> IO ([BlockHash], BlockHeight, Int)
    go ([], _, _) cur = throwM $ InternalInvariantViolation
        $ "PruneForks.pruneForks_: no pivots left at block " <> encodeToText (ObjectEncoded cur)
    go (!pivots, !prevHeight, !n) !cur

        -- This checks some structural consistency. It's not a comprehensive
        -- check. Comprehensive checks on various levels are availabe through
        -- callbacks that are offered in the module "Chainweb.Chainweb.PruneChainDatabase"
        | prevHeight /= curHeight && prevHeight /= curHeight + 1 =
            throwM $ InternalInvariantViolation
                $ "PruneForks.pruneForks_: detected a corrupted database. Some block headers are missing"
                <> ". Current pivots: " <> encodeToText pivots
                <> ". Current header: " <> encodeToText (ObjectEncoded cur)
                <> ". Previous height: " <> sshow prevHeight
        | _blockHash cur `elem` pivots = do
            callback False cur
            let !pivots' = force $ L.nub $ _blockParent cur : L.delete (_blockHash cur) pivots
            return (pivots', curHeight, n)
        | otherwise = do
            deleteHdr cur
            callback True cur
            return (pivots, curHeight, n+1)
      where
        curHeight = _blockHeight cur

    deleteHdr k = do
        -- TODO: make this atomic (create boilerplate to combine queries for
        -- different tables)
        casDelete (_chainDbCas cdb) (casKey $ RankedBlockHeader k)
        tableDelete (_chainDbRankTable cdb) (_blockHash k)
        logg Debug
            $ "pruned block header " <> encodeToText (_blockHash k)
            <> " at height " <> sshow (_blockHeight k)

-- -------------------------------------------------------------------------- --
-- Utils

-- TODO: provide this function in chainweb-storage
--
withReverseHeaderStream
    :: BlockHeaderDb
    -> MaxRank
    -> MinRank
    -> (S.Stream (S.Of BlockHeader) IO () -> IO a)
    -> IO a
withReverseHeaderStream db mar mir inner = withTableIter headerTbl $ \it -> do
    tableIterSeek it $ RankedBlockHash (BlockHeight $ int $ _getMaxRank mar + 1) nullBlockHash
    tableIterPrev it
    inner $ iterToReverseValueStream it
        & S.map _getRankedBlockHeader
        & S.takeWhile (\a -> int (_blockHeight a) >= mir)
  where
    headerTbl = _chainDbCas db

{-# INLINE withReverseHeaderStream #-}
