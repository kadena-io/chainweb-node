{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Chainweb.Pact.Service.BlockValidation
-- Copyright   :  Copyright © 2018 Kadena LLC.
-- License     :  (see the file LICENSE)
-- Maintainer  :  Emily Pillmore <emily@kadena.io>
-- Stability   :  experimental
--
-- The block validation logic for Pact Service
--
-- This exists due to moving things around resolving
-- chainweb dependencies. This should find a new home.
--
module Chainweb.Pact.Service.BlockValidation
( validateBlock
, newBlock
, local
, lookupPactTxs
, pactPreInsertCheck
, pactBlockTxHistory
, pactHistoricalLookup
, pactSyncToBlock
) where


import Control.Concurrent.MVar.Strict

import Data.Aeson (Value)
import Data.Tuple.Strict
import Data.Vector (Vector)

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Persistence (RowKey, TxLog)

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Mempool.Mempool (InsertError)
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Transaction


newBlock :: Miner -> ParentHeader -> PactQueueAccess ->
            IO (MVar (Either PactException PayloadWithOutputs))
newBlock mi bHeader pqa = do
    !resultVar <- newEmptyMVar :: IO (MVar (Either PactException PayloadWithOutputs))
    let !msg = NewBlockMsg NewBlockReq
          { _newBlockHeader = bHeader
          , _newMiner = mi
          , _newResultVar = resultVar }
    addRequest pqa msg
    return resultVar

validateBlock
    :: BlockHeader
    -> PayloadData
    -> PactQueueAccess
    -> IO (MVar (Either PactException PayloadWithOutputs))
validateBlock bHeader plData pqa = do
    !resultVar <- newEmptyMVar :: IO (MVar (Either PactException PayloadWithOutputs))
    let !msg = ValidateBlockMsg ValidateBlockReq
          { _valBlockHeader = bHeader
          , _valResultVar = resultVar
          , _valPayloadData = plData }
    addRequest pqa msg
    return resultVar

local :: ChainwebTransaction -> PactQueueAccess -> IO (MVar (Either PactException (CommandResult Hash)))
local ct pq = do
    !resultVar <- newEmptyMVar
    let !msg = LocalMsg LocalReq
          { _localRequest = ct
          , _localResultVar = resultVar }
    addRequest pq msg
    return resultVar

lookupPactTxs
    :: Rewind
    -> Vector PactHash
    -> PactQueueAccess
    -> IO (MVar (Either PactException (Vector (Maybe (T2 BlockHeight BlockHash)))))
lookupPactTxs restorePoint txs pq = do
    resultVar <- newEmptyMVar
    let !req = LookupPactTxsReq restorePoint txs resultVar
    let !msg = LookupPactTxsMsg req
    addRequest pq msg
    return resultVar

pactPreInsertCheck
    :: Vector ChainwebTransaction
    -> PactQueueAccess
    -> IO (MVar (Either PactException (Vector (Either InsertError ()))))
pactPreInsertCheck txs pq = do
    resultVar <- newEmptyMVar
    let !req = PreInsertCheckReq txs resultVar
    let !msg = PreInsertCheckMsg req
    addRequest pq msg
    return resultVar

pactBlockTxHistory
  :: BlockHeader
  -> Domain'
  -> PactQueueAccess
  -> IO (MVar (Either PactException BlockTxHistory))
pactBlockTxHistory bh d pqa = do
  resultVar <- newEmptyMVar
  let !req = BlockTxHistoryReq bh d resultVar
  let !msg = BlockTxHistoryMsg req
  addRequest pqa msg
  return resultVar

pactHistoricalLookup
    :: BlockHeader
    -> Domain'
    -> RowKey
    -> PactQueueAccess
    -> IO (MVar (Either PactException (Maybe (TxLog Value))))
pactHistoricalLookup bh d k pqa = do
  resultVar <- newEmptyMVar
  let !req = HistoricalLookupReq bh d k resultVar
  let !msg = HistoricalLookupMsg req
  addRequest pqa msg
  return resultVar

pactSyncToBlock
    :: BlockHeader
    -> PactQueueAccess
    -> IO (MVar (Either PactException ()))
pactSyncToBlock bh pqa = do
    !resultVar <- newEmptyMVar
    let !msg = SyncToBlockMsg SyncToBlockReq
          { _syncToBlockHeader = bh
          , _syncToResultVar = resultVar
          }
    addRequest pqa msg
    return resultVar
