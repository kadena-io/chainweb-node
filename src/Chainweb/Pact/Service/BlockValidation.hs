{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
, pactReadOnlyReplay
) where


import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)

import Pact.Types.Hash
import Pact.Types.Persistence (RowKey, TxLog, Domain)
import Pact.Types.RowData (RowData)

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Mempool.Mempool (InsertError)
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Backend.Types(Historical)
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils (T2)


newBlock :: Miner -> PactQueue ->
            IO (T2 ParentHeader PayloadWithOutputs)
newBlock mi reqQ = do
    let !msg = NewBlockMsg NewBlockReq
          { _newMiner = mi }
    submitRequestAndWait reqQ msg

validateBlock
    :: BlockHeader
    -> CheckablePayload
    -> PactQueue
    -> IO PayloadWithOutputs
validateBlock bHeader payload reqQ = do
    let !msg = ValidateBlockMsg ValidateBlockReq
          { _valBlockHeader = bHeader
          , _valCheckablePayload = payload
          }
    submitRequestAndWait reqQ msg

local
    :: Maybe LocalPreflightSimulation
    -> Maybe LocalSignatureVerification
    -> Maybe RewindDepth
    -> ChainwebTransaction
    -> PactQueue
    -> IO LocalResult
local preflight sigVerify rd ct reqQ = do
    let !msg = LocalMsg LocalReq
          { _localRequest = ct
          , _localPreflight = preflight
          , _localSigVerification = sigVerify
          , _localRewindDepth = rd
          }
    submitRequestAndWait reqQ msg

lookupPactTxs
    :: Maybe ConfirmationDepth
    -> Vector PactHash
    -> PactQueue
    -> IO (HashMap PactHash (T2 BlockHeight BlockHash))
lookupPactTxs confDepth txs reqQ = do
    let !req = LookupPactTxsReq confDepth txs
    let !msg = LookupPactTxsMsg req
    submitRequestAndWait reqQ msg

pactReadOnlyReplay
    :: BlockHeader
    -> Maybe BlockHeader
    -> PactQueue
    -> IO ()
pactReadOnlyReplay l u reqQ = do
    let !msg = ReadOnlyReplayMsg ReadOnlyReplayReq
          { _readOnlyReplayLowerBound = l
          , _readOnlyReplayUpperBound = u
          }
    submitRequestAndWait reqQ msg

pactPreInsertCheck
    :: Vector ChainwebTransaction
    -> PactQueue
    -> IO (Vector (Either InsertError ()))
pactPreInsertCheck txs reqQ = do
    let !req = PreInsertCheckReq txs
    let !msg = PreInsertCheckMsg req
    submitRequestAndWait reqQ msg

pactBlockTxHistory
  :: BlockHeader
  -> Domain RowKey RowData
  -> PactQueue
  -> IO (Historical BlockTxHistory)
pactBlockTxHistory bh d reqQ = do
  let !req = BlockTxHistoryReq bh d
  let !msg = BlockTxHistoryMsg req
  submitRequestAndWait reqQ msg

pactHistoricalLookup
    :: BlockHeader
    -> Domain RowKey RowData
    -> RowKey
    -> PactQueue
    -> IO (Historical (Maybe (TxLog RowData)))
pactHistoricalLookup bh d k reqQ = do
  let !req = HistoricalLookupReq bh d k
  let !msg = HistoricalLookupMsg req
  submitRequestAndWait reqQ msg

pactSyncToBlock
    :: BlockHeader
    -> PactQueue
    -> IO ()
pactSyncToBlock bh reqQ = do
    let !msg = SyncToBlockMsg SyncToBlockReq
          { _syncToBlockHeader = bh
          }
    submitRequestAndWait reqQ msg
