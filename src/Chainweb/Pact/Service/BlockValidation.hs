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
, continueBlock
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

import qualified Pact.Core.Persistence as Pact5

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Mempool.Mempool (InsertError)
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Types
import Chainweb.Payload
import qualified Chainweb.Pact4.Transaction as Pact4
import Chainweb.Utils
import Chainweb.Version
import Data.ByteString.Short (ShortByteString)
import qualified Pact.Core.Names as Pact5
import qualified Pact.Core.Builtin as Pact5
import qualified Pact.Core.Evaluate as Pact5
import qualified Pact.Types.ChainMeta as Pact4
import Data.Text (Text)
import qualified Pact.Types.Command as Pact4

newBlock :: Miner -> NewBlockFill -> ParentHeader -> PactQueue -> IO (Historical (ForSomePactVersion BlockInProgress))
newBlock mi fill parent reqQ = do
    let
        !msg = NewBlockMsg NewBlockReq
            { _newBlockMiner = mi
            , _newBlockFill = fill
            , _newBlockParent = parent
            }
    submitRequestAndWait reqQ msg

continueBlock :: BlockInProgress pv -> PactQueue -> IO (Historical (BlockInProgress pv))
continueBlock bip reqQ = do
    let !msg = ContinueBlockMsg (ContinueBlockReq bip)
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
    -> Pact4.UnparsedTransaction
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
    -> Vector ShortByteString
    -> PactQueue
    -> IO (HashMap ShortByteString (T2 BlockHeight BlockHash))
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
    :: Vector (Pact4.Command (Pact4.PayloadWithText Pact4.PublicMeta Text))
    -> PactQueue
    -> IO (Vector (Maybe InsertError))
pactPreInsertCheck txs reqQ = do
    let !req = PreInsertCheckReq txs
    let !msg = PreInsertCheckMsg req
    submitRequestAndWait reqQ msg

pactBlockTxHistory
  :: BlockHeader
  -> Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info
  -> PactQueue
  -> IO (Historical BlockTxHistory)
pactBlockTxHistory bh d reqQ = do
  let !req = BlockTxHistoryReq bh d
  let !msg = BlockTxHistoryMsg req
  submitRequestAndWait reqQ msg

pactHistoricalLookup
    :: BlockHeader
    -> Pact5.Domain Pact5.RowKey Pact5.RowData Pact5.CoreBuiltin Pact5.Info
    -> Pact5.RowKey
    -> PactQueue
    -> IO (Historical (Maybe (Pact5.TxLog Pact5.RowData)))
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
