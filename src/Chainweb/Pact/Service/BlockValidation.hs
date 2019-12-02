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
) where


import Control.Concurrent.MVar.Strict

import Data.Tuple.Strict
import Data.Vector (Vector)

import Pact.Types.Command
import Pact.Types.Hash

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Mempool.Mempool (InsertError)
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Transaction


newBlock :: Miner -> BlockHeader -> BlockCreationTime -> PactQueue ->
            IO (MVar (Either PactException PayloadWithOutputs))
newBlock mi bHeader creationTime reqQ = do
    !resultVar <- newEmptyMVar :: IO (MVar (Either PactException PayloadWithOutputs))
    let !msg = NewBlockMsg NewBlockReq
          { _newBlockHeader = bHeader
          , _newMiner = mi
          , _newCreationTime = creationTime
          , _newResultVar = resultVar }
    addRequest reqQ msg
    return resultVar

validateBlock
    :: BlockHeader
    -> PayloadData
    -> PactQueue
    -> IO (MVar (Either PactException PayloadWithOutputs))
validateBlock bHeader plData reqQ = do
    !resultVar <- newEmptyMVar :: IO (MVar (Either PactException PayloadWithOutputs))
    let !msg = ValidateBlockMsg ValidateBlockReq
          { _valBlockHeader = bHeader
          , _valResultVar = resultVar
          , _valPayloadData = plData }
    addRequest reqQ msg
    return resultVar

local :: ChainwebTransaction -> PactQueue -> IO (MVar (Either PactException (CommandResult Hash)))
local ct reqQ = do
    !resultVar <- newEmptyMVar
    let !msg = LocalMsg LocalReq
          { _localRequest = ct
          , _localResultVar = resultVar }
    addRequest reqQ msg
    return resultVar

lookupPactTxs
    :: Rewind
    -> Vector PactHash
    -> PactQueue
    -> IO (MVar (Either PactException (Vector (Maybe (T2 BlockHeight BlockHash)))))
lookupPactTxs restorePoint txs reqQ = do
    resultVar <- newEmptyMVar
    let !req = LookupPactTxsReq restorePoint txs resultVar
    let !msg = LookupPactTxsMsg req
    addRequest reqQ msg
    return resultVar

pactPreInsertCheck
    :: Vector ChainwebTransaction
    -> PactQueue
    -> IO (MVar (Either PactException (Vector (Either InsertError ()))))
pactPreInsertCheck txs reqQ = do
    resultVar <- newEmptyMVar
    let !req = PreInsertCheckReq txs resultVar
    let !msg = PreInsertCheckMsg req
    addRequest reqQ msg
    return resultVar
