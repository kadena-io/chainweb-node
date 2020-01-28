{-# LANGUAGE BangPatterns #-}

module Chainweb.WebPactExecutionService.Types
  ( WebPactExecutionService(..)
  , PactExecutionService(..)
  ) where

import Data.Tuple.Strict
import Data.Vector (Vector)

import Pact.Types.Command
import Pact.Types.Hash

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Mempool.Mempool (InsertError)
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Transaction

data PactExecutionService = PactExecutionService
    { _pactValidateBlock :: BlockHeader -> PayloadData -> IO PayloadWithOutputs
    , _pactNewBlock :: Miner -> BlockHeader -> BlockCreationTime -> IO PayloadWithOutputs
    , _pactLocal :: ChainwebTransaction -> IO (Either PactException (CommandResult Hash))
    , _pactLookup
        :: Rewind
            -- restore point. 'NoRewind' means we
            -- don't care about the restore point.
        -> Vector PactHash
            -- txs to lookup
        -> IO (Either PactException (Vector (Maybe (T2 BlockHeight BlockHash))))
    , _pactPreInsertCheck
        :: ChainId
        -> Vector ChainwebTransaction
        -> IO (Either PactException (Vector (Either InsertError ())))
    }

newtype WebPactExecutionService = WebPactExecutionService
    { _webPactExecutionService :: PactExecutionService
    }
