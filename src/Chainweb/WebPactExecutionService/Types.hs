{-# LANGUAGE BangPatterns #-}

module Chainweb.WebPactExecutionService.Types
  ( WebPactExecutionService(..)
  , PactExecutionService(..)
  ) where

import Data.Tuple.Strict
import Data.Vector (Vector)
import qualified Pact.Types.Hash as P

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Transaction

data PactExecutionService = PactExecutionService
    { _pactValidateBlock :: BlockHeader -> PayloadData -> IO PayloadWithOutputs
    , _pactNewBlock :: Miner -> BlockHeader -> BlockCreationTime -> IO PayloadWithOutputs
    , _pactLocal :: ChainwebTransaction -> IO (Either PactException HashCommandResult)
    , _pactLookup
        :: Rewind
            -- restore point. 'NoRewind' means we
            -- don't care about the restore point.
        -> Vector P.PactHash
            -- txs to lookup
        -> IO (Either PactException (Vector (Maybe (T2 BlockHeight BlockHash))))
    }

newtype WebPactExecutionService = WebPactExecutionService
    { _webPactExecutionService :: PactExecutionService
    }
