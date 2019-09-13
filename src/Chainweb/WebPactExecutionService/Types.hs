{-# LANGUAGE BangPatterns #-}

module Chainweb.WebPactExecutionService.Types
  ( WebPactExecutionService(..)
  , PactExecutionService(..)
  ) where

import Chainweb.BlockHeader
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Transaction

import Pact.Types.Hash


data PactExecutionService = PactExecutionService
  { _pactValidateBlock :: BlockHeader -> PayloadData -> IO PayloadWithOutputs
  , _pactNewBlock :: Miner -> BlockHeader -> IO PayloadWithOutputs
  , _pactLocal :: ChainwebTransaction -> IO (Either PactException HashCommandResult)
  , _pactSpv :: PactHash -> BlockHeader -> IO Base64TxOutputProof
  }

newtype WebPactExecutionService = WebPactExecutionService
  { _webPactExecutionService :: PactExecutionService
  }
