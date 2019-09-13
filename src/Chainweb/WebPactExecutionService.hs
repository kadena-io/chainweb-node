{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Chainweb.WebPactExecutionService
( -- * Data
  WebPactExecutionService(..)
, PactExecutionService(..)
  -- * Combinators
, webPactNewBlock
, webPactValidateBlock
, mkWebPactExecutionService
, mkPactExecutionService
, emptyPactExecutionService
) where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TQueue
import Control.Exception (evaluate)
import Control.Monad.Catch

import qualified Data.HashMap.Strict as HM

-- internal chainweb modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.WebPactExecutionService.Types



webPactNewBlock :: WebPactExecutionService -> Miner -> BlockHeader -> IO PayloadWithOutputs
webPactNewBlock = _pactNewBlock . _webPactExecutionService
{-# INLINE webPactNewBlock #-}

webPactValidateBlock :: WebPactExecutionService -> BlockHeader -> PayloadData -> IO PayloadWithOutputs
webPactValidateBlock = _pactValidateBlock . _webPactExecutionService
{-# INLINE webPactValidateBlock #-}

mkWebPactExecutionService :: HM.HashMap ChainId PactExecutionService -> WebPactExecutionService
mkWebPactExecutionService hm = WebPactExecutionService $ PactExecutionService
  { _pactValidateBlock = \bh pd -> withChainService hm bh $ \p -> _pactValidateBlock p bh pd
  , _pactNewBlock = \m bh -> withChainService hm bh $ \p -> _pactNewBlock p m bh
  , _pactLocal = \_ct -> throwM $ userError "No web-level local execution supported"
  , _pactSpv = \ph bh -> withChainService hm bh $ \p -> _pactSpv p ph bh
  }

withChainService
    :: forall a
    . HM.HashMap ChainId PactExecutionService
    -> BlockHeader
    -> (PactExecutionService -> IO a)
    -> IO a
withChainService hm bh act = case HM.lookup (_chainId bh) hm of
    Just p -> act p
    Nothing -> throwM
      . userError
      $ "PactExecutionService: Invalid chain ID in header: "
      ++ show bh

mkPactExecutionService
    :: TQueue RequestMsg
    -> PactExecutionService
mkPactExecutionService q = PactExecutionService
  { _pactValidateBlock = \h pd -> do
      mv <- validateBlock h pd q
      r <- takeMVar mv
      case r of
        Right !pdo -> return pdo
        Left e -> throwM e
  , _pactNewBlock = \m h -> do
      mv <- newBlock m h q
      r <- takeMVar mv
      either throwM evaluate r
  , _pactLocal = \ct -> do
      mv <- local ct q
      takeMVar mv
  , _pactSpv = \ph bh -> do
      mv <- spvReq ph bh q
      r <- takeMVar mv
      either throwM evaluate r
  }

-- | A mock execution service for testing scenarios. Throws out anything it's
-- given.
--
emptyPactExecutionService :: PactExecutionService
emptyPactExecutionService = PactExecutionService
    { _pactValidateBlock = \_ _ -> pure emptyPayload
    , _pactNewBlock = \_ _ -> pure emptyPayload
    , _pactLocal = \_ -> throwM $ userError "emptyPactExecutionService: attempted `local` call"
    , _pactSpv = \_ _ -> throwM $ userError "emptyPactExecutionService: attempted `spv` call"
    }
