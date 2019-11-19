{-# LANGUAGE BangPatterns #-}

module Chainweb.WebPactExecutionService
  ( WebPactExecutionService(..)
  , _webPactNewBlock
  , _webPactValidateBlock
  , PactExecutionService(..)
  , mkWebPactExecutionService
  , mkPactExecutionService
  , emptyPactExecutionService
  ) where

import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad.Catch
import qualified Data.HashMap.Strict as HM
import Data.Tuple.Strict
import qualified Data.Vector as V

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.WebPactExecutionService.Types

_webPactNewBlock
    :: WebPactExecutionService
    -> Miner
    -> BlockHeader
    -> BlockCreationTime
    -> IO PayloadWithOutputs
_webPactNewBlock = _pactNewBlock . _webPactExecutionService
{-# INLINE _webPactNewBlock #-}

_webPactValidateBlock
    :: WebPactExecutionService
    -> BlockHeader
    -> PayloadData
    -> IO PayloadWithOutputs
_webPactValidateBlock = _pactValidateBlock . _webPactExecutionService
{-# INLINE _webPactValidateBlock #-}

mkWebPactExecutionService
    :: HM.HashMap ChainId PactExecutionService
    -> WebPactExecutionService
mkWebPactExecutionService hm = WebPactExecutionService $ PactExecutionService
    { _pactValidateBlock = \h pd -> withChainService (_chainId h) $ \p -> _pactValidateBlock p h pd
    , _pactNewBlock = \m h ct -> withChainService (_chainId h) $ \p -> _pactNewBlock p m h ct
    , _pactLocal = \_ct -> throwM $ userError "No web-level local execution supported"
    , _pactLookup = \h txs -> withChainService (_chainId h) $ \p -> _pactLookup p h txs
    }
  where
    withChainService cid act =  maybe (err cid) act $ HM.lookup cid hm
    err cid = throwM $ userError
      $ "PactExecutionService: Invalid chain ID: "
      ++ show cid


mkPactExecutionService
    :: PactQueue
    -> PactExecutionService
mkPactExecutionService q = PactExecutionService
    { _pactValidateBlock = \h pd -> do
        mv <- validateBlock h pd q
        r <- takeMVar mv
        case r of
          Right !pdo -> return pdo
          Left e -> throwM e
    , _pactNewBlock = \m h ct -> do
        mv <- newBlock m h ct q
        r <- takeMVar mv
        either throwM evaluate r
    , _pactLocal = \ct -> do
        mv <- local ct q
        takeMVar mv
    , _pactLookup = \h txs -> do
        mv <- lookupPactTxs (blockHeaderToRestorePoint h) txs q
        takeMVar mv
    }
  where
    blockHeaderToRestorePoint =
      fmap (\(!bh) -> T2 (_blockHeight bh) (_blockHash bh))

-- | A mock execution service for testing scenarios. Throws out anything it's
-- given.
--
emptyPactExecutionService :: PactExecutionService
emptyPactExecutionService = PactExecutionService
    { _pactValidateBlock = \_ _ -> pure emptyPayload
    , _pactNewBlock = \_ _ _ -> pure emptyPayload
    , _pactLocal = \_ -> throwM (userError $ "emptyPactExecutionService: attempted `local` call")
    , _pactLookup = \_ v -> return $! Right $! V.map (const Nothing) v
    }
