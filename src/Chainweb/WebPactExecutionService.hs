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

import Chainweb.BlockHash
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
  { _pactValidateBlock = \h pd -> withChainService h $ \p -> _pactValidateBlock p h pd
  , _pactNewBlock = \m h ct -> withChainService h $ \p -> _pactNewBlock p m h ct
  , _pactLocal = \_ct -> throwM $ userError "No web-level local execution supported"
  , _pactLookup = \eh txs ->
        case eh of
          Left cid -> withChainServiceForId cid $ \p -> _pactLookup p eh txs
          Right h -> withChainService h $ \p -> _pactLookup p eh txs
  }
  where
    withChainServiceForId cid act = case HM.lookup cid hm of
        Just p -> act p
        Nothing ->
            let msg = "PactExecutionService: Invalid chain ID: " ++ show cid
            in throwM $ userError msg
    withChainService h = withChainServiceForId (_chainId h)

mkPactExecutionService
    :: PactQueue
    -> PactExecutionService
mkPactExecutionService q = PactExecutionService
  { _pactValidateBlock = \h pd -> do
      mv <- validateBlock h pd q
      r <- takeMVar mv
      case r of
        (Right !pdo) -> return pdo
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

blockHeaderToRestorePoint :: Either ChainId BlockHeader -> Maybe (T2 BlockHeight BlockHash)
blockHeaderToRestorePoint e =
    case e of
        Left _ -> Nothing
        Right bh -> let !height = _blockHeight bh
                        !hash = _blockHash bh
                    in Just $! T2 height hash

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
