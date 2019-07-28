{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
import Control.Concurrent.STM.TQueue
import Control.Monad.Catch
import qualified Data.Either as Either
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils (codecDecode)
import Chainweb.WebPactExecutionService.Types

import Data.LogMessage

import Utils.Logging.Trace

_webPactNewBlock :: WebPactExecutionService -> MinerInfo -> BlockHeader -> IO PayloadWithOutputs
_webPactNewBlock = _pactNewBlock . _webPactExecutionService
{-# INLINE _webPactNewBlock #-}

_webPactValidateBlock :: WebPactExecutionService -> BlockHeader -> PayloadData -> IO PayloadWithOutputs
_webPactValidateBlock = _pactValidateBlock . _webPactExecutionService
{-# INLINE _webPactValidateBlock #-}

mkWebPactExecutionService :: HM.HashMap ChainId PactExecutionService -> WebPactExecutionService
mkWebPactExecutionService hm = WebPactExecutionService $ PactExecutionService
  { _pactValidateBlock = \h pd -> withChainService h $ \p -> _pactValidateBlock p h pd
  , _pactNewBlock = \m h -> withChainService h $ \p -> _pactNewBlock p m h
  , _pactLocal = \_ct -> throwM $ userError "No web-level local execution supported"
  }
  where withChainService h act = case HM.lookup (_chainId h) hm of
          Just p -> act p
          Nothing -> throwM (userError $ "PactExecutionService: Invalid chain ID in header: " ++ show h)

mkPactExecutionService
    :: LogFunction
    -> MempoolBackend ChainwebTransaction
    -> TQueue RequestMsg
    -> PactExecutionService
mkPactExecutionService logfun mempool q = PactExecutionService
  { _pactValidateBlock = \h pd -> do
      let l = length (_payloadDataTransactions pd)
      mv <- trace logfun "mkPactExecutionService.validateBlock.submit" (_blockHash h) l
        $ validateBlock h pd q
      r <- trace logfun "mkPactExecutionService.validateBlock.result" (_blockHash h) l
        $ takeMVar mv
      case r of
        (Right !pdo) -> trace logfun "mkPactExecutionService.makeAllValidated" (_blockHash h) l $ do
          markAllValidated mempool pdo (_blockHeight h) (_blockHash h)
          return pdo
        Left e -> throwM e
  , _pactNewBlock = \m h -> do
      mv <- newBlock m h q
      r <- takeMVar mv
      case r of
        (Right !pdo) -> return pdo
        Left e -> throwM e
  , _pactLocal = \ct -> do
      mv <- local ct q
      takeMVar mv
  }

-- | A mock execution service for testing scenarios. Throws out anything it's
-- given.
--
emptyPactExecutionService :: PactExecutionService
emptyPactExecutionService = PactExecutionService
    { _pactValidateBlock = \_ _ -> pure emptyPayload
    , _pactNewBlock = \_ _ -> pure emptyPayload
    , _pactLocal = \_ -> throwM (userError $ "emptyPactExecutionService: attempted `local` call")
    }

markAllValidated
    :: MempoolBackend ChainwebTransaction
    -> PayloadWithOutputs
    -> BlockHeight
    -> BlockHash
    -> IO ()
markAllValidated mempool payload height hash = mempoolMarkValidated mempool validatedTxs
  where
    txcfg = mempoolTxConfig mempool
    decodeTx = codecDecode $ txCodec txcfg
    decodedTxs = Either.rights $ fmap (decodeTx . _transactionBytes . fst)
                   $ toList $ _payloadWithOutputsTransactions payload
    !validatedTxs = V.fromList $ map ( \t -> ValidatedTransaction
                                         { validatedHeight = height
                                         , validatedHash = hash
                                         , validatedTransaction = t }
                                     ) decodedTxs
