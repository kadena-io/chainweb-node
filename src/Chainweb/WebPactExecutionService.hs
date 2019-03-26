{-# LANGUAGE BangPatterns #-}

module Chainweb.WebPactExecutionService
  ( WebPactExecutionService(..)
  , PactExecutionService(..)
  , mkWebPactExecutionService
  , mkPactExecutionService
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TQueue
import Control.Monad.Catch
import qualified Data.Either as Either
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils (codecDecode)

data PactExecutionService = PactExecutionService
  { _pactValidateBlock :: BlockHeader -> PayloadData -> IO PayloadWithOutputs
  , _pactNewBlock :: MinerInfo -> BlockHeader -> IO PayloadWithOutputs
  }

newtype WebPactExecutionService = WebPactExecutionService
  { _webPactExecutionService :: PactExecutionService
  }


mkWebPactExecutionService :: HM.HashMap ChainId PactExecutionService -> WebPactExecutionService
mkWebPactExecutionService hm = WebPactExecutionService $ PactExecutionService
  { _pactValidateBlock = \h pd -> withChainService h $ \p -> _pactValidateBlock p h pd
  , _pactNewBlock = \m h -> withChainService h $ \p -> _pactNewBlock p m h
  }
  where withChainService h act = case HM.lookup (_chainId h) hm of
          Just p -> act p
          Nothing -> throwM (userError $ "PactExecutionService: Invalid chain ID in header: " ++ show h)

mkPactExecutionService :: MempoolBackend ChainwebTransaction -> TQueue RequestMsg -> PactExecutionService
mkPactExecutionService mempool q = PactExecutionService
  { _pactValidateBlock = \h pd -> do
      mv <- validateBlock h pd q
      r <- takeMVar mv
      case r of
        Right pdo -> markAllConfirmed mempool pdo >> return pdo
        Left e -> throwM e
  , _pactNewBlock = \m h -> do
      mv <- newBlock m h q
      r <- takeMVar mv
      case r of
        Right pdo -> return pdo
        Left e -> throwM e
  }

-- TODO: to support mempool transaction reintroduction we need to hook into
-- consensus instead of just killing every tx that ever made it into a valid
-- block
markAllConfirmed
    :: MempoolBackend ChainwebTransaction
    -> PayloadWithOutputs
    -> IO ()
markAllConfirmed mempool payload = mempoolMarkConfirmed mempool txHashes
  where
    txcfg = mempoolTxConfig mempool
    decodeTx = codecDecode $ txCodec txcfg
    decodedTxs = Either.rights $ fmap (decodeTx . _transactionBytes . fst)
                   $ toList $ _payloadWithOutputsTransactions payload
    !txHashes = V.fromList $ map (txHasher txcfg) decodedTxs

