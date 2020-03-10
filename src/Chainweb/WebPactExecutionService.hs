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
import qualified Data.Vector as V

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (emptyPayload)
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Mempool.Mempool (InsertError)
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Transaction

import Data.Tuple.Strict
import Data.Vector (Vector)

import Pact.Types.Command
import Pact.Types.Hash

-- -------------------------------------------------------------------------- --
-- PactExecutionService

data PactExecutionService = PactExecutionService
    { _pactValidateBlock :: BlockHeader -> PayloadData -> IO PayloadWithOutputs
    , _pactNewBlock :: Miner -> ParentHeader -> IO PayloadWithOutputs
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

_webPactNewBlock
    :: WebPactExecutionService
    -> Miner
    -> ParentHeader
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
    , _pactNewBlock = \m h -> withChainService (_chainId h) $ \p -> _pactNewBlock p m h
    , _pactLocal = \_ct -> throwM $ userError "No web-level local execution supported"
    , _pactLookup = \h txs -> withChainService (_chainId h) $ \p -> _pactLookup p h txs
    , _pactPreInsertCheck = \cid txs -> withChainService cid $ \p -> _pactPreInsertCheck p cid txs
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
          Right (!pdo) -> return pdo
          Left e -> throwM e
    , _pactNewBlock = \m h -> do
        mv <- newBlock m h q
        r <- takeMVar mv
        either throwM evaluate r
    , _pactLocal = \ct -> do
        mv <- local ct q
        takeMVar mv
    , _pactLookup = \h txs -> do
        mv <- lookupPactTxs h txs q
        takeMVar mv
    , _pactPreInsertCheck = \_ txs ->
        pactPreInsertCheck txs q >>= takeMVar
    }

-- | A mock execution service for testing scenarios. Throws out anything it's
-- given.
--
emptyPactExecutionService :: PactExecutionService
emptyPactExecutionService = PactExecutionService
    { _pactValidateBlock = \_ _ -> pure emptyPayload
    , _pactNewBlock = \_ _ -> pure emptyPayload
    , _pactLocal = \_ -> throwM (userError "emptyPactExecutionService: attempted `local` call")
    , _pactLookup = \_ v -> return $! Right $! V.map (const Nothing) v
    , _pactPreInsertCheck = \_ txs -> return $ Right $ V.map (const (Right ())) txs
    }
