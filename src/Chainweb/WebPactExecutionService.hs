
module Chainweb.WebPactExecutionService
  ( WebPactExecutionService(..)
  , PactExecutionService(..)
  , mkWebPactExecutionService
  , mkPactExecutionService
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TQueue
import Control.Monad.Catch
import qualified Data.HashMap.Strict as HM

import Chainweb.BlockHeader
import Chainweb.Payload
import Chainweb.ChainId
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Service.PactInProcApi

data PactExecutionService = PactExecutionService
  { _pactValidateBlock :: BlockHeader -> PayloadData -> IO PayloadWithOutputs
  , _pactNewBlock :: BlockHeader -> IO PayloadWithOutputs
  }

newtype WebPactExecutionService = WebPactExecutionService
  { _webPactExecutionService :: PactExecutionService
  }


mkWebPactExecutionService :: HM.HashMap ChainId PactExecutionService -> WebPactExecutionService
mkWebPactExecutionService hm = WebPactExecutionService $ PactExecutionService
  { _pactValidateBlock = \h pd -> withChainService h $ \p -> _pactValidateBlock p h pd
  , _pactNewBlock = \h -> withChainService h $ \p -> _pactNewBlock p h
  }
  where withChainService h act = case HM.lookup (_chainId h) hm of
          Just p -> act p
          Nothing -> throwM (userError $ "PactExecutionService: Invalid chain ID in header: " ++ show h)

mkPactExecutionService :: TQueue RequestMsg -> PactExecutionService
mkPactExecutionService q = PactExecutionService
  { _pactValidateBlock = \h pd -> do
      mv <- validateBlock h pd q
      r <- takeMVar mv
      case r of
        Right pdo -> return pdo
        Left e -> throwM e
  , _pactNewBlock = \h -> do
      mv <- newBlock h q
      r <- takeMVar mv
      case r of
        Right pdo -> return pdo
        Left e -> throwM e
  }
