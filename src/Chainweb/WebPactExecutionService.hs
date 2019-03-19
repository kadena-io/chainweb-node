
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
import Chainweb.MerkleLogHash (nullHashBytes)

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
  { _pactValidateBlock = \h _pd -> do
      mv <- validateBlock h q
      tempBBToPWO <$> takeMVar mv
  , _pactNewBlock = \h -> do
      mv <- newBlock h q
      tempBBToPWO <$> takeMVar mv
  }

tempBBToPWO :: (BlockTransactions, a) -> PayloadWithOutputs
tempBBToPWO _ = PayloadWithOutputs mempty (BlockPayloadHash nullHashBytes) (BlockTransactionsHash nullHashBytes) (BlockOutputsHash nullHashBytes)
