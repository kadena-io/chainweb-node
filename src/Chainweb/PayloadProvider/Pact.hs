{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Chainweb.PayloadProvider.Pact
    ( PactPayloadProvider(..)
    ) where
import Chainweb.Version
import Control.Concurrent.STM
import Chainweb.PayloadProvider
import Chainweb.PayloadProvider.P2P
import qualified Chainweb.Payload.PayloadStore as PDB
import Chainweb.Storage.Table.RocksDB
import Chainweb.Storage.Table.Map
import Chainweb.BlockPayloadHash
import Chainweb.Pact.Backend.Types
import Data.LogMessage
import Chainweb.Time
import Chainweb.Pact.Types
import Data.Text (Text)
import Chainweb.Counter
import Chainweb.Miner.Pact
import Chainweb.Logger

data Payload

data PactPayloadProvider logger = PactPayloadProvider
    { _pactChainwebVersion :: !ChainwebVersion
    , _pactChainId :: !ChainId
    , _pactPayloadVar :: !(TMVar NewPayload)
    , _pactPayloadStore :: !(PDB.PayloadDb RocksDbTable)
    , _pactCandidatePayloads :: !(MapTable RankedBlockPayloadHash Payload)
    , _pactCheckpointer :: !(Checkpointer logger)
    , _pactLogger :: !LogFunction
    , _pactPreInsertCheckTimeout :: !Micros
    -- ^ Maximum allowed execution time for the transactions validation.
    , _pactReorgLimit :: !RewindLimit
    -- ^ The limit of checkpointer's rewind in the `execValidationBlock` command.
    , _pactGasLogger :: !(Maybe logger)
    , _pactTxFailuresCounter :: !(Maybe (Counter "txFailures"))
    , _pactTxTimeLimit :: !(Maybe Micros)
    , _pactMiner :: !(Maybe Miner)
    }

instance HasChainId (PactPayloadProvider logger) where
    _chainId = _pactChainId
instance HasChainwebVersion (PactPayloadProvider logger) where
    _chainwebVersion = _pactChainwebVersion

instance Logger logger => PayloadProvider (PactPayloadProvider logger) where
    prefetchPayloads :: Logger logger => PactPayloadProvider logger -> Maybe Hints -> ForkInfo -> IO ()
    prefetchPayloads pp hints forkInfo = undefined
    syncToBlock :: Logger logger => PactPayloadProvider logger -> Maybe Hints -> ForkInfo -> IO ConsensusState
    syncToBlock pp hints forkInfo = undefined
    latestPayloadSTM :: Logger logger => PactPayloadProvider logger -> STM NewPayload
    latestPayloadSTM = readTMVar . _pactPayloadVar
    eventProof :: Logger logger => PactPayloadProvider logger -> XEventId -> IO SpvProof
    eventProof = error "not figured out yet"
