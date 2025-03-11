{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Chainweb.PayloadProvider.Pact
    ( PactPayloadProvider(..)
    ) where

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeight
import Chainweb.BlockPayloadHash
import Chainweb.ChainId
import Chainweb.Counter
import Chainweb.Counter (Counter)
import Chainweb.Logger
import Chainweb.Mempool.Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import qualified Chainweb.Pact.PactService as PS
import qualified Chainweb.Pact.Transaction as Pact
import Chainweb.Pact.Types
import Chainweb.Payload.PayloadStore
import qualified Chainweb.Payload.PayloadStore as PDB
import Chainweb.PayloadProvider
import Chainweb.PayloadProvider.P2P
import Chainweb.Storage.Table.Map
import Chainweb.Storage.Table.RocksDB
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.IORef
import Data.LogMessage
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Stack (HasCallStack)
import System.LogLevel
import Chainweb.Payload (PayloadData)
import Data.Coerce
import Control.Lens
import qualified Network.HTTP.Client as HTTP

newtype PactPayloadProvider logger tbl = PactPayloadProvider (PactServiceEnv logger tbl)
makePrisms ''PactPayloadProvider

instance (Logger logger, CanPayloadCas tbl) => PayloadProvider (PactPayloadProvider logger tbl) where
    prefetchPayloads :: Logger logger => PactPayloadProvider logger tbl -> Maybe Hints -> ForkInfo -> IO ()
    prefetchPayloads pp hints forkInfo = undefined
    syncToBlock :: Logger logger => PactPayloadProvider logger tbl -> Maybe Hints -> ForkInfo -> IO ConsensusState
    syncToBlock pp hints forkInfo = undefined
    latestPayloadSTM :: Logger logger => PactPayloadProvider logger tbl -> STM NewPayload
    latestPayloadSTM = readTMVar . _psMiningPayloadVar . view _PactPayloadProvider
    eventProof :: Logger logger => PactPayloadProvider logger tbl -> XEventId -> IO SpvProof
    eventProof = error "not figured out yet"

-- | Initialization for Pact (in process) Api
withPactPayloadProvider
    :: CanReadablePayloadCas tbl
    => Logger logger
    => HTTP.Manager
    -> ChainwebVersion
    -> ChainId
    -> logger
    -> Maybe (Counter "txFailures")
    -> MempoolBackend Pact.Transaction
    -> PayloadDb tbl
    -> FilePath
    -> PactServiceConfig
    -> (PactPayloadProvider logger tbl -> IO a)
    -> IO a
withPactPayloadProvider http ver cid logger txFailuresCounter mp pdb pactDbDir config action =
    withSqliteDb cid logger pactDbDir False $ \sqlenv ->
        PS.withPactService http ver cid mp logger txFailuresCounter mpa pdb sqlenv config action
    where
    mpa = pactMemPoolAccess mp $ addLabel ("sub-component", "MempoolAccess") logger

pactMemPoolAccess
    :: Logger logger
    => MempoolBackend Pact.Transaction
    -> logger
    -> MemPoolAccess
pactMemPoolAccess mp logger = MemPoolAccess
    { mpaGetBlock = pactMemPoolGetBlock mp logger
    , mpaProcessFork = pactProcessFork mp logger
    , mpaBadlistTx = mempoolAddToBadList mp
    }

pactMemPoolGetBlock
    :: Logger logger
    => MempoolBackend Pact.Transaction
    -> logger
    -> BlockFill
    -> (MempoolPreBlockCheck Pact.Transaction to
            -> BlockHeight
            -> BlockHash
            -> BlockCreationTime
            -> IO (Vector to))
pactMemPoolGetBlock mp theLogger bf validate height hash _btime = do
    logFn theLogger Debug $! "pactMemPoolAccess - getting new block of transactions for "
        <> "height = " <> sshow height <> ", hash = " <> sshow hash
    mempoolGetBlock mp bf validate height hash
    where
    logFn :: Logger l => l -> LogFunctionText -- just for giving GHC some type hints
    logFn l = logFunction l

pactProcessFork
    :: Logger logger
    => MempoolBackend Pact.Transaction
    -> logger
    -> ((Vector Pact.Transaction, Vector TransactionHash) -> IO ())
pactProcessFork mp theLogger (reintroTxs, validatedTxs) = do
    logFn theLogger Debug $!
        "pactMemPoolAccess - " <> sshow (length reintroTxs) <> " transactions to reintroduce"
    -- No need to run pre-insert check here -- we know these are ok, and
    -- calling the pre-check would block here (it calls back into pact service)
    mempoolInsert mp UncheckedInsert reintroTxs
    mempoolMarkValidated mp validatedTxs

    where
    logFn :: Logger l => l -> LogFunctionText
    logFn lg = logFunction lg
