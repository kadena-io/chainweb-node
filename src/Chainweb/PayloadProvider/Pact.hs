{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chainweb.PayloadProvider.Pact
    ( PactPayloadProvider(..)
    , withPactPayloadProvider
    , pactMemPoolAccess
    ) where

import Control.Concurrent.STM
import Data.LogMessage
import Data.Vector (Vector)
import System.LogLevel
import Control.Lens
import qualified Network.HTTP.Client as HTTP
import qualified Data.Vector as V

import Chainweb.ChainId
import Chainweb.Counter
import Chainweb.Logger
import Chainweb.Mempool.Mempool
import qualified Chainweb.MinerReward as MinerReward
import Chainweb.Pact.Backend.Utils
import qualified Chainweb.Pact.PactService as PactService
import qualified Chainweb.Pact.Transaction as Pact
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.PayloadProvider
import Chainweb.Utils
import Chainweb.Version
import qualified Data.Pool as Pool
import Control.Monad.Trans.Resource (ResourceT, allocate)

data PactPayloadProvider logger tbl = PactPayloadProvider logger (ServiceEnv tbl)

makePrisms ''PactPayloadProvider

instance HasChainId (PactPayloadProvider logger tbl) where
    chainId = _PactPayloadProvider . _2 . chainId

instance HasChainwebVersion (PactPayloadProvider logger tbl) where
    chainwebVersion = _PactPayloadProvider . _2 . chainwebVersion

instance (Logger logger, CanPayloadCas tbl) => PayloadProvider (PactPayloadProvider logger tbl) where
    prefetchPayloads :: Logger logger => PactPayloadProvider logger tbl -> Maybe Hints -> ForkInfo -> IO ()
    prefetchPayloads _pp _hints _forkInfo = return ()

    syncToBlock :: Logger logger => PactPayloadProvider logger tbl -> Maybe Hints -> ForkInfo -> IO ConsensusState
    syncToBlock (PactPayloadProvider logger e) hints forkInfo =
        PactService.syncToFork logger e hints forkInfo

    latestPayloadSTM :: Logger logger => PactPayloadProvider logger tbl -> STM NewPayload
    latestPayloadSTM (PactPayloadProvider _logger e) = do
        (_, bip) <- readTMVar (_psMiningPayloadVar e)
        let pwo = toPayloadWithOutputs
                (fromJuste $ _psMiner e)
                (_blockInProgressTransactions bip)
        return NewPayload
            { _newPayloadChainwebVersion = _chainwebVersion e
            , _newPayloadChainId = _chainId e
            , _newPayloadParentHeight = _bctxParentHeight $ _blockInProgressBlockCtx bip
            , _newPayloadParentHash = _bctxParentHash $ _blockInProgressBlockCtx bip
            , _newPayloadBlockPayloadHash = _payloadWithOutputsPayloadHash pwo
            , _newPayloadEncodedPayloadData = Just $ EncodedPayloadData $ encodePayloadData $ payloadWithOutputsToPayloadData pwo
            -- this doesn't make it anywhere in storage, right?
            , _newPayloadEncodedPayloadOutputs = Just $ EncodedPayloadOutputs $ encodeBlockOutputs $
                    BlockOutputs (_payloadWithOutputsOutputsHash pwo) (snd <$> _payloadWithOutputsTransactions pwo) (_payloadWithOutputsCoinbase pwo)
            , _newPayloadNumber = _blockInProgressNumber bip
            -- Informative:
            , _newPayloadTxCount = int $ V.length $ _payloadWithOutputsTransactions pwo
                -- ^ The number of user transactions in the block. The exact way how
                -- transactions are counted is provider specific.
            , _newPayloadSize = 0
                -- ^ what do we do here? why do we care?
            , _newPayloadOutputSize = 0
                -- ^ what do we do here? why do we care?
            , _newPayloadFees = MinerReward.Stu 0
                -- I suppose this is the sum of the gas in the command results?
            }

    eventProof :: Logger logger => PactPayloadProvider logger tbl -> XEventId -> IO SpvProof
    eventProof = error "not figured out yet"

-- | Initialization for Pact (in process) Api
withPactPayloadProvider
    :: CanReadablePayloadCas tbl
    => Logger logger
    => ChainwebVersion
    -> ChainId
    -> Maybe HTTP.Manager
    -> logger
    -> Maybe (Counter "txFailures")
    -> MempoolBackend Pact.Transaction
    -> PayloadDb tbl
    -> FilePath
    -> PactServiceConfig
    -> ResourceT IO (PactPayloadProvider logger tbl)
withPactPayloadProvider ver cid http logger txFailuresCounter mp pdb pactDbDir config = do
    readWriteSqlenv <- withSqliteDb cid logger pactDbDir False
    (_, readOnlySqlPool) <- allocate
        (Pool.newPool $ Pool.defaultPoolConfig
            (startReadSqliteDb cid logger pactDbDir)
            stopSqliteDb
            10 -- seconds to keep them around unused
            2 -- connections at most
            & Pool.setNumStripes (Just 2) -- two stripes, one connection per stripe
        )
        Pool.destroyAllResources
    PactPayloadProvider logger <$> PactService.withPactService ver cid http mpa logger txFailuresCounter pdb readOnlySqlPool readWriteSqlenv config
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
            -> EvaluationCtx ()
            -> IO (Vector to))
pactMemPoolGetBlock mp theLogger bf validate ctx = do
    logFn theLogger Debug $! "pactMemPoolAccess - getting new block of transactions for "
        <> "height = " <> sshow (_evaluationCtxCurrentHeight ctx) <> ", hash = " <> sshow (_evaluationCtxParentHash ctx)
    mempoolGetBlock mp bf validate ctx
    where
    logFn :: Logger l => l -> LogFunctionText -- just for giving GHC some type hints
    logFn l = logFunction l

pactProcessFork
    :: Logger logger
    => MempoolBackend Pact.Transaction
    -> logger
    -> ((Vector Pact.Transaction, Vector Pact.Transaction) -> IO ())
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
