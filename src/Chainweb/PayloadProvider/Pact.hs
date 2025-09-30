{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Chainweb.PayloadProvider.Pact
( PactPayloadProvider(..)
, withPactPayloadProvider
, pactMemPoolAccess
, decodeNewPayload
) where

import Chainweb.BlockHeaderDB (withBlockHeaderDb)
import Chainweb.ChainId
import Chainweb.Core.Brief
import Chainweb.Counter
import Chainweb.Logger
import Chainweb.MerkleUniverse
import Chainweb.MinerReward qualified as MinerReward
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Mempool.Mempool
import Chainweb.Pact.PactService qualified as PactService
import Chainweb.Pact.Payload
import Chainweb.Pact.Payload.PayloadStore
import Chainweb.Pact.Transaction qualified as Pact
import Chainweb.Pact.Types
import Chainweb.PayloadProvider
import Chainweb.PayloadProvider.Pact.BlockHistoryMigration
import Chainweb.Storage.Table.RocksDB
import Chainweb.Utils
import Chainweb.Version
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (ResourceT)
import Data.LogMessage
import Data.Vector (Vector)
import Data.Vector qualified as V
import Network.HTTP.Client qualified as HTTP
import System.LogLevel

data PactPayloadProvider logger tbl = PactPayloadProvider
    { pactPayloadProviderLogger :: logger
    , pactPayloadProviderServiceEnv :: ServiceEnv tbl
    }

makePrisms ''PactPayloadProvider

instance HasChainId (PactPayloadProvider logger tbl) where
    chainId = _PactPayloadProvider . _2 . chainId

instance (Logger logger, CanPayloadCas tbl) => PayloadProvider (PactPayloadProvider logger tbl) where
    prefetchPayloads _pp _hints _forkInfo = return ()

    syncToBlock (PactPayloadProvider logger e) = PactService.syncToFork logger e

    latestPayloadSTM (PactPayloadProvider _logger e) = do
        (_, bip) <- readTMVar (_psMiningPayloadVar e)
        let pwo = toPayloadWithOutputs
                (fromJuste $ _psMiner e)
                (_blockInProgressTransactions bip)
        return NewPayload
            { _newPayloadChainId = _chainId e
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

decodeNewPayload :: MonadThrow m => NewPayload -> m PayloadWithOutputs
decodeNewPayload NewPayload{..} = do
    pd <- decodePayloadData @_ @ChainwebMerkleHashAlgorithm $
        _encodedPayloadData $ fromJuste _newPayloadEncodedPayloadData
    bo <- decodeBlockOutputs @_ @ChainwebMerkleHashAlgorithm $
        _encodedPayloadOutputs $ fromJuste _newPayloadEncodedPayloadOutputs
    return $ payloadWithOutputs pd (_blockCoinbaseOutput bo) (_blockOutputs bo)

-- | Initialization for Pact (in process) Api
withPactPayloadProvider
    :: CanPayloadCas tbl
    => Logger logger
    => HasVersion
    => ChainId
    -> RocksDb
    -- ^ Temporary requirement for the `migrateBlockHistoryTable` step.
    -> Maybe HTTP.Manager
    -> logger
    -> Maybe (Counter "txFailures")
    -> MempoolBackend Pact.Transaction
    -> PayloadDb tbl
    -> FilePath
    -> PactServiceConfig
    -> Maybe PayloadWithOutputs
    -> ResourceT IO (PactPayloadProvider logger tbl)
withPactPayloadProvider cid rdb http logger txFailuresCounter mp pdb pactDbDir config maybeGenesisPayload = do
    readWriteSqlenv <- withSqliteDb cid logger pactDbDir False

    -- perform the database migration of the `BlockHeader` Table.
    bhdb <- withBlockHeaderDb rdb cid

    liftIO $ do
        needsMigration <- tableNeedsMigration logger readWriteSqlenv
        when needsMigration $
            -- We cleanup potential old state and start migrating the entire database
            -- from scratch.
            migrateBlockHistoryTable logger readWriteSqlenv bhdb True

    readOnlySqlPool <- withReadSqlitePool cid pactDbDir
    PactPayloadProvider logger <$>
        PactService.withPactService cid http mpa logger txFailuresCounter pdb readOnlySqlPool readWriteSqlenv config
        (maybe GenesisNotNeeded GenesisPayload maybeGenesisPayload)
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
    logFn theLogger Debug $! "pactMemPoolAccess - getting new block of transactions for parent "
        <> brief (_evaluationCtxRankedParentHash ctx)
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
