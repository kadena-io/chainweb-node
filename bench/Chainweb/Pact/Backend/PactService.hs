{-# language
    BangPatterns
    , DataKinds
    , FlexibleContexts
    , ImpredicativeTypes
    , ImportQualifiedPost
    , LambdaCase
    , NumericUnderscores
    , OverloadedRecordDot
    , OverloadedStrings
    , PackageImports
    , ScopedTypeVariables
    , TypeApplications
    , TemplateHaskell
    , RecordWildCards
    , TupleSections
#-}

{-# options_ghc -fno-warn-gadt-mono-local-binds -fno-warn-orphans #-}

module Chainweb.Pact.Backend.PactService
    ( bench
    , eventProfile
    ) where

import Utils.Logging.Trace (withEvent)
import Control.DeepSeq
import Chainweb.Pact.Backend.Types (SQLiteEnv)
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb
--import Chainweb.Cut
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.InMem
import Chainweb.Mempool.Mempool (InsertType (..), MempoolBackend (..))
import Chainweb.Miner.Pact
import Chainweb.Pact.PactService
import Chainweb.Pact.PactService.Pact4.ExecBlock ()
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Types
import Chainweb.Pact4.Transaction qualified as Pact4
--import Chainweb.Pact5.Transaction qualified as Pact5
import Chainweb.Payload
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb (_bdbPayloadDb, _bdbWebBlockHeaderDb), addTestBlockDb, getParentTestBlockDb, mkTestBlockDb{-, getCutTestBlockDb, setCutTestBlockDb-})
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.Pact5.Utils hiding (withTempSQLiteResource, testRocksDb)
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)
import Chainweb.WebPactExecutionService
import Control.Concurrent hiding (throwTo)
import Control.Concurrent.Async (forConcurrently)
import Control.Exception (AsyncException (..))
import Control.Exception.Safe
import Control.Lens hiding (only)
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as LBS
import Data.Decimal
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Pact.Core.Capabilities
import Pact.Core.Gas.Types
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Types.Gas qualified as Pact4
import PropertyMatchers ((?))
import PropertyMatchers qualified as P
import Test.Tasty.HUnit (assertEqual)
import Text.Printf (printf)
import Chainweb.Pact.Backend.Utils (openSQLiteConnection, closeSQLiteConnection, chainwebPragmas)
import Criterion.Main qualified as C
import Database.SQLite3.Direct (Database (..))

eventProfile :: RocksDb -> IO ()
eventProfile rdb = do
    --oneBlockIO (instantCpmTestVersion singletonChainGraph) rdb 1
    oneBlockIO (pact5InstantCpmTestVersion singletonChainGraph) rdb 1

bench :: RocksDb -> C.Benchmark
bench rdb = do
    C.bgroup "PactService"
        [ C.bgroup "Pact4" (List.map (oneBlock (instantCpmTestVersion singletonChainGraph) rdb) [1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100])
        , C.bgroup "Pact5" (List.map (oneBlock (pact5InstantCpmTestVersion singletonChainGraph) rdb) [1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100])
        ]

data Fixture = Fixture
    { _chainwebVersion :: ChainwebVersion
    , _fixtureBlockDb :: TestBlockDb
    , _fixtureMempools :: ChainMap (MempoolBackend Pact4.UnparsedTransaction)
    , _fixturePactQueues :: ChainMap PactQueue
    , _fixturePactServiceThreads :: ChainMap ThreadId
    , _fixturePactServiceSqls :: ChainMap SQLiteEnv
    }

instance NFData (MempoolBackend a) where
    rnf !_ = ()

instance NFData PactQueue where
    rnf !_ = ()

instance NFData Database where
    rnf !_ = ()

instance NFData Fixture where
    rnf Fixture{..} =
        rnf _chainwebVersion
        `seq` rnf _fixtureBlockDb
        `seq` rnf _fixtureMempools
        `seq` rnf _fixturePactQueues
        `seq` rnf _fixturePactServiceThreads
        `seq` rnf _fixturePactServiceSqls

createFixture :: ChainwebVersion -> RocksDb -> PactServiceConfig -> IO Fixture
createFixture v rdb pactServiceConfig = do
    tdb <- mkTestBlockDb v =<< testRocksDb "fixture" rdb
    logger <- testLogger

    perChain <- iforM (HashSet.toMap (chainIds v)) $ \chain () -> do
        sql <- openSQLiteConnection "" chainwebPragmas
        bhdb <- liftIO $ getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) chain
        pactQueue <- liftIO $ newPactQueue 2_000
        pactExecutionServiceVar <- liftIO $ newMVar (mkPactExecutionService pactQueue)
        let mempoolCfg = validatingMempoolConfig chain v (Pact4.GasLimit 150_000) (Pact4.GasPrice 1e-8) pactExecutionServiceVar
        mempool <- liftIO $ startInMemoryMempoolTest mempoolCfg
        mempoolConsensus <- liftIO $ mkMempoolConsensus mempool bhdb (Just (_bdbPayloadDb tdb))
        let mempoolAccess = pactMemPoolAccess mempoolConsensus logger
        tid <- forkIO $ runPactService v chain logger Nothing pactQueue mempoolAccess bhdb (_bdbPayloadDb tdb) sql pactServiceConfig
        return (mempool, pactQueue, tid, sql)

    let fixture = Fixture
            { _chainwebVersion = v
            , _fixtureBlockDb = tdb
            , _fixtureMempools = OnChains $ view _1 <$> perChain
            , _fixturePactQueues = OnChains $ view _2 <$> perChain
            , _fixturePactServiceThreads = OnChains $ view _3 <$> perChain
            , _fixturePactServiceSqls = OnChains $ view _4 <$> perChain
            }
    -- The mempool expires txs based on current time, but newBlock expires txs based on parent creation time.
    -- So by running an empty block with the creationTime set to the current time, we get these goals to align
    -- for future blocks we run.
    _ <- liftIO $ advanceAllChains fixture $ onChains []

    return fixture

destroyFixture :: Fixture -> IO ()
destroyFixture fx = do
    forM_ fx._fixturePactServiceThreads $ \tid -> do
        throwTo tid ThreadKilled
    forM_ fx._fixturePactServiceSqls $ \sql -> do
        closeSQLiteConnection sql

oneBlock :: ChainwebVersion -> RocksDb -> Word -> C.Benchmark
oneBlock v rdb numTxs =
    let cid = unsafeChainId 0
        cfg = testPactServiceConfig

        setupEnv = do
            fx <- createFixture v rdb cfg
            return fx

        cleanupEnv fx = do
            destroyFixture fx
    in
    C.envWithCleanup setupEnv cleanupEnv $ \ ~fx ->
        C.bench ("oneBlock " <> show numTxs) $ C.whnfIO $ withEvent "bench" $ do
            advanceAllChains fx $ onChain cid $ \ph pactQueue mempool -> do
                mempoolClear mempool
                txs <- forM [1..numTxs] $ \_ -> do
                    buildCwCmd v (transferCmd cid 1.0)
                mempoolInsertPact5 (fx._fixtureMempools ^?! atChain cid) UncheckedInsert txs

                bip <- throwIfNoHistory =<<
                    newBlock noMiner NewBlockFill (ParentHeader ph) pactQueue
                let block = forAnyPactVersion finalizeBlock bip
                fromIntegral @_ @Word (Vector.length (_payloadWithOutputsTransactions block))
                    & P.equals numTxs
                return block

oneBlockIO :: ChainwebVersion -> RocksDb -> Word -> IO ()
oneBlockIO v rdb numTxs = do
    let cid = unsafeChainId 0
    let cfg = testPactServiceConfig
    bracket (createFixture v rdb cfg) destroyFixture $ \fx -> withEvent "bench" $ do
        void $ advanceAllChains fx $ onChain cid $ \ph pactQueue mempool -> do
            mempoolClear mempool
            txs <- forM [1..numTxs] $ \_ -> do
                buildCwCmd v (transferCmd cid 1.0)
            mempoolInsertPact5 (fx._fixtureMempools ^?! atChain cid) UncheckedInsert txs

            bip <- throwIfNoHistory =<<
                newBlock noMiner NewBlockFill (ParentHeader ph) pactQueue
            let block = forAnyPactVersion finalizeBlock bip
            fromIntegral @_ @Word (Vector.length (_payloadWithOutputsTransactions block))
                & P.equals numTxs
            return block

{-
advanceAllChainsWithTxs :: Fixture -> ChainMap [Pact5.Transaction] -> IO (ChainMap (Vector TestPact5CommandResult))
advanceAllChainsWithTxs fixture txsPerChain =
    advanceAllChains fixture $
        txsPerChain <&> \txs ph pactQueue mempool -> do
            mempoolClear mempool
            mempoolInsertPact5 mempool CheckedInsert txs
            nb <- throwIfNotPact5 =<< throwIfNoHistory =<<
                newBlock noMiner NewBlockFill (ParentHeader ph) pactQueue
            return $ finalizeBlock nb
-}

-- this mines a block on *all chains*. if you don't specify a payload on a chain,
-- it adds empty blocks!
advanceAllChains :: ()
    => Fixture
    -> ChainMap (BlockHeader -> PactQueue -> MempoolBackend Pact4.UnparsedTransaction -> IO PayloadWithOutputs)
    -> IO (ChainMap (Vector TestPact5CommandResult))
advanceAllChains Fixture{..} blocks = do
    commandResults <-
        forConcurrently (HashSet.toList (chainIds _chainwebVersion)) $ \c -> do
            ph <- getParentTestBlockDb _fixtureBlockDb c
            creationTime <- getCurrentTimeIntegral
            let pactQueue = _fixturePactQueues ^?! atChain c
            let mempool = _fixtureMempools ^?! atChain c
            let makeEmptyBlock p _ _ = do
                    bip <- throwIfNoHistory =<<
                        newBlock noMiner NewBlockEmpty (ParentHeader p) pactQueue
                    return $! forAnyPactVersion finalizeBlock bip

            payload <- fromMaybe makeEmptyBlock (blocks ^? atChain c) ph pactQueue mempool
            added <- addTestBlockDb _fixtureBlockDb
                (succ $ view blockHeight ph)
                (Nonce 0)
                (\_ _ -> creationTime)
                c
                payload
            when (not added) $
                error "failed to mine block"
            ph' <- getParentTestBlockDb _fixtureBlockDb c
            payload' <- validateBlock ph' (CheckablePayloadWithOutputs payload) pactQueue
            assertEqual "payloads must not be altered by validateBlock" payload payload'
            commandResults :: Vector TestPact5CommandResult
                <- forM
                    (_payloadWithOutputsTransactions payload')
                    (decodeOrThrow'
                    . LBS.fromStrict
                    . _transactionOutputBytes
                    . snd)
            -- assert on the command results
            return (c, commandResults)

    return (onChains commandResults)

{-
getCut :: Fixture -> IO Cut
getCut Fixture{..} = getCutTestBlockDb _fixtureBlockDb

revert :: Fixture -> Cut -> IO ()
revert Fixture{..} c = do
    setCutTestBlockDb _fixtureBlockDb c
    forM_ (HashSet.toList (chainIds _chainwebVersion)) $ \chain -> do
        ph <- getParentTestBlockDb _fixtureBlockDb chain
        pactSyncToBlock ph (_fixturePactQueues ^?! atChain chain)
-}

transferCmd :: ChainId -> Decimal -> CmdBuilder
transferCmd chain transferAmount = (defaultCmd chain)
    { _cbRPC = mkExec' $
        "(coin.transfer \"sender00\" \"sender01\" " <>
        -- if the number doesn't end with a decimal part, even if it's zero, Pact will
        -- throw an error
        T.pack (printf "%.4f" (realToFrac transferAmount :: Double)) <>
        ")"
    , _cbSigners =
        [ mkEd25519Signer' sender00
            [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) []
            , CapToken (QualifiedName "TRANSFER" coinModuleName) [PString "sender00", PString "sender01", PDecimal transferAmount]
            ]
        ]
    , _cbGasPrice = GasPrice 0.000_000_000_001
    , _cbGasLimit = GasLimit (Gas 1000)
    }

testLogger :: IO GenericLogger
testLogger = do
    logLevel <- liftIO getTestLogLevel
    pure $ genericLogger logLevel Text.putStrLn
