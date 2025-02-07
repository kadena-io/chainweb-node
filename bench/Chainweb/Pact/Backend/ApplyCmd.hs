{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.Pact.Backend.ApplyCmd
    ( bench
    )
    where

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact.Backend.Utils (openSQLiteConnection, closeSQLiteConnection, chainwebPragmas)
import Chainweb.Pact.PactService (initialPayloadState, withPactService)
import Chainweb.Pact.PactService.Checkpointer (readFrom, SomeBlockM(..))
import Chainweb.Pact.Types
import Chainweb.Pact4.Backend.ChainwebPactDb qualified as Pact
import Chainweb.Pact.Transaction qualified as Pact
import Chainweb.Pact.TransactionExec qualified as Pact
import Chainweb.Pact4.Types qualified as Pact
import Chainweb.Pact.Transaction
import Chainweb.Pact.TransactionExec qualified as Pact5
import Chainweb.Pact.Types qualified as Pact5
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb(..), mkTestBlockDbIO)
import Chainweb.Test.Pact4.Utils qualified as Pact
import Chainweb.Test.Pact.CmdBuilder qualified as Pact5
import Chainweb.Test.TestVersions
import Chainweb.Utils (T2(..), T3(..))
import Chainweb.Utils.Bench
import Chainweb.Pact.Backend.Types (SQLiteEnv)
import Chainweb.Version
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)
import Control.Concurrent (ThreadId, forkIO, throwTo)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.DeepSeq
import Control.Exception (AsyncException(..))
import Control.Lens hiding (only)
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Criterion.Main qualified as C
import Data.ByteString (ByteString)
import Data.Functor.Product
import Pact.Core.Command.Types qualified as Pact5
import Pact.Core.Errors qualified as Pact5
import Pact.Core.Evaluate qualified as Pact5
import Pact.Core.Gas.Types qualified as Pact5
import Pact.Core.Persistence qualified as Pact5
import Pact.Core.SPV qualified as Pact5
import Pact.Types.Command qualified as Pact
import Pact.Types.Gas qualified as Pact
import Pact.Types.Runtime qualified as Pact
import Pact.Types.SPV qualified as Pact

bench :: RocksDb -> C.Benchmark
bench rdb = C.bgroup "applyCmd"
    [ C.bench "Pact5" $ withVersion pact5Version benchApplyCmd rdb (SomeBlockM $ Pair (error "Pact4") applyCmd5)
    , C.bench "Pact4" $ withVersion pact4Version benchApplyCmd rdb (SomeBlockM $ Pair applyCmd4 (error "Pact5"))
    ]

data Env = Env
    { sqlite :: !SQLiteEnv
    , testBlockDb :: !TestBlockDb
    , testBlockDbRocksDb :: !RocksDb
    , blockHeaderDb :: !BlockHeaderDb
    , logger :: !GenericLogger
    , pactServiceThreadId :: !ThreadId
    , pactServiceEnv :: !(ServiceEnv GenericLogger RocksDbTable)
    }

instance NFData Env where
    rnf !_ = ()

benchApplyCmd :: HasVersion => RocksDb -> SomeBlockM GenericLogger RocksDbTable a -> C.Benchmarkable
benchApplyCmd rdb act =
    let setupEnv _ = do
            sql <- openSQLiteConnection "" chainwebPragmas
            T2 tdb tdbRdb <- mkTestBlockDbIO rdb
            bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) chain0
            lgr <- testLogger

            psEnvVar <- newEmptyMVar
-- <<<<<<< Conflict 1 of 1
-- %%%%%%% Changes from base to side #1
-- -            tid <- forkIO $ void $ withPactService ver chain0 lgr Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
-- +            tid <- forkIO $ void $ withPactService ver chain0 lgr Nothing bhdb (_bdbPayloadDb tdb) sql defaultPactServiceConfig $ do
--                  initialPayloadState ver chain0
-- +++++++ Contents of side #2
--             tid <- forkIO $ void $ withPactService chain0 lgr Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
--                 initialPayloadState chain0
-- >>>>>>> Conflict 1 of 1 ends
                psEnv <- ask
                liftIO $ putMVar psEnvVar psEnv
            psEnv <- readMVar psEnvVar

            pure $ Env
                { sqlite = sql
                , testBlockDb = tdb
                , testBlockDbRocksDb = tdbRdb
                , blockHeaderDb = bhdb
                , logger = lgr
                , pactServiceThreadId = tid
                , pactServiceEnv = psEnv
                }

        cleanupEnv _ env = do
            closeSQLiteConnection env.sqlite
            throwTo env.pactServiceThreadId ThreadKilled
            deleteNamespaceRocksDb env.testBlockDbRocksDb
    in
    C.perBatchEnvWithCleanup setupEnv cleanupEnv $ \ ~env -> do
        T2 a _finalPactState <- runPactServiceM (PactServiceState mempty) env.pactServiceEnv $ do
            throwIfNoHistory =<<
                readFrom
                    (Just $ ParentHeader (gh chain0))
                    act

        return (NoopNFData a)

applyCmd4 :: Pact4.PactBlockM GenericLogger RocksDbTable (Pact4.CommandResult [Pact4.TxLogJson]) --(CommandResult [TxLog ByteString] (PactError Info))
applyCmd4 = withVersion pact4Version $ do
    lgr <- view (psServiceEnv . psLogger)
    let txCtx = Pact4.TxContext
            { Pact4._tcParentHeader = ParentHeader (gh chain0)
            , Pact4._tcPublicMeta = Pact4.noPublicMeta
            , Pact4._tcMiner = noMiner
            }
    let gasModel = Pact4.getGasModel txCtx
    pactDbEnv <- view (psBlockDbEnv . Pact4.cpPactDbEnv)

    cmd <- liftIO $ Pact4.buildCwCmd "fakeNonce"
        $ set Pact4.cbSigners
            [ Pact4.mkEd25519Signer' Pact4.sender00 []
            ]
        $ set Pact4.cbChainId chain0
        $ set Pact4.cbRPC (Pact4.mkExec' "(fold + 0 [1 2 3 4 5])")
        $ Pact4.defaultCmd

    T3 cmdResult _moduleCache _warnings <- liftIO $
        Pact4.applyCmd
            lgr
            Nothing
            Nothing
            pactDbEnv
            noMiner
            gasModel
            txCtx
            (TxBlockIdx 0)
            Pact4.noSPVSupport
            (fmap Pact4.payloadObj cmd)
            (Pact4.Gas 1)
            mempty -- module cache
            ApplySend

    pure cmdResult
{-# noinline applyCmd4 #-}

applyCmd5 :: Pact5.PactBlockM GenericLogger RocksDbTable (Pact5.CommandResult [Pact5.TxLog ByteString] (Pact5.PactError Pact5.Info))
applyCmd5 = withVersion pact5Version $ do
    cmd <- liftIO $ Pact5.buildCwCmd (Pact5.defaultCmd chain0)
        { Pact5._cbRPC = Pact5.mkExec' "(fold + 0 [1 2 3 4 5])"
        , Pact5._cbGasPrice = Pact5.GasPrice 2
        , Pact5._cbGasLimit = Pact5.GasLimit (Pact5.Gas 500)
        -- no caps should be equivalent to the GAS cap
        , Pact5._cbSigners = [Pact5.mkEd25519Signer' Pact5.sender00 []]
        }
    lgr <- view (psServiceEnv . psLogger)
    let txCtx = Pact5.TxContext {Pact5._tcParentHeader = ParentHeader (gh chain0), Pact5._tcMiner = noMiner}

    Pact5.pactTransaction Nothing $ \pactDb -> do
        r <- Pact5.applyCmd lgr Nothing pactDb txCtx (TxBlockIdx 0) Pact5.noSPVSupport (Pact5.Gas 1) (view payloadObj <$> cmd)
        case r of
            Left err -> error $ show err
            Right a -> pure a
{-# noinline applyCmd5 #-}

chain0 :: ChainId
chain0 = unsafeChainId 0

gh :: HasVersion => ChainId -> BlockHeader
gh = genesisBlockHeader

pact4Version :: ChainwebVersion
pact4Version = instantCpmTestVersion singletonChainGraph

pact5Version :: ChainwebVersion
pact5Version = pact5InstantCpmTestVersion singletonChainGraph
