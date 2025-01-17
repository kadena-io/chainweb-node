{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
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
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact.Backend.Utils (openSQLiteConnection, closeSQLiteConnection, chainwebPragmas)
import Chainweb.Pact.PactService (initialPayloadState, withPactService)
import Chainweb.Pact.PactService.Checkpointer (readFrom, SomeBlockM(..))
import Chainweb.Pact.Types
import Chainweb.Pact4.Backend.ChainwebPactDb qualified as Pact4
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Pact4.TransactionExec qualified as Pact4
import Chainweb.Pact4.Types qualified as Pact4
import Chainweb.Pact5.Transaction
import Chainweb.Pact5.TransactionExec qualified as Pact5
import Chainweb.Pact5.Types qualified as Pact5
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb (_bdbPayloadDb, _bdbWebBlockHeaderDb), mkTestBlockDb)
import Chainweb.Test.Pact4.Utils qualified as Pact4
import Chainweb.Test.Pact5.CmdBuilder qualified as Pact5
import Chainweb.Test.Pact5.Utils (getTestLogLevel)
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Utils (T2(..), T3(..))
import Chainweb.Version
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)
import Control.Concurrent (forkIO, throwTo)
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
import Data.Text.IO qualified as T
import Database.SQLite3.Direct (Database(..))
import Pact.Core.Command.Types qualified as Pact5
import Pact.Core.Errors qualified as Pact5
import Pact.Core.Evaluate qualified as Pact5
import Pact.Core.Gas.Types qualified as Pact5
import Pact.Core.Persistence qualified as Pact5
import Pact.Core.SPV qualified as Pact5
import Pact.Types.Command qualified as Pact4
import Pact.Types.Gas qualified as Pact4
import Pact.Types.Runtime qualified as Pact4
import Pact.Types.SPV qualified as Pact4

deriving newtype instance NFData Database

instance NFData (PactServiceEnv logger tbl) where
    rnf !_ = ()

bench :: RocksDb -> C.Benchmark
bench rdb = C.bgroup "applyCmd"
    [ C.bgroup "Pact5" [benchApplyCmd pact5Version rdb (SomeBlockM $ Pair (error "Pact4") applyCmd5)]
    , C.bgroup "Pact4" [benchApplyCmd pact4Version rdb (SomeBlockM $ Pair applyCmd4 (error "Pact5"))]
    ]

benchApplyCmd :: ChainwebVersion -> RocksDb -> SomeBlockM GenericLogger RocksDbTable a -> C.Benchmark
benchApplyCmd ver rdb act =
    let setupEnv = do
            sql <- openSQLiteConnection "" chainwebPragmas
            tdb <- mkTestBlockDb ver =<< testRocksDb "readFromAfterGenesis" rdb
            bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) chain0
            logger <- testLogger

            psEnvVar <- newEmptyMVar
            tid <- forkIO $ void $ withPactService ver chain0 logger Nothing bhdb (_bdbPayloadDb tdb) sql testPactServiceConfig $ do
                initialPayloadState ver chain0
                psEnv <- ask
                liftIO $ putMVar psEnvVar psEnv
            psEnv <- readMVar psEnvVar

            pure (sql, tdb, bhdb, logger, tid, psEnv)

        cleanupEnv (sql, _, _, _, tid, _) = do
            closeSQLiteConnection sql
            throwTo tid ThreadKilled
    in
    C.envWithCleanup setupEnv cleanupEnv $ \ ~(_sql, _tdb, _bhdb, _logger, _tid, psEnv) ->
        C.bench "todo change this" $ C.whnfIO $ do
            T2 a _finalPactState <- runPactServiceM (PactServiceState mempty) psEnv $ do
                throwIfNoHistory =<<
                    readFrom
                        (Just $ ParentHeader (gh ver chain0))
                        act

            return a

applyCmd4 :: Pact4.PactBlockM GenericLogger RocksDbTable (Pact4.CommandResult [Pact4.TxLogJson]) --(CommandResult [TxLog ByteString] (PactError Info))
applyCmd4 = do
    logger <- view (psServiceEnv . psLogger)
    let txCtx = Pact4.TxContext
            { Pact4._tcParentHeader = ParentHeader (gh pact4Version chain0)
            , Pact4._tcPublicMeta = Pact4.noPublicMeta
            , Pact4._tcMiner = noMiner
            }
    let gasModel = Pact4.getGasModel txCtx
    pactDbEnv <- view (psBlockDbEnv . Pact4.cpPactDbEnv)

    cmd <- liftIO $ Pact4.buildCwCmd "fakeNonce" pact4Version
        $ set Pact4.cbSigners
            [ Pact4.mkEd25519Signer' Pact4.sender00 []
            ]
        $ set Pact4.cbChainId chain0
        $ set Pact4.cbRPC (Pact4.mkExec' "(fold + 0 [1 2 3 4 5])")
        $ Pact4.defaultCmd

    T3 cmdResult _moduleCache _warnings <- liftIO $
        Pact4.applyCmd
            pact4Version
            logger
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
applyCmd5 = do
    cmd <- liftIO $ Pact5.buildCwCmd pact5Version (Pact5.defaultCmd chain0)
        { Pact5._cbRPC = Pact5.mkExec' "(fold + 0 [1 2 3 4 5])"
        , Pact5._cbGasPrice = Pact5.GasPrice 2
        , Pact5._cbGasLimit = Pact5.GasLimit (Pact5.Gas 500)
        -- no caps should be equivalent to the GAS cap
        , Pact5._cbSigners = [Pact5.mkEd25519Signer' Pact5.sender00 []]
        }
    logger <- view (psServiceEnv . psLogger)
    let txCtx = Pact5.TxContext {Pact5._tcParentHeader = ParentHeader (gh pact5Version chain0), Pact5._tcMiner = noMiner}

    Pact5.pactTransaction Nothing $ \pactDb -> do
        r <- Pact5.applyCmd logger Nothing pactDb txCtx (TxBlockIdx 0) Pact5.noSPVSupport (Pact5.Gas 1) (view payloadObj <$> cmd)
        case r of
            Left err -> error $ show err
            Right a -> pure a
{-# noinline applyCmd5 #-}

chain0 :: ChainId
chain0 = unsafeChainId 0

gh :: ChainwebVersion -> ChainId -> BlockHeader
gh = genesisBlockHeader

pact4Version :: ChainwebVersion
pact4Version = instantCpmTestVersion singletonChainGraph

pact5Version :: ChainwebVersion
pact5Version = pact5InstantCpmTestVersion singletonChainGraph

testLogger :: IO GenericLogger
testLogger = do
    logLevel <- liftIO getTestLogLevel
    pure $ genericLogger logLevel T.putStrLn
