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
import Chainweb.Pact.PactService (withPactService)
import Chainweb.Pact.PactService.Checkpointer (readFrom)
import Chainweb.Pact.Types
import Chainweb.Pact.Transaction
import Chainweb.Pact.TransactionExec qualified as Pact5
import Chainweb.Pact.Types qualified as Pact5
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb(..), mkTestBlockDb)
import Chainweb.Test.Pact.CmdBuilder qualified as Pact5
import Chainweb.Test.TestVersions
import Chainweb.Utils.Bench
import Chainweb.Pact.Backend.Types (SQLiteEnv, BlockHandle, throwIfNoHistory)
import Chainweb.Version
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)
import Control.DeepSeq
import Control.Lens hiding (only)
import Control.Monad.IO.Class
import Criterion.Main qualified as C
import Data.ByteString (ByteString)
import Pact.Core.Command.Types qualified as Pact5
import Pact.Core.Errors qualified as Pact5
import Pact.Core.Evaluate qualified as Pact5
import Pact.Core.Gas.Types qualified as Pact5
import Pact.Core.Persistence qualified as Pact5
import Control.Monad.State.Strict
import Chainweb.Test.Utils (withTempChainSqlite)
import qualified Control.Monad.Trans.Resource as ResourceT
import Data.IORef
import Chainweb.Parent
import Chainweb.Time
import Chainweb.BlockCreationTime

bench :: RocksDb -> C.Benchmark
bench rdb = C.bgroup "applyCmd"
    [ C.bench "Pact5" $ withVersion pact5Version $ benchApplyCmd rdb applyCmd
    ]

data Env = Env
    { sqlite :: !SQLiteEnv
    , testBlockDb :: !TestBlockDb
    , blockHeaderDb :: !BlockHeaderDb
    , logger :: !GenericLogger
    , pactServiceEnv :: !(ServiceEnv RocksDbTable)
    }

instance NFData Env where
    rnf !_ = ()

benchApplyCmd :: HasVersion => RocksDb -> (BlockEnv -> BlockHandle -> IO a) -> C.Benchmarkable
benchApplyCmd rdb act =
    let setupEnv _ = ResourceT.runResourceT $ do
            (sql, sqlPool) <- withTempChainSqlite chain0
            tdb <- mkTestBlockDb rdb
            bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb tdb) chain0
            lgr <- liftIO testLogger

            serviceEnv <- withPactService chain0 Nothing mempty lgr Nothing (_bdbPayloadDb tdb) sqlPool sql defaultPactServiceConfig GeneratingGenesis

            sRef <- ResourceT.getInternalState
            s <- liftIO $ readIORef sRef
            liftIO $ writeIORef sRef =<< readIORef =<< ResourceT.createInternalState
            pure $ (Env
                { sqlite = sql
                , testBlockDb = tdb
                , blockHeaderDb = bhdb
                , logger = lgr
                , pactServiceEnv = serviceEnv
                }, NoopNFData s)

        cleanupEnv _ (_, NoopNFData s) = ResourceT.closeInternalState =<< newIORef s
    in
    C.perBatchEnvWithCleanup setupEnv cleanupEnv $ \ ~(env, _) -> do
        a <- do
            (throwIfNoHistory =<<) $ readFrom
                env.logger chain0 env.sqlite
                (Parent $ BlockCreationTime epoch) (Parent (view rankedBlockHash $ gh chain0))
                act

        return (NoopNFData a)

applyCmd :: HasVersion => BlockEnv -> BlockHandle -> IO (Pact5.CommandResult [Pact5.TxLog ByteString] (Pact5.PactError Pact5.Info))
applyCmd bEnv bHandle = do
    cmd <- liftIO $ Pact5.buildCwCmd (Pact5.defaultCmd chain0)
        { Pact5._cbRPC = Pact5.mkExec' "(fold + 0 [1 2 3 4 5])"
        , Pact5._cbGasPrice = Pact5.GasPrice 2
        , Pact5._cbGasLimit = Pact5.GasLimit (Pact5.Gas 500)
        -- no caps should be equivalent to the GAS cap
        , Pact5._cbSigners = [Pact5.mkEd25519Signer' Pact5.sender00 []]
        }
    lgr <- testLogger

    flip evalStateT bHandle $ Pact5.pactTransaction bEnv Nothing $ \pactDb spvSupport -> do
        r <- Pact5.applyCmd lgr Nothing pactDb noMiner (_psBlockCtx bEnv) (TxBlockIdx 0) spvSupport (Pact5.Gas 1) (view payloadObj <$> cmd)
        case r of
            Left err -> error $ show err
            Right a -> pure a
{-# noinline applyCmd #-}

chain0 :: ChainId
chain0 = unsafeChainId 0

gh :: HasVersion => ChainId -> BlockHeader
gh = genesisBlockHeader

pact5Version :: ChainwebVersion
pact5Version = instantCpmTestVersion singletonChainGraph
