{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Chainweb.Pact.Backend.Bench
  ( bench )
  where

import Control.Concurrent
import Chainweb.Pact.Backend.Utils
import Criterion.Main (envWithCleanup,bgroup,Benchmark)
import System.IO.Extra
import Chainweb.Pact.Backend.Types
import Chainweb.Utils.Bench
import Pact.Types.Logger
import Chainweb.Pact.Backend.ChainwebPactDb

bench :: Benchmark
bench = envWithCleanup setup teardown $ \ ~(NoopNFData (e,_)) -> bgroup "checkpointer" (benches e)
  where

    setup = do
      (f,deleter) <- newTempFile
      !sqliteEnv <- openSQLiteConnection f fastNoJournalPragmas
      let nolog = newLogger neverLog ""
      blockEnv <- newMVar $ BlockEnv (BlockDbEnv sqliteEnv nolog) initBlockState
      runBlockEnv blockEnv initSchema
      return $ NoopNFData (sqliteEnv, deleter)

    teardown (NoopNFData (sqliteEnv, deleter)) = do
      closeSQLiteConnection sqliteEnv
      deleter

    benches :: SQLiteEnv -> [Benchmark]
    benches _env = []
