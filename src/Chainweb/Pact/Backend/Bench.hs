{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Chainweb.Pact.Backend.Bench
  ( bench )
  where

import Control.Monad
import Control.Concurrent
import Control.Monad.Catch
import Chainweb.Pact.Backend.Utils
import qualified Criterion.Main as C
import System.IO.Extra
import Chainweb.Pact.Backend.Types
import Chainweb.Utils.Bench
import Pact.Types.Logger
import Chainweb.Pact.Backend.ChainwebPactDb
import Pact.Types.Persistence
import Pact.Interpreter (PactDbEnv(..),mkPactDbEnv)
import qualified Pact.Interpreter as PI
import Pact.Types.Term
import Pact.Types.Exp
import Pact.Types.PactValue
import Pact.PersistPactDb (DbEnv(..),pactdb,initDbEnv)
import qualified Data.Map.Strict as M
import qualified Pact.Types.SQLite as PSQL
import qualified Pact.Persist.SQLite as PSQL


bench :: C.Benchmark
bench = C.bgroup "pact-backend"
  [ cpBench
  , pactSqliteBench
  ]

cpBench :: C.Benchmark
cpBench = C.envWithCleanup setup teardown $ \ ~(NoopNFData (e,_)) -> C.bgroup "checkpointer" (benches e)
  where

    setup = do
      (f,deleter) <- newTempFile
      !sqliteEnv <- openSQLiteConnection f PSQL.fastNoJournalPragmas
      let nolog = newLogger neverLog ""
      blockEnv <- newMVar $ BlockEnv (BlockDbEnv sqliteEnv nolog) initBlockState
      runBlockEnv blockEnv initSchema
      runBlockEnv blockEnv $ beginSavepoint Block
      return $ NoopNFData (PactDbEnv chainwebPactDb blockEnv, deleter)

    teardown (NoopNFData (PactDbEnv _ e, deleter)) = do
      runBlockEnv e $ commitSavepoint Block
      c <- readMVar e
      closeSQLiteConnection $ _bdbenvDb $ _benvDb c
      deleter

    benches :: PactDbEnv e -> [C.Benchmark]
    benches dbEnv =
      [
        benchUserTable dbEnv "usertable"
      ]

pactSqliteBench :: C.Benchmark
pactSqliteBench = C.envWithCleanup setup teardown $ \ ~(NoopNFData (e,_)) -> C.bgroup "pact-sqlite" (benches e)
  where

    setup = do
      (f,deleter) <- newTempFile
      !sqliteEnv <- PSQL.initSQLite (PSQL.SQLiteConfig f PSQL.fastNoJournalPragmas) neverLog
      dbe <- mkPactDbEnv pactdb (initDbEnv neverLog PSQL.persister sqliteEnv)
      PI.initSchema dbe
      return $ NoopNFData (dbe, deleter)

    teardown (NoopNFData (PactDbEnv _ e, deleter)) = do
      c <- readMVar e
      void $ PSQL.closeSQLite $ _db c
      deleter

    benches :: PactDbEnv e -> [C.Benchmark]
    benches dbEnv =
      [
        benchUserTable dbEnv "usertable"
      ]

begin :: PactDbEnv e -> IO ()
begin db = void $ _beginTx (pdPactDb db) Transactional (pdPactDbVar db)

commit :: PactDbEnv e -> IO ()
commit db = void $ _commitTx (pdPactDb db) (pdPactDbVar db)

die :: String -> IO a
die = throwM . userError

benchUserTable :: PactDbEnv e -> String -> C.Benchmark
benchUserTable dbEnv name = C.env (setup dbEnv) $ \ ~(ut) -> C.bench name $ C.nfIO (go dbEnv ut)

  where

    setup db@(PactDbEnv pdb e) = do
      let tn = "user1"
          ut = UserTables "user1"
      begin db
      _createUserTable pdb tn "someModule" e
      writeRow db Insert ut 1
      commit db
      return $ NoopNFData ut

    k = "k"
    f = "f"

    writeRow (PactDbEnv pdb e) writeType ut i = do
      _writeRow pdb writeType ut k (ObjectMap $ M.fromList [(f,(PLiteral (LInteger i)))]) e

    go db@(PactDbEnv pdb e) (NoopNFData ut) = do
      begin db
      r <- _readRow pdb ut k e
      case r of
        Nothing -> die "no row read"
        Just (ObjectMap m) -> case M.lookup f m of
          Nothing -> die "field not found"
          Just (PLiteral (LInteger i)) -> do
            let j = succ i
            writeRow db Update ut j
            commit db
            return j
          Just _ -> die "field not integer"
