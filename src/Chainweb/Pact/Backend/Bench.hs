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
import Pact.Interpreter (PactDbEnv(..))
import Pact.Types.Term
import Pact.Types.Exp
import Pact.Types.PactValue
import qualified Data.Map.Strict as M

bench :: C.Benchmark
bench = C.envWithCleanup setup teardown $ \ ~(NoopNFData (e,_)) -> C.bgroup "checkpointer" (benches e)
  where

    setup = do
      (f,deleter) <- newTempFile
      !sqliteEnv <- openSQLiteConnection f fastNoJournalPragmas
      let nolog = newLogger neverLog ""
      blockEnv <- newMVar $ BlockEnv (BlockDbEnv sqliteEnv nolog) initBlockState
      runBlockEnv blockEnv initSchema
      return $ NoopNFData (PactDbEnv chainwebPactDb blockEnv, deleter)

    teardown (NoopNFData (PactDbEnv _ e, deleter)) = do
      c <- readMVar e
      closeSQLiteConnection $ _bdbenvDb $ _benvDb c -- sqliteEnv
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

    setup db@(PactDbEnv pactdb e) = do
      let tn = "user1"
          ut = UserTables "user1"
      begin db
      _createUserTable pactdb tn "someModule" e
      writeRow db Insert ut 1
      commit db
      return $ NoopNFData ut

    k = "k"
    f = "f"

    writeRow (PactDbEnv pactdb e) writeType ut i = do
      _writeRow pactdb writeType ut k (ObjectMap $ M.fromList [(f,(PLiteral (LInteger i)))]) e

    go db@(PactDbEnv pactdb e) (NoopNFData ut) = do
      r <- _readRow pactdb ut k e
      case r of
        Nothing -> die "no row read"
        Just (ObjectMap m) -> case M.lookup f m of
          Nothing -> die "field not found"
          Just (PLiteral (LInteger i)) -> do
            let j = succ i
            writeRow db Update ut j
            return j
          Just _ -> die "field not integer"
