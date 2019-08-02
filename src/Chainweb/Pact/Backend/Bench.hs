{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
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
-- import Chainweb.Pact.Backend.ChainwebPactDb
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
import Chainweb.Pact.Backend.RelationalCheckpointer
import Pact.Gas
import Chainweb.BlockHeader
import Chainweb.BlockHash
import Chainweb.MerkleLogHash
import Data.Monoid
import Data.Maybe

bench :: C.Benchmark
bench = C.bgroup "pact-backend" $
  map pactSqliteBench [1,5,10,20,50,100,200,500,1000]
  ++
  map cpBenchOverBlock [1,5,10,20,50,100,200,500,1000]

{-
_cpBench :: C.Benchmark
_cpBench = C.envWithCleanup setup teardown $ \ ~(NoopNFData (e,_)) -> C.bgroup "checkpointer" (benches e)
  where

    setup = do
      (f,deleter) <- newTempFile
      !sqliteEnv <- openSQLiteConnection f chainwebPragmas
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
-}

cpBenchOverBlock :: Int -> C.Benchmark
cpBenchOverBlock transactionCount = C.envWithCleanup setup teardown $ \ ~(NoopNFData (_,e,_)) -> C.bgroup ("batchedCheckpointer/transactionCount=" ++ show transactionCount) (benches e)
  where

    setup = do
      (f,deleter) <- newTempFile
      !sqliteEnv <- openSQLiteConnection f chainwebPragmas
      let nolog = newLogger neverLog ""
      !cenv <- initRelationalCheckpointer initBlockState sqliteEnv nolog freeGasEnv
      return $ NoopNFData (sqliteEnv, cenv, deleter)

    teardown (NoopNFData (sqliteEnv, _cenv, deleter)) = do
        closeSQLiteConnection sqliteEnv
        deleter

    benches :: CheckpointEnv -> [C.Benchmark]
    benches cpenv =
      [
        datbench cpenv "usertable"
      ]
      where

        datbench cp name = C.env (setup' cp) $ \ ~(ut) -> C.bench name $ C.nfIO (go cp ut)

        setup' CheckpointEnv{..} = do
          usertablename <- restore _cpeCheckpointer Nothing >>= \case
              PactDbEnv' db@(PactDbEnv pdb e) -> do
                let tn = "user1"
                    ut = UserTables tn
                begin db
                _createUserTable pdb tn "someModule" e
                writeRow db Insert ut 1
                commit db
                return $ NoopNFData ut
          save _cpeCheckpointer hash01
          return usertablename


        f = "f"
        k = "k"

        hash01 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000001a"
        hash02 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000002a"

        writeRow (PactDbEnv pdb e) writeType ut i =
            _writeRow pdb writeType ut k (ObjectMap $ M.fromList [(f,(PLiteral (LInteger i)))]) e

        go CheckpointEnv{..} (NoopNFData ut) = do
            result <- restore _cpeCheckpointer (Just (BlockHeight 1, hash01)) >>= \case
                PactDbEnv' pactdbenv -> fromMaybe err . getLast <$> foldMap (fmap (Last . Just)) (replicate transactionCount (transaction pactdbenv))
            save _cpeCheckpointer hash02
            return result
          where
            err = error "Something went in one of the transactions."
            transaction db@(PactDbEnv pdb e) = do
              begin db
              r <- _readRow pdb ut k e
              case r of
                Nothing  -> die "no row read"
                Just (ObjectMap m) -> case M.lookup f m of
                  Nothing -> die "field not found"
                  Just (PLiteral (LInteger i)) -> do
                    let j = succ i
                    writeRow db Update ut j
                    commit db
                    return j
                  Just _ -> die "field not integer"

pactSqliteBench :: Int -> C.Benchmark
pactSqliteBench _transactionCount = C.envWithCleanup setup teardown $ \ ~(NoopNFData (e,_)) -> C.bgroup ("pact-sqlite/transactionCount=" ++ show _transactionCount) (benches e)
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
        benchUserTable _transactionCount dbEnv "usertable"
      ]

begin :: PactDbEnv e -> IO ()
begin db = void $ _beginTx (pdPactDb db) Transactional (pdPactDbVar db)

commit :: PactDbEnv e -> IO ()
commit db = void $ _commitTx (pdPactDb db) (pdPactDbVar db)

die :: String -> IO a
die = throwM . userError

benchUserTable :: Int -> PactDbEnv e -> String -> C.Benchmark
benchUserTable _transactionCount dbEnv name = C.env (setup dbEnv) $ \ ~(ut) -> C.bench name $ C.nfIO (go dbEnv ut)

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

    go db@(PactDbEnv pdb e) (NoopNFData ut) = fromJust . getLast <$> foldMap (fmap (Last . Just)) (replicate _transactionCount transaction)
      where
        transaction = do
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
