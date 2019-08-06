{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Chainweb.Pact.Backend.Bench
  ( bench )
  where

import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Utils.Bench
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import qualified Criterion.Main as C
import Pact.Types.Logger
import System.IO.Extra
-- import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.MerkleLogHash
import Chainweb.Pact.Backend.RelationalCheckpointer
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Data.String.Conv
import Pact.Gas
import Pact.Interpreter (PactDbEnv(..), mkPactDbEnv)
import qualified Pact.Interpreter as PI
import qualified Pact.Persist.SQLite as PSQL
import Pact.PersistPactDb (DbEnv(..), initDbEnv, pactdb)
import Pact.Types.Exp
import Pact.Types.PactValue
import Pact.Types.Persistence
import qualified Pact.Types.SQLite as PSQL
import Pact.Types.Term

bench :: C.Benchmark
bench = C.bgroup "pact-backend" $
        play [ pactSqliteBench False
             , pactSqliteBench True
             , cpBenchOverBlock
             , cpBenchNoRewindOverBlock
             ]
  where
    testPoints = [1,5,10,20,50,100,200,500,1000]

    play benches = concatMap playOne testPoints
      where
        playOne n = map ($ n) benches

cpBenchNoRewindOverBlock :: Int -> C.Benchmark
cpBenchNoRewindOverBlock transactionCount =
    C.envWithCleanup setup teardown $ \ ~(NoopNFData (_,e,_)) ->  C.bgroup name (benches e)
  where
    name = "batchedNoRewindCheckpointer/transactionCount="
      ++ show transactionCount

    setup = do
      (f, deleter) <- newTempFile
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

        datbench cp name' = C.env (setup' cp) $ \ ~(ut) -> C.bench name' $ C.nfIO $ do
          mv <- newMVar (BlockHeight 1, initbytestring, hash01)
          go cp mv ut

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

        (initbytestring, hash01) = (bytestring, BlockHash $ unsafeMerkleLogHash bytestring)
            where
              bytestring =
                "0000000000000000000000000000001a"

        nextHash bytestring =
          (bytestring, BlockHash $ unsafeMerkleLogHash $ B.pack $ B.zipWith (+) bytestring inc)
          where
            inc = toS $ replicate 30 '\NUL' ++ ['\SOH', '\NUL']

        writeRow (PactDbEnv pdb e) writeType ut i =
            _writeRow pdb writeType ut k (ObjectMap $ M.fromList [(f,(PLiteral (LInteger i)))]) e

        go CheckpointEnv{..} mblock (NoopNFData ut) = do
            (blockheight, bytestring, hash) <- readMVar mblock
            void $ restore _cpeCheckpointer (Just (blockheight, hash)) >>= \case
              PactDbEnv' pactdbenv -> replicateM_ transactionCount (transaction pactdbenv)
            let (bytestring', hash') = nextHash bytestring
            modifyMVar_ mblock (const $  return (blockheight + 1, bytestring', hash'))
            void $ save _cpeCheckpointer hash'

          where
            transaction db@(PactDbEnv pdb e) = do
              begin db
              r <- _readRow pdb ut k e
              case r of
                Nothing -> die "no now read"
                Just (ObjectMap m) -> case M.lookup f m of
                  Nothing -> die "field not found"
                  Just (PLiteral (LInteger i)) -> do
                    let j = succ i
                    writeRow db Update ut j
                    commit db
                    return j
                  Just _ -> die "field not integer"



cpBenchOverBlock :: Int -> C.Benchmark
cpBenchOverBlock transactionCount = C.envWithCleanup setup teardown $ \ ~(NoopNFData (_,e,_)) -> C.bgroup name (benches e)
  where

    name = "batchedCheckpointer/transactionCount="
      ++ show transactionCount

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

        datbench cp benchname = C.env (setup' cp) $ \ ~(ut) -> C.bench benchname $ C.nfIO (go cp ut)

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
            restore _cpeCheckpointer (Just (BlockHeight 1, hash01)) >>= \case
                PactDbEnv' pactdbenv -> replicateM_ transactionCount (transaction pactdbenv)
            void $ save _cpeCheckpointer hash02
          where
            transaction db@(PactDbEnv pdb e) = do
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

pactSqliteBench :: Bool -> Int -> C.Benchmark
pactSqliteBench unsafe transactionCount =
    C.envWithCleanup setup teardown $ \ ~(NoopNFData (e,_)) ->
                                        C.bgroup tname (benches e)
  where
    tname = mconcat [ "pact-sqlite/"
                    , if unsafe then "unsafe" else "safe"
                    , "/tc="
                    , show transactionCount
                    ]
    prags = if unsafe then PSQL.fastNoJournalPragmas else chainwebPragmas
    setup = do
      (f,deleter) <- newTempFile
      !sqliteEnv <- PSQL.initSQLite (PSQL.SQLiteConfig f prags) neverLog
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
        benchUserTable transactionCount dbEnv "usertable"
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

    go db@(PactDbEnv pdb e) (NoopNFData ut) = replicateM _transactionCount transaction
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
