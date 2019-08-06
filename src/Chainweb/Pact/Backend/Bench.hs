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
import qualified Data.ByteString as B
import Data.String.Conv

bench :: C.Benchmark
bench = C.bgroup "pact-backend" $ merge3
    (map pactSqliteBench [1,5,10,20,50,100,200,500,1000])
    (map cpBenchOverBlock [1,5,10,20,50,100,200,500,1000])
    (map (cpBenchNoRewindOverBlocks 10) [1,5,10,20,50,100,200,500,1000])

merge3 :: [a] -> [a] -> [a] -> [a]
merge3 [] [] [] = []
merge3 xs [] [] = xs
merge3 [] ys [] = ys
merge3 [] [] zs = zs
merge3 (x:xs) (y:ys) [] = x : y : merge3 xs ys []
merge3 [] (y:ys) (z:zs) = y : z : merge3 [] ys zs
merge3 (x:xs) [] (z:zs) = x : z : merge3 xs [] zs
merge3 (x:xs) (y:ys) (z:zs) = x : y : z : merge3 xs ys zs

cpBenchNoRewindOverBlocks :: Int -> Int -> C.Benchmark
cpBenchNoRewindOverBlocks finalBlockHeight transactionCount =
    C.envWithCleanup setup teardown $ \ ~(NoopNFData (_,e,_)) ->  C.bgroup name (benches e)
  where
    name = "batchedNoRewindCheckpointer/transactionCount="
      ++ show transactionCount
      ++ "/finalBlockHeight="
      ++ show finalBlockHeight

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
        datbench cpenv finalBlockHeight "usertable"
      ]

      where

        datbench cp fbh name' = C.env (setup' cp) $ \ ~(ut) -> C.bench name' $ C.nfIO $ do
          mv <- newMVar (BlockHeight 1, initbytestring, hash01)
          sequence (replicate (pred fbh) (go cp mv ut))

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
            result <- restore _cpeCheckpointer (Just (blockheight, hash)) >>= \case
              PactDbEnv' pactdbenv -> fromMaybe err . getLast <$> foldMap (fmap (Last . Just)) (replicate transactionCount (transaction pactdbenv))
            let (bytestring',hash') = nextHash bytestring
            modifyMVar_ mblock (const $  return (blockheight + 1, bytestring', hash'))
            save _cpeCheckpointer hash'
            return result

          where
            err = error "Something went wrong in one of the transactions."
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
            result <- restore _cpeCheckpointer (Just (BlockHeight 1, hash01)) >>= \case
                PactDbEnv' pactdbenv -> fromMaybe err . getLast <$> foldMap (fmap (Last . Just)) (replicate transactionCount (transaction pactdbenv))
            save _cpeCheckpointer hash02
            return result
          where
            err = error "Something went wrong in one of the transactions."
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

pactSqliteBench :: Int -> C.Benchmark
pactSqliteBench transactionCount = C.envWithCleanup setup teardown $ \ ~(NoopNFData (e,_)) -> C.bgroup ("pact-sqlite/transactionCount=" ++ show transactionCount) (benches e)
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
