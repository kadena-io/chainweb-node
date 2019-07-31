{-# LANGUAGE RecordWildCards #-}
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
import Chainweb.Pact.Backend.RelationalCheckpointer
import Pact.Gas (freeGasEnv)
import Data.Aeson (Value)
import Data.Text (Text,pack)
import qualified Pact.Types.Hash as H
import Pact.Types.Server (CommandEnv(..))
import Chainweb.Pact.TransactionExec (applyExec', buildExecParsedCode,initCapabilities,magic_COINBASE,initModules)
import Data.Default
import Pact.Types.SPV (noSPVSupport)
import qualified Data.Text.IO as T
import Chainweb.BlockHash (nullBlockHash)
import Pact.ApiReq
import Pact.Types.Runtime (Capability)
import Chainweb.Pact.Types (ModuleCache)

bench :: C.Benchmark
bench = C.bgroup "pact-backend"
  [ cpBench
  , pactSqliteBench
  , coinCPBench
  ]

cpBench :: C.Benchmark
cpBench = C.envWithCleanup setup teardown $ \ ~(NoopNFData (e,_)) -> C.bgroup "cp-sqlite" (benches e)
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



coinCPBench :: C.Benchmark
coinCPBench = C.envWithCleanup setup teardown $ \ ~(NoopNFData ((_,c,p,ms),_)) -> C.bgroup "coin-cp" (benches c p ms)
  where

    setup = do

      (f,deleter) <- newTempFile
      !sqliteEnv <- openSQLiteConnection f chainwebPragmas
      let nolog = newLogger neverLog "" -- change to alwaysLog to troubleshoot

      cpe@CheckpointEnv{..} <-
        initRelationalCheckpointer initBlockState sqliteEnv nolog freeGasEnv

      g <- restore _cpeCheckpointer Nothing

      coinSig <- T.readFile "pact/coin-contract/coin-sig.pact"
      void $ runExec cpe g [] mempty Nothing coinSig

      coinContract <- T.readFile "pact/coin-contract/coin.pact"
      PI.EvalResult{..} <- runExec cpe g [] mempty Nothing coinContract

      ((ApiReq{..},_,_,_),_) <- mkApiReq "pact/genesis/testnet00/grants.yaml"
      case (_ylCode) of
        Nothing -> die "failed reading grants.yaml"
        (Just c) -> do
          void $ runExec cpe g [magic_COINBASE] mempty _ylData (pack c)

          save _cpeCheckpointer nullBlockHash

          sqlenv <- restore _cpeCheckpointer $ Just (1,nullBlockHash)

          return $ NoopNFData ((sqliteEnv,cpe,sqlenv,_erLoadedModules), deleter)

    teardown (NoopNFData ((s,_,_,_), deleter)) = do

      closeSQLiteConnection s
      deleter

    benches :: CheckpointEnv -> PactDbEnv' -> ModuleCache -> [C.Benchmark]
    benches cpe pde mc =
      [
        benchAcctBal cpe pde mc
      ]


benchAcctBal :: CheckpointEnv -> PactDbEnv' -> ModuleCache -> C.Benchmark
benchAcctBal cpe pde mc = C.bench "account-balance" $ C.nfIO $ do
  PI.EvalResult{..} <- runExec cpe pde [] mc Nothing "(coin.account-balance 'sender01)"
  return $ PI._erOutput




runExec :: CheckpointEnv -> PactDbEnv' -> [Capability] -> ModuleCache -> Maybe Value -> Text -> IO PI.EvalResult
runExec CheckpointEnv{..} (PactDbEnv' pactdbenv) caps mc eData eCode = do
  let cmdenv = CommandEnv Nothing Transactional pactdbenv _cpeLogger _cpeGasEnv def noSPVSupport
  execMsg <- buildExecParsedCode eData eCode
  applyExec' cmdenv (initModules mc $ initCapabilities caps) execMsg [] (H.toUntypedHash (H.hash "" :: H.PactHash))
