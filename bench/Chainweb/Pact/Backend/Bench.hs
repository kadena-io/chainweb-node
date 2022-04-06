{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Chainweb.Pact.Backend.Bench
  ( bench )
  where


import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import qualified Criterion.Main as C

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M

import System.IO.Extra
import System.Random

-- pact imports

import Pact.Interpreter (PactDbEnv(..), mkPactDbEnv)
import Pact.PersistPactDb (DbEnv(..), initDbEnv, pactdb)
import Pact.Types.Exp
import Pact.Types.Logger
import Pact.Types.Persistence
import Pact.Types.RowData
import Pact.Types.Term
import Pact.Types.Util
import qualified Pact.Interpreter as PI
import qualified Pact.Persist.SQLite as PSQL
import qualified Pact.Types.SQLite as PSQL

-- chainweb imports

import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Utils.Bench
import Chainweb.Utils (sshow)
import Chainweb.Version

v :: ChainwebVersion
v = FastTimedCPM petersonChainGraph

bench :: C.Benchmark
bench = C.bgroup "pact-backend" $
        play [ pactSqliteWithBench False . benchUserTable
             , pactSqliteWithBench True . benchUserTable
             , pactSqliteWithBench False . benchUserTableForKeys
             , pactSqliteWithBench True . benchUserTableForKeys
             , cpWithBench . cpBenchNoRewindOverBlock
             , cpWithBench . cpBenchOverBlock
             , cpWithBench . cpBenchSampleKeys
             -- , cpWithBench . cpBenchKeys
             ]
  where
    testPoints = [100,1000]

    play benches = concatMap playOne testPoints
      where
        playOne n = map ($ n) benches

pactSqliteWithBench
    :: Bool
    -> (PactDbEnv (DbEnv PSQL.SQLite) -> C.Benchmark)
    -> C.Benchmark
pactSqliteWithBench unsafe benchtorun =
    C.envWithCleanup setup teardown
    $ \ ~(NoopNFData (e,_)) -> C.bgroup tname (benches e)
  where
    tname = mconcat [ "pact-sqlite/"
                    , if unsafe then "unsafe" else "safe"
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

    benches :: PactDbEnv (DbEnv PSQL.SQLite)  -> [C.Benchmark]
    benches dbEnv =
        [
          benchtorun dbEnv
        ]

cpWithBench :: (CheckpointEnv -> C.Benchmark) -> C.Benchmark
cpWithBench torun =
    C.envWithCleanup setup teardown $ \ ~(NoopNFData (_,e,_)) ->
                                        C.bgroup name (benches e)
  where
    name = "batchedCheckpointer"
    cid = unsafeChainId 0

    initialBlockState = initBlockState $ genesisHeight v cid

    setup = do
        (f, deleter) <- newTempFile
        !sqliteEnv <- openSQLiteConnection f chainwebPragmas
        let nolog = newLogger neverLog ""
        !cenv <-
          initRelationalCheckpointer initialBlockState sqliteEnv nolog v cid
        return $ NoopNFData (sqliteEnv, cenv, deleter)

    teardown (NoopNFData (sqliteEnv, _cenv, deleter)) = do
        closeSQLiteConnection sqliteEnv
        deleter

    benches :: CheckpointEnv -> [C.Benchmark]
    benches cpenv =
        [
          torun cpenv
        ]

cpBenchNoRewindOverBlock :: Int -> CheckpointEnv -> C.Benchmark
cpBenchNoRewindOverBlock transactionCount cp = C.env (setup' cp) $ \ ~(ut) ->
  C.bench name $ C.nfIO $ do
      mv <- newMVar (BlockHeight 1, initbytestring, hash01)
      go cp mv ut
  where
    name = "noRewind/transactionCount="
      ++ show transactionCount
    setup' CheckpointEnv{..} = do
        usertablename <- _cpRestore _cpeCheckpointer Nothing >>= \case
          PactDbEnv' db ->
            setupUserTable db $ \ut -> writeRow db Insert ut f k 1
        _cpSave _cpeCheckpointer hash01
        return usertablename

    f = "f"
    k = "k"

    (initbytestring, hash01) =
      (bytestring, BlockHash $ unsafeMerkleLogHash bytestring)
        where
          bytestring =
            "0000000000000000000000000000001a"

    nextHash bytestring =
      (bytestring,
       BlockHash $ unsafeMerkleLogHash $ B.pack $ B.zipWith (+) bytestring inc)
      where
        inc = B8.replicate 30 '\NUL' <> "\SOH\NUL"

    go CheckpointEnv{..} mblock (NoopNFData ut) = do
        (blockheight, bytestring, hash) <- readMVar mblock
        void $ _cpRestore _cpeCheckpointer (Just (blockheight, hash)) >>= \case
          PactDbEnv' pactdbenv ->
            replicateM_ transactionCount (transaction pactdbenv)
        let (bytestring', hash') = nextHash bytestring
        modifyMVar_ mblock
            (const $  return (blockheight + 1, bytestring', hash'))
        void $ _cpSave _cpeCheckpointer hash'

      where
        transaction db = incIntegerAtKey db ut f k 1

cpBenchOverBlock :: Int -> CheckpointEnv -> C.Benchmark
cpBenchOverBlock transactionCount cp = C.env (setup' cp) $ \ ~(ut) ->
    C.bench benchname $ C.nfIO (go cp ut)
  where
    benchname = "overBlock/transactionCount=" ++ show transactionCount
    setup' CheckpointEnv{..} = do
        usertablename <- _cpRestore _cpeCheckpointer Nothing >>= \case
            PactDbEnv' db ->
              setupUserTable db $ \ut -> writeRow db Insert ut f k 1
        _cpSave _cpeCheckpointer hash01
        return usertablename

    f = "f"
    k = "k"

    hash01 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000001a"
    hash02 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000002a"

    go CheckpointEnv{..} (NoopNFData ut) = do
        _cpRestore _cpeCheckpointer (Just (BlockHeight 1, hash01)) >>= \case
            PactDbEnv' pactdbenv ->
              replicateM_ transactionCount (transaction pactdbenv)
        void $ _cpSave _cpeCheckpointer hash02
      where
        transaction db = incIntegerAtKey db ut f k 1

begin :: PactDbEnv e -> IO ()
begin db = void $ _beginTx (pdPactDb db) Transactional (pdPactDbVar db)

commit :: PactDbEnv e -> IO ()
commit db = void $ _commitTx (pdPactDb db) (pdPactDbVar db)

die :: String -> IO a
die = throwM . userError

setupUserTable
    :: PactDbEnv e
    -> (Domain RowKey RowData -> IO ())
    -> IO (NoopNFData (Domain RowKey RowData))
setupUserTable db@(PactDbEnv pdb e) setupio = do
    let tn = "user1"
        ut = UserTables tn
    begin db
    _createUserTable pdb tn "someModule" e
    setupio ut
    commit db
    return $ NoopNFData ut

writeRow
    :: AsString k
    => PactDbEnv e
    -> WriteType
    -> Domain k RowData
    -> FieldKey
    -> k
    -> Integer
    -> IO ()
writeRow (PactDbEnv pdb e) writeType ut f k i =
    _writeRow pdb writeType ut k
        (RowData RDV1 (ObjectMap $ M.fromList [(f,(RDLiteral (LInteger i)))])) e

benchUserTable :: Int -> PactDbEnv e -> C.Benchmark
benchUserTable transactionCount dbEnv = C.env (setup dbEnv) $ \ ~(ut) ->
    C.bench name $ C.nfIO (go dbEnv ut)
  where

    setup db = setupUserTable db $ \ut -> writeRow db Insert ut f k 1

    name = "user-table/tc=" ++ show transactionCount

    k = "k"
    f = "f"

    go db (NoopNFData ut) =
      replicateM transactionCount (incIntegerAtKey db ut f k 1)

incIntegerAtKey
    :: PactDbEnv e
    -> Domain RowKey RowData
    -> FieldKey
    -> RowKey
    -> Integer
    -> IO Integer
incIntegerAtKey db@(PactDbEnv pdb e) ut f k z = do
    begin db
    r <- _readRow pdb ut k e
    case r of
        Nothing -> die "no row read"
        Just (RowData _v (ObjectMap m)) -> case M.lookup f m of
            Nothing -> die "field not found"
            Just (RDLiteral (LInteger i)) -> do
                let j = i + z
                writeRow db Update ut f k j
                commit db
                return j
            Just _ -> die "field not integer"

benchUserTableForKeys :: Int -> PactDbEnv e -> C.Benchmark
benchUserTableForKeys numSampleEvents dbEnv =
    C.env (setup dbEnv) $ \ ~(ut) -> C.bench name $ C.nfIO (go dbEnv ut)
  where

    numberOfKeys :: Integer
    numberOfKeys = 10

    setup db = setupUserTable db $ \ut ->
      forM_ [1 .. numberOfKeys] $ \i -> do
          let rowkey = RowKey $ "k" <> sshow i
          writeRow db Insert ut f rowkey i

    f = "f"

    name = "user-table-keys/sampleEvents=" ++ show numSampleEvents

    unpack = \case
      Nothing -> die "no row read"
      Just (RowData _ (ObjectMap m)) -> case M.lookup f m of
        Nothing -> die "field not found"
        Just (RDLiteral (LInteger result)) -> return result
        Just _ -> die "field not integer"


    go db@(PactDbEnv pdb e) (NoopNFData ut) =
      forM_ [1 .. numSampleEvents] $ \_ -> do
          let torowkey ind = RowKey $ "k" <> sshow ind
          rowkeya <- torowkey <$> randomRIO (1,numberOfKeys)
          rowkeyb <- torowkey <$> randomRIO (1,numberOfKeys)
          a <- _readRow pdb ut rowkeya e >>= unpack
          b <- _readRow pdb ut rowkeyb e >>= unpack
          writeRow db Update ut f rowkeya b
          writeRow db Update ut f rowkeyb a


_cpBenchKeys :: Int -> CheckpointEnv -> C.Benchmark
_cpBenchKeys numKeys cp =
    C.env (setup' cp) $ \ ~(ut) -> C.bench name $ C.nfIO (go cp ut)
  where
    name = "withKeys/keyCount="
      ++ show numKeys
    setup' CheckpointEnv{..} = do
        usertablename <- _cpRestore _cpeCheckpointer Nothing >>= \case
            PactDbEnv' db ->
              setupUserTable db $ \ut -> forM_ [1 .. numKeys] $ \i -> do
                  let rowkey = RowKey $ "k" <> sshow i
                  writeRow db Insert ut f rowkey (fromIntegral i)

        _cpSave _cpeCheckpointer hash01
        return usertablename

    f = "f"

    hash01 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000001a"
    hash02 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000002a"

    go CheckpointEnv{..} (NoopNFData ut) = do
        _cpRestore _cpeCheckpointer (Just (BlockHeight 1, hash01)) >>= \case
            PactDbEnv' pactdbenv -> forM_ [1 .. numKeys] (transaction pactdbenv)
        void $ _cpSave _cpeCheckpointer hash02
      where
        transaction db numkey = do
            let rowkey = RowKey $ "k" <> sshow numkey
            incIntegerAtKey db ut f rowkey 1

cpBenchSampleKeys :: Int -> CheckpointEnv -> C.Benchmark
cpBenchSampleKeys numSampleEvents cp =
    C.env (setup' cp) $ \ ~(ut) -> C.bench name $ C.nfIO (go cp ut)
  where
    name = "user-table-keys/sampleEvents=" ++ show numSampleEvents
    numberOfKeys :: Integer
    numberOfKeys = 10
    setup' CheckpointEnv {..} = do
        usertablename <- _cpRestore _cpeCheckpointer Nothing >>= \case
            PactDbEnv' db ->
              setupUserTable db $ \ut -> forM_ [1 .. numberOfKeys] $ \i -> do
                  let rowkey = RowKey $ "k" <> sshow i
                  writeRow db Insert ut f rowkey i

        _cpSave _cpeCheckpointer hash01
        return usertablename

    unpack = \case
      Nothing -> die "no row read"
      Just (RowData _v (ObjectMap m)) -> case M.lookup f m of
        Nothing -> die "field not found"
        Just (RDLiteral (LInteger result)) -> return result
        Just _ -> die "field not integer"


    hash01 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000001a"
    hash02 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000002a"

    f = "f"

    go CheckpointEnv {..} (NoopNFData ut) = do
        _cpRestore _cpeCheckpointer (Just (BlockHeight 1, hash01)) >>= \case
              PactDbEnv' db@(PactDbEnv pdb e) ->
                forM_ [1 .. numSampleEvents] $ \_ -> do
                    let torowkey ind = RowKey $ "k" <> sshow ind
                    rowkeya <- torowkey <$> randomRIO (1,numberOfKeys)
                    rowkeyb <- torowkey <$> randomRIO (1,numberOfKeys)
                    a <- _readRow pdb ut rowkeya e >>= unpack
                    b <- _readRow pdb ut rowkeyb e >>= unpack
                    writeRow db Update ut f rowkeya b
                    writeRow db Update ut f rowkeyb a
        void $ _cpSave _cpeCheckpointer hash02
