{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Chainweb.Pact.Backend.Bench
  ( bench )
  where


import Control.Concurrent
import Control.Lens (view, (.~))
import Control.Monad
import Control.Monad.Catch
import qualified Criterion.Main as C
import Data.Function ((&))

import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M

import System.LogLevel
import System.Random

import qualified Streaming.Prelude as Stream

-- pact imports

import Pact.Interpreter (PactDbEnv(..), mkPactDbEnv)
import Pact.PersistPactDb (DbEnv(..), initDbEnv, pactdb)
import Pact.Types.Exp
import qualified Pact.Types.Logger as P
import Pact.Types.Persistence
import Pact.Types.RowData
import Pact.Types.Term
import Pact.Types.Util
import qualified Pact.Types.Hash as Pact
import qualified Pact.Interpreter as PI
import qualified Pact.Persist.SQLite as PSQL
import qualified Pact.Types.SQLite as PSQL

-- chainweb imports

import Chainweb.BlockHash
import Chainweb.BlockHeader.Internal
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.MerkleLogHash
import Chainweb.Pact.PactService.Checkpointer.Internal

import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Types
import Chainweb.Test.TestVersions
import Chainweb.Utils.Bench
import Chainweb.Utils (sshow)
import Chainweb.Version
import qualified Chainweb.Pact4.Backend.ChainwebPactDb as Pact4
import qualified Pact.Types.Command as Pact

testVer :: ChainwebVersion
testVer = instantCpmTestVersion petersenChainGraph

testChainId :: ChainId
testChainId = unsafeChainId 0

-- allowing a straightforward list of blocks to be passed to the API,
-- and only exposing the PactDbEnv part of the block context
cpRestoreAndSave
  :: (Monoid q, Logger logger)
  => Checkpointer logger
  -> Maybe BlockHeader
  -> [(BlockHeader, PactDbEnv (Pact4.BlockEnv logger) -> IO q)]
  -> IO q
cpRestoreAndSave cp pc blks = snd <$> restoreAndSave cp (ParentHeader <$> pc)
  (traverse Stream.yield [Pact4RunnableBlock $ \dbEnv _ -> (,bh) <$> fun (Pact4._cpPactDbEnv dbEnv) | (bh, fun) <- blks])

-- | fabricate a `BlockHeader` for a block given its hash and its parent.
childOf :: Maybe BlockHeader -> BlockHash -> BlockHeader
childOf m bhsh = case m of
  Just bh -> bh
    & blockHash .~ bhsh
    & blockParent .~ view blockHash bh
    & blockHeight .~ view blockHeight bh + 1
  Nothing -> genesisBlockHeader testVer testChainId
    & blockHash .~ bhsh

bench :: C.Benchmark
bench = C.bgroup "pact-backend" $
        play [ pactSqliteWithBench False . benchUserTable
             , pactSqliteWithBench True . benchUserTable
             , pactSqliteWithBench False . benchUserTableForKeys
             , pactSqliteWithBench True . benchUserTableForKeys
             , cpWithBench . cpBenchNoRewindOverBlock
             , cpWithBench . cpBenchOverBlock
             , cpWithBench . cpBenchSampleKeys
             , cpWithBench . cpBenchLookupProcessedTx
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
    $ \ ~(NoopNFData e) -> C.bgroup tname (benches e)
  where
    tname = mconcat [ "pact-sqlite/"
                    , if unsafe then "unsafe" else "safe"
                    ]
    prags = if unsafe then PSQL.fastNoJournalPragmas else chainwebPragmas
    setup = do
        let dbFile = "" {- temporary sqlite db -}
        !sqliteEnv <- PSQL.initSQLite (PSQL.SQLiteConfig dbFile  prags) P.neverLog
        dbe <- mkPactDbEnv pactdb (initDbEnv P.neverLog PSQL.persister sqliteEnv)
        PI.initSchema dbe
        return $ NoopNFData dbe

    teardown (NoopNFData (PactDbEnv _ e)) = do
        c <- readMVar e
        void $ PSQL.closeSQLite $ _db c

    benches :: PactDbEnv (DbEnv PSQL.SQLite)  -> [C.Benchmark]
    benches dbEnv =
        [
          benchtorun dbEnv
        ]

cpWithBench :: forall logger. (Logger logger, logger ~ GenericLogger)
  => (Checkpointer logger -> C.Benchmark)
  -> C.Benchmark
cpWithBench torun =
    C.envWithCleanup setup teardown $ \ ~(NoopNFData (_,e)) ->
        C.bgroup name (benches e)
  where
    name = "batchedCheckpointer"

    setup = do
        let dbFile = "" {- temporary SQLite db -}
        let neverLogger = genericLogger Error (\_ -> return ())
        !sqliteEnv <- openSQLiteConnection dbFile chainwebPragmas
        !cenv <-
          initCheckpointerResources defaultModuleCacheLimit sqliteEnv DoNotPersistIntraBlockWrites neverLogger testVer testChainId
        return $ NoopNFData (sqliteEnv, cenv)

    teardown (NoopNFData (sqliteEnv, _cenv)) = closeSQLiteConnection sqliteEnv

    benches :: Checkpointer logger -> [C.Benchmark]
    benches cpenv =
        [ torun cpenv ]

cpBenchNoRewindOverBlock :: Logger logger => Int -> Checkpointer logger -> C.Benchmark
cpBenchNoRewindOverBlock transactionCount cp = C.env setup' $ \ ~ut ->
  C.bench name $ C.nfIO $ do
      mv <- newMVar (initbytestring, pc01)
      go mv ut
  where
    name = "noRewind/transactionCount="
      ++ show transactionCount
    pc01 = childOf Nothing hash01
    setup' = do
        [usertablename] <- cpRestoreAndSave cp Nothing
          [(pc01, \db -> fmap (:[]) $ setupUserTable db $ \ut -> writeRow db Insert ut f k 1)]
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

    go mblock (NoopNFData ut) = do
        (bytestring, pc) <- readMVar mblock
        let (bytestring', hash') = nextHash bytestring
        let pc' = childOf (Just pc) hash'
        modifyMVar_ mblock
            $ \_ -> do
              return (bytestring', pc')
        void $ cpRestoreAndSave cp (Just pc)
            [(pc', \pactdbenv -> replicateM_ transactionCount (transaction pactdbenv))]

      where
        transaction db = incIntegerAtKey db ut f k 1

cpBenchOverBlock :: Logger logger => Int -> Checkpointer logger -> C.Benchmark
cpBenchOverBlock transactionCount cp = C.env setup' $ \ ~(ut) ->
    C.bench benchname $ C.nfIO (go ut)
  where
    benchname = "overBlock/transactionCount=" ++ show transactionCount
    setup' = do
        [usertablename] <- cpRestoreAndSave cp Nothing [(pc01, \db ->
            fmap (:[]) $ setupUserTable db $ \ut -> writeRow db Insert ut f k 1
            )]
        return usertablename

    f = "f"
    k = "k"

    hash01 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000001a"
    hash02 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000002a"
    pc01 = childOf Nothing hash01
    pc02 = childOf (Just pc01) hash02

    go (NoopNFData ut) = do
        cpRestoreAndSave cp (Just pc01)
            [(pc02, \pactdbenv ->
                replicateM_ transactionCount (transaction pactdbenv)
            )]
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


_cpBenchKeys :: Logger logger => Int -> Checkpointer logger -> C.Benchmark
_cpBenchKeys numKeys cp =
    C.env setup' $ \ ~(ut) -> C.bench name $ C.nfIO (go ut)
  where
    name = "withKeys/keyCount="
      ++ show numKeys
    setup' = do
        [usertablename] <- cpRestoreAndSave cp Nothing
            [(pc01, \db ->
                fmap (:[]) $ setupUserTable db $ \ut -> forM_ [1 .. numKeys] $ \i -> do
                    let rowkey = RowKey $ "k" <> sshow i
                    writeRow db Insert ut f rowkey (fromIntegral i)
            )]

        return usertablename

    f = "f"

    hash01 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000001a"
    hash02 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000002a"
    pc01 = childOf Nothing hash01
    pc02 = childOf (Just pc01) hash02

    go (NoopNFData ut) = do
        cpRestoreAndSave cp (Just pc01)
            [(pc02, \pactdbenv ->
                forM_ [1 .. numKeys] (transaction pactdbenv)
            )]
      where
        transaction db numkey = do
            let rowkey = RowKey $ "k" <> sshow numkey
            incIntegerAtKey db ut f rowkey 1

cpBenchSampleKeys :: Logger logger => Int -> Checkpointer logger -> C.Benchmark
cpBenchSampleKeys numSampleEvents cp =
    C.env setup' $ \ ~(ut) -> C.bench name $ C.nfIO (go ut)
  where
    name = "user-table-keys/sampleEvents=" ++ show numSampleEvents
    numberOfKeys :: Integer
    numberOfKeys = 10
    setup' = do
        [usertablename] <- cpRestoreAndSave cp Nothing
            [(pc01, \db ->
                fmap (:[]) $ setupUserTable db $ \ut -> forM_ [1 .. numberOfKeys] $ \i -> do
                    let rowkey = RowKey $ "k" <> sshow i
                    writeRow db Insert ut f rowkey i
            )]

        return usertablename

    unpack = \case
      Nothing -> die "no row read"
      Just (RowData _v (ObjectMap m)) -> case M.lookup f m of
        Nothing -> die "field not found"
        Just (RDLiteral (LInteger result)) -> return result
        Just _ -> die "field not integer"


    hash01 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000001a"
    hash02 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000002a"
    pc01 = childOf Nothing hash01
    pc02 = childOf (Just pc01) hash02

    f = "f"

    go (NoopNFData ut) = do
        cpRestoreAndSave cp (Just pc01)
          [(pc02, \db@(PactDbEnv pdb e) ->
              forM_ [1 .. numSampleEvents] $ \_ -> do
                  let torowkey ind = RowKey $ "k" <> sshow ind
                  rowkeya <- torowkey <$> randomRIO (1,numberOfKeys)
                  rowkeyb <- torowkey <$> randomRIO (1,numberOfKeys)
                  a <- _readRow pdb ut rowkeya e >>= unpack
                  b <- _readRow pdb ut rowkeyb e >>= unpack
                  writeRow db Update ut f rowkeya b
                  writeRow db Update ut f rowkeyb a
          )]


cpBenchLookupProcessedTx :: Logger logger => Int -> Checkpointer logger -> C.Benchmark
cpBenchLookupProcessedTx transactionCount cp = C.env setup' $ \ ~(ut) ->
    C.bench benchname $ C.nfIO (go ut)
  where
    benchname = "lookupProcessedTx/transactionCount=" ++ show transactionCount
    transaction (NoopNFData ut) db = incIntegerAtKey db ut f k 1
    setup' = do
        [usertablename] <- cpRestoreAndSave cp Nothing
          [(pc01, \db ->
              fmap (:[]) $ setupUserTable db $ \ut -> writeRow db Insert ut f k 1
          )]

        cpRestoreAndSave cp (Just pc01)
          [(pc02, \pactdbenv ->
              replicateM_ transactionCount (transaction usertablename pactdbenv)
          )]

        return usertablename

    f = "f"
    k = "k"

    hash01 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000001a"
    hash02 = BlockHash $ unsafeMerkleLogHash "0000000000000000000000000000002a"
    pc01 = childOf Nothing hash01
    pc02 = childOf (Just pc01) hash02

    go (NoopNFData _) = do
        readFrom cp (Just (ParentHeader pc02)) Pact4T $ \dbEnv _ ->
          Pact4._cpLookupProcessedTx dbEnv (V.fromList [Pact.RequestKey (Pact.toUntypedHash $ Pact.TypedHash "") | _ <- [1..transactionCount]])
