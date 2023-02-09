{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Pact.Compaction (tests) where

import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Data.List (sort)
import qualified Data.Map.Strict as M


import Pact.Interpreter (PactDbEnv(..))
import Pact.Types.Logger (newLogger)
import Pact.Types.RowData
import Pact.Types.Runtime hiding (ChainId)

import Test.Tasty
import Test.Tasty.HUnit

import System.Logger (LogLevel(..))

-- internal imports

import Chainweb.Pact.Types
import Chainweb.Pact.Backend.Compaction
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Backend.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Version

import Chainweb.Test.Orphans.Internal ({- Arbitrary BlockHash -})

-- -------------------------------------------------------------------------- --
-- Tests

tests :: ScheduledTest
tests = testGroupSch "Chainweb.Test.Pact.Compaction"
    [ testCompactCheckpointer
    ]

testCompactCheckpointer :: TestTree
testCompactCheckpointer =
  withTempSQLiteResource $ runSQLite' $ \resIO ->
  testCase "testCompactCheckpointer" $ do

    (CheckpointEnv {..}, SQLiteEnv {..}) <- resIO

    -- init genesis (block 0)
    let hash00 = getArbitrary 0
    void $ _cpRestore _cpeCheckpointer Nothing
    _cpSave _cpeCheckpointer hash00

    let hash01 = getArbitrary 1
        hash02 = getArbitrary 2
        hash03 = getArbitrary 3

    -- block 1, empty block
    void $ _cpRestore _cpeCheckpointer (Just (1, hash00))
    _cpSave _cpeCheckpointer hash01

    -- block 2: create tables, add/mutate values
    (PactDbEnv' (PactDbEnv pactdb mvar)) <- _cpRestore _cpeCheckpointer (Just (2, hash01))

    let withTx f = do
          void $ _beginTx pactdb Transactional mvar
          void f
          void $ _commitTx pactdb mvar
        rd1 = RowData RDV1 $ ObjectMap $ M.fromList [("a", RDLiteral (LBool False))]
        rd2 = RowData RDV1 $ ObjectMap $ M.fromList [("a", RDLiteral (LBool True))]

    -- tx: create tables
    withTx $ do
      _createUserTable pactdb "tA" "mod" mvar
      _createUserTable pactdb "tAA" "mod" mvar

    -- setup potential hash collisions.
    withTx $ do
      _writeRow pactdb Insert (UserTables "tA") "AA" rd1 mvar -- test table+key collision
      _writeRow pactdb Insert (UserTables "tAA") "A" rd1 mvar
      _writeRow pactdb Insert (UserTables "tA") "B" rd2 mvar -- test row+key collision
      _writeRow pactdb Insert (UserTables "tAA") "B" rd2 mvar

    -- tx: update, insert
    withTx $ do
      _writeRow pactdb Update (UserTables "tA") "B" rd2 mvar -- test update to same data collision
      _writeRow pactdb Insert (UserTables "tA") "C" rd1 mvar -- new insert

    _cpSave _cpeCheckpointer hash02

    -- block 3: updates
    void $ _cpRestore _cpeCheckpointer $ Just (3,hash02)

    withTx $ do
      _writeRow pactdb Update (UserTables "tA") "B" rd1 mvar -- test updated hash x2
      _writeRow pactdb Update (UserTables "tA") "C" rd2 mvar -- test updated hash

    _cpSave _cpeCheckpointer hash03

    let checkTable tbl expectedRows = do
          ks <- sort <$> _keys pactdb (UserTables tbl) mvar
          assertEqual ("rowcount match: " <> show tbl) (length expectedRows) (length ks)
          forM_ (zip expectedRows ks) $ \((ke,rde),ka) -> do
            assertEqual ("key match: " <> show tbl) ke ka
            rda <- _readRow pactdb (UserTables tbl) ka mvar
            assertEqual ("data match: " <> show (tbl,ke)) (Just rde) rda

    -- compact to height 2
    withDefaultLogger Debug $ \l ->

      runCompactM (mkCompactEnv l _sConn 2 [Flag_KeepCompactTables]) $ do

        -- compact and capture global grand hash
        void $ compact

    checkTable "tA"
      [ ( "AA", rd1 )
      , ( "B", rd2 )
      , ( "C", rd1 ) ]

    checkTable "tAA"
      [ ( "A", rd1 )
      , ( "B", rd2 ) ]

    -- test coverage TODOs
    -- create table
    -- restart robustness
    -- check hashes against haskell-computed sha256




-- -------------------------------------------------------------------------- --
-- Chainweb Settings

testVer :: ChainwebVersion
testVer = FastTimedCPM peterson

testChainId :: ChainId
testChainId = unsafeChainId 0

-- -------------------------------------------------------------------------- --
-- Checkpointer Utils

runSQLite'
    :: (IO (CheckpointEnv,SQLiteEnv) -> TestTree)
    -> IO SQLiteEnv
    -> TestTree
runSQLite' runTest sqlEnvIO = runTest $ do
    sqlenv <- sqlEnvIO
    cp <- initRelationalCheckpointer initialBlockState sqlenv logger testVer testChainId
    return (cp, sqlenv)
  where
    initialBlockState = set bsModuleNameFix True $
        initBlockState defaultModuleCacheLimit $ genesisHeight testVer testChainId
    logger = newLogger (pactTestLogger False) "RelationalCheckpointer"
