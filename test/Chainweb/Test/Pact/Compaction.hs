{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Pact.Compaction (tests) where

import Control.Lens hiding ((.=))
import Control.Monad.Reader

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M

import Database.SQLite3.Direct (Utf8)


import Pact.Interpreter (PactDbEnv(..))
import Pact.Types.Logger (newLogger)
import Pact.Types.RowData
import Pact.Types.Runtime hiding (ChainId)
import Pact.Types.SQLite

import Test.Tasty
import Test.Tasty.HUnit

import System.Logger (LogLevel(..))

-- internal imports

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

    -- init genesis
    let hash00 = getArbitrary 0
    void $ _cpRestore _cpeCheckpointer Nothing
    _cpSave _cpeCheckpointer hash00

    let hash01 = getArbitrary 1
        hash02 = getArbitrary 2
        hash03 = getArbitrary 3
    void $ _cpRestore _cpeCheckpointer (Just (1, hash00))
    _cpSave _cpeCheckpointer hash01
    (PactDbEnv' (PactDbEnv pactdb mvar)) <- _cpRestore _cpeCheckpointer (Just (2, hash01))

    let withTx f = do
          void $ _beginTx pactdb Transactional mvar
          void f
          void $ _commitTx pactdb mvar
        rd1 = RowData RDV1 $ ObjectMap $ M.fromList [("a", RDLiteral (LBool False))]
        rd2 = RowData RDV1 $ ObjectMap $ M.fromList [("a", RDLiteral (LBool True))]

    withTx $ do
      _createUserTable pactdb "tA" "mod" mvar
      _createUserTable pactdb "tAA" "mod" mvar

    withTx $ do
      _writeRow pactdb Insert (UserTables "tA") "AA" rd1 mvar -- test table+key collision
      _writeRow pactdb Insert (UserTables "tAA") "A" rd1 mvar
      _writeRow pactdb Insert (UserTables "tA") "B" rd2 mvar -- test row+key collision
      _writeRow pactdb Insert (UserTables "tAA") "B" rd2 mvar

    withTx $ do
      _writeRow pactdb Update (UserTables "tA") "B" rd2 mvar -- test update to same data collision
      _writeRow pactdb Insert (UserTables "tA") "C" rd1 mvar -- new insert

    _cpSave _cpeCheckpointer hash02
    void $ _cpRestore _cpeCheckpointer $ Just (3,hash02)

    withTx $ do
      _writeRow pactdb Update (UserTables "tA") "B" rd1 mvar -- test updated hash x2
      _writeRow pactdb Update (UserTables "tA") "C" rd2 mvar -- test updated hash

    _cpSave _cpeCheckpointer hash03

    let calcHash tbl rk txid hsh = qry _sConn
            ( "SELECT sha3_256('T',?1,'K',rowkey,'I',txid,'D',rowdata,'H',?4) FROM "
              <> tbl <> " where rowkey=?2 and txid=?3" )
            [SText tbl,SText rk,SInt txid,hsh] [RBlob]

        getHash tbl rk txid = qry _sConn
            ( "SELECT hash FROM " <> tbl <> " where rowkey=?2 and txid=?3" )
            [SText tbl,SText rk,SInt txid] [RBlob]

        -- to test, regenerate hash from db with provided hash value
        checkHash tbl rk txid hsh = do
          h <- calcHash tbl rk txid hsh
          h' <- getHash tbl rk txid
          assertEqual ("checkHash: tbl=" ++ show tbl ++ ",key=" ++ show rk ++ ",txid=" ++ show txid) h h'
          case h of
            [[h'']] -> return h''
            _ -> assertFailure $ "expected single col/row:" ++ show (tbl,rk,txid,h)

        -- sha3 funs treats NULL as empty string
        nullHash = SBlob mempty

        assertNotEquals msg v1 v2 = assertSatisfies msg v1 (/= v2)


    hA_AA1 <- checkHash "tA" "AA" 1 nullHash
    hAA_A1 <- checkHash "tAA" "A" 1 nullHash
    assertNotEquals "table+key collision" hAA_A1 hA_AA1

    hA_B1 <- checkHash "tA" "B" 1 nullHash
    hAA_B1 <- checkHash "tAA" "B" 1 nullHash
    assertNotEquals "row+key collision" hA_B1 hAA_B1

    hA_B2 <- checkHash "tA" "B" 2 hA_B1
    assertNotEquals "update to same data collision" hA_B2 hA_B1
    hC2 <- checkHash "tA" "C" 2 nullHash

    hA_B3 <- checkHash "tA" "B" 3 hA_B2
    assertNotEquals "B 2->3 hash" hA_B3 hA_B2
    hC3 <- checkHash "tA" "C" 3 hC2
    assertNotEquals "C 2->3 hash" hC3 hC2


    withDefaultLogger Debug $ \l ->

      runCompactM (mkCompactEnv l _sConn 2 [Flag_KeepCompactTables]) $ do

        gh <- compact

        -- use DB to compute grand hashes to check per-table and global results

        let checkGrandHash :: Maybe Utf8 -> [[SType]] -> CompactM ByteString
            checkGrandHash tbl [[SBlob hsh]] = do
              h <- readGrandHash tbl
              liftIO $ assertEqual ("checkHash: " ++ show tbl) hsh h
              return $ hsh
            checkGrandHash tbl _ = liftIO $ assertFailure $ "query failure: " ++ show tbl

        hshA <-
          liftIO (qry _sConn "select sha3_256(?1,?2,?3)"
                  [hA_AA1,hA_B2,hC2] [RBlob])
            >>= checkGrandHash (Just "tA")
        hshAA <-
          liftIO (qry _sConn "select sha3_256(?1,?2)"
                  [hAA_A1, hAA_B1] [RBlob])
            >>= checkGrandHash (Just "tAA")
        gh' <- liftIO (qry _sConn "select sha3_256(?1,?2)"
                       [SBlob hshA,SBlob hshAA] [RBlob])
            >>= checkGrandHash Nothing

        liftIO $ assertEqual "global hash check" gh' gh


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
    initialBlockState = set bsModuleNameFix True $ initBlockState $ genesisHeight testVer testChainId
    logger = newLogger (pactTestLogger False) "RelationalCheckpointer"
