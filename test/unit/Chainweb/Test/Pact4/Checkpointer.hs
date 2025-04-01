{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Chainweb.Test.Pact4.Checkpointer (tests) where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad (when, void)
import Control.Monad.Reader

import Data.Aeson (Value(..), object, (.=), Key)
import Data.Function
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import qualified Streaming.Prelude as Stream

import Pact.Gas
import Pact.Interpreter (EvalResult(..), PactDbEnv(..), defaultInterpreter)
import Pact.JSON.Legacy.Value
import Pact.Native (nativeDefs)
import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Command
import qualified Pact.Types.Hash as H
import Pact.Types.PactValue
import Pact.Types.RowData
import Pact.Types.Runtime hiding (ChainId)
import Pact.Types.SPV (noSPVSupport)
import Pact.Types.SQLite
import qualified Pact.JSON.Encode as J
import qualified Pact.Utils.StableHashMap as SHM

import Test.Tasty
import Test.Tasty.HUnit

-- internal imports
import Chainweb.BlockHash
import Chainweb.BlockHeader.Internal
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.MerkleLogHash (merkleLogHash)
import Chainweb.MerkleUniverse
import Chainweb.Pact4.Backend.ChainwebPactDb

import Chainweb.Pact.Backend.Utils
import Chainweb.Pact4.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Test.Pact4.Utils
import Chainweb.Test.Utils
import Chainweb.Test.TestVersions
import Chainweb.Utils
import Chainweb.Version

import Chainweb.Test.Orphans.Internal ({- Arbitrary BlockHash -})
import Chainweb.Pact.Backend.Types
import qualified Chainweb.Pact.PactService.Checkpointer.Internal as Checkpointer
import qualified Chainweb.Pact5.Backend.ChainwebPactDb as Pact5

-- -------------------------------------------------------------------------- --
-- Tests

tests :: TestTree
tests = testGroup "Checkpointer"
    [ testRelational
    , testKeyset
    , testModuleName
    , testCaseSteps "PactDb Regression" testRegress
    , testCaseSteps "readRow unitTest" readRowUnitTest
    ]

-- -------------------------------------------------------------------------- --
-- Module Name Test

testModuleName :: TestTree
testModuleName = withResourceT withTempSQLiteResource $ \s ->
    runSQLite' f s
    where
      f = \resIO -> testCase "testModuleName" $ do

        (cp, sql) <- resIO

        -- init genesis
        let
          hash00 = getArbitrary 0
          pc00 = childOf Nothing hash00
        cpRestoreAndSave cp Nothing
          [(pc00, \_ -> return ())]

        let
          hash01 = getArbitrary 1
          pc01 = childOf (Just pc00) hash01
        cpRestoreAndSave cp (Just pc00)
          [(pc01, \_ -> return ())
          ]
        let
          hash02 = getArbitrary 2
          pc02 = childOf (Just pc01) hash02
        cpRestoreAndSave cp (Just pc01)
          [(pc02, \(PactDbEnv pactdb mvar) -> do
            -- block 2: write module records
            (_,_,mod') <- loadModule
            -- write qualified
            _writeRow pactdb Insert Modules "nsname.qualmod" mod' mvar
            -- write unqualified
            _writeRow pactdb Insert Modules "baremod" mod' mvar
            )]

        r1 <- qry_ sql "SELECT rowkey FROM [SYS:Modules] WHERE rowkey LIKE '%qual%'" [RText]
        assertEqual "correct namespaced module name" [[SText "nsname.qualmod"]] r1

        r2 <- qry_ sql "SELECT rowkey FROM [SYS:Modules] WHERE rowkey LIKE '%bare%'" [RText]
        assertEqual "correct bare module name" [[SText "baremod"]] r2

-- -------------------------------------------------------------------------- --
-- Key Set Test

testKeyset :: TestTree
testKeyset = withResource initializeSQLite freeSQLiteResource $ \s -> runSQLite keysetTest s

keysetTest :: Logger logger => IO (Checkpointer logger) -> TestTree
keysetTest c = testCaseSteps "Keyset test" $ \next -> do
    cp <- c
    let
      hash00 = nullBlockHash
      pc00 = childOf Nothing hash00


    next "init"
    cpRestoreAndSave cp Nothing [(pc00, \_ -> pure ())]

    next "next block (blockheight 1, version 0)"
    cpReadFrom cp (Just pc00) $ \dbEnv ->
      addKeyset dbEnv "k2" (mkKeySet [] ">=")


    next "fork on blockheight = 1"
    hash11 <- BlockHash <$> liftIO (merkleLogHash @_ @ChainwebMerkleHashAlgorithm "0000000000000000000000000000001b")
    let pc11 = childOf (Just pc00) hash11
    cpRestoreAndSave cp (Just pc00) [(pc11, \dbEnv ->
      addKeyset dbEnv "k1" (mkKeySet [] ">=")
      )]

-- -------------------------------------------------------------------------- --
-- CheckPointer Test

testRelational :: TestTree
testRelational =
  withRelationalCheckpointerResource $
    checkpointerTest "Relational Checkpointer" True

checkpointerTest :: (Logger logger) => String -> Bool -> IO (Checkpointer logger) -> TestTree
checkpointerTest name relational cenvIO = testCaseSteps name $ \next -> do
    cenv <- cenvIO
    let
      cp = cenv
      readFrom = cpReadFrom cp
      restoreAndSave = cpRestoreAndSave cp
    ------------------------------------------------------------------
    -- s01 : new block workflow (restore -> discard), genesis
    ------------------------------------------------------------------

    runTwice next $ do
        next "Step 1 : new block workflow (restore -> discard), genesis"
        readFrom Nothing $ \dbEnv -> do
          void $ runExec cenv dbEnv (Just $ ksData "1") $ defModule "1"
          runExec cenv dbEnv Nothing "(m1.readTbl)" >>=
            \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]


    -----------------------------------------------------------
    -- s02 : validate block workflow (restore -> save), genesis
    -----------------------------------------------------------

    let hash00 = nullBlockHash
    next "Step 2 : validate block workflow (restore -> save), genesis"
    let pc00 = childOf Nothing hash00
    restoreAndSave Nothing
      [(pc00, \dbEnv -> do
        void $ runExec cenv dbEnv (Just $ ksData "1") $ defModule "1"
        void $ runExec cenv dbEnv Nothing "(m1.readTbl)"
          >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
        )]

    ------------------------------------------------------------------
    -- s03 : new block 00
    ------------------------------------------------------------------

    next "Step 3 : new block 00"
    let pactCheckStep = preview (_Just . peStep) . _erExec
    let pactId = "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g"
    void $ readFrom (Just pc00) $ \dbEnv -> do
    -- start a pact
    -- test is that exec comes back with proper step
      void $ runExec cenv dbEnv Nothing "(m1.insertTbl 'b 2)"
      runExec cenv dbEnv Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]
      runExec cenv dbEnv Nothing "(m1.dopact 'pactA)" >>= ((Just 0 @=?) . pactCheckStep)

    ------------------------------------------------------------------
    -- s04: validate block 1
    ------------------------------------------------------------------

    next "Step 4: validate block 1"
    hash01 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001a")
    let pc01 = childOf (Just pc00) hash01
    restoreAndSave (Just pc00)
      [(pc01, \dbEnv -> do
        void $ runExec cenv dbEnv Nothing "(m1.insertTbl 'b 2)"
        runExec cenv dbEnv Nothing "(m1.readTbl)"
          >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]
        runExec cenv dbEnv Nothing "(m1.dopact 'pactA)"
          >>= ((Just 0 @=?) . pactCheckStep)
      )]

    ------------------------------------------------------------------
    -- s05: validate block 02
    -- create m2 module, exercise RefStore checkpoint
    -- exec next part of pact
    ------------------------------------------------------------------

    let msg = "Step 5: validate block 02\n create m2 module, exercise RefStore checkpoint\n exec next part of pact"
    next msg
    hash02 <- BlockHash <$> merkleLogHash "0000000000000000000000000000002a"
    let pc02 = childOf (Just pc01) hash02
    restoreAndSave (Just pc01)
      [(pc02, \dbEnv -> do
        void $ runExec cenv dbEnv (Just $ ksData "2") $ defModule "2"
        runExec cenv dbEnv Nothing "(m2.readTbl)"
          >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
        runCont cenv dbEnv pactId 1
          >>= ((Just 1 @=?) . pactCheckStep)
        )]

    ------------------------------------------------------------------
    -- s06 : new block 03
    ------------------------------------------------------------------

    next "Step 6 : new block 03"
    readFrom (Just pc02) $ \dbEnv -> do
      void $ runExec cenv dbEnv Nothing "(m2.insertTbl 'b 2)"
      runExec cenv dbEnv Nothing "(m2.readTbl)"
        >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

    ------------------------------------------------------------------
    -- s07 : validate block 03
    ------------------------------------------------------------------

    next "Step 7 : validate block 03"
    hash03 <- BlockHash <$> merkleLogHash "0000000000000000000000000000003a"
    let pc03 = childOf (Just pc02) hash03
    restoreAndSave (Just pc02)
      [(pc03, \dbEnv -> do
        void $ runExec cenv dbEnv Nothing "(m2.insertTbl 'b 2)"
        runExec cenv dbEnv Nothing "(m2.readTbl)"
          >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]
        )]
    -- insert here would fail if new block 03 had not been discarded

    ------------------------------------------------------------------
    -- s08: FORK! block 02, new hash
    -- recreate m2 module, exercise RefStore checkpoint
    -- exec next part of pact
    ------------------------------------------------------------------

    next "Step 8: FORK! block 02, new hash\n recreate m2 module, exercise RefStore checkpoint\n exec next part of pact"
    hash02Fork <- BlockHash <$> merkleLogHash "0000000000000000000000000000002b"
    let pc02Fork = childOf (Just pc01) hash02Fork
    restoreAndSave (Just pc01)
      [(pc02Fork, \dbEnv -> do
        void $ runExec cenv dbEnv (Just $ ksData "2") $ defModule "2"
        runExec cenv dbEnv Nothing "(m2.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
        -- this would fail if not a fork
        runCont cenv dbEnv pactId 1 >>= ((Just 1 @=?) . pactCheckStep)
        )]

    next "step 9: test update row: new block 03"
    readFrom (Just pc02Fork) $ \dbEnv -> do
      void $ runExec cenv dbEnv Nothing "(m1.updateTbl 'b 3)"
      runExec cenv dbEnv Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,3]]
    -- updating key previously written at blockheight 1 (the "restore point")

    next "step 10: test update row: validate block 03"
    hash13 <- BlockHash <$> merkleLogHash "0000000000000000000000000000003b"
    let pc13 = childOf (Just pc02Fork) hash13
    restoreAndSave (Just pc02Fork)
      [(pc13, \dbEnv -> do
        void $ runExec cenv dbEnv Nothing "(m1.updateTbl 'b 3)"
        runExec cenv dbEnv Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,3]]
        )]
    -- updating key previously written at blockheight 1 (the "restore point")

    next "mini-regression test for dropping user tables (part 1) empty block"
    hash04 <- BlockHash <$> merkleLogHash "0000000000000000000000000000004a"
    let pc04 = childOf (Just pc13) hash04
    restoreAndSave (Just pc13)
      [(pc04, \_ -> return ())]

    next "mini-regression test for dropping user tables (part 2) (create module with offending table)"
    hash05 <- BlockHash <$> merkleLogHash "0000000000000000000000000000005a"
    let pc05 = childOf (Just pc04) hash05
    restoreAndSave (Just pc04)
      [(pc05, \dbEnv -> do
        void $ runExec cenv dbEnv (Just $ ksData "5") $ defModule "5"
        runExec cenv dbEnv Nothing "(m5.readTbl)" >>=
          \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
        )]

    next "mini-regression test for dropping user tables (part 3) (reload the offending table)"
    hash05Fork <- BlockHash <$> merkleLogHash "0000000000000000000000000000005b"
    let pc05Fork = childOf (Just pc04) hash05Fork
    restoreAndSave (Just pc04)
      [(pc05Fork, \dbEnv -> do
        void $ runExec cenv dbEnv (Just $ ksData "5") $ defModule "5"
        runExec cenv dbEnv Nothing "(m5.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
        )]

    next "2nd mini-regression test for debugging updates (part 1) empty block"
    hash14 <- BlockHash <$> merkleLogHash "0000000000000000000000000000004b"
    let pc14 = childOf (Just pc13) hash14
    restoreAndSave (Just pc13)
      [(pc14, \_ -> return ())]

    next "2nd mini-regression test for debugging updates (part 2) create module & table"
    hash15 <- BlockHash <$> merkleLogHash "0000000000000000000000000000005c"
    let pc15 = childOf (Just pc14) hash15
    restoreAndSave (Just pc14)
      [(pc15, \dbEnv -> do
        void $ runExec cenv dbEnv (Just $ ksData "6") $ defModule "6"
        runExec cenv dbEnv Nothing "(m6.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
        )]

    next "2nd mini-regression test for debugging updates (part 3) step 1 of insert value then update twice"
    hash06 <- BlockHash <$> merkleLogHash "0000000000000000000000000000006a"
    let pc06 = childOf (Just pc15) hash06
    restoreAndSave (Just pc15)
      [(pc06, \dbEnv -> do
        void $ runExec cenv dbEnv Nothing "(m6.insertTbl 'b 2)"
        )]

    next "2nd mini-regression test for debugging updates (part 4) step 2 of insert value then update twice"
    hash07 <- BlockHash <$> merkleLogHash "0000000000000000000000000000007a"
    let pc07 = childOf (Just pc06) hash07
    restoreAndSave (Just pc06)
      [(pc07, \dbEnv -> do
        void $ runExec cenv dbEnv Nothing "(m6.weirdUpdateTbl 'b 4)"
        )]

    next "2nd mini-regression test for debugging updates (part 5) step 3 of insert value then update twice"
    hash08 <- BlockHash <$> merkleLogHash "0000000000000000000000000000008a"
    let pc08 = childOf (Just pc07) hash08
    restoreAndSave (Just pc07)
      [(pc08, \dbEnv -> do
        runExec cenv dbEnv Nothing "(m6.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,4]]
        )]
    -- FOR DEBUGGING/INSPECTING VALUES AT SPECIFIC KEYS
    -- void $ runExec cenv blockEnv09 Nothing "(let ((written (at 'col (read m6.tbl 'a [\"col\"])))) (enforce (= written 1) \"key a\"))"
    -- void $ runExec cenv blockEnv09 Nothing "(let ((written (at 'col (read m6.tbl 'b [\"col\"])))) (enforce (= written 4) \"key b\"))"
    -- FOR DEBUGGING/INSPECTING VALUES AT SPECIFIC KEYS

    next "Create the free namespace for the test"
    hash09 <- BlockHash <$> merkleLogHash "0000000000000000000000000000009a"
    let pc09 = childOf (Just pc08) hash09
    restoreAndSave (Just pc08)
      [(pc09, \dbEnv -> do
        void $ runExec cenv dbEnv Nothing defFree
      )]
    hash10 <- BlockHash <$> merkleLogHash "0000000000000000000000000000010a"

    next "Don't create the same table twice in the same block"
    let tKeyset = object ["test-keyset" .= object ["keys" .= ([] :: [Text]), "pred" .= String ">="]]
    readFrom (Just pc09) $ \dbEnv -> do
      void $ runExec cenv dbEnv (Just tKeyset) tablecode
      expectException "The table duplication somehow went through. Investigate this error." $
        runExec cenv dbEnv (Just tKeyset) tablecode


    next "Don't create the same table twice in the same transaction."

    readFrom (Just pc09) $ \dbEnv ->
      expectException "The table duplication somehow went through. Investigate this error." $
        runExec cenv dbEnv (Just tKeyset) (tablecode <> tablecode)


    next "Don't create the same table twice over blocks."

    let pc10 = childOf (Just pc09) hash10
    restoreAndSave (Just pc09)
      [(pc10, \dbEnv -> do
        void $ runExec cenv dbEnv (Just tKeyset) tablecode
        )
      ]


    hash11 <- BlockHash <$> merkleLogHash "0000000000000000000000000000011a"
    let pc11 = childOf (Just pc10) hash11
    restoreAndSave (Just pc10)
      [(pc11, \dbEnv -> do
        expectException "The table duplication somehow went through. Investigate this error." $
          runExec cenv dbEnv (Just tKeyset) tablecode
        )]


    next "Purposefully restore to an illegal checkpoint."

    let pc10Invalid = pc10 & blockHeight .~ 13
    void $ expectException "Illegal checkpoint successfully restored to" $
      readFrom (Just pc10Invalid) $ \_ -> return ()

    when relational $ do

        next "Rewind to block 5"

        next "Run block 5b with pact 4.2.0 changes"

        readFrom (Just pc14) $ \dbEnv -> do

          void $ runExec cenv dbEnv (Just $ ksData "7") (defModule "7")
          void $ runExec cenv dbEnv Nothing "(m7.insertTbl 'b 2)"
          void $ runExec cenv dbEnv Nothing "(m7.insertTbl 'd 3)"
          void $ runExec cenv dbEnv Nothing "(m7.insertTbl 'c 4)"
          void $ runExec cenv dbEnv Nothing "(keys m7.tbl)" >>= \EvalResult{..} ->
            Right _erOutput @?= traverse toPactValue [tStringList $ T.words "a b c d"]

        next "Rollback to block 4 and expect failure with pact 4.2.0 changes"

        readFrom (Just pc13) $ \dbEnv -> do
          void $ runExec cenv dbEnv (Just $ ksData "7") (defModule "7")
          void $ runExec cenv dbEnv Nothing "(m7.insertTbl 'b 2)"
          void $ runExec cenv dbEnv Nothing "(m7.insertTbl 'd 3)"
          void $ runExec cenv dbEnv Nothing "(m7.insertTbl 'c 4)"
          runExec cenv dbEnv Nothing "(keys m7.tbl)" >>= \EvalResult{..} ->
            assertBool "order should be different"
              (Right _erOutput /= traverse toPactValue [tStringList $ T.words "a b c d"])

  where

    h :: SomeException -> IO (Maybe String)
    h = const (return Nothing)

    expectException msg act = do
        result <- (act >> return (Just msg)) `catchAllSynchronous` h
        maybe (return ()) (`assertBool` False) result

    ksData :: Key -> Value
    ksData idx = object
        [ ("k" <> idx) .= object
            [ "keys" .= ([] :: [Text])
            , "pred" .= String ">="
            ]
        ]

-- -------------------------------------------------------------------------- --
-- Read Row Unit Test

readRowUnitTest :: (String -> IO ()) -> Assertion
readRowUnitTest logBackend = simpleBlockEnvInit logger runUnitTest
  where
    logger = hunitDummyLogger logBackend
    writeRow' pactdb writeType conn i =
      _writeRow pactdb writeType (UserTables "user1") "key1"
      (RowData RDV1 (ObjectMap $ M.fromList [("f", (RDLiteral (LInteger i)))])) conn
    runUnitTest pactdb e schemaInit = do
      conn <- newMVar e
      void $ schemaInit conn
      let user1 = "user1"
          usert = UserTables user1
      void $ begin pactdb conn
      _createUserTable pactdb user1 "someModule" conn
      writeRow' pactdb Insert conn 1
      void $ commit pactdb conn
      let numWrites = 100 :: Int
          loop current =
            if current == numWrites
              then return ()
              else do
                void $ begin pactdb conn
                writeRow' pactdb Update conn $
                   fromIntegral $ current + 1
                void $ commit pactdb conn
                loop (current + 1)
      loop 1
      r <- _readRow pactdb usert "key1" conn
      case r of
        Nothing -> assertFailure "Unsuccessful write"
        Just (RowData _ (ObjectMap m)) -> case M.lookup "f" m of
          Just l -> assertEquals "Unsuccesful write at field key \"f\"" l (RDLiteral (LInteger 100))
          Nothing -> assertFailure "Field not found"

-- -------------------------------------------------------------------------- --
-- Test Regress

testRegress :: (String -> IO ()) -> Assertion
testRegress logBackend =
    regressChainwebPactDb logger
        >>= fmap (toTup . _benvBlockState) . readMVar
        >>= assertEquals "The final block state is" finalBlockState
  where
    logger = hunitDummyLogger logBackend
    finalBlockState = 2
    toTup BlockState { _bsTxId = txid } = txid

regressChainwebPactDb :: (Logger logger) => logger -> IO (MVar (BlockEnv logger))
regressChainwebPactDb logger = simpleBlockEnvInit logger runRegression

{- this should be moved to pact -}
runRegression
    :: PactDb e -- your pactdb instance
    -> e -- ambient environment
    -> (MVar e -> IO ()) -- schema "creator"
    -> IO (MVar e) -- the final state of the environment
runRegression pactdb e schemaInit = do
    conn <- newMVar e
    schemaInit conn
    Just t1 <- begin pactdb conn
    let user1 = "user1"
        usert = UserTables user1
        toPV :: ToTerm a => a -> RowDataValue
        toPV = pactValueToRowData . toPactValueLenient . toTerm'
    _createUserTable pactdb user1 "someModule" conn
    assertEquals' "output of commit2"
        [ encodeTxLog $ TxLog "SYS:usertables" "user1" $
            J.object
                [ "utModule" J..= J.object
                    [ "namespace" J..= J.null
                    , "name" J..= J.text "someModule"
                    ]
               ]
        ]
        (commit pactdb conn)

    void $ begin pactdb conn
    let row = RowData RDV1 $ ObjectMap $ M.fromList [("gah", RDLiteral (LDecimal 123.454345))]
    _writeRow pactdb Insert usert "key1" row conn
    assertEquals' "usert insert" (Just row) (_readRow pactdb usert "key1" conn)
    let row' = RowData RDV1 $ ObjectMap $ M.fromList [("gah",toPV False),("fh",toPV (1 :: Int))]
    _writeRow pactdb Update usert "key1" row' conn
    assertEquals' "user update" (Just row') (_readRow pactdb usert "key1" conn)
    let ks = mkKeySet [PublicKeyText "skdjhfskj"] "predfun"
    _writeRow pactdb Write KeySets "ks1" ks conn
    assertEquals' "keyset write" (Just ks) $ _readRow pactdb KeySets "ks1" conn
    (modName,modRef,mod') <- loadModule
    _writeRow pactdb Write Modules modName mod' conn
    assertEquals' "module write" (Just mod') $ _readRow pactdb Modules modName conn
    assertEquals "module native repopulation" (Right modRef) $
      traverse (traverse (fromPersistDirect nativeLookup)) mod'
    assertEquals' "result of commit 3"
        [ encodeTxLog TxLog
            { _txDomain = "SYS:KeySets"
            , _txKey = "ks1"
            , _txValue = ks
            }
        , encodeTxLog TxLog
            { _txDomain = "SYS:Modules"
            , _txKey = asString modName
            , _txValue = mod'
            }
        , encodeTxLog TxLog
            { _txDomain = "user1"
            , _txKey = "key1"
            , _txValue = row
            }
        , encodeTxLog TxLog
            { _txDomain = "user1"
            , _txKey = "key1"
            , _txValue = row'
            }
        ]
        (commit pactdb conn)
    void $ begin pactdb conn
    tids <- _txids pactdb user1 t1 conn
    assertEquals "user txids" [1] tids
    -- assertEquals' "user txlogs"
    --   [TxLog "user1" "key1" row,
    --    TxLog "user1" "key1" row'] $
    --   _getTxLog chainwebpactdb usert (head tids) conn
    assertEquals' "user txlogs" [TxLog "user1" "key1" (RowData RDV1 (ObjectMap $ on M.union (_objectMap . _rdData) row' row))] $
        _getTxLog pactdb usert (head tids) conn
    _writeRow pactdb Insert usert "key2" row conn
    assertEquals' "user insert key2 pre-rollback" (Just row) (_readRow pactdb usert "key2" conn)
    assertEquals' "keys pre-rollback" ["key1","key2"] $ _keys pactdb (UserTables user1) conn
    _rollbackTx pactdb conn
    assertEquals' "rollback erases key2" Nothing $ _readRow pactdb usert "key2" conn
    assertEquals' "keys" ["key1"] $ _keys pactdb (UserTables user1) conn
    return conn

-- -------------------------------------------------------------------------- --
-- Chainweb Settings

testVer :: ChainwebVersion
testVer = slowForkingCpmTestVersion petersen

testChainId :: ChainId
testChainId = unsafeChainId 0

-- -------------------------------------------------------------------------- --
-- Testing Utils

assertEquals' :: (Eq a, Show a, NFData a) => String -> a -> IO a -> IO ()
assertEquals' msg a b = assertEquals msg a =<< b

assertEquals :: (Eq a,Show a,NFData a) => String -> a -> a -> IO ()
assertEquals msg a b
    | [a,b] `deepseq` a == b = return ()
    | otherwise = throwFail
        $ "FAILURE: " ++ msg
        ++ ": expected \n  " ++ show a ++ "\n got \n  " ++ show b

throwFail :: String -> IO a
throwFail = throwIO . userError

-- -------------------------------------------------------------------------- --
-- Checkpointer Utils

withRelationalCheckpointerResource
    :: (Logger logger, logger ~ GenericLogger)
    => (IO (Checkpointer logger) -> TestTree)
    -> TestTree
withRelationalCheckpointerResource f =
    withResource initializeSQLite freeSQLiteResource $ \s -> runSQLite f s

addKeyset :: PactDbEnv (BlockEnv logger) -> KeySetName -> KeySet -> IO ()
addKeyset (PactDbEnv pactdb mvar) keysetname keyset =
    _writeRow pactdb Insert KeySets keysetname keyset mvar

runTwice :: MonadIO m => (String -> IO ()) -> m () -> m ()
runTwice step action = do
  liftIO $ step "Running the first time!"
  action
  liftIO $ step "Running the second time!"
  action

runSQLite
    :: (Logger logger, logger ~ GenericLogger)
    => (IO (Checkpointer logger) -> TestTree)
    -> IO SQLiteEnv
    -> TestTree
runSQLite f = runSQLite' (f . fmap fst)

runSQLite'
    :: (Logger logger, logger ~ GenericLogger)
    => (IO (Checkpointer logger, SQLiteEnv) -> TestTree)
    -> IO SQLiteEnv
    -> TestTree
runSQLite' runTest sqlEnvIO = runTest $ do
    sqlenv <- sqlEnvIO
    cp <- Checkpointer.initCheckpointerResources defaultModuleCacheLimit sqlenv DoNotPersistIntraBlockWrites logger testVer testChainId
    return (cp, sqlenv)
  where
    logger = addLabel ("sub-component", "relational-checkpointer") $ dummyLogger

runExec :: forall logger. (Logger logger) => Checkpointer logger -> PactDbEnv (BlockEnv logger) -> Maybe Value -> Text -> IO EvalResult
runExec cp pactdbenv eData eCode = do
    execMsg <- buildExecParsedCode maxBound {- use latest parser version -} eData eCode
    evalTransactionM cmdenv cmdst $
      applyExec' 0 defaultInterpreter execMsg [] [] h' permissiveNamespacePolicy
  where
    h' = H.toUntypedHash (H.hash "" :: H.PactHash)
    cmdenv :: TransactionEnv logger (BlockEnv logger)
    cmdenv = TransactionEnv
        { _txMode = Transactional
        , _txDbEnv = pactdbenv
        , _txLogger = cpLogger cp
        , _txGasLogger = Nothing
        , _txPublicData = noPublicData
        , _txSpvSupport = noSPVSupport
        , _txNetworkId = Nothing
        , _txGasPrice = 0.0
        , _txRequestKey = RequestKey h'
        , _txGasLimit = 0
        , _txExecutionConfig = emptyExecutionConfig
        , _txQuirkGasFee = Nothing
        , _txTxFailuresCounter = Nothing
        }
    cmdst = TransactionState mempty mempty 0 Nothing (_geGasModel freeGasEnv) mempty

runCont :: Logger logger => Checkpointer logger -> PactDbEnv (BlockEnv logger) -> PactId -> Int -> IO EvalResult
runCont cp pactdbenv pactId step = do
    evalTransactionM cmdenv cmdst $
      applyContinuation' 0 defaultInterpreter contMsg [] h' permissiveNamespacePolicy
  where
    contMsg = ContMsg pactId step False (toLegacyJson Null) Nothing

    h' = H.toUntypedHash (H.hash "" :: H.PactHash)
    cmdenv = TransactionEnv
        { _txMode = Transactional
        , _txDbEnv = pactdbenv
        , _txLogger = cpLogger cp
        , _txGasLogger = Nothing
        , _txPublicData = noPublicData
        , _txSpvSupport = noSPVSupport
        , _txNetworkId = Nothing
        , _txGasPrice = 0.0
        , _txRequestKey = RequestKey h'
        , _txGasLimit = 0
        , _txExecutionConfig = emptyExecutionConfig
        , _txQuirkGasFee = Nothing
        , _txTxFailuresCounter = Nothing
        }
    cmdst = TransactionState mempty mempty 0 Nothing (_geGasModel freeGasEnv) mempty

-- -------------------------------------------------------------------------- --
-- Pact Utils

-- witnessing that we only use the PactDbEnv portion
-- of the CurrentBlockDbEnv
cpReadFrom
  :: Logger logger
  => Checkpointer logger
  -> Maybe BlockHeader
  -> (PactDbEnv (BlockEnv logger) -> IO q)
  -> IO q
cpReadFrom cp pc f = do
  Checkpointer.readFrom
    cp
    (ParentHeader <$> pc)
    Pact4T
    (\env _blockHandle -> f $ (_cpPactDbEnv env)) >>= \case
    NoHistory -> error $ unwords
      [ "Chainweb.Test.Pact4.Checkpointer.cpReadFrom:"
      , "parent header missing from the database"
      ]
    Historical r -> return r

-- allowing a straightforward list of blocks to be passed to the API,
-- and only exposing the PactDbEnv part of the block context
cpRestoreAndSave
  :: (Logger logger, Monoid q)
  => Checkpointer logger
  -> Maybe BlockHeader
  -> [(BlockHeader, PactDbEnv (BlockEnv logger) -> IO q)]
  -> IO q
cpRestoreAndSave cp pc blks = snd <$> Checkpointer.restoreAndSave cp (ParentHeader <$> pc)
  (traverse Stream.yield
    [Pact4RunnableBlock $ \dbEnv _ -> (,bh) <$> (fun $ _cpPactDbEnv dbEnv) | (bh, fun) <- blks])

-- | fabricate a `BlockHeader` for a block given its hash and its parent.
childOf :: Maybe BlockHeader -> BlockHash -> BlockHeader
childOf m bhsh = case m of
  Just bh -> bh
    & blockHash .~ bhsh
    & blockParent .~ view blockHash bh
    & blockHeight .~ view blockHeight bh + 1
  Nothing -> genesisBlockHeader testVer testChainId
    & blockHash .~ bhsh

-- initialize a block env without actually restoring the checkpointer, before
-- genesis.
simpleBlockEnvInit
    :: (Logger logger)
    => logger
    -> (PactDb (BlockEnv logger) -> BlockEnv logger -> (MVar (BlockEnv logger) -> IO ()) -> IO a)
    -> IO a
simpleBlockEnvInit logger f = withTempSQLiteConnection chainwebPragmas $ \sqlenv ->
    f chainwebPactDb (blockEnv sqlenv) (\_ -> Pact5.initSchema sqlenv)
  where
    blockEnv sqlenv = BlockEnv
      (mkBlockHandlerEnv testVer testChainId (BlockHeight 0) sqlenv DoNotPersistIntraBlockWrites logger)
      (initBlockState defaultModuleCacheLimit (TxId 0))

{- this should be moved to pact -}
begin :: PactDb e -> Method e (Maybe TxId)
begin pactdb = _beginTx pactdb Transactional

{- this should be moved to pact -}
commit :: PactDb e -> Method e [TxLogJson]
commit pactdb = _commitTx pactdb

loadModule :: IO (ModuleName, ModuleData Ref, PersistModuleData)
loadModule = do
    (r,s) <- execScript' (Script False fn) fn
    case r of
        Left a -> throwFail $ "module load failed: " ++ show a
        Right _ -> case preview (rEvalState . evalRefs . rsLoadedModules . ix mn) s of
            Just (md,_) -> case traverse (traverse toPersistDirect) md of
                Right md' -> return (mn,md,md')
                Left e -> throwFail $ "toPersistDirect failed: " ++ show e
            Nothing -> throwFail $ "Failed to find module 'simple': " ++
                show (view (rEvalState . evalRefs . rsLoadedModules) s)
  where
    mn = ModuleName "simple" Nothing
    fn = "test/pact/simple.repl"

nativeLookup :: NativeDefName -> Maybe (Term Name)
nativeLookup (NativeDefName n) = case SHM.lookup n nativeDefs of
    Just (Direct t) -> Just t
    _ -> Nothing

tIntList :: [Int] -> Term Name
tIntList = toTList (TyPrim TyInteger) noInfo . map toTerm

tStringList :: [Text] -> Term Name
tStringList = toTList (TyPrim TyString) noInfo . map toTerm

toTerm' :: ToTerm a => a -> Term Name
toTerm' = toTerm

defFree :: Text
defFree = T.unlines
  [ "(module ezfree G (defcap G () true) (defun ALLOW () true))"
  , "(define-namespace 'free (create-user-guard (ALLOW)) (create-user-guard (ALLOW)))"
  ]

defModule :: Text -> Text
defModule idx = T.unlines
    [ " ;;"
    , ""
    , "(module m" <> idx <> " G"
    , "  (defcap G () true)"
    , "  (defschema sch col:integer)"
    , ""
    , "  (deftable tbl:{sch})"
    , ""
    , "  (defun insertTbl (a i)"
    , "    (insert tbl a { 'col: i }))"
    , ""
    , "  (defun updateTbl (a i)"
    , "    (update tbl a { 'col: i}))"
    , ""
    , "  (defun weirdUpdateTbl (a i)"
    , "    (update tbl a { 'col: 0})"
    , "    (update tbl a { 'col: i}))"
    , ""
    , "  (defun readTbl ()"
    , "    (sort (map (at 'col)"
    , "      (select tbl (constantly true)))))"
    , ""
    , "  (defpact dopact (n)"
    , "    (step { 'name: n, 'value: 1 })"
    , "    (step { 'name: n, 'value: 2 }))"
    , ""
    , ")"
    , "(create-table tbl)"
    , "(readTbl)"
    , "(insertTbl \"a\" 1)"
    ]

tablecode :: Text
tablecode = T.unlines
    [ "(namespace 'free)"
    , "(define-keyset \"free.table-admin-keyset\""
    , "  (read-keyset \"test-keyset\"))"
    , ""
    , "(module table-example \"free.table-admin-keyset\""
    , ""
    , "  (defschema test-schema"
    , "    content:string)"
    , ""
    , "  (deftable test-table:{test-schema})"
    , ""
    , "  (defun add-row (row:string content:string)"
    , "    (insert test-table row {"
    , "      \"content\": content"
    , "      })"
    , "  )"
    , "  (defun read-table ()"
    , "    (select test-table (constantly true))"
    , "  )"
    , ")"
    , ""
    , "(create-table test-table)"
    ]
