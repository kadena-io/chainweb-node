{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Pact.Checkpointer (tests) where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Reader

import Data.Aeson (Value(..), object, toJSON, (.=))
import Data.Default (def)
import Data.Function
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Pact.Gas
import Pact.Interpreter (EvalResult(..), PactDbEnv(..), defaultInterpreter)
import Pact.Native (nativeDefs)
import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Command
import qualified Pact.Types.Hash as H
import Pact.Types.Logger (newLogger)
import Pact.Types.PactValue
import Pact.Types.RowData
import Pact.Types.Runtime hiding (ChainId)
import Pact.Types.SPV (noSPVSupport)
import Pact.Types.SQLite

import Test.Tasty
import Test.Tasty.HUnit

-- internal imports
import Chainweb.BlockHash
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.MerkleLogHash (merkleLogHash)
import Chainweb.MerkleUniverse
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.TransactionExec
    (applyContinuation', applyExec', buildExecParsedCode)
import Chainweb.Pact.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Utils (catchAllSynchronous)
import Chainweb.Version


import Chainweb.Test.Orphans.Internal ({- Arbitrary BlockHash -})

-- -------------------------------------------------------------------------- --
-- Tests

tests :: ScheduledTest
tests = testGroupSch "Checkpointer"
    [ testRelational
    , testKeyset
    , testModuleName
    , testCase "PactDb Regression" testRegress
    , testCase "readRow unitTest" readRowUnitTest
    ]

-- -------------------------------------------------------------------------- --
-- Module Name Test

testModuleName :: TestTree
testModuleName = withTempSQLiteResource $
    runSQLite' $ \resIO -> testCase "testModuleName" $ do

        (CheckpointEnv {..}, SQLiteEnv {..}) <- resIO

        -- init genesis
        let hash00 = getArbitrary 0
        void $ _cpRestore _cpeCheckpointer Nothing
        _cpSave _cpeCheckpointer hash00

        let hash01 = getArbitrary 1
            hash02 = getArbitrary 2
        void $ _cpRestore _cpeCheckpointer (Just (1, hash00))
        _cpSave _cpeCheckpointer hash01
        (PactDbEnv' (PactDbEnv pactdb mvar)) <- _cpRestore _cpeCheckpointer (Just (2, hash01))


        -- block 2: write module records
        (_,_,mod') <- loadModule
        -- write qualified
        _writeRow pactdb Insert Modules "nsname.qualmod" mod' mvar
        -- write unqualified
        _writeRow pactdb Insert Modules "baremod" mod' mvar
        _cpSave _cpeCheckpointer hash02

        r1 <- qry_ _sConn "SELECT rowkey FROM [SYS:Modules] WHERE rowkey LIKE '%qual%'" [RText]
        assertEqual "correct namespaced module name" [[SText "nsname.qualmod"]] r1

        r2 <- qry_ _sConn "SELECT rowkey FROM [SYS:Modules] WHERE rowkey LIKE '%bare%'" [RText]
        assertEqual "correct bare module name" [[SText "baremod"]] r2

-- -------------------------------------------------------------------------- --
-- Key Set Test

testKeyset :: TestTree
testKeyset = withResource initializeSQLite freeSQLiteResource (runSQLite keysetTest)

keysetTest ::  IO CheckpointEnv -> TestTree
keysetTest c = testCaseSteps "Keyset test" $ \next -> do
    CheckpointEnv {..} <- c
    let hash00 = nullBlockHash

    next "init"
    _blockenv00 <- _cpRestore _cpeCheckpointer Nothing
    _cpSave _cpeCheckpointer hash00

    next "next block (blockheight 1, version 0)"
    let bh01 = BlockHeight 1
    _hash01 <- BlockHash <$> liftIO (merkleLogHash @_ @ChainwebMerkleHashAlgorithm "0000000000000000000000000000001a")
    blockenv01 <- _cpRestore _cpeCheckpointer (Just (bh01, hash00))
    addKeyset blockenv01 "k2" (mkKeySet [] ">=")
    _cpDiscard _cpeCheckpointer

    next "fork on blockheight = 1"
    let bh11 = BlockHeight 1
    hash11 <- BlockHash <$> liftIO (merkleLogHash @_ @ChainwebMerkleHashAlgorithm "0000000000000000000000000000001b")
    blockenv11 <- _cpRestore _cpeCheckpointer (Just (bh11, hash00))
    addKeyset blockenv11 "k1" (mkKeySet [] ">=")
    _cpSave _cpeCheckpointer hash11

-- -------------------------------------------------------------------------- --
-- CheckPointer Test

testRelational :: TestTree
testRelational =
  withRelationalCheckpointerResource $
    checkpointerTest "Relational Checkpointer" True

checkpointerTest :: String -> Bool -> IO CheckpointEnv -> TestTree
checkpointerTest name relational cenvIO = testCaseSteps name $ \next -> do
    cenv <- cenvIO
    let cp = _cpeCheckpointer cenv
    ------------------------------------------------------------------
    -- s01 : new block workflow (restore -> discard), genesis
    ------------------------------------------------------------------

    runTwice next $ do
        next "Step 1 : new block workflow (restore -> discard), genesis"
        blockenvGenesis0 <- _cpRestore cp Nothing
        void $ runExec cenv blockenvGenesis0 (Just $ ksData "1") $ defModule "1"
        runExec cenv blockenvGenesis0 Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
        _cpDiscard cp

    -----------------------------------------------------------
    -- s02 : validate block workflow (restore -> save), genesis
    -----------------------------------------------------------

    let hash00 = nullBlockHash
    next "Step 2 : validate block workflow (restore -> save), genesis"
    blockenvGenesis1 <- _cpRestore cp Nothing
    void $ runExec cenv blockenvGenesis1 (Just $ ksData "1") $ defModule "1"
    runExec cenv blockenvGenesis1 Nothing "(m1.readTbl)"
      >>=  \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
    _cpSave cp hash00

    ------------------------------------------------------------------
    -- s03 : new block 00
    ------------------------------------------------------------------

    next "Step 3 : new block 00"
    blockenv00 <- _cpRestore cp (Just (BlockHeight 1, hash00))
    -- start a pact
    -- test is that exec comes back with proper step
    let pactId = "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g"
        pactCheckStep = preview (_Just . peStep) . _erExec
    void $ runExec cenv blockenv00 Nothing "(m1.insertTbl 'b 2)"
    runExec cenv blockenv00 Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]
    runExec cenv blockenv00 Nothing "(m1.dopact 'pactA)" >>= ((Just 0 @=?) . pactCheckStep)
    _cpDiscard cp

    ------------------------------------------------------------------
    -- s04: validate block 1
    ------------------------------------------------------------------

    next "Step 4: validate block 1"
    hash01 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001a")
    blockenv01 <- _cpRestore cp (Just (BlockHeight 1, hash00))
    void $ runExec cenv blockenv01 Nothing "(m1.insertTbl 'b 2)"
    runExec cenv blockenv01 Nothing "(m1.readTbl)"
      >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]
    runExec cenv blockenv01 Nothing "(m1.dopact 'pactA)"
      >>= ((Just 0 @=?) . pactCheckStep)
    _cpSave cp hash01

    ------------------------------------------------------------------
    -- s05: validate block 02
    -- create m2 module, exercise RefStore checkpoint
    -- exec next part of pact
    ------------------------------------------------------------------

    let msg = "Step 5: validate block 02\n create m2 module, exercise RefStore checkpoint\n exec next part of pact"
    next msg
    hash02 <- BlockHash <$> merkleLogHash "0000000000000000000000000000002a"
    blockenv02 <- _cpRestore cp (Just (BlockHeight 2, hash01))
    void $ runExec cenv blockenv02 (Just $ ksData "2") $ defModule "2"
    runExec cenv blockenv02 Nothing "(m2.readTbl)"
      >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
    runCont cenv blockenv02 pactId 1
      >>= ((Just 1 @=?) . pactCheckStep)
    _cpSave cp hash02

    ------------------------------------------------------------------
    -- s06 : new block 03
    ------------------------------------------------------------------

    next "Step 6 : new block 03"
    blockenv03 <- _cpRestore cp (Just (BlockHeight 3, hash02))
    void $ runExec cenv blockenv03 Nothing "(m2.insertTbl 'b 2)"
    runExec cenv blockenv03 Nothing "(m2.readTbl)"
      >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]
    _cpDiscard cp

    ------------------------------------------------------------------
    -- s07 : validate block 03
    ------------------------------------------------------------------

    next "Step 7 : validate block 03"
    hash03 <- BlockHash <$> merkleLogHash "0000000000000000000000000000003a"
    blockenv13 <- _cpRestore cp (Just (BlockHeight 3, hash02))
    -- insert here would fail if new block 03 had not been discarded
    void $ runExec cenv blockenv13 Nothing "(m2.insertTbl 'b 2)"
    runExec cenv blockenv13 Nothing "(m2.readTbl)"
      >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]
    _cpSave cp hash03

    ------------------------------------------------------------------
    -- s08: FORK! block 02, new hash
    -- recreate m2 module, exercise RefStore checkpoint
    -- exec next part of pact
    ------------------------------------------------------------------

    let msgFork = "Step 8: FORK! block 02, new hash\n recreate m2 module, exercise RefStore checkpoint\n exec next part of pact"
    next msgFork
    hash02Fork <- BlockHash <$> merkleLogHash "0000000000000000000000000000002b"
    blockenv02Fork <- _cpRestore cp (Just (BlockHeight 2, hash01))
    void $ runExec cenv blockenv02Fork (Just $ ksData "2") $ defModule "2"
    runExec cenv blockenv02Fork Nothing "(m2.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
    -- this would fail if not a fork
    runCont cenv blockenv02Fork pactId 1 >>= ((Just 1 @=?) . pactCheckStep)
    _cpSave cp hash02Fork

    let updatemsgA = "step 9: test update row: new block 03"
    next updatemsgA
    blockenv23 <- _cpRestore cp (Just (BlockHeight 3, hash02Fork))
    -- updating key previously written at blockheight 1 (the "restore point")
    void $ runExec cenv blockenv23 Nothing "(m1.updateTbl 'b 3)"
    runExec cenv blockenv23 Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,3]]
    _cpDiscard cp

    let updatemsgB = "step 9: test update row: validate block 03"
    next updatemsgB
    hash13 <- BlockHash <$> merkleLogHash "0000000000000000000000000000003b"
    blockenv33 <- _cpRestore cp (Just (BlockHeight 3, hash02Fork))
    -- updating key previously written at blockheight 1 (the "restore point")
    void $ runExec cenv blockenv33 Nothing "(m1.updateTbl 'b 3)"
    runExec cenv blockenv33 Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,3]]
    _cpSave cp hash13

    next "mini-regression test for dropping user tables (part 1) empty block"
    hash04 <- BlockHash <$> merkleLogHash "0000000000000000000000000000004a"
    _blockenv04 <- _cpRestore cp (Just (BlockHeight 4, hash13))
    _cpSave cp hash04

    next "mini-regression test for dropping user tables (part 2) (create module with offending table)"
    hash05 <- BlockHash <$> merkleLogHash "0000000000000000000000000000005a"
    blockenv05 <- _cpRestore cp (Just (BlockHeight 5, hash04))
    void $ runExec cenv blockenv05 (Just $ ksData "5") $ defModule "5"
    runExec cenv blockenv05 Nothing "(m5.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
    _cpSave cp hash05

    next "mini-regression test for dropping user tables (part 3) (reload the offending table)"
    hash05Fork  <- BlockHash <$> merkleLogHash "0000000000000000000000000000005b"
    blockenv05Fork <- _cpRestore cp (Just (BlockHeight 5, hash04))
    void $ runExec cenv blockenv05Fork (Just $ ksData "5") $ defModule "5"
    runExec cenv blockenv05Fork Nothing "(m5.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
    _cpSave cp hash05Fork

    next "mini-regression test for dropping user tables (part 4) fork on the empty block"
    _blockenv04Fork <- _cpRestore cp (Just (BlockHeight 4, hash13))
    _cpDiscard cp

    next "2nd mini-regression test for debugging updates (part 1) empty block"
    hash14 <- BlockHash <$> merkleLogHash "0000000000000000000000000000004b"
    _blockenv04 <- _cpRestore cp (Just (BlockHeight 4, hash13))
    _cpSave cp hash14

    next "2nd mini-regression test for debugging updates (part 2) create module & table"
    hash15 <- BlockHash <$> merkleLogHash "0000000000000000000000000000005c"
    blockEnv06 <- _cpRestore cp (Just (BlockHeight 5, hash14))
    void $ runExec cenv blockEnv06 (Just $ ksData "6") $ defModule "6"
    runExec cenv blockEnv06 Nothing "(m6.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
    _cpSave cp hash15

    next "2nd mini-regression test for debugging updates (part 3) step 1 of insert value then update twice"
    hash06 <- BlockHash <$> merkleLogHash "0000000000000000000000000000006a"
    blockEnv07 <- _cpRestore cp (Just (BlockHeight 6, hash15))
    void $ runExec cenv blockEnv07 Nothing "(m6.insertTbl 'b 2)"
    _cpSave cp hash06

    next "2nd mini-regression test for debugging updates (part 4) step 2 of insert value then update twice"
    hash07 <- BlockHash <$> merkleLogHash "0000000000000000000000000000007a"
    blockEnv08 <- _cpRestore cp (Just (BlockHeight 7, hash06))
    void $ runExec cenv blockEnv08 Nothing "(m6.weirdUpdateTbl 'b 4)"
    _cpSave cp hash07

    next "2nd mini-regression test for debugging updates (part 5) step 3 of insert value then update twice"
    hash08 <- BlockHash <$> merkleLogHash "0000000000000000000000000000008a"
    blockEnv09 <- _cpRestore cp (Just (BlockHeight 8, hash07))
    -- FOR DEBUGGING/INSPECTING VALUES AT SPECIFIC KEYS
    -- void $ runExec cenv blockEnv09 Nothing "(let ((written (at 'col (read m6.tbl 'a [\"col\"])))) (enforce (= written 1) \"key a\"))"
    -- void $ runExec cenv blockEnv09 Nothing "(let ((written (at 'col (read m6.tbl 'b [\"col\"])))) (enforce (= written 4) \"key b\"))"
    -- FOR DEBUGGING/INSPECTING VALUES AT SPECIFIC KEYS
    runExec cenv blockEnv09 Nothing "(m6.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,4]]
    _cpSave cp hash08

    next "Create the free namespace for the test"
    hash09 <- BlockHash <$> merkleLogHash "0000000000000000000000000000009a"
    blockEnv10 <- _cpRestore cp (Just (BlockHeight 9, hash08))
    void $ runExec cenv blockEnv10 Nothing defFree
    _cpSave cp hash09

    hash10 <- BlockHash <$> merkleLogHash "0000000000000000000000000000010a"

    next "Don't create the same table twice in the same block"
    blockEnv11 <- _cpRestore cp (Just (BlockHeight 10, hash09))

    let tKeyset = object ["test-keyset" .= object ["keys" .= ([] :: [Text]), "pred" .= String ">="]]
    void $ runExec cenv blockEnv11 (Just tKeyset) tablecode
    expectException $ runExec cenv blockEnv11 (Just tKeyset) tablecode
    _cpDiscard cp

    next "Don't create the same table twice in the same transaction."

    blockEnv11a <- _cpRestore cp (Just (BlockHeight 10, hash09))
    expectException $ runExec cenv blockEnv11a (Just tKeyset) (tablecode <> tablecode)

    _cpDiscard cp

    next "Don't create the same table twice over blocks."

    blockEnv11b <- _cpRestore cp (Just (BlockHeight 10, hash09))
    void $ runExec cenv blockEnv11b (Just tKeyset) tablecode

    _cpSave cp hash10

    hash11 <- BlockHash <$> merkleLogHash "0000000000000000000000000000011a"

    blockEnv12 <- _cpRestore cp (Just (BlockHeight 11, hash10))
    expectException $ runExec cenv blockEnv12 (Just tKeyset) tablecode

    _cpSave cp hash11

    next "Purposefully restore to an illegal checkpoint."

    _blockEnvFailure <- expectException $ _cpRestore cp (Just (BlockHeight 13, hash10))

    when relational $ do

        next "Rewind to block 5"

        next "Run block 5b with pact 4.2.0 changes"

        blockEnv5b <- _cpRestore cp (Just (BlockHeight 5, hash14))
        void $ runExec cenv blockEnv5b (Just $ ksData "7") (defModule "7")
        void $ runExec cenv blockEnv5b Nothing "(m7.insertTbl 'b 2)"
        void $ runExec cenv blockEnv5b Nothing "(m7.insertTbl 'd 3)"
        void $ runExec cenv blockEnv5b Nothing "(m7.insertTbl 'c 4)"
        void $ runExec cenv blockEnv5b Nothing "(keys m7.tbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tStringList $ T.words "a b c d"]
        _cpDiscard cp

        next "Rollback to block 4 and expect failure with pact 4.2.0 changes"

        blockEnv4b <- _cpRestore cp (Just (BlockHeight 4, hash13))
        void $ runExec cenv blockEnv4b (Just $ ksData "7") (defModule "7")
        void $ runExec cenv blockEnv4b Nothing "(m7.insertTbl 'b 2)"
        void $ runExec cenv blockEnv4b Nothing "(m7.insertTbl 'd 3)"
        void $ runExec cenv blockEnv4b Nothing "(m7.insertTbl 'c 4)"
        expectException $ runExec cenv blockEnv4b Nothing "(keys m7.tbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tStringList $ T.words "a b c d"]
        _cpDiscard cp

  where

    h :: SomeException -> IO (Maybe String)
    h = const (return Nothing)

    expectException act = do
        result <- (act >> return (Just msg)) `catchAllSynchronous` h
        maybe (return ()) (`assertBool` False) result
      where
        msg = "The table duplication somehow went through. Investigate this error."

    ksData :: Text -> Value
    ksData idx = object
        [ ("k" <> idx) .= object
            [ "keys" .= ([] :: [Text])
            , "pred" .= String ">="
            ]
        ]

-- -------------------------------------------------------------------------- --
-- Read Row Unit Test

readRowUnitTest :: Assertion
readRowUnitTest = simpleBlockEnvInit runUnitTest
  where
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

testRegress :: Assertion
testRegress =
    regressChainwebPactDb
        >>= fmap (toTup . _benvBlockState) . readMVar
        >>= assertEquals "The final block state is" finalBlockState
  where
    finalBlockState = (2, 0)
    toTup BlockState { _bsTxId = txid, _bsBlockHeight = blockVersion } = (txid, blockVersion)

regressChainwebPactDb :: IO (MVar (BlockEnv SQLiteEnv))
regressChainwebPactDb =  simpleBlockEnvInit runRegression

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
        [ TxLog "SYS:usertables" "user1" $
            object
                [ "utModule" .= object
                    [ "name" .= String "someModule"
                    , "namespace" .= Null
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
        [ TxLog
            { _txDomain = "SYS:KeySets"
            , _txKey = "ks1"
            , _txValue = toJSON ks
            }
        , TxLog
            { _txDomain = "SYS:Modules"
            , _txKey = asString modName
            , _txValue = toJSON mod'
            }
        , TxLog
            { _txDomain = "user1"
            , _txKey = "key1"
            , _txValue = toJSON row
            }
        , TxLog
            { _txDomain = "user1"
            , _txKey = "key1"
            , _txValue = toJSON row'
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
testVer = FastTimedCPM peterson

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

withRelationalCheckpointerResource :: (IO CheckpointEnv -> TestTree) -> TestTree
withRelationalCheckpointerResource =
    withResource initializeSQLite freeSQLiteResource . runSQLite

addKeyset :: PactDbEnv' -> KeySetName -> KeySet -> IO ()
addKeyset (PactDbEnv' (PactDbEnv pactdb mvar)) keysetname keyset =
    _writeRow pactdb Insert KeySets keysetname keyset mvar

runTwice :: MonadIO m => (String -> IO ()) -> m () -> m ()
runTwice step action = do
  liftIO $ step "Running the first time!"
  action
  liftIO $ step "Running the second time!"
  action

runSQLite
    :: (IO CheckpointEnv -> TestTree)
    -> IO SQLiteEnv
    -> TestTree
runSQLite f = runSQLite' (f . fmap fst)

runSQLite'
    :: (IO (CheckpointEnv,SQLiteEnv) -> TestTree)
    -> IO SQLiteEnv
    -> TestTree
runSQLite' runTest sqlEnvIO = runTest $ do
    sqlenv <- sqlEnvIO
    cp <- initRelationalCheckpointer initialBlockState sqlenv logger testVer testChainId
    return (cp, sqlenv)
  where
    initialBlockState = set bsModuleNameFix True $ initBlockState defaultModuleCacheLimit $ genesisHeight testVer testChainId
    logger = newLogger (pactTestLogger False) "RelationalCheckpointer"

runExec :: CheckpointEnv -> PactDbEnv'-> Maybe Value -> Text -> IO EvalResult
runExec cp (PactDbEnv' pactdbenv) eData eCode = do
    execMsg <- buildExecParsedCode Nothing {- use latest parser version -} eData eCode
    evalTransactionM cmdenv cmdst $
      applyExec' 0 defaultInterpreter execMsg [] h' permissiveNamespacePolicy
  where
    h' = H.toUntypedHash (H.hash "" :: H.PactHash)
    cmdenv = TransactionEnv Transactional pactdbenv (_cpeLogger cp) Nothing def
             noSPVSupport Nothing 0.0 (RequestKey h') 0 def
    cmdst = TransactionState mempty mempty 0 Nothing (_geGasModel freeGasEnv) mempty

runCont :: CheckpointEnv -> PactDbEnv' -> PactId -> Int -> IO EvalResult
runCont cp (PactDbEnv' pactdbenv) pactId step = do
    evalTransactionM cmdenv cmdst $
      applyContinuation' 0 defaultInterpreter contMsg [] h' permissiveNamespacePolicy
  where
    contMsg = ContMsg pactId step False Null Nothing

    h' = H.toUntypedHash (H.hash "" :: H.PactHash)
    cmdenv = TransactionEnv Transactional pactdbenv (_cpeLogger cp) Nothing def
             noSPVSupport Nothing 0.0 (RequestKey h') 0 def
    cmdst = TransactionState mempty mempty 0 Nothing (_geGasModel freeGasEnv) mempty

-- -------------------------------------------------------------------------- --
-- Pact Utils

simpleBlockEnvInit
    :: (PactDb (BlockEnv SQLiteEnv) -> BlockEnv SQLiteEnv -> (MVar (BlockEnv SQLiteEnv) -> IO ()) -> IO a)
    -> IO a
simpleBlockEnvInit f = withTempSQLiteConnection chainwebPragmas $ \sqlenv ->
    f chainwebPactDb (blockEnv sqlenv) (\v -> runBlockEnv v initSchema)
  where
    loggers = pactTestLogger False
    blockEnv e = BlockEnv
        (BlockDbEnv e (newLogger loggers "BlockEnvironment"))
        (initBlockState defaultModuleCacheLimit $ genesisHeight testVer testChainId)

{- this should be moved to pact -}
begin :: PactDb e -> Method e (Maybe TxId)
begin pactdb = _beginTx pactdb Transactional

{- this should be moved to pact -}
commit :: PactDb e -> Method e [TxLog Value]
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
nativeLookup (NativeDefName n) = case HM.lookup n nativeDefs of
    Just (Direct t) -> Just t
    _ -> Nothing

tIntList :: [Int] -> Term Name
tIntList = toTList (TyPrim TyInteger) def . map toTerm

tStringList :: [Text] -> Term Name
tStringList = toTList (TyPrim TyString) def . map toTerm

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
