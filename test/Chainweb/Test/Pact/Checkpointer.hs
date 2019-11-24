{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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

import NeatInterpolation (text)

import Pact.Gas (freeGasEnv)
import Pact.Interpreter (EvalResult(..), PactDbEnv(..), defaultInterpreter)
import Pact.Native (nativeDefs)
import Pact.Repl
import Pact.Repl.Types
import qualified Pact.Types.Hash as H
import Pact.Types.Logger (newLogger)
import Pact.Types.PactValue
import Pact.Types.RPC (ContMsg(..))
import Pact.Types.Runtime
import Pact.Types.Server (CommandEnv(..))
import Pact.Types.SPV (noSPVSupport)

import Test.Tasty
import Test.Tasty.HUnit

-- internal imports

import Chainweb.BlockHash (BlockHash(..), nullBlockHash)
import Chainweb.BlockHeader (BlockHeight(..))
import Chainweb.MerkleLogHash (merkleLogHash)
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.InMemoryCheckpointer (initInMemoryCheckpointEnv)
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.TransactionExec
    (applyContinuation', applyExec', buildExecParsedCode)
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils

tests :: ScheduledTest
tests = testGroupSch "Checkpointer"
    [testInMemory
    , testKeyset
    , testRelational
    , testCase "PactDb Regression" testRegress
    , testCase "readRow unitTest" readRowUnitTest]

testInMemory :: TestTree
testInMemory = checkpointerTest "In-memory Checkpointer" InMem

defModule :: Text -> Text
defModule idx = [text| ;;

(define-keyset 'k$idx (read-keyset 'k$idx))

(module m$idx 'k$idx

  (defschema sch col:integer)

  (deftable tbl:{sch})

  (defun insertTbl (a i)
    (insert tbl a { 'col: i }))

  (defun updateTbl (a i)
    (update tbl a { 'col: i}))

  (defun weirdUpdateTbl (a i)
    (update tbl a { 'col: 0})
    (update tbl a { 'col: i}))

  (defun readTbl ()
    (sort (map (at 'col)
      (select tbl (constantly true)))))

  (defpact dopact (n)
    (step { 'name: n, 'value: 1 })
    (step { 'name: n, 'value: 2 }))

)
(create-table tbl)
(readTbl)
(insertTbl "a" 1)
|]

tIntList :: [Int] -> Term n
tIntList = toTList (TyPrim TyInteger) def . map toTerm

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

  _hash01 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001a")

  blockenv01 <- _cpRestore _cpeCheckpointer (Just (bh01, hash00))
  addKeyset blockenv01 "k2" (mkKeySet [] ">=")

  _cpDiscard _cpeCheckpointer

  next "fork on blockheight = 1"

  let bh11 = BlockHeight 1

  hash11 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001b")
  blockenv11 <- _cpRestore _cpeCheckpointer (Just (bh11, hash00))
  addKeyset blockenv11 "k1" (mkKeySet [] ">=")
  _cpSave _cpeCheckpointer hash11

addKeyset :: PactDbEnv' -> KeySetName -> KeySet -> IO ()
addKeyset (PactDbEnv' (PactDbEnv pactdb mvar)) keysetname keyset = _writeRow pactdb Insert KeySets keysetname keyset mvar


data InitData = OnDisk | InMem

testRelational :: TestTree
testRelational = checkpointerTest "Relational Checkpointer" OnDisk

runSQLite :: (IO CheckpointEnv -> TestTree) -> IO (IO (), SQLiteEnv) -> TestTree
runSQLite runTest = runTest . make
  where
    make :: IO (IO (), SQLiteEnv) -> IO CheckpointEnv
    make iosqlenv = do
      (_,sqlenv) <- iosqlenv
      let loggers = pactTestLogger False
      initRelationalCheckpointer initBlockState sqlenv (newLogger loggers "RelationalCheckpointer")

runTwice :: MonadIO m => (String -> IO ()) -> m () -> m ()
runTwice step action = do
  liftIO $ step "Running the first time!"
  action
  liftIO $ step "Running the second time!"
  action

checkpointerTest :: String -> InitData -> TestTree
checkpointerTest name initdata =
      case initdata of
        OnDisk -> withResource initializeSQLite freeSQLiteResource (runSQLite runTest)
        InMem -> let loggers = pactTestLogger False
          in withResource (initInMemoryCheckpointEnv loggers (newLogger loggers "inMemCheckpointer")) (const $ return ()) runTest
  where
    h :: SomeException -> IO (Maybe String)
    h = const (return Nothing)

    expectException act = do
        result <- (act >> return (Just msg)) `catch` h
        maybe (return ()) (flip assertBool False) result
      where
        msg = "The table duplication somehow went through. Investigate this error."

    runTest :: IO CheckpointEnv -> TestTree
    runTest c = testCaseSteps name $ \next -> do
          (CheckpointEnv {..}) <-    c
          let ksData :: Text -> Value
              ksData idx = object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]

              runExec :: PactDbEnv'-> Maybe Value -> Text -> IO EvalResult
              runExec (PactDbEnv' pactdbenv) eData eCode = do
                  let cmdenv = CommandEnv Nothing Transactional pactdbenv _cpeLogger freeGasEnv def noSPVSupport Nothing
                  execMsg <- buildExecParsedCode eData eCode
                  applyExec' cmdenv defaultInterpreter execMsg [] (H.toUntypedHash (H.hash "" :: H.PactHash)) permissiveNamespacePolicy


              runCont :: PactDbEnv' -> PactId -> Int -> IO EvalResult
              runCont (PactDbEnv' pactdbenv) pactId step = do
                  let contMsg = ContMsg pactId step False Null Nothing
                      cmdenv = CommandEnv Nothing Transactional pactdbenv _cpeLogger freeGasEnv def noSPVSupport Nothing
                  applyContinuation' cmdenv defaultInterpreter contMsg [] (H.toUntypedHash (H.hash "" :: H.PactHash)) permissiveNamespacePolicy
            ------------------------------------------------------------------
            -- s01 : new block workflow (restore -> discard), genesis
            ------------------------------------------------------------------

          let hash00 = nullBlockHash
          runTwice next $  do
            next "Step 1 : new block workflow (restore -> discard), genesis"
            blockenvGenesis0 <- _cpRestore _cpeCheckpointer Nothing
            void $ runExec blockenvGenesis0 (Just $ ksData "1") $ defModule "1"
            runExec blockenvGenesis0 Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
            _cpDiscard _cpeCheckpointer

          -----------------------------------------------------------
          -- s02 : validate block workflow (restore -> save), genesis
          -----------------------------------------------------------

            next "Step 2 : validate block workflow (restore -> save), genesis"
            blockenvGenesis1 <- _cpRestore _cpeCheckpointer Nothing
            void $ runExec blockenvGenesis1 (Just $ ksData "1") $ defModule "1"
            runExec blockenvGenesis1 Nothing "(m1.readTbl)"
              >>=  \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
            _cpSave _cpeCheckpointer hash00

        ------------------------------------------------------------------
        -- s03 : new block 00
        ------------------------------------------------------------------

          next "Step 3 : new block 00"
          blockenv00 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 1, hash00))
          -- start a pact
          -- test is that exec comes back with proper step
          let pactId = "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g"
              pactCheckStep = preview (_Just . peStep) . _erExec
          void $ runExec blockenv00 Nothing "(m1.insertTbl 'b 2)"
          runExec blockenv00 Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]
          runExec blockenv00 Nothing "(m1.dopact 'pactA)" >>= ((Just 0 @=?) . pactCheckStep)
          _cpDiscard _cpeCheckpointer

        ------------------------------------------------------------------
        -- s04: validate block 1
        ------------------------------------------------------------------

          next "Step 4: validate block 1"
          hash01 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001a")
          blockenv01 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 1, hash00))
          void $ runExec blockenv01 Nothing "(m1.insertTbl 'b 2)"
          runExec blockenv01 Nothing "(m1.readTbl)"
            >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]
          runExec blockenv01 Nothing "(m1.dopact 'pactA)"
            >>= ((Just 0 @=?) . pactCheckStep)
          _cpSave _cpeCheckpointer hash01

        ------------------------------------------------------------------
        -- s05: validate block 02
        -- create m2 module, exercise RefStore checkpoint
        -- exec next part of pact
        ------------------------------------------------------------------

          let msg =   "Step 5: validate block 02\n create m2 module, exercise RefStore checkpoint\n exec next part of pact"
          next msg
          hash02 <- BlockHash <$> merkleLogHash "0000000000000000000000000000002a"
          blockenv02 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 2, hash01))
          void $ runExec blockenv02 (Just $ ksData "2") $ defModule "2"
          runExec blockenv02 Nothing "(m2.readTbl)"
            >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
          runCont blockenv02 pactId 1
            >>= ((Just 1 @=?) . pactCheckStep)
          _cpSave _cpeCheckpointer hash02

        ------------------------------------------------------------------
        -- s06 : new block 03
        ------------------------------------------------------------------

          next "Step 6 : new block 03"
          blockenv03 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 3, hash02))
          void $ runExec blockenv03 Nothing "(m2.insertTbl 'b 2)"
          runExec blockenv03 Nothing "(m2.readTbl)"
            >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]
          _cpDiscard _cpeCheckpointer

        ------------------------------------------------------------------
        -- s07 : validate block 03
        ------------------------------------------------------------------

          next "Step 7 : validate block 03"
          hash03 <- BlockHash <$> merkleLogHash "0000000000000000000000000000003a"
          blockenv13 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 3, hash02))
          -- insert here would fail if new block 03 had not been discarded
          void $ runExec blockenv13 Nothing "(m2.insertTbl 'b 2)"
          runExec blockenv13 Nothing "(m2.readTbl)"
            >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]
          _cpSave _cpeCheckpointer hash03
      ------------------------------------------------------------------
        -- s08: FORK! block 02, new hash
        -- recreate m2 module, exercise RefStore checkpoint
        -- exec next part of pact
        ------------------------------------------------------------------

          let msgFork = "Step 8: FORK! block 02, new hash\n recreate m2 module, exercise RefStore checkpoint\n exec next part of pact"
          next msgFork
          hash02Fork <- BlockHash <$> merkleLogHash "0000000000000000000000000000002b"
          blockenv02Fork <- _cpRestore _cpeCheckpointer (Just (BlockHeight 2, hash01))
          void $ runExec blockenv02Fork (Just $ ksData "2") $ defModule "2"
          runExec blockenv02Fork Nothing "(m2.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
          -- this would fail if not a fork
          runCont blockenv02Fork pactId 1 >>= ((Just 1 @=?) . pactCheckStep)
          _cpSave _cpeCheckpointer hash02Fork

          let updatemsgA = "step 9: test update row: new block 03"
          next updatemsgA
          blockenv23 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 3, hash02Fork))
          -- updating key previously written at blockheight 1 (the "restore point")
          void $ runExec blockenv23 Nothing "(m1.updateTbl 'b 3)"
          runExec blockenv23 Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,3]]
          _cpDiscard _cpeCheckpointer

          let updatemsgB = "step 9: test update row: validate block 03"
          next updatemsgB
          hash13 <- BlockHash <$> merkleLogHash "0000000000000000000000000000003b"
          blockenv33 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 3, hash02Fork))
          -- updating key previously written at blockheight 1 (the "restore point")
          void $ runExec blockenv33 Nothing "(m1.updateTbl 'b 3)"
          runExec blockenv33 Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,3]]
          _cpSave _cpeCheckpointer hash13

          next "mini-regression test for dropping user tables (part 1) empty block"
          hash04 <- BlockHash <$> merkleLogHash "0000000000000000000000000000004a"
          _blockenv04 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 4, hash13))
          _cpSave _cpeCheckpointer hash04

          next "mini-regression test for dropping user tables (part 2) (create module with offending table)"
          hash05 <- BlockHash <$> merkleLogHash "0000000000000000000000000000005a"
          blockenv05 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 5, hash04))
          void $ runExec blockenv05 (Just $ ksData "5") $ defModule "5"
          runExec blockenv05 Nothing "(m5.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
          _cpSave _cpeCheckpointer hash05

          next "mini-regression test for dropping user tables (part 3) (reload the offending table)"
          hash05Fork  <- BlockHash <$> merkleLogHash "0000000000000000000000000000005b"
          blockenv05Fork <- _cpRestore _cpeCheckpointer (Just (BlockHeight 5, hash04))
          void $ runExec blockenv05Fork (Just $ ksData "5") $ defModule "5"
          runExec blockenv05Fork Nothing "(m5.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
          _cpSave _cpeCheckpointer hash05Fork

          next "mini-regression test for dropping user tables (part 4) fork on the empty block"
          _blockenv04Fork <- _cpRestore _cpeCheckpointer (Just (BlockHeight 4, hash13))
          _cpDiscard _cpeCheckpointer

          next "2nd mini-regression test for debugging updates (part 1) empty block"
          hash14 <- BlockHash <$> merkleLogHash "0000000000000000000000000000004b"
          _blockenv04 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 4, hash13))
          _cpSave _cpeCheckpointer hash14

          next "2nd mini-regression test for debugging updates (part 2) create module & table"
          hash15 <- BlockHash <$> merkleLogHash "0000000000000000000000000000005c"
          blockEnv06 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 5, hash14))
          void $ runExec blockEnv06 (Just $ ksData "6") $ defModule "6"
          runExec blockEnv06 Nothing "(m6.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
          _cpSave _cpeCheckpointer hash15

          next "2nd mini-regression test for debugging updates (part 3) step 1 of insert value then update twice"
          hash06 <- BlockHash <$> merkleLogHash "0000000000000000000000000000006a"
          blockEnv07 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 6, hash15))
          void $ runExec blockEnv07 Nothing "(m6.insertTbl 'b 2)"
          _cpSave _cpeCheckpointer hash06

          next "2nd mini-regression test for debugging updates (part 4) step 2 of insert value then update twice"
          hash07 <- BlockHash <$> merkleLogHash "0000000000000000000000000000007a"
          blockEnv08 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 7, hash06))
          void $ runExec blockEnv08 Nothing "(m6.weirdUpdateTbl 'b 4)"
          _cpSave _cpeCheckpointer hash07

          next "2nd mini-regression test for debugging updates (part 5) step 3 of insert value then update twice"
          hash08 <- BlockHash <$> merkleLogHash "0000000000000000000000000000008a"
          blockEnv09 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 8, hash07))
          -- FOR DEBUGGING/INSPECTING VALUES AT SPECIFIC KEYS
          -- void $ runExec blockEnv09 Nothing "(let ((written (at 'col (read m6.tbl 'a [\"col\"])))) (enforce (= written 1) \"key a\"))"
          -- void $ runExec blockEnv09 Nothing "(let ((written (at 'col (read m6.tbl 'b [\"col\"])))) (enforce (= written 4) \"key b\"))"
          -- FOR DEBUGGING/INSPECTING VALUES AT SPECIFIC KEYS
          runExec blockEnv09 Nothing "(m6.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,4]]
          _cpSave _cpeCheckpointer hash08

          next "Don't create the same table twice in the same block"
          hash09 <- BlockHash <$> merkleLogHash "0000000000000000000000000000009a"
          blockEnv10 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 9, hash08))

          let tKeyset = object ["test-keyset" .= object ["keys" .= ([] :: [Text]), "pred" .= String ">="]]
          void $ runExec blockEnv10 (Just tKeyset) tablecode
          expectException $ runExec blockEnv10 (Just tKeyset) tablecode
          _cpDiscard _cpeCheckpointer

          next "Don't create the same table twice in the same transaction."

          blockEnv10a <- _cpRestore _cpeCheckpointer (Just (BlockHeight 9, hash08))
          expectException $ runExec blockEnv10a (Just tKeyset) (tablecode <> tablecode)

          _cpDiscard _cpeCheckpointer

          next "Don't create the same table twice over blocks."

          blockEnv10b <- _cpRestore _cpeCheckpointer (Just (BlockHeight 9, hash08))
          void $ runExec blockEnv10b (Just tKeyset) tablecode

          _cpSave _cpeCheckpointer hash09

          hash10 <- BlockHash <$> merkleLogHash "0000000000000000000000000000010a"

          blockEnv11 <- _cpRestore _cpeCheckpointer (Just (BlockHeight 10, hash09))
          expectException $ runExec blockEnv11 (Just tKeyset) tablecode

          _cpSave _cpeCheckpointer hash10

toTerm' :: ToTerm a => a -> Term Name
toTerm' = toTerm

testRegress :: Assertion
testRegress =
    regressChainwebPactDb
        >>= fmap (toTup . _benvBlockState) . readMVar
        >>= assertEquals "The final block state is" finalBlockState
  where
    finalBlockState = (2, 0)
    toTup (BlockState txid _ blockVersion _ _) = (txid, blockVersion)


simpleBlockEnvInit ::
     (PactDb (BlockEnv SQLiteEnv) -> BlockEnv SQLiteEnv -> (MVar (BlockEnv SQLiteEnv) -> IO ()) -> IO a)
     -> IO a
simpleBlockEnvInit f =
    withTempSQLiteConnection chainwebPragmas $ \sqlenv ->
           f chainwebPactDb
            (BlockEnv
                (BlockDbEnv sqlenv (newLogger loggers "BlockEnvironment"))
                    initBlockState)
            (\v -> runBlockEnv v initSchema)
  where
    loggers = pactTestLogger False

regressChainwebPactDb :: IO (MVar (BlockEnv SQLiteEnv))
regressChainwebPactDb =  simpleBlockEnvInit runRegression

 {- this should be moved to pact -}
begin :: PactDb e -> Method e (Maybe TxId)
begin pactdb = _beginTx pactdb Transactional

{- this should be moved to pact -}
commit :: PactDb e -> Method e [TxLog Value]
commit pactdb = _commitTx pactdb

readRowUnitTest :: Assertion
readRowUnitTest = simpleBlockEnvInit runUnitTest
  where
    writeRow' pactdb writeType conn i =
      _writeRow pactdb writeType (UserTables "user1") "key1"
      (ObjectMap $ M.fromList [("f", (PLiteral (LInteger i)))]) conn
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
        Just (ObjectMap m) -> case M.lookup "f" m of
          Just l -> assertEquals "Unsuccesful write at field key \"f\"" l (PLiteral (LInteger 100))
          Nothing -> assertFailure "Field not found"

{- this should be moved to pact -}
runRegression ::
  PactDb e -- your pactdb instance
  -> e -- ambient environment
  -> (MVar e -> IO ()) -- schema "creator"
  -> IO (MVar e) -- the final state of the environment
runRegression pactdb e schemaInit = do
  conn <- newMVar e
  schemaInit conn
  Just t1 <- begin pactdb conn
  let user1 = "user1"
      usert = UserTables user1
      toPV :: ToTerm a => a -> PactValue
      toPV = toPactValueLenient . toTerm'
  _createUserTable pactdb user1 "someModule" conn
  assertEquals' "output of commit2"
    [TxLog "SYS:usertables" "user1" $
      object [ ("utModule" .= object [ ("name" .= String "someModule"), ("namespace" .= Null)])
             ]
    ]
    (commit pactdb conn)
  void $ begin pactdb conn
  {- the below line is commented out because we no longer support _getUserTableInfo -}
  -- assertEquals' "user table info correct" "someModule" $ _getUserTableInfo chainwebpactdb user1 conn
  let row = ObjectMap $ M.fromList [("gah", PLiteral (LDecimal 123.454345))]
  _writeRow pactdb Insert usert "key1" row conn
  assertEquals' "usert insert" (Just row) (_readRow pactdb usert "key1" conn)
  let row' = ObjectMap $ M.fromList [("gah",toPV False),("fh",toPV (1 :: Int))]
  _writeRow pactdb Update usert "key1" row' conn
  assertEquals' "user update" (Just row') (_readRow pactdb usert "key1" conn)
  let ks = mkKeySet [PublicKey "skdjhfskj"] "predfun"
  _writeRow pactdb Write KeySets "ks1" ks conn
  assertEquals' "keyset write" (Just ks) $ _readRow pactdb KeySets "ks1" conn
  (modName,modRef,mod') <- loadModule
  _writeRow pactdb Write Modules modName mod' conn
  assertEquals' "module write" (Just mod') $ _readRow pactdb Modules modName conn
  assertEquals "module native repopulation" (Right modRef) $
    traverse (traverse (fromPersistDirect nativeLookup)) mod'
  assertEquals' "result of commit 3"

    [ TxLog { _txDomain = "SYS:KeySets"
            , _txKey = "ks1"
            , _txValue = toJSON ks
            }
    , TxLog { _txDomain = "SYS:Modules"
            , _txKey = asString modName
            , _txValue = toJSON mod'
            }
    , TxLog { _txDomain = "user1"
            , _txKey = "key1"
            , _txValue = toJSON row
            }
    , TxLog { _txDomain = "user1"
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
  assertEquals' "user txlogs" [TxLog "user1" "key1" (ObjectMap $ on M.union _objectMap row' row)] $
    _getTxLog pactdb usert (head tids) conn
  _writeRow pactdb Insert usert "key2" row conn
  assertEquals' "user insert key2 pre-rollback" (Just row) (_readRow pactdb usert "key2" conn)
  assertEquals' "keys pre-rollback" ["key1","key2"] $ _keys pactdb (UserTables user1) conn
  _rollbackTx pactdb conn
  assertEquals' "rollback erases key2" Nothing $ _readRow pactdb usert "key2" conn
  assertEquals' "keys" ["key1"] $ _keys pactdb (UserTables user1) conn
  return conn

assertEquals' :: (Eq a, Show a, NFData a) => String -> a -> IO a -> IO ()
assertEquals' msg a b = assertEquals msg a =<< b

assertEquals :: (Eq a,Show a,NFData a) => String -> a -> a -> IO ()
assertEquals msg a b | [a,b] `deepseq` a == b = return ()
                     | otherwise =
                         throwFail $ "FAILURE: " ++ msg ++ ": expected \n  " ++ show a ++ "\n got \n  " ++ show b

throwFail :: String -> IO a
throwFail = throwIO . userError

loadModule :: IO (ModuleName, ModuleData Ref, PersistModuleData)
loadModule = do
  let fn = "test/pact/simple.repl"
  (r,s) <- execScript' (Script False fn) fn
  let mn = ModuleName "simple" Nothing
  case r of
    Left a -> throwFail $ "module load failed: " ++ show a
    Right _ -> case preview (rEvalState . evalRefs . rsLoadedModules . ix mn) s of
      Just (md,_) -> case traverse (traverse toPersistDirect) md of
        Right md' -> return (mn,md,md')
        Left e -> throwFail $ "toPersistDirect failed: " ++ show e
      Nothing -> throwFail $ "Failed to find module 'simple': " ++
        show (view (rEvalState . evalRefs . rsLoadedModules) s)

nativeLookup :: NativeDefName -> Maybe (Term Name)
nativeLookup (NativeDefName n) = case HM.lookup (Name $ BareName n def) nativeDefs of
  Just (Direct t) -> Just t
  _ -> Nothing

tablecode :: Text
tablecode = [text|
(define-keyset 'table-admin-keyset
  (read-keyset "test-keyset"))

(module table-example 'table-admin-keyset

  (defschema test-schema
    content:string)

  (deftable test-table:{test-schema})

  (defun add-row (row:string content:string)
    (insert test-table row {
      "content": content
      })
  )
  (defun read-table ()
    (select test-table (constantly true))
  )
)

(create-table test-table)
|]
