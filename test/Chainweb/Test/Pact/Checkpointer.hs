{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Pact.Checkpointer (tests, _testRegress, testRelational, testInMemory) where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad (void)
import Control.Monad.Reader
-- import Control.Monad.Catch (MonadCatch(..))

import Data.Aeson (Value(..), object, (.=), toJSON)
import Data.Default (def)
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Map.Strict as M

import NeatInterpolation (text)

import Pact.Gas (freeGasEnv)
import Pact.Interpreter (EvalResult(..))
import Pact.Native (nativeDefs)
import Pact.Repl
import Pact.Repl.Types
-- import Pact.Types.Info
import Pact.Types.Runtime (ExecutionMode(Transactional))
import qualified Pact.Types.Hash as H
-- import Pact.Types.Logger (Loggers, newLogger)
import Pact.Types.Logger (newLogger)
import Pact.Types.PactValue
import Pact.Types.Persistence
import Pact.Types.RPC (ContMsg(..))
import Pact.Types.Runtime
import Pact.Types.Server (CommandEnv(..))
-- import Pact.Types.SQLite
import Pact.Types.Type (PrimType(..), Type(..))
import Pact.Types.Exp (Literal(..))


import Test.Tasty.HUnit

-- internal imports

import Chainweb.BlockHash (BlockHash(..), nullBlockHash)
import Chainweb.BlockHeader (BlockHeight(..))
import Chainweb.MerkleLogHash (merkleLogHash)
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.InMemoryCheckpointer (initInMemoryCheckpointEnv)
import Chainweb.Pact.Backend.RelationalCheckpointer
-- import Chainweb.Pact.Backend.MemoryDb (mkPureState)
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.TransactionExec
    (applyContinuation', applyExec', buildExecParsedCode)
-- import Chainweb.Pact.Utils (toEnv', toEnvPersist')
import Chainweb.Pact.Backend.Utils
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils

tests :: ScheduledTest
tests = testGroupSch "Checkpointer"
  [
    testCase "testInMemory" testInMemory
  -- , testCase "testRelationalKeyset" testKeyset
  , testCase "testRelational" testRelational ]


testInMemory :: Assertion
testInMemory = do
  let loggers = pactTestLogger False -- set to True to see debug logs
  (_, cpEnv) <- initInMemoryCheckpointEnv loggers
        (newLogger loggers "inMemCheckpointer") freeGasEnv

  relationalTest cpEnv


defModule :: Text -> Text
defModule idx = [text| ;;

(define-keyset 'k$idx (read-keyset 'k$idx))

(module m$idx 'k$idx

  (defschema sch col:integer)

  (deftable tbl:{sch})

  (defun insertTbl (a i)
    (insert tbl a { 'col: i }))

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

{-
testCheckpointer :: Loggers -> CheckpointEnv -> PactDbEnv' -> Assertion
testCheckpointer loggers CheckpointEnv{..} _dbState00 = do

  let logger = newLogger loggers "testCheckpointer"

      runExec :: PactDbEnv' -> Maybe Value -> Text -> IO EvalResult
      runExec (PactDbEnv' pactDbEnv) eData eCode = do
          let cmdenv = CommandEnv Nothing Transactional pactDbEnv logger freeGasEnv def
          execMsg <- buildExecParsedCode eData eCode
          applyExec' cmdenv def execMsg [] (H.toUntypedHash (H.hash "" :: H.PactHash)) noSPVSupport

      runCont :: PactDbEnv' -> PactId -> Int -> IO EvalResult
      runCont (PactDbEnv' pactDbEnv) pactId step = do
          let contMsg = ContMsg pactId step False Null
              cmdenv = CommandEnv Nothing Transactional pactDbEnv logger freeGasEnv def
          applyContinuation' cmdenv def contMsg [] (H.toUntypedHash (H.hash "" :: H.PactHash)) noSPVSupport

      ksData :: Text -> Value
      ksData idx =
          object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]

      unwrapState :: PactDbEnv' -> IO PactDbEnv'
      unwrapState  = pure

      wrapState :: PactDbEnv' -> IO PactDbEnv'
      wrapState = pure


  -- saveInitial _cpeCheckpointer dbState00

  ------------------------------------------------------------------
  -- s01 : new block workflow (restore -> discard), genesis
  ------------------------------------------------------------------

  -- hmm `restoreInitial` has implicit dependency on (BlockHeight 0), nullBlockHash

  let bh00 = BlockHeight 0
      hash00 = nullBlockHash

  s01 <- restoreInitial _cpeCheckpointer >>= unwrapState

  void $ runExec s01 (Just $ ksData "1") $ defModule "1"

  runExec s01 Nothing "(m1.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

  void $ wrapState s01
    >> discard _cpeCheckpointer



  ------------------------------------------------------------------
  -- s02 : validate block workflow (restore -> save), genesis
  ------------------------------------------------------------------

  s02 <- restoreInitial _cpeCheckpointer >>= unwrapState

  -- by defining m1/inserting anew, we ensure it didn't exist before
  -- otherwise would fail at least on table create

  void $ runExec s02 (Just $ ksData "1") $ defModule "1"

  runExec s02 Nothing "(m1.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

  void $ wrapState s02 >> save _cpeCheckpointer hash00



  ------------------------------------------------------------------
  -- s03 : new block 01
  ------------------------------------------------------------------

  s03 <- restore _cpeCheckpointer bh00 hash00 >>= unwrapState

  void $ runExec s03 Nothing "(m1.insertTbl 'b 2)"

  runExec s03 Nothing "(m1.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

  -- start a pact
  -- test is that exec comes back with proper step
  let pactId = "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g"
      pactCheckStep = preview (_Just . peStep) . _erExec

  runExec s03 Nothing "(m1.dopact 'pactA)"
    >>= ((Just 0 @=?) . pactCheckStep)


  void $ wrapState s03
    >> discard _cpeCheckpointer
  -- ^^ note that this is the same discard as on s01 above, and yet ... it works
  -- (ie, doesn't blow away genesis??)


  ------------------------------------------------------------------
  -- s04 : validate block 01
  ------------------------------------------------------------------

  let bh01 = BlockHeight 01
  hash01 <- BlockHash <$> merkleLogHash "0000000000000000000000000000001a"

  s04 <- restore _cpeCheckpointer bh00 hash00 >>= unwrapState

  -- insert here would fail if new block 01 had not been discarded
  void $ runExec s04 Nothing "(m1.insertTbl 'b 2)"

  runExec s04 Nothing "(m1.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

  -- start a pact at txid 4, would fail if new block 01 had not been discarded
  runExec s04 Nothing "(m1.dopact 'pactA)"
    >>= ((Just 0 @=?) . pactCheckStep)

  void $ wrapState s04 >> save _cpeCheckpointer hash01


  ------------------------------------------------------------------
  -- s05: validate block 02
  -- create m2 module, exercise RefStore checkpoint
  -- exec next part of pact
  ------------------------------------------------------------------

  let bh02 = BlockHeight 02
  hash02 <- BlockHash <$> merkleLogHash "0000000000000000000000000000002a"

  s05 <- restore _cpeCheckpointer bh01 hash01 >>= unwrapState

  void $ runExec s05 (Just $ ksData "2") $ defModule "2"

  runExec s05 Nothing "(m2.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

  runCont s05 pactId 1
    >>= ((Just 1 @=?) . pactCheckStep)

  void $ wrapState s05 >> save _cpeCheckpointer hash02

  ------------------------------------------------------------------
  -- s06 : new block 03
  ------------------------------------------------------------------

  s06 <- restore _cpeCheckpointer bh02 hash02 >>= unwrapState

  void $ runExec s06 Nothing "(m2.insertTbl 'b 2)"

  runExec s06 Nothing "(m2.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

  void $ wrapState s06 >> discard _cpeCheckpointer

  ------------------------------------------------------------------
  -- s07 : validate block 03
  ------------------------------------------------------------------

  let _bh03 = BlockHeight 03
  hash03 <- BlockHash <$> merkleLogHash "0000000000000000000000000000003a"

  s07 <- restore _cpeCheckpointer bh02 hash02 >>= unwrapState

  -- insert here would fail if new block 03 had not been discarded
  void $ runExec s07 Nothing "(m2.insertTbl 'b 2)"

  runExec s07 Nothing "(m2.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

  void $ wrapState s07 >> save _cpeCheckpointer hash03


------------------------------------------------------------------
  -- s08: FORK! block 02, new hash
  -- recreate m2 module, exercise RefStore checkpoint
  -- exec next part of pact
  ------------------------------------------------------------------

  hash02Fork <- BlockHash <$> merkleLogHash "0000000000000000000000000000002b"

  s08 <- restore _cpeCheckpointer bh01 hash01 >>= unwrapState

  void $ runExec s08 (Just $ ksData "2") $ defModule "2"

  runExec s08 Nothing "(m2.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

  -- this would fail if not a fork
  runCont s08 pactId 1
    >>= ((Just 1 @=?) . pactCheckStep)

  void $ wrapState s08 >> save _cpeCheckpointer hash02Fork
-}
-- testKeyset :: Assertion
-- testKeyset =
--   void $ withTempSQLiteConnection []  $ \sqlenv -> do
--     let initBlockState = BlockState 0 Nothing (BlockVersion 0 0) M.empty (newLogger loggers "BlockEnvironment")
--         loggers = pactTestLogger False
--     cpEnv <- initRelationalCheckpointerNew initBlockState sqlenv (newLogger loggers "RelationalCheckpointer") freeGasEnv
--     keysetTest cpEnv

-- type Table = [[SType]]

{-
runOnVersionTables ::
     (MonadCatch m, MonadReader SQLiteEnv m, MonadIO m)
  => (Table -> IO ())
  -> (Table -> IO ())
  -> (Table -> IO ())
  -> m ()
runOnVersionTables f g h = do
  blocktable <- callDb functionName $ \db -> qry_ db "SELECT * FROM BlockHistory;" [RInt, RBlob]
  versionhistory <- callDb functionName $ \db -> qry_ db "SELECT * FROM VersionHistory;" [RInt, RInt]
  vtables <-  callDb functionName $ \db -> qry_ db "SELECT * FROM UserTables;" [RText, RInt, RInt]
  void $ liftIO $ f blocktable
  void $ liftIO $ g versionhistory
  liftIO $ h vtables
  where
    functionName = "runOnVersionTables"
-}


{-
keysetTest ::  CheckpointEnvNew -> Assertion
keysetTest CheckpointEnvNew {..} = do

  let hash00 = nullBlockHash

  (PactDbEnv' blockenv00) <- restoreInitialNew _cpeCheckpointerNew

  runDbEnv blockenv00 runBlockEnv $ do
    keysetsTable <- callDb "keysetTest" $ \db -> do
      qry_ db ("SELECT * FROM [" <> domainTableName KeySets <> "];")
        [RText, RInt, RInt, RInt, RBlob]
    liftIO $ assertEqual "keyset table" keysetsTable ks1


  -- withBlockEnv (pdPactDbVar blockenv00) $ \benv -> do
  -- withPactDbEnv blockenv00 $ \funrec mvar -> do
  --   addKeyset funrec "k1" (KeySet [] (Name ">=" (Info Nothing))) mvar
  --   runBlockEnv mvar $ do
  --     keysetsTable <- callDb "keysetTest" $
  --       \db -> qry_ db ("SELECT * FROM [" <> domainTableName KeySets <> "];")
  --              [RText, RInt, RInt, RInt, RBlob]
  --     liftIO $ assertEqual "keyset table" keysetsTable ks1

  saveNew _cpeCheckpointerNew hash00
  -- saveNew _cpeCheckpointerNew (BlockHeight 0) hash00

  -- next block (blockheight 1, version 0)

  let bh01 = BlockHeight 1
  hash01 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001a")
  (PactDbEnv' blockenv01) <- restoreNew _cpeCheckpointerNew bh01 (Just hash00)
  -- withBlockEnv blockenv01 $ \benv -> do
  addKeyset chainwebpactdb "k2" (KeySet [] (Name ">=" (Info Nothing))) (pdPactDbVar blockenv01)
  runBlockEnv benv $ do
      keysetsTable <- callDb $
        \db -> qry_ db ("SELECT * FROM [" <> domainTableName KeySets <> "];")
               [RText, RInt, RInt, RInt, RBlob]
      liftIO $ assertEqual "keyset table" keysetsTable ks2
      liftIO $ saveNew _cpeCheckpointerNew bh01 hash01
      runOnVersionTables
        (\blocktable -> assertEqual "block table " blocktable bt01)
        (\versionhistory -> assertEqual "version history" versionhistory vht01)
        (\vtables -> assertEqual "user tables table " vtables vtt)

  -- fork on blockheight = 1

  let bh11 = BlockHeight 1

  hash11 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001b")
  blockenv11 <- restoreNew _cpeCheckpointerNew bh11 (Just hash00)

  withBlockEnv blockenv11 $ \benv -> do

    addKeyset "k1" (KeySet [] (Name ">=" (Info Nothing))) benv

    runBlockEnv benv $ do
      liftIO $ saveNew _cpeCheckpointerNew bh11 hash11
      runOnVersionTables
        (\blocktable -> assertEqual "block table " blocktable bt11)
        (\versionhistory -> assertEqual "version history " versionhistory vht11)
        (\vtables -> assertEqual "user tables table " vtables vtt)

  where
    vtt = []
    ks1 = [[SText "k1",SInt 0,SInt 0,SInt 0,SBlob "{\"pred\":\">=\",\"keys\":[]}"]]
    ks2 = [[SText "k1",SInt 0,SInt 0,SInt 0,SBlob "{\"pred\":\">=\",\"keys\":[]}"],[SText "k2",SInt 1,SInt 0,SInt 0,SBlob "{\"pred\":\">=\",\"keys\":[]}"]]
    bt01 = [[SInt 0,SBlob "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"],[SInt 1,SBlob "0000000000000000000000000000001a"]]
    vht01 = [[SInt 0,SInt 1]]
    bt11 = [[SInt 0,SBlob "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"],[SInt 1,SBlob "0000000000000000000000000000001b"]]
    vht11 = [[SInt 0,SInt 1],[SInt 1,SInt 1]]
-}

{-
addKeyset :: PactDb e -> KeySetName -> KeySet -> Method e ()
addKeyset pactdb = _writeRow pactdb Insert KeySets
-}

testRelational :: Assertion
testRelational =
  withTempSQLiteConnection []  $ \sqlenv -> do
    let initBlockState = BlockState 0 Nothing (BlockVersion 0 0) M.empty (newLogger loggers "BlockEnvironment")
        loggers = pactTestLogger False
    cpEnv <- initRelationalCheckpointer initBlockState sqlenv (newLogger loggers "RelationalCheckpointer") freeGasEnv
    relationalTest cpEnv

{-
  relationalTest is a copy of testCheckpointer. Eventually, this will
  become one test once the API of the in-memory checkpointer has ben changed.
-}

relationalTest :: CheckpointEnv -> Assertion
relationalTest CheckpointEnv {..} = do
  let ksData :: Text -> Value
      ksData idx =
          object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]

      runExec :: PactDbEnv'-> Maybe Value -> Text -> IO EvalResult
      runExec (PactDbEnv' pactdbenv) eData eCode = do
        let cmdenv = CommandEnv Nothing Transactional pactdbenv _cpeLogger _cpeGasEnv def
        execMsg <- buildExecParsedCode eData eCode
        applyExec' cmdenv def execMsg [] (H.toUntypedHash (H.hash "" :: H.PactHash)) noSPVSupport


      runCont :: PactDbEnv' -> PactId -> Int -> IO EvalResult
      runCont (PactDbEnv' pactdbenv) pactId step = do
        let contMsg = ContMsg pactId step False Null
            cmdenv = CommandEnv Nothing Transactional pactdbenv _cpeLogger _cpeGasEnv def
        applyContinuation' cmdenv def contMsg [] (H.toUntypedHash (H.hash "" :: H.PactHash)) noSPVSupport

  ------------------------------------------------------------------
  -- s01 : new block workflow (restore -> discard), genesis
  ------------------------------------------------------------------

  let hash00 = nullBlockHash
  blockenvGenesis0 <- restore _cpeCheckpointer Nothing

  void $ runExec blockenvGenesis0 (Just $ ksData "1") $ defModule "1"
  runExec blockenvGenesis0 Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
  discard _cpeCheckpointer

  -----------------------------------------------------------
  -- s02 : validate block workflow (restore -> save), genesis
  -----------------------------------------------------------

  blockenvGenesis1 <- restore _cpeCheckpointer Nothing
  void $ runExec blockenvGenesis1 (Just $ ksData "1") $ defModule "1"

  runExec blockenvGenesis1 Nothing "(m1.readTbl)"
    >>=  \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

  liftIO $ save _cpeCheckpointer hash00
  ------------------------------------------------------------------
  -- s03 : new block 00
  ------------------------------------------------------------------

  blockenv00 <- restore _cpeCheckpointer (Just (BlockHeight 1, hash00))

  -- start a pact
  -- test is that exec comes back with proper step
  let pactId = "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g"
      pactCheckStep = preview (_Just . peStep) . _erExec

  void $ runExec blockenv00 Nothing "(m1.insertTbl 'b 2)"

  runExec blockenv00 Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

  runExec blockenv00 Nothing "(m1.dopact 'pactA)" >>= ((Just 0 @=?) . pactCheckStep)

  discard _cpeCheckpointer

  ------------------------------------------------------------------
  -- s04: validate block 1
  ------------------------------------------------------------------

  hash01 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001a")

  blockenv01 <- restore _cpeCheckpointer (Just (BlockHeight 1, hash00))

  void $ runExec blockenv01 Nothing "(m1.insertTbl 'b 2)"

  runExec blockenv01 Nothing "(m1.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

  runExec blockenv01 Nothing "(m1.dopact 'pactA)"
    >>= ((Just 0 @=?) . pactCheckStep)

  save _cpeCheckpointer hash01


  ------------------------------------------------------------------
  -- s05: validate block 02
  -- create m2 module, exercise RefStore checkpoint
  -- exec next part of pact
  ------------------------------------------------------------------

  hash02 <- BlockHash <$> merkleLogHash "0000000000000000000000000000002a"

  blockenv02 <- restore _cpeCheckpointer (Just (BlockHeight 2, hash01))

  void $ runExec blockenv02 (Just $ ksData "2") $ defModule "2"

  runExec blockenv02 Nothing "(m2.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

  runCont blockenv02 pactId 1
    >>= ((Just 1 @=?) . pactCheckStep)

  save _cpeCheckpointer hash02

  ------------------------------------------------------------------
  -- s06 : new block 03
  ------------------------------------------------------------------

  blockenv03 <- restore _cpeCheckpointer (Just (BlockHeight 3, hash02))

  void $ runExec blockenv03 Nothing "(m2.insertTbl 'b 2)"

  runExec blockenv03 Nothing "(m2.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

  discard _cpeCheckpointer

  ------------------------------------------------------------------
  -- s07 : validate block 03
  ------------------------------------------------------------------

  hash03 <- BlockHash <$> merkleLogHash "0000000000000000000000000000003a"

  blockenv13 <- restore _cpeCheckpointer (Just (BlockHeight 3, hash02))

  -- insert here would fail if new block 03 had not been discarded
  void $ runExec blockenv13 Nothing "(m2.insertTbl 'b 2)"

  runExec blockenv13 Nothing "(m2.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]


  save _cpeCheckpointer hash03

------------------------------------------------------------------
  -- s08: FORK! block 02, new hash
  -- recreate m2 module, exercise RefStore checkpoint
  -- exec next part of pact
  ------------------------------------------------------------------

  hash02Fork <- BlockHash <$> merkleLogHash "0000000000000000000000000000002b"

  blockenv02Fork <- restore _cpeCheckpointer (Just (BlockHeight 3, hash01))

  void $ runExec blockenv02Fork (Just $ ksData "2") $ defModule "2"

  runExec blockenv02Fork Nothing "(m2.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

  -- this would fail if not a fork
  runCont blockenv02Fork pactId 1 >>= ((Just 1 @=?) . pactCheckStep)

  save _cpeCheckpointer hash02Fork

toTerm' :: ToTerm a => a -> Term Name
toTerm' = toTerm

{- You should probably think about the orphan, their name is
 ExecutionMode, that you are leaving behind. They deserve a home! -}
_testRegress :: Assertion
_testRegress =
  regressChainwebPactDb >>= fmap (toTup . _benvBlockState) . readMVar >>=
  assertEquals "The final block state is" finalBlockState
  where
    finalBlockState = (2, BlockVersion 0 0, M.empty)
    toTup (BlockState txid _ blockVersion txRecord _) =
      (txid, blockVersion, txRecord)

regressChainwebPactDb :: IO (MVar (BlockEnv SQLiteEnv))
regressChainwebPactDb = do
 withTempSQLiteConnection []  $ \sqlenv -> do
        let initBlockState = BlockState 0 Nothing (BlockVersion 0 0) M.empty (newLogger loggers "BlockEnvironment")
            loggers = pactTestLogger False
        runRegression chainwebpactdb
          (BlockEnv sqlenv initBlockState)
          (\v -> runBlockEnv v initSchema)

 {- this should be moved to pact -}
begin :: PactDb e -> Method e (Maybe TxId)
begin pactdb = _beginTx pactdb Transactional

{- this should be moved to pact -}
commit :: PactDb e -> Method e [TxLog Value]
commit pactdb = _commitTx pactdb

{- this should be moved to pact -}
runRegression ::
  PactDb e -- your pactdb instance
  -> e -- ambient environment
  -> (MVar e -> IO ()) -- schema "creator"
  -> IO (MVar e) -- the final state of the environment
runRegression pactdb e schemaInit = do
  v <- newMVar e
  schemaInit v
  Just t1 <- begin pactdb v
  let user1 = "user1"
      usert = UserTables user1
      toPV :: ToTerm a => a -> PactValue
      toPV = toPactValueLenient . toTerm'
  _createUserTable pactdb user1 "someModule" v
  assertEquals' "output of commit2"
    [TxLog "SYS:usertables" "user1" $
      object [ ("utModule" .= object [ ("name" .= String "someModule"), ("namespace" .= Null)])
             ]
    ]
    (commit pactdb v)
  void $ begin pactdb v
  {- the below line is commented out because we no longer support _getUserTableInfo -}
  -- assertEquals' "user table info correct" "someModule" $ _getUserTableInfo chainwebpactdb user1 v
  let row = ObjectMap $ M.fromList [("gah", PLiteral (LDecimal 123.454345))]
  _writeRow pactdb Insert usert "key1" row v
  assertEquals' "usert insert" (Just row) (_readRow pactdb usert "key1" v)
  let row' = ObjectMap $ M.fromList [("gah",toPV False),("fh",toPV (1 :: Int))]
  _writeRow pactdb Update usert "key1" row' v
  assertEquals' "user update" (Just row') (_readRow pactdb usert "key1" v)
  let ks = KeySet [PublicKey "skdjhfskj"] (Name "predfun" def)
  _writeRow pactdb Write KeySets "ks1" ks v
  assertEquals' "keyset write" (Just ks) $ _readRow pactdb KeySets "ks1" v
  (modName,modRef,mod') <- loadModule
  _writeRow pactdb Write Modules modName mod' v
  assertEquals' "module write" (Just mod') $ _readRow pactdb Modules modName v
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
    (commit pactdb v)
  void $ begin pactdb v
  tids <- _txids pactdb user1 t1 v
  assertEquals "user txids" [1] tids
  -- assertEquals' "user txlogs"
  --   [TxLog "user1" "key1" row,
  --    TxLog "user1" "key1" row'] $
  --   _getTxLog chainwebpactdb usert (head tids) v
  assertEquals' "user txlogs" [TxLog "user1" "key1" (ObjectMap $ on M.union _objectMap row' row)] $
    _getTxLog pactdb usert (head tids) v
  _writeRow pactdb Insert usert "key2" row v
  assertEquals' "user insert key2 pre-rollback" (Just row) (_readRow pactdb usert "key2" v)
  assertEquals' "keys pre-rollback" ["key1","key2"] $ _keys pactdb (UserTables user1) v
  _rollbackTx pactdb v
  assertEquals' "rollback erases key2" Nothing $ _readRow pactdb usert "key2" v
  assertEquals' "keys" ["key1"] $ _keys pactdb (UserTables user1) v
  return v

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
nativeLookup (NativeDefName n) = case HM.lookup (Name n def) nativeDefs of
  Just (Direct t) -> Just t
  _ -> Nothing
