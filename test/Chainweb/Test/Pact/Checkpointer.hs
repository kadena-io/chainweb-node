{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Test.Pact.Checkpointer
  ( tests
  ) where

import Control.Arrow ((&&&))
import Control.Concurrent (MVar, modifyMVar, newMVar, readMVar)
import Control.Lens (preview, _Just)
import Control.Monad (void)

import Data.Aeson (Value(..), object, (.=))
import Data.Default (def)
import Data.Text (Text)

import NeatInterpolation (text)

import Pact.Gas (freeGasEnv)
import Pact.Interpreter (EvalResult(..), mkPureEnv)
import Pact.Types.Command (ExecutionMode(Transactional))
import qualified Pact.Types.Hash as H
import Pact.Types.Logger (Loggers, newLogger)
import Pact.Types.PactValue
import Pact.Types.RPC (ContMsg(..))
import Pact.Types.Runtime (TxId, noSPVSupport, peStep)
import Pact.Types.Server (CommandConfig(..), CommandEnv(..), CommandState)
import Pact.Types.Term (PactId(..), Term(..), toTList, toTerm)
import Pact.Types.Type (PrimType(..), Type(..))

import Test.Tasty.HUnit

-- internal imports

import Chainweb.BlockHash (BlockHash(..), nullBlockHash)
import Chainweb.BlockHeader (BlockHeight(..))
import Chainweb.MerkleLogHash (merkleLogHash)
import Chainweb.Pact.Backend.InMemoryCheckpointer (initInMemoryCheckpointEnv)
import Chainweb.Pact.Backend.MemoryDb (mkPureState)
import Chainweb.Pact.Backend.Types
    (CheckpointEnv(..), Checkpointer(..), Env'(..), PactDbState(..))
import Chainweb.Pact.TransactionExec
    (applyContinuation', applyExec', buildExecParsedCode)
import Chainweb.Pact.Utils (toEnv', toEnvPersist')
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils

tests :: ScheduledTest
tests = testGroupSch "Checkpointer"
  [ testCase "testInMemory" testInMemory ]

testInMemory :: Assertion
testInMemory = do
  let conf = CommandConfig Nothing Nothing Nothing Nothing
      loggers = pactTestLogger
  cpEnv <- initInMemoryCheckpointEnv conf
        (newLogger loggers "inMemCheckpointer") freeGasEnv
  env <- mkPureEnv loggers
  state <- mkPureState env conf

  testCheckpointer loggers cpEnv state

assertEitherSuccess :: Show l => String -> Either l r -> IO r
assertEitherSuccess msg (Left l) = assertFailure (msg ++ ": " ++ show l)
assertEitherSuccess _ (Right r) = return r

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
(insertTbl "a" 1)
|]

tIntList :: [Int] -> Term n
tIntList = toTList (TyPrim TyInteger) def . map toTerm

testCheckpointer :: Loggers -> CheckpointEnv -> PactDbState -> Assertion
testCheckpointer loggers CheckpointEnv{..} dbState00 = do

  let logger = newLogger loggers "testCheckpointer"

      incTxId mv = modifyMVar mv (return . (id &&& succ))

      runExec :: (MVar CommandState, Env',MVar TxId) -> Maybe Value -> Text -> IO EvalResult
      runExec (mcs, Env' pactDbEnv, txIdV) eData eCode = do
          txId <- incTxId txIdV
          let cmdenv = CommandEnv Nothing (Transactional txId) pactDbEnv mcs logger freeGasEnv def
          execMsg <- buildExecParsedCode eData eCode
          applyExec' cmdenv def execMsg [] (H.toUntypedHash (H.hash "" :: H.PactHash)) noSPVSupport

      runCont :: (MVar CommandState, Env',MVar TxId) -> PactId -> Int -> IO EvalResult
      runCont (mcs, Env' pactDbEnv, txIdV) pactId step = do
          txId <- incTxId txIdV
          let contMsg = ContMsg pactId step False Null
              cmdenv = CommandEnv Nothing (Transactional txId) pactDbEnv mcs logger freeGasEnv def
          applyContinuation' cmdenv def contMsg [] (H.toUntypedHash (H.hash "" :: H.PactHash)) noSPVSupport

      ksData :: Text -> Value
      ksData idx =
          object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]

      unwrapState :: PactDbState -> IO (MVar CommandState, Env', MVar TxId)
      unwrapState dbs = (,,) <$> newMVar (_pdbsState dbs) <*> toEnv' (_pdbsDbEnv dbs) <*> newMVar (_pdbsTxId dbs)

      wrapState :: (MVar CommandState, Env', MVar TxId) -> IO PactDbState
      wrapState (mcs,dbe',txidV) = PactDbState <$> toEnvPersist' dbe' <*> readMVar mcs <*> readMVar txidV


  void $ saveInitial _cpeCheckpointer dbState00
    >>= assertEitherSuccess "saveInitial"

  ------------------------------------------------------------------
  -- s01 : new block workflow (restore -> discard), genesis
  ------------------------------------------------------------------

  -- hmm `restoreInitial` has implicit dependency on (BlockHeight 0), nullBlockHash

  let bh00 = BlockHeight 0
      hash00 = nullBlockHash

  s01 <- restoreInitial _cpeCheckpointer
    >>= assertEitherSuccess "restoreInitial (new block)"
    >>= unwrapState

  void $ runExec s01 (Just $ ksData "1") $ defModule "1"

  runExec s01 Nothing "(m1.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

  void $ wrapState s01
    >>= discard _cpeCheckpointer
    >>= assertEitherSuccess "discard (initial) (new block)"


  ------------------------------------------------------------------
  -- s02 : validate block workflow (restore -> save), genesis
  ------------------------------------------------------------------

  s02 <- restoreInitial _cpeCheckpointer
    >>= assertEitherSuccess "restoreInitial (validate)"
    >>= unwrapState

  -- by defining m1/inserting anew, we ensure it didn't exist before
  -- otherwise would fail at least on table create

  void $ runExec s02 (Just $ ksData "1") $ defModule "1"

  runExec s02 Nothing "(m1.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

  void $ wrapState s02
    >>= save _cpeCheckpointer bh00 hash00
    >>= assertEitherSuccess "save (validate)"


  ------------------------------------------------------------------
  -- s03 : new block 01
  ------------------------------------------------------------------

  s03 <- restore _cpeCheckpointer bh00 hash00
    >>= assertEitherSuccess "restore for new block 01"
    >>= unwrapState

  void $ runExec s03 Nothing "(m1.insertTbl 'b 2)"

  runExec s03 Nothing "(m1.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

  -- start a pact
  -- test is that exec comes back with proper step
  let pactId = 1
      pactCheckStep = preview (_Just . peStep) . _erExec

  runExec s03 Nothing "(m1.dopact 'pactA)"
    >>= ((Just 0 @=?) . pactCheckStep)


  void $ wrapState s03
    >>= discard _cpeCheckpointer
    >>= assertEitherSuccess "discard for block 01"
  -- ^^ note that this is the same discard as on s01 above, and yet ... it works
  -- (ie, doesn't blow away genesis??)


  ------------------------------------------------------------------
  -- s04 : validate block 01
  ------------------------------------------------------------------

  let bh01 = BlockHeight 01
  hash01 <- BlockHash <$> merkleLogHash "0000000000000000000000000000001a"

  s04 <- restore _cpeCheckpointer bh00 hash00
    >>= assertEitherSuccess "restore for validate block 01"
    >>= unwrapState

  -- insert here would fail if new block 01 had not been discarded
  void $ runExec s04 Nothing "(m1.insertTbl 'b 2)"

  runExec s04 Nothing "(m1.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

  -- start a pact at txid 4, would fail if new block 01 had not been discarded
  runExec s04 Nothing "(m1.dopact 'pactA)"
    >>= ((Just 0 @=?) . pactCheckStep)

  void $ wrapState s04
    >>= save _cpeCheckpointer bh01 hash01
    >>= assertEitherSuccess "save block 01"


  ------------------------------------------------------------------
  -- s05: validate block 02
  -- create m2 module, exercise RefStore checkpoint
  -- exec next part of pact
  ------------------------------------------------------------------

  let bh02 = BlockHeight 02
  hash02 <- BlockHash <$> merkleLogHash "0000000000000000000000000000002a"

  s05 <- restore _cpeCheckpointer bh01 hash01
    >>= assertEitherSuccess "restore for validate block 02"
    >>= unwrapState

  void $ runExec s05 (Just $ ksData "2") $ defModule "2"

  runExec s05 Nothing "(m2.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

  runCont s05 pactId 1
    >>= ((Just 1 @=?) . pactCheckStep)

  void $ wrapState s05
    >>= save _cpeCheckpointer bh02 hash02
    >>= assertEitherSuccess "save block 02"

  ------------------------------------------------------------------
  -- s06 : new block 03
  ------------------------------------------------------------------

  s06 <- restore _cpeCheckpointer bh02 hash02
    >>= assertEitherSuccess "restore for new block 03"
    >>= unwrapState

  void $ runExec s06 Nothing "(m2.insertTbl 'b 2)"

  runExec s06 Nothing "(m2.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

  void $ wrapState s06
    >>= discard _cpeCheckpointer
    >>= assertEitherSuccess "discard for block 02"

  ------------------------------------------------------------------
  -- s07 : validate block 03
  ------------------------------------------------------------------

  let bh03 = BlockHeight 03
  hash03 <- BlockHash <$> merkleLogHash "0000000000000000000000000000003a"

  s07 <- restore _cpeCheckpointer bh02 hash02
    >>= assertEitherSuccess "restore for validate block 03"
    >>= unwrapState

  -- insert here would fail if new block 03 had not been discarded
  void $ runExec s07 Nothing "(m2.insertTbl 'b 2)"

  runExec s07 Nothing "(m2.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

  void $ wrapState s07
    >>= save _cpeCheckpointer bh03 hash03
    >>= assertEitherSuccess "save block 01"


------------------------------------------------------------------
  -- s08: FORK! block 02, new hash
  -- recreate m2 module, exercise RefStore checkpoint
  -- exec next part of pact
  ------------------------------------------------------------------

  hash02Fork <- BlockHash <$> merkleLogHash "0000000000000000000000000000002b"

  s08 <- restore _cpeCheckpointer bh01 hash01
    >>= assertEitherSuccess "restore for validate block 02"
    >>= unwrapState

  void $ runExec s08 (Just $ ksData "2") $ defModule "2"

  runExec s08 Nothing "(m2.readTbl)"
    >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

  -- this would fail if not a fork
  runCont s08 pactId 1
    >>= ((Just 1 @=?) . pactCheckStep)

  void $ wrapState s08
    >>= save _cpeCheckpointer bh02 hash02Fork
    >>= assertEitherSuccess "save block 02"
