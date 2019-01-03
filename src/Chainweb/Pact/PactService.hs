-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact service for Chainweb

{-#Language LambdaCase#-}
{-#Language RecordWildCards#-}
{-#Language RankNTypes#-}

module Chainweb.Pact.PactService
  ( initPactService
  , newTransactionBlock
  , validateBlock
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Trans.RWS.Lazy
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.String.Conv (toS)
import qualified Data.Yaml as Y
import Control.Monad.IO.Class

import qualified Chainweb.BlockHeader as C
import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import Pact.Types.Server as P
import qualified Pact.Types.SQLite as P (SQLiteConfig (..), Pragma(..))

import Chainweb.Pact.Backend.Types
import Chainweb.Pact.MemoryDb
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.SqliteDb
import Chainweb.Pact.Types

initPactService :: IO ()
initPactService = do
  let loggers = P.neverLog
  let logger = P.newLogger loggers $ P.LogName "PactService"
  pactCfg <- setupConfig "pact.yaml" -- TODO: file name/location from configuration
  let cmdConfig = toCommandConfig pactCfg
  theState' <- case _ccSqlite cmdConfig of
    Nothing -> do
      env <- P.mkPureEnv loggers
      mkPureState env cmdConfig logger
    Just sqlc -> do
      env <- P.mkSQLiteEnv logger False sqlc loggers
      mkSQLiteState env cmdConfig logger
  checkpointer <- initPactCheckpointer
  theStore <- initPactCheckpointStore
  let env = CheckpointEnv {_cpeCheckpointer = checkpointer, _cpeCommandConfig = cmdConfig, _cpeCheckpointStore = theStore  }
  void $ runRWST serviceRequests env theState'
  return ()

serviceRequests :: PactT p c ()
serviceRequests =
  forever $ do
  return () --TODO: get / service requests for new blocks and verification

newTransactionBlock :: P.Hash -> C.BlockHeight -> PactT p c Block
newTransactionBlock parentHash blockHeight = do
  newTrans <- requestTransactions TransactionCriteria
  unless (isFirstBlock parentHash blockHeight) $ do
    checkpointer <- view cpeCheckpointer
    checkpointStore <- view cpeCheckpointStore
    st <- buildCurrentPactState
    mRestoredState <-
      liftIO $
      _cPrepare
        checkpointer
        blockHeight
        parentHash
        NewBlock
        (liftA3 CheckpointData _pdbsDbEnv (P._csRefStore . _pdbsState) (P._csPacts . _pdbsState) st)
        checkpointStore
    either
      error
      (\s -> do dbstate <- liftIO $ getDbState checkpointer blockHeight parentHash s
                put dbstate)
      mRestoredState
  theState <- get
  results <- liftIO $ execTransactions theState newTrans
  return
    Block
      { _bHash = Nothing -- not yet computed
      , _bParentHash = parentHash
      , _bBlockHeight = succ blockHeight
      , _bTransactions = zip newTrans results
      }

getDbState :: Checkpointer p c -> C.BlockHeight -> P.Hash -> c -> IO PactDbState'
getDbState = undefined

setupConfig :: FilePath -> IO PactDbConfig
setupConfig configFile = do
  Y.decodeFileEither configFile >>= \case
    Left e -> do
      putStrLn usage
      throwIO (userError ("Error loading config file: " ++ show e))
    (Right v) -> return  v

toCommandConfig :: PactDbConfig -> P.CommandConfig
toCommandConfig PactDbConfig {..} =
  P.CommandConfig
  { _ccSqlite = mkSqliteConfig _pdbcPersistDir _pdbcPragmas
  , _ccEntity = Nothing
  , _ccGasLimit = _pdbcGasLimit
  , _ccGasRate = _pdbcGasRate
  }

-- SqliteConfig is part of Pact' CommandConfig datatype, which is used with both in-memory and
-- squlite databases -- hence this is here and not in the Sqlite specific module
mkSqliteConfig :: Maybe FilePath -> [P.Pragma] -> Maybe P.SQLiteConfig
mkSqliteConfig (Just f) xs = Just (P.SQLiteConfig {dbFile = f, pragmas = xs})
mkSqliteConfig _ _ = Nothing

-- TODO: determing correct way to check for the first block
isFirstBlock :: P.Hash -> C.BlockHeight -> Bool
isFirstBlock _hash _height = False

validateBlock :: forall p c. PactDbBackend p => Block -> PactT p c ()
validateBlock Block {..} = do
  checkpointer <- view cpeCheckpointer
  case _bHash of
    Nothing -> liftIO $ putStrLn "Block to be validated is missing hash" -- TBD log, throw, etc.
    Just theHash -> do
      unless (isFirstBlock _bParentHash _bBlockHeight) $ do
        st <- buildCurrentPactState
        checkpointStore <- view cpeCheckpointStore
        mRestoredState <-
          liftIO $
          _cPrepare
            checkpointer
            _bBlockHeight
            theHash
            Validation
            (liftA3 CheckpointData _pdbsDbEnv (P._csRefStore . _pdbsState) (P._csPacts . _pdbsState) st)
            checkpointStore
        either error (\s -> do dbstate <- liftIO $ getDbState checkpointer _bBlockHeight _bParentHash s
                               put dbstate) mRestoredState
          -- (put . _cGetPactDbState checkpointStore _bBlockHeight _bParentHash)
      currentState <- get
      _results <-
        liftIO $ execTransactions currentState (fmap fst _bTransactions)
      newState <- buildCurrentPactState
      put (PactDbState' newState)
      st <- buildCurrentPactState
      checkpointStore <- view cpeCheckpointStore
      liftIO $
        _cSave
          checkpointer
          _bBlockHeight
          _bParentHash
          Validation
          (liftA3
             CheckpointData
             _pdbsDbEnv
             (P._csRefStore . _pdbsState)
             (P._csPacts . _pdbsState)
             st)
          checkpointStore

      -- liftIO $ makeCheckpoint theHash _bBlockHeight newState checkpointStore
      -- TODO: TBD what do we need to do for validation and what is the return type?

--placeholder - get transactions from mem pool
requestTransactions :: TransactionCriteria -> PactT p c [Transaction]
requestTransactions _crit = return []

execTransactions :: PactDbState' -> [Transaction] -> IO [TransactionOutput]
execTransactions pactState' xs =
  forM xs (\Transaction {..} -> do
    let txId = P.Transactional (P.TxId _tTxId)
    liftIO $ TransactionOutput <$> applyPactCmd pactState' txId _tCmd)

applyPactCmd :: PactDbState' -> P.ExecutionMode -> P.Command ByteString -> IO P.CommandResult
applyPactCmd (PactDbState' pactState) eMode cmd = do
  let cmdState = _pdbsState pactState
  newVar <-  newMVar cmdState
  let logger = _pdbsLogger pactState
  let gasEnv = _pdbsGasEnv pactState
  let pactDbEnv = _pdbsDbEnv pactState
  applyCmd logger Nothing pactDbEnv newVar gasEnv eMode cmd (P.verifyCommand cmd)

_hashResults :: [P.CommandResult] -> P.Hash
_hashResults cmdResults =
  let bs = foldMap (A.encode . P._crResult ) cmdResults
  in P.hash $ toS bs

buildCurrentPactState :: PactT p c (PactDbState p)
buildCurrentPactState = undefined


-- buildCurrentPactState :: forall p c. PactDbBackend p => PactT p c PactDbState'
-- buildCurrentPactState = undefined
-- -- this could be `get`
