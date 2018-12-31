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

module Chainweb.Pact.PactService
  ( initPactService
  , newTransactionBlock
  , validateBlock
  ) where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.RWS.Lazy
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Maybe
import Data.String.Conv (toS)
import qualified Data.Yaml as Y
import Control.Monad.IO.Class

import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import Pact.Types.Server as P
import qualified Pact.Types.SQLite as P (SQLiteConfig (..), Pragma(..))

import Chainweb.Pact.Backend.MapCheckpoint
import Chainweb.Pact.Backend.MemoryDb
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Backend.SqliteDb
import Chainweb.Pact.Types

initPactService :: IO ()
initPactService = do
  let loggers = P.neverLog
  let logger = P.newLogger loggers $ P.LogName "PactService"
  pactCfg <- setupConfig "pact.yaml" -- TODO: file name/location from configuration
  let cmdConfig = toCommandConfig pactCfg
  let gasLimit = fromMaybe 0 (_ccGasLimit cmdConfig)
  let gasRate = fromMaybe 0 (_ccGasRate cmdConfig)
  let gasEnv = P.GasEnv (fromIntegral gasLimit) 0.0 (P.constGasModel (fromIntegral gasRate))
  theState' <- case _ccSqlite cmdConfig of
    Nothing -> do
      env <- P.mkPureEnv loggers
      mkPureState env
    Just sqlc -> do
      env <- P.mkSQLiteEnv logger False sqlc loggers
      mkSQLiteState env
  theStore <- initPactCheckpointStore
  let env = CheckpointEnv { _cpeCheckpointStore = theStore, _cpeCommandConfig = cmdConfig
                          , _cpeLogger = logger, _cpeGasEnv = gasEnv }
  _ <- runRWST serviceRequests env theState'
  return ()

serviceRequests :: PactT ()
serviceRequests =
  forever $ do
  return () --TODO: get / service requests for new blocks and verification

newTransactionBlock :: P.Hash -> Integer -> PactT Block
newTransactionBlock parentHash blockHeight = do
  newTrans <- requestTransactions TransactionCriteria
  unless (isFirstBlock parentHash blockHeight) $ do
    checkpointStore <- view cpeCheckpointStore
    mRestoredState <- liftIO $ restoreCheckpoint parentHash blockHeight checkpointStore
    whenJust mRestoredState put
  theState <- get
  cpEnv <- ask
  results <- liftIO $ execTransactions cpEnv theState newTrans
  return Block
    { _bHash = Nothing -- not yet computed
    , _bParentHash = parentHash
    , _bBlockHeight = blockHeight + 1
    , _bTransactions = zip newTrans results
    }

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
isFirstBlock :: P.Hash -> Integer -> Bool
isFirstBlock _hash _height = False

validateBlock :: Block -> PactT ()
validateBlock Block {..} = do
  checkpointStore <- view cpeCheckpointStore
  case _bHash of
    Nothing -> liftIO $ putStrLn "Block to be validated is missing hash"  -- TBD log, throw, etc.
    Just theHash -> do
      unless (isFirstBlock _bParentHash _bBlockHeight) $ do
        mRestoredState <- liftIO $ restoreCheckpoint theHash _bBlockHeight checkpointStore
        whenJust mRestoredState put
      currentState <- get
      cpEnv <- ask
      _results <- liftIO $ execTransactions cpEnv currentState (fmap fst _bTransactions)
      newState <- buildCurrentPactState
      put newState
      liftIO $ makeCheckpoint theHash _bBlockHeight newState checkpointStore
      -- TODO: TBD what do we need to do for validation and what is the return type?
      return ()

--placeholder - get transactions from mem pool
requestTransactions :: TransactionCriteria -> PactT [Transaction]
requestTransactions _crit = return []

execTransactions :: CheckpointEnv -> PactDbState' -> [Transaction] -> IO [TransactionOutput]
execTransactions cpEnv pactState' xs =
  forM xs (\Transaction {..} -> do
    let txId = P.Transactional (P.TxId _tTxId)
    liftIO $ TransactionOutput <$> applyPactCmd cpEnv pactState' txId _tCmd)

applyPactCmd :: CheckpointEnv -> PactDbState' -> P.ExecutionMode
             -> P.Command ByteString -> IO P.CommandResult
applyPactCmd cpEnv (PactDbState' pactState) eMode cmd = do
  let cmdState = view pdbsState pactState
  newVar <-  newMVar cmdState
  let logger = _cpeLogger cpEnv
  let gasEnv = _cpeGasEnv cpEnv
  let pactDbEnv = view pdbsDbEnv pactState
  applyCmd logger Nothing pactDbEnv newVar gasEnv eMode cmd (P.verifyCommand cmd)

_hashResults :: [P.CommandResult] -> P.Hash
_hashResults cmdResults =
  let bs = foldMap (A.encode . P._crResult ) cmdResults
  in P.hash $ toS bs

buildCurrentPactState :: PactT PactDbState'
buildCurrentPactState = undefined
