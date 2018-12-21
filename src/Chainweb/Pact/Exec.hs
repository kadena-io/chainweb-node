-- |
-- Module: Chainweb.Pact.Exec
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact execution module for Chainweb

{-#Language LambdaCase#-}
{-#Language RecordWildCards#-}

module Chainweb.Pact.Exec
  ( SQLite(..)
  , TableStmts(..)
  , TxStmts(..)
  , initPactService
  , newTransactionBlock
  , validateBlock
  ) where

----------------------------------------------------------------------------------------------------
-- At least for now, compile-time change to change between in-memory db and Sqlite
import qualified Chainweb.Pact.MemoryDb as DB
-- import quaified Chainweb.Pact.SqliteDb as DB

----------------------------------------------------------------------------------------------------
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.RWS.Lazy
import qualified Data.Aeson as A
import Data.String.Conv (toS)
import qualified Data.Yaml as Y
import qualified Database.SQLite3.Direct as SQ3
import Control.Monad.IO.Class
import Data.Map (Map)

import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.RPC as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P
import qualified Pact.Types.SQLite as P (SQLiteConfig (..), Pragma(..))

import Chainweb.Pact.MapPureCheckpoint
import Chainweb.Pact.PactService
import Chainweb.Pact.Types

-- TODO: SQLite, TxStmts, TableStmts can be exported from Pact.Persist.SQLite and then these
-- definitions of SQLite and TxStmts can be removed
data SQLite = SQLite
  { conn :: SQ3.Database
  , config :: P.SQLiteConfig
  , logger :: P.Logger
  , tableStmts :: Map SQ3.Utf8 TableStmts
  , txStmts :: TxStmts
  }

data TxStmts = TxStmts
  { tBegin :: SQ3.Statement
  , tCommit :: SQ3.Statement
  , tRollback :: SQ3.Statement
  }

data TableStmts = TableStmts
  { sInsertReplace :: SQ3.Statement
  , sInsert :: SQ3.Statement
  , sReplace :: SQ3.Statement
  , sRead :: SQ3.Statement
  }

initPactService :: IO ()
initPactService = do
  pactCfg <- setupConfig "pact.yaml" -- TODO: file name/location from configuration
  let cmdConfig = toCommandConfig pactCfg
  theStore <- initPactCheckpointStore :: IO MapPurePactCheckpointStore
  let env = CheckpointEnv {_cpeCheckpointStore = theStore, _cpeCommandConfig = cmdConfig }
  theState <- DB.initService cmdConfig P.neverLog
  _ <- runRWST serviceRequests env theState
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
  cei <- liftIO $ restoreCEI theState
  results <- liftIO $ execTransactions cei newTrans
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
      theCei <- liftIO $ restoreCEI currentState
      _results <- liftIO $ execTransactions theCei (fmap fst _bTransactions)
      newState <- buildCurrentPactState
      put newState
      liftIO $ makeCheckpoint theHash _bBlockHeight newState checkpointStore
      -- TODO: TBD what do we need to do for validation and what is the return type?
      return ()

--placeholder - get transactions from mem pool
requestTransactions :: TransactionCriteria -> PactT [Transaction]
requestTransactions _crit = return []

execTransactions :: P.CommandExecInterface (P.PactRPC P.ParsedCode)
                 -> [Transaction] -> IO [P.CommandResult]
execTransactions P.CommandExecInterface {..} xs =
  forM xs (\Transaction {..} -> do
    let txId = P.TxId _tTxId
    liftIO $ _ceiApplyCmd (P.Transactional txId) _tCmd)

_hashResults :: [P.CommandResult] -> P.Hash
_hashResults cmdResults =
  let bs = foldMap (\cr -> A.encode (P._crResult cr)) cmdResults
  in P.hash $ toS bs

buildCurrentPactState :: PactT PactDbState
buildCurrentPactState = undefined
