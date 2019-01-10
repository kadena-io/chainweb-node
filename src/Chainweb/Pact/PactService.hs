-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact service for Chainweb

{-#language LambdaCase#-}
{-#language RecordWildCards#-}

module Chainweb.Pact.PactService
    ( initPactService
    , mkPureState
    , mkSQLiteState
    , newTransactionBlock
    , serviceRequests
    , setupConfig
    , toCommandConfig
    , validateBlock
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.RWS.Lazy
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.Yaml as Y
import Control.Monad.IO.Class

import qualified Pact.Gas as P
import Chainweb.BlockHeader
import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P
import qualified Pact.Types.SQLite as P (SQLiteConfig (..), Pragma(..))

import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.Backend.SQLiteCheckpointer
import Chainweb.Pact.Backend.Types
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
    let gasLimit = fromMaybe 0 (P._ccGasLimit cmdConfig)
    let gasRate = fromMaybe 0 (P._ccGasRate cmdConfig)
    let gasEnv = P.GasEnv (fromIntegral gasLimit) 0.0 (P.constGasModel (fromIntegral gasRate))
    (checkpointEnv, theState) <-
        case P._ccSqlite cmdConfig of
            Nothing -> do
                env <- P.mkPureEnv loggers
                liftA2 (,) (initInMemoryCheckpointEnv cmdConfig logger gasEnv) (mkPureState env cmdConfig)
            Just sqlc -> do
                env <- P.mkSQLiteEnv logger False sqlc loggers
                liftA2 (,) (initSQLiteCheckpointEnv cmdConfig logger gasEnv) (mkSQLiteState env cmdConfig)
    void $ runRWST serviceRequests checkpointEnv theState

serviceRequests :: PactT ()
serviceRequests =
    forever $ do
    return () --TODO: get / service requests for new blocks and verification

newTransactionBlock :: BlockHeader -> BlockHeight -> PactT Block
newTransactionBlock parentHeader bHeight = do
    let parentPayloadHash = _blockPayloadHash parentHeader
    newTrans <- requestTransactions TransactionCriteria
    unless (isFirstBlock bHeight) $ do
        CheckpointEnv' cpEnv <- ask
        let checkpointer =    _cpeCheckpointer cpEnv
            checkpointStore = _cpeCheckpointStore cpEnv
        st <- buildCurrentPactState
        mRestoredState <-
            liftIO $
            _cPrepare
                checkpointer bHeight parentPayloadHash NewBlock
                (liftA3 CheckpointData _pdbsDbEnv (P._csRefStore . _pdbsState) (P._csPacts . _pdbsState) st)
                checkpointStore
        either error (liftIO . getDbState checkpointer bHeight parentPayloadHash >=> put) mRestoredState
    theState <- get
    CheckpointEnv' cpEnv <- ask
    results <- liftIO $ execTransactions cpEnv theState newTrans
    return
        Block
            { _bHash = Nothing -- not yet computed
            , _bParentHeader = parentHeader
            , _bBlockHeight = succ bHeight
            , _bTransactions = zip newTrans results
            }

getDbState :: Checkpointer c -> BlockHeight -> BlockPayloadHash -> c -> IO PactDbState
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
mkSqliteConfig (Just f) xs = Just P.SQLiteConfig {dbFile = f, pragmas = xs}
mkSqliteConfig _ _ = Nothing

isFirstBlock :: BlockHeight -> Bool
isFirstBlock height = height == 0

validateBlock :: Block -> PactT ()
validateBlock Block {..} = do
    let parentPayloadHash = _blockPayloadHash _bParentHeader
    CheckpointEnv' cpEnv <- ask
    let checkpointer = _cpeCheckpointer cpEnv
        checkpointStore = _cpeCheckpointStore cpEnv
    unless (isFirstBlock _bBlockHeight) $ do
        st <- buildCurrentPactState
        mRestoredState <-
            liftIO $ _cPrepare checkpointer _bBlockHeight parentPayloadHash Validation
                (liftA3 CheckpointData _pdbsDbEnv (P._csRefStore . _pdbsState) (P._csPacts . _pdbsState) st)
                checkpointStore
        either error ((liftIO . getDbState checkpointer _bBlockHeight parentPayloadHash) >=> put) mRestoredState
        currentState <- get
        _results <- liftIO $ execTransactions cpEnv currentState (fmap fst _bTransactions)
        buildCurrentPactState >>= put
    st <- get
    CheckpointEnv' cpEnvNew <- ask
    let checkpointStoreNew = _cpeCheckpointStore cpEnvNew
        checkpointerNew = _cpeCheckpointer cpEnvNew
    liftIO $
        _cSave checkpointerNew _bBlockHeight parentPayloadHash Validation
            (liftA3 CheckpointData _pdbsDbEnv (P._csRefStore . _pdbsState) (P._csPacts . _pdbsState) st)
            checkpointStoreNew
    -- TODO: TBD what do we need to do for validation and what is the return type?

--placeholder - get transactions from mem pool
requestTransactions :: TransactionCriteria -> PactT [Transaction]
requestTransactions _crit = return []

execTransactions :: CheckpointEnv c -> PactDbState -> [Transaction] -> IO [TransactionOutput]
execTransactions cpEnv pactState xs =
    forM xs (\Transaction {..} -> do
        let txId = P.Transactional (P.TxId _tTxId)
        liftIO $ TransactionOutput <$> applyPactCmd cpEnv pactState txId _tCmd)

applyPactCmd :: CheckpointEnv c -> PactDbState -> P.ExecutionMode -> P.Command ByteString -> IO P.CommandResult
applyPactCmd cpEnv pactState eMode cmd = do
    let cmdState = _pdbsState pactState
    newVar <-  newMVar cmdState
    let logger = _cpeLogger cpEnv
    let gasEnv = _cpeGasEnv cpEnv
    let pactDbEnv' = _pdbsDbEnv pactState
    case pactDbEnv' of
        Env' pactDbEnv -> applyCmd logger Nothing pactDbEnv newVar gasEnv eMode cmd (P.verifyCommand cmd)

buildCurrentPactState :: PactT PactDbState
buildCurrentPactState = undefined
