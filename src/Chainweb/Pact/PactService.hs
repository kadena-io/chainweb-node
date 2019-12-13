{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Mark Nichols <mark@kadena.io>, Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Pact service for Chainweb
--
module Chainweb.Pact.PactService
    ( -- * SQLite Database
      withSqliteDb
    , startSqliteDb
    , stopSqliteDb
      -- * For Chainweb
    , initialPayloadState
    , execNewBlock
    , execValidateBlock
    , execTransactions
    , initPactService
    , readCoinAccount
    , readAccountBalance
    , readAccountGuard
      -- * For Side-tooling
    , execNewGenesisBlock
    , initPactService'
    , minerReward
    ) where
------------------------------------------------------------------------------
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception (SomeAsyncException, evaluate)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Aeson as A
import Data.Bifoldable (bitraverse_)
import Data.Bifunctor (first)
import Data.Bool (bool)
import qualified Data.ByteString.Short as SB
import Data.Decimal
import Data.Default (def)
import Data.DList (DList(..))
import qualified Data.DList as DL
import Data.Either
import Data.Foldable (foldl', toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tuple.Strict (T2(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word

import System.Directory
import System.IO
import System.LogLevel

import Prelude hiding (lookup)


------------------------------------------------------------------------------
-- external pact modules

import Pact.Compile (compileExps)
import qualified Pact.Gas as P
import Pact.Gas.Table
import qualified Pact.Interpreter as P
import qualified Pact.Parse as P
import qualified Pact.Types.ChainMeta as P
import qualified Pact.Types.Command as P
import Pact.Types.ExpParser (mkTextInfo)
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.PactValue as P
import Pact.Types.RPC
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.SPV as P
import Pact.Types.Term (Term(..))

------------------------------------------------------------------------------
-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeader, genesisBlockPayload)
import Chainweb.BlockHeaderDB
import Chainweb.Logger
import Chainweb.Mempool.Mempool as Mempool
import Chainweb.Miner.Pact
import Chainweb.NodeId
import Chainweb.Pact.Backend.RelationalCheckpointer (initRelationalCheckpointer)
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Service.PactQueue (PactQueue, getNextRequest)
import Chainweb.Pact.Service.Types
import Chainweb.Pact.SPV
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Pact.Utils
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.TreeDB (collectForkBlocks, lookup, lookupM)
import Chainweb.Utils
import Chainweb.Version
import Data.CAS (casLookupM)

-- -------------------------------------------------------------------------- --

withSqliteDb
    :: Logger logger
    => ChainwebVersion
    -> ChainId
    -> logger
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> (SQLiteEnv -> IO a)
    -> IO a
withSqliteDb ver cid logger dbDir nodeid resetDb = bracket
    (startSqliteDb ver cid logger dbDir nodeid resetDb)
    stopSqliteDb

startSqliteDb
    :: Logger logger
    => ChainwebVersion
    -> ChainId
    -> logger
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> IO SQLiteEnv
startSqliteDb ver cid logger dbDir nodeid doResetDb = do
    sqlitedir <- getSqliteDir
    when doResetDb $ resetDb sqlitedir
    createDirectoryIfMissing True sqlitedir
    textLog Info $ mconcat
        [ "opened sqlitedb for "
        , sshow cid
        , " in directory "
        , sshow sqlitedir
        ]
    let sqlitefile = getSqliteFile sqlitedir
    textLog Info $ "opening sqlitedb named " <> T.pack sqlitefile
    openSQLiteConnection sqlitefile chainwebPragmas
  where
    textLog = logFunctionText logger

    resetDb sqlitedir = do
      exist <- doesDirectoryExist sqlitedir
      when exist $ removeDirectoryRecursive sqlitedir

    getSqliteFile dir = mconcat
        [ dir
        , "/pact-v1-chain-"
        , T.unpack (chainIdToText cid)
        , ".sqlite"
        ]

    getSqliteDir = case dbDir of
        Nothing -> getXdgDirectory XdgData $ mconcat
            [ "chainweb-node/"
            , show ver
            , maybe mempty (("/" <>) . T.unpack . toText) nodeid
            , "/sqlite"
            ]
        Just d -> return (d <> "sqlite")

stopSqliteDb :: SQLiteEnv -> IO ()
stopSqliteDb = closeSQLiteConnection

------------------------------------------------------------------------------

pactLogLevel :: String -> LogLevel
pactLogLevel "INFO" = Info
pactLogLevel "ERROR" = Error
pactLogLevel "DEBUG" = Debug
pactLogLevel "WARN" = Warn
pactLogLevel _ = Info

pactLoggers :: Logger logger => logger -> P.Loggers
pactLoggers logger = P.Loggers $ P.mkLogger (error "ignored") fun def
  where
    fun :: P.LoggerLogFun
    fun _ (P.LogName n) cat msg = do
        let namedLogger = addLabel ("logger", T.pack n) logger
        logFunctionText namedLogger (pactLogLevel cat) $ T.pack msg

initPactService
    :: Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> ChainId
    -> logger
    -> PactQueue
    -> MemPoolAccess
    -> BlockHeaderDb
    -> PayloadDb cas
    -> SQLiteEnv
    -> Word64
    -> IO ()
initPactService ver cid chainwebLogger reqQ mempoolAccess bhDb pdb sqlenv deepForkLimit =
    initPactService' ver cid chainwebLogger bhDb pdb sqlenv deepForkLimit $ do
        initialPayloadState chainwebLogger ver cid
        serviceRequests mempoolAccess reqQ

initPactService'
    :: Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> ChainId
    -> logger
    -> BlockHeaderDb
    -> PayloadDb cas
    -> SQLiteEnv
    -> Word64
    -> PactServiceM cas a
    -> IO a
initPactService' ver cid chainwebLogger bhDb pdb sqlenv reorgLimit act = do
    checkpointEnv <- initRelationalCheckpointer initBlockState sqlenv logger
    let !rs = readRewards ver
        !gasModel = officialGasModel
        !t0 = BlockCreationTime $ Time (TimeSpan (Micros 0))
        !pse = PactServiceEnv
                { _psMempoolAccess = Nothing
                , _psCheckpointEnv = checkpointEnv
                , _psPdb = pdb
                , _psBlockHeaderDb = bhDb
                , _psGasModel = gasModel
                , _psMinerRewards = rs
                , _psEnableUserContracts = enableUserContracts ver
                , _psReorgLimit = reorgLimit
                , _psOnFatalError = defaultOnFatalError (logFunctionText chainwebLogger)
                }
        !pst = PactServiceState Nothing mempty 0 t0 Nothing P.noSPVSupport
    evalPactServiceM pst pse act
  where
    loggers = pactLoggers chainwebLogger
    logger = P.newLogger loggers $ P.LogName ("PactService" <> show cid)

initialPayloadState
    :: Logger logger
    => PayloadCas cas
    => logger
    -> ChainwebVersion
    -> ChainId
    -> PactServiceM cas ()
initialPayloadState _ Test{} _ = pure ()
initialPayloadState _ TimedConsensus{} _ = pure ()
initialPayloadState _ PowConsensus{} _ = pure ()
initialPayloadState logger v@TimedCPM{} cid =
    initializeCoinContract logger v cid $ genesisBlockPayload v cid
initialPayloadState logger v@FastTimedCPM{} cid =
    initializeCoinContract logger v cid $ genesisBlockPayload v cid
initialPayloadState logger  v@Development cid =
    initializeCoinContract logger v cid $ genesisBlockPayload v cid
initialPayloadState logger v@Testnet04 cid =
    initializeCoinContract logger v cid $ genesisBlockPayload v cid
initialPayloadState logger v@Mainnet01 cid =
    initializeCoinContract logger v cid $ genesisBlockPayload v cid

initializeCoinContract
    :: forall cas logger. (PayloadCas cas, Logger logger)
    => logger
    -> ChainwebVersion
    -> ChainId
    -> PayloadWithOutputs
    -> PactServiceM cas ()
initializeCoinContract logger v cid pwo = do
    cp <- getCheckpointer
    genesisExists <- liftIO $ _cpLookupBlockInCheckpointer cp (0, ghash)
    if genesisExists
      then do
          pdb <- asks _psPdb
          bhdb <- asks _psBlockHeaderDb
          reloadedCache <- liftIO $
              withTempSQLiteConnection chainwebPragmas $ \sqlenv ->
                  -- Note: initPactService' here creates its own isolated (in terms
                  -- of it values) version of the PactServiceM monad. Yeah purity!
                  initPactService' v cid logger bhdb pdb sqlenv defaultReorgLimit $ do
                      -- it is reasonable to assume genesis doesn't exist here
                      validateGenesis
                      gets _psInitCache
          psInitCache .= reloadedCache
      else validateGenesis

  where
    validateGenesis = do
        txs <- execValidateBlock genesisHeader inputPayloadData
        bitraverse_ throwM pure $ validateHashes genesisHeader txs inputPayloadData

    ghash :: BlockHash
    ghash = _blockHash genesisHeader

    inputPayloadData :: PayloadData
    inputPayloadData = payloadWithOutputsToPayloadData pwo

    genesisHeader :: BlockHeader
    genesisHeader = genesisBlockHeader v cid

-- | Loop forever, serving Pact execution requests and reponses from the queues
serviceRequests
    :: forall cas
    . PayloadCas cas
    => MemPoolAccess
    -> PactQueue
    -> PactServiceM cas ()
serviceRequests memPoolAccess reqQ = do
    logInfo "Starting service"
    go `finally` logInfo "Stopping service"
  where
    go = do
        logDebug "serviceRequests: wait"
        msg <- liftIO $ getNextRequest reqQ
        logDebug $ "serviceRequests: " <> sshow msg
        case msg of
            CloseMsg -> return ()
            LocalMsg LocalReq{..} -> do
                tryOne "execLocal" _localResultVar $ execLocal _localRequest
                go
            NewBlockMsg NewBlockReq {..} -> do
                tryOne "execNewBlock" _newResultVar $
                    execNewBlock memPoolAccess _newBlockHeader _newMiner
                        _newCreationTime
                go
            ValidateBlockMsg ValidateBlockReq {..} -> do
                tryOne' "execValidateBlock"
                        _valResultVar
                        (flip (validateHashes _valBlockHeader) _valPayloadData)
                        (execValidateBlock _valBlockHeader _valPayloadData)
                go
            LookupPactTxsMsg (LookupPactTxsReq restorePoint txHashes resultVar) -> do
                tryOne "execLookupPactTxs" resultVar $
                    execLookupPactTxs restorePoint txHashes
                go
            PreInsertCheckMsg (PreInsertCheckReq txs resultVar) -> do
                tryOne "execPreInsertCheckReq" resultVar $
                    fmap (V.map (() <$)) $ execPreInsertCheckReq txs
                go

    toPactInternalError e = Left $ PactInternalError $ T.pack $ show e

    tryOne
        :: String
        -> MVar (Either PactException a)
        -> PactServiceM cas a
        -> PactServiceM cas ()
    tryOne which mvar m = tryOne' which mvar Right m

    tryOne'
        :: String
        -> MVar (Either PactException b)
        -> (a -> Either PactException b)
        -> PactServiceM cas a
        -> PactServiceM cas ()
    tryOne' which mvar post m =
        (evalPactOnThread (post <$> m) >>= (liftIO . putMVar mvar))
        `catches`
            [ Handler $ \(e :: SomeAsyncException) -> do
                logError $ mconcat
                    [ "Received asynchronous exception running pact service ("
                    , which
                    , "): "
                    , show e
                    ]
                liftIO $ do
                    void $ tryPutMVar mvar $! toPactInternalError e
                    throwM e
            , Handler $ \(e :: SomeException) -> do
                logError $ mconcat
                    [ "Received exception running pact service ("
                    , which
                    , "): "
                    , show e
                    ]
                liftIO $ void $ tryPutMVar mvar $! toPactInternalError e
           ]
      where
        -- Pact turns AsyncExceptions into textual exceptions within
        -- PactInternalError. So there is no easy way for us to distinguish
        -- whether an exception originates from within pact or from the outside.
        --
        -- A common strategy to deal with this is to run the computation (pact)
        -- on a "hidden" internal thread. Lifting `forkIO` into a state
        -- monad is generally note thread-safe. It is fine to do here, since
        -- there is no concurrency. We use a thread here only to shield the
        -- computation from external exceptions.
        --
        -- This solution isn't bullet-proof and only meant as a temporary fix. A
        -- proper solution is to fix pact, to handle asynchronous exceptions
        -- gracefully.
        --
        -- No mask is needed here. Asynchronous exceptions are handled
        -- by the outer handlers and cause an abort. So no state is lost.
        --
        evalPactOnThread :: PactServiceM cas a -> PactServiceM cas a
        evalPactOnThread act = do
            e <- ask
            s <- get
            T2 r s' <- liftIO $
                withAsync (runPactServiceM s e act) wait
            put $! s'
            return $! r

toTransactionBytes :: P.Command Text -> Transaction
toTransactionBytes cwTrans =
    let plBytes = encodeToByteString cwTrans
    in Transaction { _transactionBytes = plBytes }


toOutputBytes :: P.CommandResult P.Hash -> TransactionOutput
toOutputBytes cr =
    let outBytes = A.encode cr
    in TransactionOutput { _transactionOutputBytes = toS outBytes }

toPayloadWithOutputs :: Miner -> Transactions -> PayloadWithOutputs
toPayloadWithOutputs mi ts =
    let oldSeq = _transactionPairs ts
        trans = fst <$> oldSeq
        transOuts = toOutputBytes . snd <$> oldSeq

        miner = toMinerData mi
        cb = CoinbaseOutput $ encodeToByteString $ _transactionCoinbase ts
        blockTrans = snd $ newBlockTransactions miner trans
        blockOuts = snd $ newBlockOutputs cb transOuts

        blockPL = blockPayload blockTrans blockOuts
        plData = payloadData blockTrans blockPL
     in payloadWithOutputs plData cb transOuts


validateHashes
    :: BlockHeader
    -> PayloadWithOutputs
    -> PayloadData
    -> Either PactException PayloadWithOutputs
validateHashes bHeader pwo pData =
    if newHash == prevHash
    then Right pwo
    else Left $ BlockValidationFailure $ A.object
         [ "message" A..= ("Payload hash from Pact execution does not match previously stored hash" :: T.Text)
         , "actual" A..= newHash
         , "expected" A..= prevHash
         , "payloadWithOutputs" A..= pwo
         , "otherMismatchs" A..= mismatchs
         ]
    where
      newHash = _payloadWithOutputsPayloadHash pwo
      newTransactions = V.map fst (_payloadWithOutputsTransactions pwo)
      newMiner = _payloadWithOutputsMiner pwo
      newTransactionsHash = _payloadWithOutputsTransactionsHash pwo
      newOutputsHash = _payloadWithOutputsOutputsHash pwo

      prevHash = _blockPayloadHash bHeader
      prevTransactions = _payloadDataTransactions pData
      prevMiner = _payloadDataMiner pData
      prevTransactionsHash = _payloadDataTransactionsHash pData
      prevOutputsHash = _payloadDataOutputsHash pData

      checkComponents acc (desc,expect,actual) = bool ((errorMsg desc expect actual) : acc) acc (expect == actual)
      errorMsg desc expect actual = A.object
        [ "message" A..= ("Mismatched " <> desc :: T.Text)
        , "actual" A..= actual
        , "expected" A..= expect
        ]
      mismatchs = foldl' checkComponents []
        [("Transactions", (A.toJSON prevTransactions), (A.toJSON newTransactions))
        ,("Miner", (A.toJSON prevMiner), (A.toJSON newMiner))
        ,("TransactionsHash", (A.toJSON prevTransactionsHash), (A.toJSON newTransactionsHash))
        ,("OutputsHash", (A.toJSON prevOutputsHash), (A.toJSON newOutputsHash))]

-- | Restore the checkpointer and prepare the execution of a block.
--
-- The use of 'withCheckpointer' is safer and should be preferred where possible.
--
-- This function adds @Block@ savepoint to the db transaction stack. It must be
-- followed by a call to @finalizeCheckpointer (save blockHash)@ or
-- @finalizeCheckpointer discard@.
--
-- Postcondition: beginSavepoint Block
--
restoreCheckpointer
    :: PayloadCas cas
    => Maybe (BlockHeight,BlockHash)
        -- ^ The block height @height@ to which to restore and the parent header
        -- @parentHeader@.
        --
        -- It holds that @(_blockHeight parentHeader == pred height)@

    -> String
        -- ^ Putative caller
    -> PactServiceM cas PactDbEnv'
restoreCheckpointer maybeBB caller = do
    checkPointer <- getCheckpointer
    logInfo $ "restoring (with caller " <> caller <> ") " <> sshow maybeBB
    liftIO $ _cpRestore checkPointer maybeBB

data WithCheckpointerResult a
    = Discard !a
    | Save BlockHeader !a

-- | Execute an action in the context of an @Block@ that is provided by the
-- checkpointer.
--
-- Usually, one needs to rewind the checkpointer first to the target. In those
-- cases the function 'withCheckpointerRewind' should be preferred.
--
-- The result of the inner action indicates whether the resulting checkpointer
-- state should be discarded or saved.
--
-- If the inner action throws an exception the checkpointer state is discarded.
--
withCheckpointer
    :: PayloadCas cas
    => Maybe (BlockHeight, BlockHash)
    -> String
    -> (PactDbEnv' -> PactServiceM cas (WithCheckpointerResult a))
    -> PactServiceM cas a
withCheckpointer target caller act = mask $ \restore -> do
    cenv <- restore $ restoreCheckpointer target caller
    try (restore (act cenv)) >>= \case
        Left e -> discardTx >> throwM @_ @SomeException e
        Right (Discard !result) -> discardTx >> return result
        Right (Save header !result) -> saveTx header >> return result
  where
    discardTx = finalizeCheckpointer _cpDiscard
    saveTx header = do
        finalizeCheckpointer (flip _cpSave $ _blockHash header)
        psStateValidated .= Just header

-- | Same as 'withCheckpointer' but rewinds the checkpointer state to the
-- provided target.
--
withCheckpointerRewind
    :: PayloadCas cas
    => Maybe (BlockHeight, BlockHash)
    -> String
    -> (PactDbEnv' -> PactServiceM cas (WithCheckpointerResult a))
    -> PactServiceM cas a
withCheckpointerRewind target caller act = do
    rewindTo Nothing target
    withCheckpointer target caller act

finalizeCheckpointer :: (Checkpointer -> IO ()) -> PactServiceM cas ()
finalizeCheckpointer finalize = do
    checkPointer <- getCheckpointer
    liftIO $! finalize checkPointer


_liftCPErr :: Either String a -> PactServiceM cas a
_liftCPErr = either internalError' return


-- | Performs a dry run of PactExecution's `buyGas` function for transactions being validated.
--
attemptBuyGas
    :: Miner
    -> PactDbEnv'
    -> Vector (Either InsertError ChainwebTransaction)
    -> PactServiceM cas (Vector (Either InsertError ChainwebTransaction))
attemptBuyGas miner (PactDbEnv' dbEnv) txs = do
        mc <- use psInitCache
        V.fromList . toList . sfst <$> V.foldM f (T2 mempty mc) txs
  where
    f (T2 dl mcache) cmd = do
        T2 mcache' !res <- runBuyGas dbEnv mcache cmd
        pure $! T2 (DL.snoc dl res) mcache'

    createGasEnv
        :: P.PactDbEnv db
        -> P.Command (P.Payload P.PublicMeta P.ParsedCode)
        -> P.GasPrice
        -> P.Gas
        -> PactServiceM cas (TransactionEnv db)
    createGasEnv db cmd gp gl = do
        l <- view $ psCheckpointEnv . cpeLogger

        ph <- use psParentHash >>= \case
             Nothing -> internalError "attemptBuyGas: Parent hash not set"
             Just a -> return a

        pd <- mkPublicData' (publicMetaOf cmd) ph
        spv <- use psSpvSupport
        return $! TransactionEnv P.Transactional db l pd spv nid gp rk gl restrictiveExecutionConfig
      where
        !nid = networkIdOf cmd
        !rk = P.cmdToRequestKey cmd

    runBuyGas
        :: P.PactDbEnv a
        -> ModuleCache
        -> Either InsertError ChainwebTransaction
        -> PactServiceM cas (T2 ModuleCache (Either InsertError ChainwebTransaction))
    runBuyGas _db mcache l@Left {} = return (T2 mcache l)
    runBuyGas db mcache (Right tx) = do
        let cmd = payloadObj <$> tx
            gasPrice = gasPriceOf cmd
            gasLimit = fromIntegral $ gasLimitOf cmd
            txst = TransactionState mcache mempty 0 Nothing (P._geGasModel P.freeGasEnv)

        buyGasEnv <- createGasEnv db cmd gasPrice gasLimit

        cr <- liftIO
          $! P.catchesPactError
          $! execTransactionM buyGasEnv txst
          $! buyGas cmd miner

        case cr of
            Left _ -> return (T2 mcache (Left InsertErrorNoGas))
            Right t -> return (T2 (_txCache t) (Right tx))

-- | The principal validation logic for groups of Pact Transactions.
--
-- Skips validation for genesis transactions, since gas accounts, etc. don't
-- exist yet.
--
validateChainwebTxs
    :: Checkpointer
    -> BlockCreationTime
    -> BlockHeight
    -> Vector ChainwebTransaction
    -> RunGas
    -> Bool
    -> IO ValidateTxs
validateChainwebTxs cp blockOriginationTime bh txs doBuyGas allowModuleInstall
  | bh == 0 = pure $! V.map Right txs
  | V.null txs = pure V.empty
  | otherwise = go
  where

    go = V.mapM validations initTxList >>= doBuyGas

    validations t = runValid checkUnique t
      >>= runValid checkTimes
      >>= runValid (return . checkCompile allowModuleInstall)

    checkUnique :: ChainwebTransaction -> IO (Either InsertError ChainwebTransaction)
    checkUnique t = do
      found <- _cpLookupProcessedTx cp (P._cmdHash t)
      case found of
        Nothing -> pure $ Right t
        Just _ -> pure $ Left $ InsertErrorDuplicate

    checkTimes :: ChainwebTransaction -> IO (Either InsertError ChainwebTransaction)
    checkTimes t | timingsCheck blockOriginationTime $ fmap payloadObj t = return $ Right t
                 | otherwise = return $ (Left InsertErrorInvalidTime)

    initTxList :: ValidateTxs
    initTxList = V.map Right txs

    runValid :: Monad m => (a -> m (Either e a)) -> Either e a -> m (Either e a)
    runValid f (Right r) = f r
    runValid _ l@Left{} = pure l

type ValidateTxs = Vector (Either InsertError ChainwebTransaction)
type RunGas = ValidateTxs -> IO ValidateTxs

checkCompile :: Bool -> ChainwebTransaction -> Either InsertError ChainwebTransaction
checkCompile allowModuleInstall tx = case payload of
  Exec (ExecMsg parsedCode _) ->
    case compileCode parsedCode of
      Left perr -> Left $ InsertErrorCompilationFailed (sshow perr)
      Right terms | allowModuleInstall -> Right tx
                  | otherwise -> foldr bailOnModule (Right tx) terms
  _ -> Right tx
  where
    payload = P._pPayload $ payloadObj $ P._cmdPayload tx
    compileCode p =
      compileExps (mkTextInfo (P._pcCode p)) (P._pcExps p)
    bailOnModule (TModule {}) _ = Left $ InsertErrorCompilationFailed "Module/interface install not supported"
    bailOnModule _ b =  b

skipDebitGas :: RunGas
skipDebitGas = return


-- | Read row from coin-table defined in coin contract, retrieving balance and keyset
-- associated with account name
--
readCoinAccount
    :: PactDbEnv'
      -- ^ pact db backend (sqlite)
    -> Text
      -- ^ account name
    -> IO (Maybe (T2 Decimal (P.Guard (P.Term P.Name))))
readCoinAccount (PactDbEnv' (P.PactDbEnv pdb pdbv)) a = row >>= \case
    Nothing -> return Nothing
    Just (P.ObjectMap o) -> case Map.toList o of
      [(P.FieldKey "balance", b), (P.FieldKey "guard", g)] ->
        case (P.fromPactValue b, P.fromPactValue g) of
          (P.TLiteral (P.LDecimal d) _, P.TGuard t _) ->
            return $! Just $ T2 d t
          _ -> internalError "unexpected pact value types"
      _ -> internalError "wrong table accessed in account lookup"
  where
    row = pdbv & P._readRow pdb (P.UserTables "coin_coin-table") (P.RowKey a)

-- | Read row from coin-table defined in coin contract, retrieving balance
-- associated with account name
--
readAccountBalance
    :: PactDbEnv'
      -- ^ pact db backend (sqlite)
    -> Text
      -- ^ account name
    -> IO (Maybe Decimal)
readAccountBalance pdb account
    = fmap sfst <$> readCoinAccount pdb account

-- | Read row from coin-table defined in coin contract, retrieving guard
-- associated with account name
--
readAccountGuard
    :: PactDbEnv'
      -- ^ pact db backend (sqlite)
    -> Text
      -- ^ account name
    -> IO (Maybe (P.Guard (P.Term P.Name)))
readAccountGuard pdb account
    = fmap ssnd <$> readCoinAccount pdb account

-- | Calculate miner reward. We want this to error hard in the case where
-- block times have finally exceeded the 120-year range. Rewards are calculated
-- at regular blockheight intervals.
--
-- See: 'rewards/miner_rewards.csv'
--
minerReward
    :: MinerRewards
    -> BlockHeight
    -> IO P.ParsedDecimal
minerReward (MinerRewards rs q) bh =
    case V.find ((<=) bh) q of
      Nothing -> err
      Just h -> case HM.lookup h rs of
        Nothing -> err
        Just v -> return v
  where
    err = internalError "block heights have been exhausted"
{-# INLINE minerReward #-}

-- | Note: The BlockHeader param here is the PARENT HEADER of the new
-- block-to-be
--
execNewBlock
    :: PayloadCas cas
    => MemPoolAccess
    -> BlockHeader
    -> Miner
    -> BlockCreationTime
    -> PactServiceM cas PayloadWithOutputs
execNewBlock mpAccess parentHeader miner creationTime =
  do
    updateMempool
    withDiscardedBatch $ do
      setBlockData parentHeader
      rewindTo newblockRewindLimit target
      newTrans <- withCheckpointer target "preBlock" doPreBlock
      withCheckpointer target "execNewBlock" (doNewBlock newTrans)

  where

    -- This is intended to mitigate mining attempts during replay.
    -- In theory we shouldn't need to rewind much ever, but values
    -- less than this are failing in PactReplay test.
    newblockRewindLimit = Just 8

    doPreBlock pdbenv = do
      cp <- getCheckpointer
      psEnv <- ask
      psState <- get
      let runDebitGas :: RunGas
          runDebitGas txs = evalPactServiceM psState psEnv runGas
            where
              runGas = attemptBuyGas miner pdbenv txs
          validate bhi _bha txs = do
            -- note that here we previously were doing a validation
            -- that target == cpGetLatestBlock
            -- which we determined was unnecessary and was a db hit
            --
            -- TODO: propagate the underlying error type?
            V.map (either (const False) (const True))
                <$> validateChainwebTxs cp creationTime bhi txs runDebitGas (_psEnableUserContracts psEnv)
      liftIO $! fmap Discard $!
        mpaGetBlock mpAccess validate bHeight pHash parentHeader

    doNewBlock newTrans pdbenv = do
        logInfo $ "execNewBlock, about to get call processFork: "
                <> " (parent height = " <> sshow pHeight <> ")"
                <> " (parent hash = " <> sshow pHash <> ")"

        -- NEW BLOCK COINBASE: Reject bad coinbase, always use precompilation
        results <- execTransactions (Just (parentHeader,creationTime)) miner newTrans
                   (EnforceCoinbaseFailure True)
                   (CoinbaseUsePrecompiled True) pdbenv

        let !pwo = toPayloadWithOutputs miner results
        return $! Discard pwo

    pHeight = _blockHeight parentHeader
    pHash = _blockHash parentHeader
    target = Just (bHeight, pHash)
    bHeight = succ pHeight

    updateMempool = liftIO $ do
      mpaProcessFork mpAccess parentHeader
      mpaSetLastHeader mpAccess parentHeader


withBatch :: PactServiceM cas a -> PactServiceM cas a
withBatch act = mask $ \r -> do
    cp <- getCheckpointer
    r $ liftIO $ _cpBeginCheckpointerBatch cp
    v <- r act `catch` hndl cp
    r $ liftIO $ _cpCommitCheckpointerBatch cp
    return v

  where
    hndl cp (e :: SomeException) = do
        liftIO $ _cpDiscardCheckpointerBatch cp
        throwM e


withDiscardedBatch :: PactServiceM cas a -> PactServiceM cas a
withDiscardedBatch act = bracket start end (const act)
  where
    start = do
        cp <- getCheckpointer
        liftIO (_cpBeginCheckpointerBatch cp)
        return cp
    end = liftIO . _cpDiscardCheckpointerBatch


-- | only for use in generating genesis blocks in tools
--
execNewGenesisBlock
    :: PayloadCas cas
    => Miner
    -> Vector ChainwebTransaction
    -> PactServiceM cas PayloadWithOutputs
execNewGenesisBlock miner newTrans = withDiscardedBatch $
    withCheckpointer Nothing "execNewGenesisBlock" $ \pdbenv -> do

        -- NEW GENESIS COINBASE: Reject bad coinbase, use date rule for precompilation
        results <- execTransactions Nothing miner newTrans
                   (EnforceCoinbaseFailure True)
                   (CoinbaseUsePrecompiled False) pdbenv
        return $! Discard (toPayloadWithOutputs miner results)

execLocal
    :: PayloadCas cas
    => ChainwebTransaction
    -> PactServiceM cas (P.CommandResult P.Hash)
execLocal cmd = withDiscardedBatch $ do
    cp <- getCheckpointer
    mbLatestBlock <- liftIO $ _cpGetLatestBlock cp
    (bhe, bhash) <- case mbLatestBlock of
                       Nothing -> throwM NoBlockValidatedYet
                       (Just !p) -> return p
    let target = Just (succ bhe, bhash)
    bhDb <- asks _psBlockHeaderDb

    parentHeader <- liftIO $! lookupM bhDb bhash

    -- NOTE: On local calls, there might be code which needs the results of
    -- (chain-data). In such a case, the function `setBlockData` provides the
    -- necessary information for this call to return sensible values.
    setBlockData parentHeader

    withCheckpointer target "execLocal" $ \(PactDbEnv' pdbenv) -> do
        PactServiceEnv{..} <- ask
        mc <- use psInitCache
        pd <- mkPublicData "execLocal" (publicMetaOf $! payloadObj <$> cmd)
        spv <- use psSpvSupport
        r <- liftIO $ applyLocal (_cpeLogger _psCheckpointEnv) pdbenv officialGasModel pd spv cmd mc
        return $! Discard (toHashCommandResult r)

logg :: String -> String -> PactServiceM cas ()
logg level msg = view (psCheckpointEnv . cpeLogger)
  >>= \l -> liftIO $ P.logLog l level msg

logInfo :: String -> PactServiceM cas ()
logInfo = logg "INFO"

logError :: String -> PactServiceM cas ()
logError = logg "ERROR"

logDebug :: String -> PactServiceM cas ()
logDebug = logg "DEBUG"

-- | Set blockheader data. Sets block height, block time,
-- parent hash, and spv support (using parent hash)
--
setBlockData :: BlockHeader -> PactServiceM cas ()
setBlockData bh = do
    psBlockHeight .= succ (_blockHeight bh)
    psBlockTime .= (_blockCreationTime bh)
    psParentHash .= (Just $ _blockParent bh)

    bdb <- view psBlockHeaderDb
    psSpvSupport .= pactSPV bdb (_blockHash bh)

-- | Execute a block -- only called in validate either for replay or for validating current block.
--
playOneBlock
    :: (PayloadCas cas)
    => BlockHeader
    -> PayloadData
    -> PactDbEnv'
    -> PactServiceM cas PayloadWithOutputs
playOneBlock currHeader plData pdbenv = do
    miner <- decodeStrictOrThrow' (_minerData $ _payloadDataMiner plData)
    trans <- liftIO $ transactionsFromPayload plData
    cp <- getCheckpointer
    allowModule <- view psEnableUserContracts
    let creationTime = _blockCreationTime currHeader
    -- prop_tx_ttl_validate
    oks <- liftIO (
        fmap (either (const False) (const True)) <$>
           validateChainwebTxs cp creationTime
               (_blockHeight currHeader) trans skipDebitGas allowModule)
    let mbad = V.elemIndex False oks
    case mbad of
        Nothing -> return ()  -- ok
        Just idx -> let badtx = (V.!) trans idx
                        hash = P._cmdHash badtx
                        msg = [ (hash, validationErr hash) ]
                    in throwM $ TransactionValidationException msg
    -- transactions are now successfully validated.
    !results <- go miner trans
    psStateValidated .= Just currHeader

    return $! toPayloadWithOutputs miner results

  where
    validationErr hash = mconcat
      ["At "
      , sshow (_blockHeight currHeader)
      , " and "
      , sshow (_blockHash currHeader)
      , " this transaction (its hash): "
      , sshow hash
      , " failed to validate"
      ]

    isGenesisBlock = isGenesisBlockHeader currHeader

    currCreationTime = _blockCreationTime currHeader

    go m txs = if isGenesisBlock
      then do
        setBlockData currHeader
        -- GENESIS VALIDATE COINBASE: Reject bad coinbase, use date rule for precompilation
        execTransactions Nothing m txs
          (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled False) pdbenv
      else do
        bhDb <- asks _psBlockHeaderDb
        ph <- liftIO $! lookupM bhDb (_blockParent currHeader)
        setBlockData ph
        -- VALIDATE COINBASE: back-compat allow failures, use date rule for precompilation
        execTransactions (Just (ph,currCreationTime)) m txs
          (EnforceCoinbaseFailure False) (CoinbaseUsePrecompiled False) pdbenv

-- | Rewinds the pact state to @mb@.
--
-- If @mb@ is 'Nothing', it rewinds to the genesis block.
--
rewindTo
    :: forall cas . PayloadCas cas
    => Maybe BlockHeight
        -- ^ if set, limit rewinds to this delta
    -> Maybe (BlockHeight, ParentHash)
        -- ^ The block height @height@ to which to restore and the parent header
        -- @parentHeader@.
        --
        -- It holds that @(_blockHeight parentHeader == pred height)@
    -> PactServiceM cas ()
rewindTo rewindLimit mb = maybe rewindGenesis doRewind mb
  where
    rewindGenesis = return ()
    doRewind (reqHeight, parentHash) = do
        payloadDb <- asks _psPdb
        lastHeader <- findLatestValidBlock >>= maybe failNonGenesisOnEmptyDb return
        failOnTooLowRequestedHeight rewindLimit lastHeader reqHeight
        bhDb <- asks _psBlockHeaderDb
        playFork bhDb payloadDb parentHash lastHeader

    failOnTooLowRequestedHeight (Just limit) lastHeader reqHeight
      | reqHeight + limit < lastHeight = -- need to stick with addition because Word64
        throwM $ RewindLimitExceeded
        ("Requested rewind exceeds limit (" <> sshow limit <> ")")
        reqHeight lastHeight
        where lastHeight = _blockHeight lastHeader
    failOnTooLowRequestedHeight _ _ _ = return ()


    failNonGenesisOnEmptyDb = fail "impossible: playing non-genesis block to empty DB"

    playFork bhdb payloadDb parentHash lastHeader = do
        parentHeader <- liftIO $ lookupM bhdb parentHash

        (!_, _, newBlocks) <-
            liftIO $ collectForkBlocks bhdb lastHeader parentHeader
        -- play fork blocks
        V.mapM_ (fastForward payloadDb) newBlocks

    fastForward :: forall c . PayloadCas c
                => PayloadDb c -> BlockHeader -> PactServiceM c ()
    fastForward payloadDb block = do
        let h = _blockHeight block
        let ph = _blockParent block
        let bpHash = _blockPayloadHash block
        withCheckpointer (Just (h, ph)) "fastForward" $ \pdbenv -> do
            payload <- liftIO (payloadWithOutputsToPayloadData <$> casLookupM payloadDb bpHash)
            void $ playOneBlock block payload pdbenv
            return $! Save block ()
        -- double check output hash here?

-- | Validate a mined block. Execute the transactions in Pact again as
-- validation. Note: The BlockHeader here is the header of the block being
-- validated.
--
execValidateBlock
    :: PayloadCas cas
    => BlockHeader
    -> PayloadData
    -> PactServiceM cas PayloadWithOutputs
execValidateBlock currHeader plData = do
    -- TODO: are we actually validating the output hash here?
    psEnv <- ask
    let reorgLimit = fromIntegral $ view psReorgLimit psEnv
    validateTxEnabled currHeader plData
    handle handleEx $ withBatch $ do
        rewindTo (Just reorgLimit) mb
        withCheckpointer mb "execValidateBlock" $ \pdbenv -> do
            !result <- playOneBlock currHeader plData pdbenv
            return $! Save currHeader result
  where
    mb = if isGenesisBlock then Nothing else Just (bHeight, bParent)
    bHeight = _blockHeight currHeader
    bParent = _blockParent currHeader
    isGenesisBlock = isGenesisBlockHeader currHeader

    -- TODO: knob to configure whether this rewind is fatal
    fatalRewindError a h1 h2 = do
        let msg = concat [
              "Fatal error: "
              , T.unpack a
              , ". Our previous cut block height: "
              , show h1
              , ", fork ancestor's block height: "
              , show h2
              , ".\nOffending new block: \n"
              , show currHeader
              , "\n\n"
              , "Your node is part of a losing fork longer than your \
                \reorg-limit, which\nis a situation that requires manual \
                \intervention. \n\
                \For information on recovering from this, please consult:\n\
                \    https://github.com/kadena-io/chainweb-node/blob/master/\
                \docs/RecoveringFromDeepForks.md"
              ]

        -- TODO: will this work? is it the best way? If we exit the process
        -- then it will be difficult to test this. An alternative is to put the
        -- "handle fatal error" routine into the PactServiceEnv
        killFunction <- asks _psOnFatalError
        liftIO $ killFunction (RewindLimitExceeded a h1 h2) (T.pack msg)

    -- Handle RewindLimitExceeded, rethrow everything else
    handleEx (RewindLimitExceeded a h1 h2) = fatalRewindError a h1 h2
    handleEx e = throwM e

validateTxEnabled :: BlockHeader -> PayloadData -> PactServiceM cas ()
validateTxEnabled bh plData = case txEnabledDate (_blockChainwebVersion bh) of
    Just end | end > blockTime && not isGenesisBlock && not payloadIsEmpty ->
        throwM . BlockValidationFailure . A.toJSON $ ObjectEncoded bh
    _ -> pure ()
  where
    blockTime = _bct $ _blockCreationTime bh
    payloadIsEmpty = V.null $ _payloadDataTransactions plData
    isGenesisBlock = isGenesisBlockHeader bh

execTransactions
    :: Maybe (BlockHeader,BlockCreationTime)
    -> Miner
    -> Vector ChainwebTransaction
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> PactDbEnv'
    -> PactServiceM cas Transactions
execTransactions nonGenesisParentHeaderCurrCreate miner ctxs enfCBFail usePrecomp (PactDbEnv' pactdbenv) = do
    mc <- use psInitCache
    coinOut <- runCoinbase nonGenesisParentHeaderCurrCreate pactdbenv miner enfCBFail usePrecomp mc
    txOuts <- applyPactCmds isGenesis pactdbenv ctxs miner mc
    return $! Transactions (paired txOuts) coinOut
  where
    !isGenesis = isNothing nonGenesisParentHeaderCurrCreate
    cmdBSToTx = toTransactionBytes
      . fmap (T.decodeUtf8 . SB.fromShort . payloadBytes)
    paired outs = V.zipWith (curry $ first cmdBSToTx) ctxs outs


runCoinbase
    :: Maybe (BlockHeader,BlockCreationTime)
    -> P.PactDbEnv p
    -> Miner
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> ModuleCache
    -> PactServiceM cas (P.CommandResult P.Hash)
runCoinbase Nothing _ _ _ _ _ = return noCoinbase
runCoinbase (Just (parentHeader,currCreateTime)) dbEnv miner enfCBFail usePrecomp mc = do
    logger <- view (psCheckpointEnv . cpeLogger)
    rs <- view psMinerRewards
    v <- view chainwebVersion
    pd <- mkPublicData "coinbase" def

    let !bh = BlockHeight $ P._pdBlockHeight pd

    reward <- liftIO $! minerReward rs bh
    (cr,upgradedCacheM) <-
      liftIO $! applyCoinbase v logger dbEnv miner reward pd parentHeader currCreateTime enfCBFail usePrecomp mc
    return $! toHashCommandResult cr

-- | Apply multiple Pact commands, incrementing the transaction Id for each.
-- The output vector is in the same order as the input (i.e. you can zip it
-- with the inputs.)
applyPactCmds
    :: Bool
    -> P.PactDbEnv p
    -> Vector ChainwebTransaction
    -> Miner
    -> ModuleCache
    -> PactServiceM cas (Vector (P.CommandResult P.Hash))
applyPactCmds isGenesis env cmds miner mc =
    V.fromList . toList . sfst <$> V.foldM f (T2 mempty mc) cmds
  where
    f  (T2 dl mcache) cmd = applyPactCmd isGenesis env cmd miner mcache dl

-- | Apply a single Pact command
applyPactCmd
    :: Bool
    -> P.PactDbEnv p
    -> ChainwebTransaction
    -> Miner
    -> ModuleCache
    -> DList (P.CommandResult P.Hash)
    -> PactServiceM cas (T2 (DList (P.CommandResult P.Hash)) ModuleCache)
applyPactCmd isGenesis dbEnv cmdIn miner mcache dl = do
    logger <- view (psCheckpointEnv . cpeLogger)
    gasModel <- view psGasModel
    excfg <- view psEnableUserContracts

    T2 result mcache' <- if isGenesis
      then liftIO $! applyGenesisCmd logger dbEnv def P.noSPVSupport (payloadObj <$> cmdIn)
      else do
        pd <- mkPublicData "applyPactCmd" (publicMetaOf $ payloadObj <$> cmdIn)
        spv <- use psSpvSupport
        liftIO $! applyCmd logger dbEnv miner gasModel pd spv cmdIn mcache excfg

    when isGenesis $
      psInitCache <>= mcache'

    cp <- getCheckpointer
    -- mark the tx as processed at the checkpointer.
    liftIO $ _cpRegisterProcessedTx cp (P._cmdHash cmdIn)
    let !res = toHashCommandResult result
    pure $! T2 (DL.snoc dl res) mcache'

toHashCommandResult :: P.CommandResult [P.TxLog A.Value] -> P.CommandResult P.Hash
toHashCommandResult = over (P.crLogs . _Just) $ P.pactHash . encodeToByteString

transactionsFromPayload :: PayloadData -> IO (Vector ChainwebTransaction)
transactionsFromPayload plData = do
    vtrans <- fmap V.fromList $
              mapM toCWTransaction $
              toList (_payloadDataTransactions plData)
    let (theLefts, theRights) = partitionEithers $ V.toList vtrans
    unless (null theLefts) $ do
        let ls = map T.pack theLefts
        throwM $ TransactionDecodeFailure $ "Failed to decode pact transactions: "
            <> (T.intercalate ". " ls)
    return $! V.fromList theRights
  where
    toCWTransaction bs = evaluate (force (codecDecode chainwebPayloadCodec $
                                          _transactionBytes bs))

execPreInsertCheckReq
    :: PayloadCas cas
    => Vector ChainwebTransaction
    -> PactServiceM cas (Vector (Either Mempool.InsertError ChainwebTransaction))
execPreInsertCheckReq txs = do
    cp <- getCheckpointer
    b <- liftIO $ _cpGetLatestBlock cp
    case b of
        Nothing -> return $! V.map Right txs
        Just (h, ha) ->
            withCheckpointer (Just (h+1, ha)) "execPreInsertCheckReq" $ \pdb -> do
                now <- liftIO getCurrentTimeIntegral
                psEnv <- ask
                psState <- get
                liftIO (Discard <$>
                        validateChainwebTxs cp (BlockCreationTime now) (h + 1) txs
                        (runGas pdb psState psEnv) (_psEnableUserContracts psEnv))
  where
    runGas pdb pst penv ts =
        evalPactServiceM pst penv (attemptBuyGas noMiner pdb ts)

execLookupPactTxs
    :: PayloadCas cas
    => Rewind
    -> Vector P.PactHash
    -> PactServiceM cas (Vector (Maybe (T2 BlockHeight BlockHash)))
execLookupPactTxs restorePoint txs
    | V.null txs = return mempty
    | otherwise = go
  where
    go = getCheckpointer >>= \(!cp) -> case restorePoint of
      NoRewind _ ->
        liftIO $! V.mapM (_cpLookupProcessedTx cp) txs
      DoRewind bh -> do
        let !t = Just $! (_blockHeight bh + 1,_blockHash bh)
        withCheckpointerRewind t "lookupPactTxs" $ \_ ->
          liftIO $ Discard <$> V.mapM (_cpLookupProcessedTx cp) txs

findLatestValidBlock :: PactServiceM cas (Maybe BlockHeader)
findLatestValidBlock = getCheckpointer >>= liftIO . _cpGetLatestBlock >>= \case
    Nothing -> return Nothing
    Just (height, hash) -> go height hash
  where
    go height hash = do
        bhdb <- view psBlockHeaderDb
        liftIO (lookup bhdb hash) >>= \case
            Nothing -> do
                logInfo $ "Latest block isn't valid."
                    <> " Failed to lookup hash " <> sshow (height, hash) <> " in block header db."
                    <> " Continuing with parent."
                cp <- getCheckpointer
                liftIO (_cpGetBlockParent cp (height, hash)) >>= \case
                    Nothing -> throwM $ PactInternalError
                        $ "missing block parent of last hash " <> sshow (height, hash)
                    Just predHash -> go (pred height) predHash
            x -> return x

getCheckpointer :: PactServiceM cas Checkpointer
getCheckpointer = view (psCheckpointEnv . cpeCheckpointer)

-- | Modified table gas module with free module loads
--
freeModuleLoadGasModel :: P.GasModel
freeModuleLoadGasModel = modifiedGasModel
  where
    defGasModel = tableGasModel defaultGasConfig
    fullRunFunction = P.runGasModel defGasModel
    modifiedRunFunction name ga = case ga of
      P.GPostRead (P.ReadModule {}) -> 0
      _ -> fullRunFunction name ga
    modifiedGasModel = defGasModel { P.runGasModel = modifiedRunFunction }

-- | Gas Model used in /send and /local
--
officialGasModel :: P.GasModel
officialGasModel = freeModuleLoadGasModel
