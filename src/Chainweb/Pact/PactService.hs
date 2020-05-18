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
    , execLocal
    , execLookupPactTxs
    , execPreInsertCheckReq
    , execBlockTxHistory
    , initPactService
    , readCoinAccount
    , readAccountBalance
    , readAccountGuard
    , toHashCommandResult
      -- * For Side-tooling
    , execNewGenesisBlock
    , initPactService'
    , minerReward
      -- * for tests
    , toPayloadWithOutputs
    , validateHashes
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
import qualified Data.ByteString.Short as SB
import Data.Decimal
import Data.Default (def)
import Data.DList (DList(..))
import qualified Data.DList as DL
import Data.Either
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tuple.Strict (T2(..))
import Data.Vector (Vector)
import qualified Data.Vector as V

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
import Pact.Types.Exp (ParsedCode(..))
import Pact.Types.ExpParser (mkTextInfo)
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.PactValue as P
import Pact.Types.RPC
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.SPV as P

------------------------------------------------------------------------------
-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisParentBlockHash, genesisBlockHeader, genesisBlockPayload)
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeight
import Chainweb.Logger
import Chainweb.Mempool.Mempool as Mempool
import Chainweb.Miner.Pact
import Chainweb.NodeId
import Chainweb.Pact.Backend.RelationalCheckpointer (initRelationalCheckpointer)
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.NoCoinbase
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
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Data.CAS (casLookupM)
import Data.LogMessage
import Utils.Logging.Trace


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
    => PayloadCasLookup cas
    => ChainwebVersion
    -> ChainId
    -> logger
    -> PactQueue
    -> MemPoolAccess
    -> BlockHeaderDb
    -> PayloadDb cas
    -> SQLiteEnv
    -> PactServiceConfig
    -> IO ()
initPactService ver cid chainwebLogger reqQ mempoolAccess bhDb pdb sqlenv config =
    void $ initPactService' ver cid chainwebLogger bhDb pdb sqlenv config $ do
        initialPayloadState chainwebLogger ver cid
        serviceRequests (logFunction chainwebLogger) mempoolAccess reqQ

initPactService'
    :: Logger logger
    => PayloadCasLookup cas
    => ChainwebVersion
    -> ChainId
    -> logger
    -> BlockHeaderDb
    -> PayloadDb cas
    -> SQLiteEnv
    -> PactServiceConfig
    -> PactServiceM cas a
    -> IO (T2 a PactServiceState)
initPactService' ver cid chainwebLogger bhDb pdb sqlenv config act = do
    checkpointEnv <- initRelationalCheckpointer initBlockState sqlenv logger ver
    let !rs = readRewards
        !gasModel = officialGasModel
        !initialParentHeader = ParentHeader $ genesisBlockHeader ver cid
        !pse = PactServiceEnv
                { _psMempoolAccess = Nothing
                , _psCheckpointEnv = checkpointEnv
                , _psPdb = pdb
                , _psBlockHeaderDb = bhDb
                , _psGasModel = gasModel
                , _psMinerRewards = rs
                , _psReorgLimit = fromIntegral $ _pactReorgLimit config
                , _psOnFatalError = defaultOnFatalError (logFunctionText chainwebLogger)
                , _psVersion = ver
                , _psValidateHashesOnReplay = _pactRevalidate config
                , _psAllowReadsInLocal = _pactAllowReadsInLocal config
                }
        !pst = PactServiceState Nothing mempty initialParentHeader P.noSPVSupport
    runPactServiceM pst pse act
  where
    loggers = pactLoggers chainwebLogger
    logger = P.newLogger loggers $ P.LogName ("PactService" <> show cid)

initialPayloadState
    :: Logger logger
    => PayloadCasLookup cas
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
    :: forall cas logger. (PayloadCasLookup cas, Logger logger)
    => logger
    -> ChainwebVersion
    -> ChainId
    -> PayloadWithOutputs
    -> PactServiceM cas ()
initializeCoinContract _logger v cid pwo = do
    cp <- getCheckpointer
    genesisExists <- liftIO $ _cpLookupBlockInCheckpointer cp (0, ghash)
    if genesisExists
      then readContracts cp
      else validateGenesis

  where
    validateGenesis = void $!
        execValidateBlock genesisHeader inputPayloadData

    ghash :: BlockHash
    ghash = _blockHash genesisHeader

    inputPayloadData :: PayloadData
    inputPayloadData = payloadWithOutputsToPayloadData pwo

    genesisHeader :: BlockHeader
    genesisHeader = genesisBlockHeader v cid

    readContracts cp = do
      mbLatestBlock <- liftIO $ _cpGetLatestBlock cp
      (bhe, bhash) <- case mbLatestBlock of
        Nothing -> throwM NoBlockValidatedYet
        (Just !p) -> return p
      let target = Just (succ bhe, bhash)
      parentHeader <- ParentHeader <$!> lookupBlockHeader bhash "initializeCoinContract"
      setParentHeader parentHeader
      withCheckpointer target "readContracts" $ \(PactDbEnv' pdbenv) -> do
        PactServiceEnv{..} <- ask
        pd <- getTxContext def
        mc <- liftIO $ readInitModules (_cpeLogger _psCheckpointEnv) pdbenv pd
        psInitCache .= mc
        return $! Discard ()

lookupBlockHeader :: BlockHash -> Text -> PactServiceM cas BlockHeader
lookupBlockHeader bhash ctx = do
  bhdb <- asks _psBlockHeaderDb
  liftIO $! lookupM bhdb bhash
        `catch` \e -> throwM $ BlockHeaderLookupFailure $
                      "failed lookup of parent header in " <> ctx <> ": " <> sshow (e :: SomeException)

isGenesisParent :: ParentHeader -> Bool
isGenesisParent (ParentHeader p)
    = _blockParent p == genesisParentBlockHash (_chainwebVersion p) p

-- | Loop forever, serving Pact execution requests and reponses from the queues
serviceRequests
    :: PayloadCasLookup cas
    => LogFunction
    -> MemPoolAccess
    -> PactQueue
    -> PactServiceM cas ()
serviceRequests logFn memPoolAccess reqQ = do
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
                trace logFn "Chainweb.Pact.PactService.execNewBlock"
                    (_parentHeader _newBlockHeader) 1 $
                    tryOne "execNewBlock" _newResultVar $
                    execNewBlock memPoolAccess _newBlockHeader _newMiner
                go
            ValidateBlockMsg ValidateBlockReq {..} -> do
                trace logFn "Chainweb.Pact.PactService.execValidateBlock"
                    _valBlockHeader
                    (length (_payloadDataTransactions _valPayloadData)) $
                    tryOne "execValidateBlock" _valResultVar $
                    execValidateBlock _valBlockHeader _valPayloadData
                go
            LookupPactTxsMsg (LookupPactTxsReq restorePoint txHashes resultVar) -> do
                trace logFn "Chainweb.Pact.PactService.execLookupPactTxs" ()
                    (length txHashes) $
                    tryOne "execLookupPactTxs" resultVar $
                    execLookupPactTxs restorePoint txHashes
                go
            PreInsertCheckMsg (PreInsertCheckReq txs resultVar) -> do
                trace logFn "Chainweb.Pact.PactService.execPreInsertCheckReq" ()
                    (length txs) $
                    tryOne "execPreInsertCheckReq" resultVar $
                    V.map (() <$) <$> execPreInsertCheckReq txs
                go
            BlockTxHistoryMsg (BlockTxHistoryReq bh d resultVar) -> do
              trace logFn "Chainweb.Pact.PactService.execBlockTxHistory" bh 1 $
                tryOne "execBlockTxHistory" resultVar $
                execBlockTxHistory bh d

    toPactInternalError e = Left $ PactInternalError $ T.pack $ show e

    tryOne
        :: String
        -> MVar (Either PactException a)
        -> PactServiceM cas a
        -> PactServiceM cas ()
    tryOne which mvar = tryOne' which mvar Right

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
        -- monad is generally not thread-safe. It is fine to do here, since
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
        trans = cmdBSToTx . fst <$> oldSeq
        transOuts = toOutputBytes . toHashCommandResult . snd <$> oldSeq

        miner = toMinerData mi
        cb = CoinbaseOutput $ encodeToByteString $ toHashCommandResult $ _transactionCoinbase ts
        blockTrans = snd $ newBlockTransactions miner trans
        cmdBSToTx = toTransactionBytes
          . fmap (T.decodeUtf8 . SB.fromShort . payloadBytes)
        blockOuts = snd $ newBlockOutputs cb transOuts

        blockPL = blockPayload blockTrans blockOuts
        plData = payloadData blockTrans blockPL
     in payloadWithOutputs plData cb transOuts

data CRLogPair = CRLogPair P.Hash [P.TxLog A.Value]
instance A.ToJSON CRLogPair where
  toJSON (CRLogPair h logs) = A.object
    [ "hash" A..= h
    , "rawLogs" A..= logs ]

validateHashes
    :: BlockHeader
        -- ^ Current Header
    -> PayloadData
    -> Miner
    -> Transactions
    -> Either PactException PayloadWithOutputs
validateHashes bHeader pData miner transactions =
    if newHash == prevHash
    then Right pwo
    else Left $ BlockValidationFailure $ A.object
         [ "mismatch" A..= errorMsg "Payload hash" prevHash newHash
         , "details" A..= details
         ]
    where

      pwo = toPayloadWithOutputs miner transactions

      newHash = _payloadWithOutputsPayloadHash pwo
      prevHash = _blockPayloadHash bHeader

      newTransactions = V.map fst (_payloadWithOutputsTransactions pwo)
      prevTransactions = _payloadDataTransactions pData

      newMiner = _payloadWithOutputsMiner pwo
      prevMiner = _payloadDataMiner pData

      newTransactionsHash = _payloadWithOutputsTransactionsHash pwo
      prevTransactionsHash = _payloadDataTransactionsHash pData

      newOutputsHash = _payloadWithOutputsOutputsHash pwo
      prevOutputsHash = _payloadDataOutputsHash pData

      check desc extra expect actual
        | expect == actual = []
        | otherwise =
          [A.object $ "mismatch" A..= errorMsg desc expect actual :  extra]

      errorMsg desc expect actual = A.object
        [ "type" A..= (desc :: Text)
        , "actual" A..= actual
        , "expected" A..= expect
        ]

      checkTransactions prev new =
        ["txs" A..= concatMap (uncurry (check "Tx" [])) (V.zip prev new)]

      addOutputs (Transactions pairs coinbase) =
        [ "outputs" A..= A.object
         [ "coinbase" A..= toPairCR coinbase
         , "txs" A..= (addTxOuts <$> pairs)
         ]
        ]

      addTxOuts :: (ChainwebTransaction, P.CommandResult [P.TxLog A.Value]) -> A.Value
      addTxOuts (tx,cr) = A.object
        [ "tx" A..= fmap (fmap _pcCode . payloadObj) tx
        , "result" A..= toPairCR cr
        ]

      toPairCR cr = over (P.crLogs . _Just)
        (CRLogPair (fromJuste $ P._crLogs (toHashCommandResult cr))) cr

      details = concat
        [ check "Miner" [] prevMiner newMiner
        , check "TransactionsHash" (checkTransactions prevTransactions newTransactions)
          prevTransactionsHash newTransactionsHash
        , check "OutputsHash" (addOutputs transactions)
          prevOutputsHash newOutputsHash
        ]

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
    :: PayloadCasLookup cas
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
    :: PayloadCasLookup cas
    => Maybe (BlockHeight, BlockHash)
        -- The current block height and the parent hash
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
    :: PayloadCasLookup cas
    => Maybe (BlockHeight, BlockHash)
        -- The current block height and the parent hash
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

        pd <- getTxContext (publicMetaOf cmd)
        spv <- use psSpvSupport
        let ec = mkExecutionConfig
              [ P.FlagDisableModuleInstall
              , P.FlagDisableHistoryInTransactionalMode ]
        return $! TransactionEnv P.Transactional db l (ctxToPublicData pd) spv nid gp rk gl ec
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
          $! buyGas False cmd miner

        case cr of
            Left err -> return (T2 mcache (Left (InsertErrorBuyGas (T.pack $ show err))))
            Right t -> return (T2 (_txCache t) (Right tx))

-- | The principal validation logic for groups of Pact Transactions.
--
-- Skips validation for genesis transactions, since gas accounts, etc. don't
-- exist yet.
--
validateChainwebTxs
    :: Checkpointer
    -> BlockCreationTime
        -- ^ reference time for tx validation.
        --
        -- This time is the creation time of the parent header for calls from
        -- newBlock. It is the creation time of the current header
        -- for block validation if
        -- @useLegacyCreationTimeForTxValidation blockHeight@ true.
        --
    -> Bool
        -- ^ lenientCreationTime flag, see 'lenientTimeSlop' for details.
    -> BlockHeight
        -- ^ Current block height
    -> Vector ChainwebTransaction
    -> RunGas
    -> IO ValidateTxs
validateChainwebTxs cp txValidationTime lenientCreationTime bh txs doBuyGas
  | bh == 0 = pure $! V.map Right txs
  | V.null txs = pure V.empty
  | otherwise = go
  where

    go = V.mapM validations initTxList >>= doBuyGas

    validations t = runValid checkUnique t
      >>= runValid checkTimes
      >>= runValid (return . checkCompile)

    checkUnique :: ChainwebTransaction -> IO (Either InsertError ChainwebTransaction)
    checkUnique t = do
      found <- _cpLookupProcessedTx cp (P._cmdHash t)
      case found of
        Nothing -> pure $ Right t
        Just _ -> pure $ Left InsertErrorDuplicate

    checkTimes :: ChainwebTransaction -> IO (Either InsertError ChainwebTransaction)
    checkTimes t
        | timingsCheck txValidationTime lenientCreationTime $ fmap payloadObj t = return $ Right t
        | otherwise = return $ Left InsertErrorInvalidTime

    initTxList :: ValidateTxs
    initTxList = V.map Right txs

    runValid :: Monad m => (a -> m (Either e a)) -> Either e a -> m (Either e a)
    runValid f (Right r) = f r
    runValid _ l@Left{} = pure l

-- | Legacy validation of tx TTL for new Blocks. This function uses the parent header
-- but guarantees that the TTL is valid for validation both with the parent header and
-- the current headers.
--
-- FOR NEWBLOCK ONLY. DON'T USE FOR BLOCK VALITION OF BLOCK ON THE CHAIN.
--
-- Running this validation in additon to other timing validations on the creation time
-- of the parent header ensures that the the new block satisfies both new and old timing
-- validations.
--
-- There are four ways how timing checks are currently performed:
--
-- 1. legacy behavior for validation: current header
-- 2. new behavior for validation: parent header
-- 2. legacy behavior for newBlock: parent header, TTL compat
-- 3. new behavior for newBlock: parent header, no TTL compat
--
-- This function covers the TTL compat for the (2.) and (3.) case.
--
-- This code can be removed once the transition is complete and the guard
-- @useLegacyCreationTimeForTxValidation@ is false for all new blocks
-- of all chainweb versions.
--
validateLegacyTTL
    :: ParentHeader
    -> Vector ChainwebTransaction
    -> ValidateTxs
validateLegacyTTL parentHeader txs
    | isGenesisParent parentHeader = V.map Right txs
    | V.null txs = V.empty
    | otherwise = V.map check txs
  where
    timeFromSeconds = Time . secondsToTimeSpan . Seconds . fromIntegral
    parentTime = _bct $ _blockCreationTime $ _parentHeader parentHeader
    check tx
        | expirationTime >= parentTime = Right tx
        | otherwise = Left InsertErrorInvalidTime
      where
        expirationTime = timeFromSeconds (txOriginationTime + ttl - compatPeriod)
        P.TTLSeconds ttl = timeToLiveOf (payloadObj <$> tx)
        P.TxCreationTime txOriginationTime = creationTimeOf (payloadObj <$> tx)

    -- ensure that every block that validates with
    -- @txValidationTime == _blockCreatinTime parentHeader@ (new behavior) also validates with
    -- @txValidationTime == _blockCreationTime currentHeader@ (old behavior).
    --
    -- The compat period puts an effective lower limit on the TTL value. During
    -- the transition period any transactions that is submitted with a lower TTL
    -- value is considered expired and rejected immediately. After the
    -- transition period, which will probably last a few days, the compat period
    -- is disabled again.
    --
    -- The time between two blocks is distributed exponentially with a rate \(r\) of 2
    -- blocks per minutes. The quantiles of the exponential distribution with
    -- parameter \(r\) is \(quantile(x) = \frac{- ln(1 - x)}{r}\).
    --
    -- Thus the 99.99% will be solved in less than @(- log (1 - 0.9999)) / 2@
    -- minutes, which is less than 5 minutes.
    --
    -- In practice, due to the effects of difficulty adjustement, the
    -- distribution is skewed such that the 99.99 percentile is actually a
    -- little less than 3 minutes.
    --
    compatPeriod
        | useLegacyCreationTimeNewBlockOrInsert parentHeader = 60 * 3
        | otherwise = 0

-- | Guard for new block or insert checks vs. parent block height + 1,
-- whereas validate checks "current block height".
useLegacyCreationTimeNewBlockOrInsert :: ParentHeader -> Bool
useLegacyCreationTimeNewBlockOrInsert parentHeader =
  useLegacyCreationTimeForTxValidation v bh
  where
    v = _chainwebVersion parentHeader
    bh = succ $ _blockHeight $ _parentHeader parentHeader

type ValidateTxs = Vector (Either InsertError ChainwebTransaction)
type RunGas = ValidateTxs -> IO ValidateTxs

checkCompile :: ChainwebTransaction -> Either InsertError ChainwebTransaction
checkCompile tx = case payload of
  Exec (ExecMsg parsedCode _) ->
    case compileCode parsedCode of
      Left perr -> Left $ InsertErrorCompilationFailed (sshow perr)
      Right _ -> Right tx
  _ -> Right tx
  where
    payload = P._pPayload $ payloadObj $ P._cmdPayload tx
    compileCode p =
      compileExps (mkTextInfo (P._pcCode p)) (P._pcExps p)

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
    :: ChainwebVersion
    -> MinerRewards
    -> BlockHeight
    -> IO P.ParsedDecimal
minerReward v (MinerRewards rs) bh =
    case Map.lookupGE bh rs of
      Nothing -> err
      Just (_, m) -> pure $! P.ParsedDecimal (roundTo 8 (m / n))
  where
    !n = view (chainGraph . to (int . order)) v
    err = internalError "block heights have been exhausted"
{-# INLINE minerReward #-}

-- | Note: The BlockHeader param here is the PARENT HEADER of the new
-- block-to-be
--
execNewBlock
    :: PayloadCasLookup cas
    => MemPoolAccess
    -> ParentHeader
    -> Miner
    -> PactServiceM cas PayloadWithOutputs
execNewBlock mpAccess parentHeader miner = handle onTxFailure $ do
    updateMempool
    withDiscardedBatch $ do
      setParentHeader parentHeader
      rewindTo newblockRewindLimit target
      newTrans <- withCheckpointer target "preBlock" doPreBlock
      withCheckpointer target "execNewBlock" (doNewBlock newTrans)
  where
    onTxFailure e@(PactTransactionExecError rk _) = do
        -- add the failing transaction to the mempool bad list, so it is not
        -- re-selected for mining.
        liftIO $ mpaBadlistTx mpAccess rk
        throwM e
    onTxFailure e = throwM e

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

            let parentTime = _blockCreationTime $ _parentHeader parentHeader
                lenientCreationTime = not $ useLegacyCreationTimeNewBlockOrInsert parentHeader
            results <- V.zipWith (>>)
                <$> validateChainwebTxs cp parentTime lenientCreationTime bhi txs runDebitGas

                -- This code can be removed once the transition is complete and the guard
                -- @useLegacyCreationTimeForTxValidation@ is false for all new blocks
                -- of all chainweb versions.
                --
                <*> pure (validateLegacyTTL parentHeader txs)

            V.forM results $ \case
                Right _ -> return True
                Left _e -> return False

      liftIO $! fmap Discard $!
        mpaGetBlock mpAccess validate bHeight pHash (_parentHeader parentHeader)

    doNewBlock newTrans pdbenv = do
        logInfo $ "execNewBlock, about to get call processFork: "
                <> " (parent height = " <> sshow pHeight <> ")"
                <> " (parent hash = " <> sshow pHash <> ")"

        setParentHeader parentHeader -- could have been overwritten in rewind, so set again

        -- NEW BLOCK COINBASE: Reject bad coinbase, always use precompilation
        results <- execTransactions False miner newTrans
          (EnforceCoinbaseFailure True)
          (CoinbaseUsePrecompiled True)
          pdbenv

        let !pwo = toPayloadWithOutputs miner results
        return $! Discard pwo

    pHeight = _blockHeight $ _parentHeader parentHeader
    pHash = _blockHash $ _parentHeader parentHeader
    target = Just (bHeight, pHash)
    bHeight = succ pHeight

    updateMempool = liftIO $ do
      mpaProcessFork mpAccess $ _parentHeader parentHeader
      mpaSetLastHeader mpAccess $ _parentHeader parentHeader


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
    :: PayloadCasLookup cas
    => Miner
    -> Vector ChainwebTransaction
    -> PactServiceM cas PayloadWithOutputs
execNewGenesisBlock miner newTrans = withDiscardedBatch $
    withCheckpointer Nothing "execNewGenesisBlock" $ \pdbenv -> do

        -- NEW GENESIS COINBASE: Reject bad coinbase, use date rule for precompilation
        results <- execTransactions True miner newTrans
                   (EnforceCoinbaseFailure True)
                   (CoinbaseUsePrecompiled False) pdbenv
        return $! Discard (toPayloadWithOutputs miner results)

execLocal
    :: PayloadCasLookup cas
    => ChainwebTransaction
    -> PactServiceM cas (P.CommandResult P.Hash)
execLocal cmd = withDiscardedBatch $ do
    cp <- getCheckpointer
    mbLatestBlock <- liftIO $ _cpGetLatestBlock cp
    (bhe, bhash) <- case mbLatestBlock of
                       Nothing -> throwM NoBlockValidatedYet
                       (Just !p) -> return p
    let target = Just (succ bhe, bhash)
    parentHeader <- ParentHeader <$!> lookupBlockHeader bhash "execLocal"
    setParentHeader parentHeader

    withCheckpointer target "execLocal" $ \(PactDbEnv' pdbenv) -> do
        PactServiceEnv{..} <- ask
        mc <- use psInitCache
        pd <- getTxContext (publicMetaOf $! payloadObj <$> cmd)
        spv <- use psSpvSupport
        execConfig <- view psAllowReadsInLocal >>= \b ->
          return $ if b then mkExecutionConfig [P.FlagAllowReadInLocal] else def
        r <- liftIO $
          applyLocal (_cpeLogger _psCheckpointEnv) pdbenv officialGasModel pd spv cmd mc execConfig
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

-- | Set parent header in state and spv support (using parent hash)
setParentHeader :: ParentHeader -> PactServiceM cas ()
setParentHeader ph@(ParentHeader bh) = do
  psParentHeader .= ph
  bdb <- view psBlockHeaderDb
  psSpvSupport .= pactSPV bdb (_blockHash bh)

-- | Execute a block -- only called in validate either for replay or for validating current block.
--
playOneBlock
    :: (PayloadCasLookup cas)
    => BlockHeader
        -- ^ this is the current header. We may consider changing this to the parent
        -- header to avoid confusion with new block and prevent using data from this
        -- header when we should use the respective values from the parent header
        -- instead.
    -> PayloadData
    -> PactDbEnv'
    -> PactServiceM cas (T2 Miner Transactions)
playOneBlock currHeader plData pdbenv = do
    miner <- decodeStrictOrThrow' (_minerData $ _payloadDataMiner plData)
    trans <- liftIO $ transactionsFromPayload plData
    cp <- getCheckpointer

    -- The reference time for tx timings validation.
    --
    -- The legacy behavior is to use the creation time of the /current/ header.
    -- The new default behavior is to use the creation time of the /parent/ header.
    --
    (txValidationTime,lenientCreationTime) <- if
        useLegacyCreationTimeForTxValidation v h || isGenesisBlockHeader currHeader
      then
        return (_blockCreationTime currHeader, False)
      else do
        -- FIXME don't do this twice. Instead store the parent header in the context
        -- (and don't use the current header at all)
        parentHeader <- ParentHeader <$!> lookupBlockHeader  (_blockParent currHeader) "playOneBlock"
        return (_blockCreationTime $ _parentHeader parentHeader, True)

    -- prop_tx_ttl_validate
    valids <- V.zip trans <$> liftIO
      (validateChainwebTxs cp txValidationTime lenientCreationTime (_blockHeight currHeader) trans skipDebitGas)

    case foldr handleValids [] valids of
      [] -> return ()
      errs -> throwM $ TransactionValidationException $ errs

    !results <- go miner trans
    psStateValidated .= Just currHeader

    -- Validate hashes if requested
    asks _psValidateHashesOnReplay >>= \x -> when x $
        either throwM (void . return) $!
        validateHashes currHeader plData miner results

    return $! T2 miner results

  where

    handleValids (tx,Left e) es = (P._cmdHash tx, sshow e):es
    handleValids _ es = es

    v = _chainwebVersion currHeader
    h = _blockHeight currHeader

    isGenesisBlock = isGenesisBlockHeader currHeader

    go m txs = if isGenesisBlock
      then do
        setParentHeader (ParentHeader currHeader)
        -- GENESIS VALIDATE COINBASE: Reject bad coinbase, use date rule for precompilation
        execTransactions True m txs
          (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled False) pdbenv
      else do
        parentHeader <- ParentHeader <$!> lookupBlockHeader (_blockParent currHeader) "playOneBlock.go"
        setParentHeader parentHeader
        -- VALIDATE COINBASE: back-compat allow failures, use date rule for precompilation
        execTransactions False m txs
          (EnforceCoinbaseFailure False) (CoinbaseUsePrecompiled False) pdbenv

-- | Rewinds the pact state to @mb@.
--
-- If @mb@ is 'Nothing', it rewinds to the genesis block.
--
rewindTo
    :: forall cas . PayloadCasLookup cas
    => Maybe BlockHeight
        -- ^ if set, limit rewinds to this delta
    -> Maybe (BlockHeight, ParentHash)
        -- ^ The block height @height@ to which to restore and the parent header
        -- @parentHeader@.
        --
        -- It holds that @(_blockHeight parentHeader == pred height)@
    -> PactServiceM cas ()
rewindTo rewindLimit = maybe rewindGenesis doRewind
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


    failNonGenesisOnEmptyDb = error "impossible: playing non-genesis block to empty DB"

    playFork bhdb payloadDb parentHash lastHeader = do
        parentHeader <- ParentHeader <$!> lookupBlockHeader parentHash "rewindTo"

        (!_, _, newBlocks) <-
            liftIO $ collectForkBlocks bhdb lastHeader $ _parentHeader parentHeader
        -- play fork blocks
        V.mapM_ (fastForward payloadDb) newBlocks

    fastForward :: forall c . PayloadCasLookup c
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
    :: PayloadCasLookup cas
    => BlockHeader
    -> PayloadData
    -> PactServiceM cas PayloadWithOutputs
execValidateBlock currHeader plData = do
    psEnv <- ask
    let reorgLimit = fromIntegral $ view psReorgLimit psEnv
    (T2 miner transactions) <- handle handleEx $ withBatch $ do
        rewindTo (Just reorgLimit) mb
        withCheckpointer mb "execValidateBlock" $ \pdbenv -> do
            !result <- playOneBlock currHeader plData pdbenv
            return $! Save currHeader result
    either throwM return $!
      validateHashes currHeader plData miner transactions
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

execTransactions
    :: Bool
    -> Miner
    -> Vector ChainwebTransaction
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> PactDbEnv'
    -> PactServiceM cas Transactions
execTransactions isGenesis miner ctxs enfCBFail usePrecomp (PactDbEnv' pactdbenv) = do
    mc <- use psInitCache
    coinOut <- runCoinbase isGenesis pactdbenv miner enfCBFail usePrecomp mc
    txOuts <- applyPactCmds isGenesis pactdbenv ctxs miner mc
    return $! Transactions (V.zip ctxs txOuts) coinOut

runCoinbase
    :: Bool
    -> P.PactDbEnv p
    -> Miner
    -> EnforceCoinbaseFailure
    -> CoinbaseUsePrecompiled
    -> ModuleCache
    -> PactServiceM cas (P.CommandResult [P.TxLog A.Value])
runCoinbase True _ _ _ _ _ = return noCoinbase
runCoinbase False dbEnv miner enfCBFail usePrecomp mc = do
    logger <- view (psCheckpointEnv . cpeLogger)
    rs <- view psMinerRewards
    v <- view chainwebVersion
    pd <- getTxContext def

    let !bh = ctxCurrentBlockHeight pd

    reward <- liftIO $! minerReward v rs bh

    (T2 cr upgradedCacheM) <-
      liftIO $! applyCoinbase v logger dbEnv miner reward pd enfCBFail usePrecomp mc
    mapM_ upgradeInitCache upgradedCacheM
    debugResult "runCoinbase" cr
    return $! cr

  where

    upgradeInitCache newCache = do
      logInfo "Updating init cache for upgrade"
      psInitCache %= HM.union newCache


-- | Apply multiple Pact commands, incrementing the transaction Id for each.
-- The output vector is in the same order as the input (i.e. you can zip it
-- with the inputs.)
applyPactCmds
    :: Bool
    -> P.PactDbEnv p
    -> Vector ChainwebTransaction
    -> Miner
    -> ModuleCache
    -> PactServiceM cas (Vector (P.CommandResult [P.TxLog A.Value]))
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
    -> DList (P.CommandResult [P.TxLog A.Value])
    -> PactServiceM cas (T2 (DList (P.CommandResult [P.TxLog A.Value])) ModuleCache)
applyPactCmd isGenesis dbEnv cmdIn miner mcache dl = do
    logger <- view (psCheckpointEnv . cpeLogger)
    gasModel <- view psGasModel
    v <- view psVersion

    T2 result mcache' <- if isGenesis
      then liftIO $! applyGenesisCmd logger dbEnv P.noSPVSupport (payloadObj <$> cmdIn)
      else do
        pd <- getTxContext (publicMetaOf $ payloadObj <$> cmdIn)
        spv <- use psSpvSupport
        liftIO $! applyCmd v logger dbEnv miner gasModel pd spv cmdIn mcache
        {- the following can be used instead of above to nerf transaction execution
        return $! T2 (P.CommandResult (P.cmdToRequestKey cmdIn) Nothing
                      (P.PactResult (Right (P.PLiteral (P.LInteger 1))))
                      0 Nothing Nothing Nothing)
                      mcache -}

    when isGenesis $
      psInitCache <>= mcache'

    unless isGenesis $ debugResult "applyPactCmd" result

    cp <- getCheckpointer
    -- mark the tx as processed at the checkpointer.
    liftIO $ _cpRegisterProcessedTx cp (P._cmdHash cmdIn)
    pure $! T2 (DL.snoc dl result) mcache'

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
            <> T.intercalate ". " ls
    return $! V.fromList theRights
  where
    toCWTransaction bs = evaluate (force (codecDecode chainwebPayloadCodec $
                                          _transactionBytes bs))

debugResult :: A.ToJSON a => Text -> a -> PactServiceM cas ()
debugResult msg result =
  logDebug $ T.unpack $ trunc $ msg <> " result: " <> encodeToText result
  where
    trunc t | T.length t < limit = t
            | otherwise = T.take limit t <> " [truncated]"
    limit = 5000

execBlockTxHistory :: BlockHeader -> Domain' -> PactServiceM cas BlockTxHistory
execBlockTxHistory bh (Domain' d) = do
  !cp <- getCheckpointer
  liftIO $ _cpGetBlockHistory cp bh d

execPreInsertCheckReq
    :: PayloadCasLookup cas
    => Vector ChainwebTransaction
    -> PactServiceM cas (Vector (Either Mempool.InsertError ChainwebTransaction))
execPreInsertCheckReq txs = do
    cp <- getCheckpointer
    b <- liftIO $ _cpGetLatestBlock cp
    case b of
        Nothing -> return $! V.map Right txs
        Just (parentHeight, parentHash) -> do
            let currHeight = parentHeight + 1
            withCheckpointer (Just (currHeight, parentHash)) "execPreInsertCheckReq" $ \pdb -> do
                psEnv <- ask
                psState <- get
                parentHeader <- ParentHeader <$!> lookupBlockHeader parentHash "execPreInsertCheckReq"

                let parentTime = _blockCreationTime $ _parentHeader parentHeader
                    lenientCreationTime = not $ useLegacyCreationTimeNewBlockOrInsert parentHeader
                liftIO $ fmap Discard $ V.zipWith (>>)
                    <$> validateChainwebTxs cp parentTime lenientCreationTime currHeight txs (runGas pdb psState psEnv)

                    -- This code can be removed once the transition is complete and the guard
                    -- @useLegacyCreationTimeForTxValidation@ is false for all new blocks
                    -- of all chainweb versions.
                    --
                    <*> pure (validateLegacyTTL parentHeader txs)
  where
    runGas pdb pst penv ts =
        evalPactServiceM pst penv (attemptBuyGas noMiner pdb ts)

execLookupPactTxs
    :: PayloadCasLookup cas
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
        let !t = Just (_blockHeight bh + 1, _blockHash bh)
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
      P.GPostRead P.ReadModule {} -> 0
      _ -> fullRunFunction name ga
    modifiedGasModel = defGasModel { P.runGasModel = modifiedRunFunction }

-- | Gas Model used in /send and /local
--
officialGasModel :: P.GasModel
officialGasModel = freeModuleLoadGasModel
