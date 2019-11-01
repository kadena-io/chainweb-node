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
    (
      -- * For Chainweb
      initialPayloadState
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
import Control.Exception (SomeAsyncException)
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
import Data.Either
import Data.Foldable (foldl', toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tuple.Strict (T2(..), T3(..))
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.Directory
import System.LogLevel

import Prelude hiding (lookup)


------------------------------------------------------------------------------
-- external pact modules

import Pact.Gas (freeGasEnv)
import Pact.Gas.Table
import qualified Pact.Interpreter as P
import qualified Pact.Parse as P
import qualified Pact.Types.ChainMeta as P
import qualified Pact.Types.Command as P
import Pact.Types.Continuation
import Pact.Types.Gas
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.PactValue as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P
import qualified Pact.Types.SPV as P
import Pact.Types.Term (DefType(..), ObjectMap(..))

------------------------------------------------------------------------------
-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeader, genesisBlockPayload)
import Chainweb.BlockHeaderDB
import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.CutDB
import Chainweb.Logger
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
import Chainweb.Version (ChainwebVersion(..), txSilenceEndDate)
import Data.CAS (casLookupM)



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
    -> MVar (CutDb cas)
    -> BlockHeaderDb
    -> PayloadDb cas
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> IO ()
initPactService ver cid chainwebLogger reqQ mempoolAccess cdbv bhDb pdb dbDir nodeid resetDb =
    initPactService' ver cid chainwebLogger spv bhDb pdb dbDir nodeid resetDb go
  where
    spv :: P.SPVSupport
    spv = pactSPV cid cdbv

    go = initialPayloadState ver cid >> serviceRequests mempoolAccess reqQ

initPactService'
    :: Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> ChainId
    -> logger
    -> P.SPVSupport
    -> BlockHeaderDb
    -> PayloadDb cas
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> PactServiceM cas a
    -> IO a
initPactService' ver cid chainwebLogger spv bhDb pdb dbDir nodeid
                 doResetDb act = do
    sqlitedir <- getSqliteDir
    when doResetDb $ resetDb sqlitedir
    createDirectoryIfMissing True sqlitedir
    logFunctionText chainwebLogger Info $
        mconcat [ "opened sqlitedb for "
                , sshow cid
                , " in directory "
                , sshow sqlitedir ]

    let sqlitefile = getSqliteFile sqlitedir
    logFunctionText chainwebLogger Info $
        "opening sqlitedb named " <> T.pack sqlitefile

    withSQLiteConnection sqlitefile chainwebPragmas False $ \sqlenv -> do
      checkpointEnv <- initRelationalCheckpointer
                           initBlockState sqlenv logger

      let !rs = readRewards ver
          gasModel = tableGasModelNoSize defaultGasConfig -- TODO get sizing working
      let !pse = PactServiceEnv Nothing checkpointEnv spv def pdb
                                bhDb gasModel rs
      evalStateT (runReaderT act pse) (PactServiceState Nothing)
  where
    loggers = pactLoggers chainwebLogger
    logger = P.newLogger loggers $ P.LogName ("PactService" <> show cid)

    resetDb sqlitedir = do
      exist <- doesDirectoryExist sqlitedir
      when exist $ removeDirectoryRecursive sqlitedir

    getSqliteFile dir = mconcat [
        dir, "/pact-v1-chain-", T.unpack (chainIdToText cid), ".sqlite"]

    getSqliteDir =
        case dbDir of
            Nothing -> getXdgDirectory XdgData $
                       mconcat [ "chainweb-node/"
                               , show ver
                               , maybe mempty (("/" <>) . T.unpack . toText) nodeid
                               , "/sqlite" ]
            Just d -> return (d <> "sqlite")

initialPayloadState
    :: PayloadCas cas
    => ChainwebVersion
    -> ChainId
    -> PactServiceM cas ()
initialPayloadState Test{} _ = pure ()
initialPayloadState TimedConsensus{} _ = pure ()
initialPayloadState PowConsensus{} _ = pure ()
initialPayloadState v@TimedCPM{} cid =
    initializeCoinContract v cid $ genesisBlockPayload v cid
initialPayloadState v@FastTimedCPM{} cid =
    initializeCoinContract v cid $ genesisBlockPayload v cid
initialPayloadState v@Development cid =
    initializeCoinContract v cid $ genesisBlockPayload v cid
initialPayloadState v@Testnet02 cid =
    initializeCoinContract v cid $ genesisBlockPayload v cid
initialPayloadState v@Mainnet01 cid =
    initializeCoinContract v cid $ genesisBlockPayload v cid

initializeCoinContract
    :: forall cas. PayloadCas cas
    => ChainwebVersion
    -> ChainId
    -> PayloadWithOutputs
    -> PactServiceM cas ()
initializeCoinContract v cid pwo = do
    cp <- getCheckpointer
    genesisExists <- liftIO $ _cpLookupBlockInCheckpointer cp (0, ghash)
    unless genesisExists $ do
        txs <- execValidateBlock genesisHeader inputPayloadData
        bitraverse_ throwM pure $ validateHashes genesisHeader txs inputPayloadData
  where
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
            (r, s') <- liftIO $
                withAsync (runStateT (runReaderT act e) s) wait
            put $! s'
            return $! r

toTransactionBytes :: P.Command Text -> Transaction
toTransactionBytes cwTrans =
    let plBytes = encodeToByteString cwTrans
    in Transaction { _transactionBytes = plBytes }


toOutputBytes :: HashCommandResult -> TransactionOutput
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
    rewindTo target
    withCheckpointer target caller act

finalizeCheckpointer :: (Checkpointer -> IO ()) -> PactServiceM cas ()
finalizeCheckpointer finalize = do
    checkPointer <- getCheckpointer
    liftIO $! finalize checkPointer


_liftCPErr :: Either String a -> PactServiceM cas a
_liftCPErr = either internalError' return


data Uniqueness = Duplicate | Unique
data AbleToBuyGas = BuyGasFailed | BuyGasPassed
type BuyGasValidation = T3 ChainwebTransaction Uniqueness AbleToBuyGas

-- | Performs a dry run of PactExecution's `buyGas` function for transactions being validated.
--
attemptBuyGas
    :: PayloadCas cas
    => Miner
    -> PactDbEnv'
    -> Vector (T2 ChainwebTransaction Uniqueness)
    -> PactServiceM cas (Vector BuyGasValidation)
attemptBuyGas miner (PactDbEnv' dbEnv) txs = do
        psEnv <- ask
        V.fromList . ($ []) . sfst <$> V.foldM (f psEnv) (T2 id mempty) txs

  where
    f psEnv (T2 dl mcache) cmd = do
        T2 mcache' !res <- runBuyGas psEnv dbEnv mcache cmd
        pure $! T2 (dl . (res :)) mcache'

    createGasEnv
        :: PayloadCas cas
        => PactServiceEnv cas
        -> P.PactDbEnv a
        -> P.Command (P.Payload P.PublicMeta P.ParsedCode)
        -> P.CommandEnv a
    createGasEnv envM db cmd =
        P.CommandEnv Nothing P.Transactional db logger
        freeGasEnv publicData spv nid
      where
        !logger = _cpeLogger . _psCheckpointEnv $ envM
        !publicData = set P.pdPublicMeta (publicMetaOf cmd) (_psPublicData envM)
        !spv = _psSpvSupport envM
        !nid = networkIdOf cmd

    runBuyGas
        :: PayloadCas cas
        => PactServiceEnv cas
        -> P.PactDbEnv a
        -> ModuleCache
        -> T2 ChainwebTransaction Uniqueness
        -> PactServiceM cas (T2 ModuleCache BuyGasValidation)
    runBuyGas _ _ mcache (T2 tx Duplicate) = return $ T2 mcache (T3 tx Duplicate BuyGasFailed)
    runBuyGas envM db mcache (T2 tx Unique) = do
        let cmd = payloadObj <$> tx
            gasPrice = gasPriceOf cmd
            gasLimit = fromIntegral $ gasLimitOf cmd
            supply = gasFeeOf gasLimit gasPrice
            buyGasEnv = createGasEnv envM db cmd

        buyGasResultE <- liftIO $! P.catchesPactError $!
          buyGas buyGasEnv cmd miner supply mcache

        case buyGasResultE of
            Left _ ->
              return (T2 mcache (T3 tx Unique BuyGasFailed))  -- TODO throw InsertError instead
            Right (Left _) ->
              return (T2 mcache (T3 tx Unique BuyGasFailed))
            Right (Right (T3 _ _ newmcache)) ->
              return (T2 newmcache (T3 tx Unique BuyGasPassed))

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
    -> IO (Vector Bool)
validateChainwebTxs cp blockOriginationTime bh txs doBuyGas
    | bh == 0 = pure $! V.replicate (V.length txs) True
    | V.null txs = pure V.empty
    | otherwise = do
        -- TODO miner attack: does the list itself contain any duplicates txs?
        let f t = do
              uniqueness <- (bool Unique Duplicate . isJust)
                <$> _cpLookupProcessedTx cp (P._cmdHash t)
              return $! T2 t uniqueness
        txs' <- liftIO $ V.mapM f txs
        doBuyGas txs' >>= V.mapM validate
  where
    validate :: T3 ChainwebTransaction Uniqueness AbleToBuyGas -> IO Bool
    validate (T3 _ Duplicate _) = pure False
    validate (T3 _ _ BuyGasFailed) = pure False
    validate (T3 tx Unique BuyGasPassed) =
      pure $! all ($ tx) [checkTimes]


    checkTimes :: ChainwebTransaction -> Bool
    checkTimes = timingsCheck blockOriginationTime . fmap payloadObj

type RunGas = Vector (T2 ChainwebTransaction Uniqueness)
  -> IO (Vector (T3 ChainwebTransaction Uniqueness AbleToBuyGas))



skipDebitGas :: RunGas
skipDebitGas = return . fmap (\(T2 a b) -> (T3 a b BuyGasPassed))


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
      rewindTo target
      newTrans <- withCheckpointer target "preBlock" doPreBlock
      withCheckpointer target "execNewBlock" (doNewBlock newTrans)

  where

    doPreBlock pdbenv = do
      cp <- getCheckpointer
      psEnv <- ask
      psState <- get
      let runDebitGas :: RunGas
          runDebitGas txs = fst <$!> runPactServiceM psState psEnv runGas
            where
              runGas = attemptBuyGas miner pdbenv txs
          validate bhi _bha txs = do
            -- note that here we previously were doing a validation
            -- that target == cpGetLatestBlock
            -- which we determined was unnecessary and was a db hit
            validateChainwebTxs cp creationTime bhi txs runDebitGas
      liftIO $! fmap Discard $!
        mpaGetBlock mpAccess validate bHeight pHash parentHeader

    doNewBlock newTrans pdbenv = do
        logInfo $ "execNewBlock, about to get call processFork: "
                <> " (parent height = " <> sshow pHeight <> ")"
                <> " (parent hash = " <> sshow pHash <> ")"

        -- locally run 'execTransactions' with updated blockheight data
        results <- withBlockData parentHeader $
            execTransactions (Just pHash) miner newTrans pdbenv

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
        results <- execTransactions Nothing miner newTrans pdbenv
        return $! Discard (toPayloadWithOutputs miner results)

execLocal
    :: PayloadCas cas
    => ChainwebTransaction
    -> PactServiceM cas HashCommandResult
execLocal cmd = withDiscardedBatch $ do
    cp <- getCheckpointer
    mbLatestBlock <- liftIO $ _cpGetLatestBlock cp
    (bh, bhash) <- case mbLatestBlock of
                       Nothing -> throwM NoBlockValidatedYet
                       (Just !p) -> return p
    let target = Just (succ bh, bhash)
    withCheckpointer target "execLocal" $ \(PactDbEnv' pdbenv) -> do
        PactServiceEnv{..} <- ask
        r <- liftIO $ applyLocal (_cpeLogger _psCheckpointEnv) pdbenv
                _psPublicData _psSpvSupport (fmap payloadObj cmd)
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

-- | Run a pact service action with parent blockheader data fed into the
-- reader environment.
--
withBlockData
    :: forall cas a
    . BlockHeader
        -- ^ this must be a -parent header- in all cases
    -> PactServiceM cas a
        -- ^ the action to be run
    -> PactServiceM cas a
withBlockData bhe action = locally psPublicData go action
  where
    (BlockHeight !bh) = _blockHeight bhe
    (BlockHash !ph) = _blockParent bhe
    (BlockCreationTime (Time (TimeSpan (Micros !bt)))) =
      _blockCreationTime bhe

    go t = t
      { P._pdBlockHeight = succ bh
      , P._pdBlockTime = bt
      , P._pdPrevBlockHash = toText ph
      }

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
    let creationTime = _blockCreationTime currHeader
    -- prop_tx_ttl_validate
    oks <- liftIO $
           validateChainwebTxs cp creationTime
               (_blockHeight currHeader) trans skipDebitGas
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

    bParent = _blockParent currHeader

    isGenesisBlock = isGenesisBlockHeader currHeader

    go m txs =
      if isGenesisBlock
      then execTransactions Nothing m txs pdbenv
      else do
        bhDb <- asks _psBlockHeaderDb
        ph <- liftIO $! lookupM bhDb (_blockParent currHeader)
        withBlockData ph $! execTransactions (Just bParent) m txs pdbenv

-- | Rewinds the pact state to @mb@.
--
-- If @mb@ is 'Nothing', it rewinds to the genesis block.
--
rewindTo
    :: forall cas . PayloadCas cas
    => Maybe (BlockHeight, ParentHash)
        -- ^ The block height @height@ to which to restore and the parent header
        -- @parentHeader@.
        --
        -- It holds that @(_blockHeight parentHeader == pred height)@
    -> PactServiceM cas ()
rewindTo mb = maybe rewindGenesis doRewind mb
  where
    rewindGenesis = return ()
    doRewind (_, parentHash) = do
        payloadDb <- asks _psPdb
        lastHeader <- findLatestValidBlock >>= maybe failNonGenesisOnEmptyDb return
        bhDb <- asks _psBlockHeaderDb
        playFork bhDb payloadDb parentHash lastHeader

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
    validateSilenceDates currHeader plData
    withBatch $ withCheckpointerRewind mb "execValidateBlock" $ \pdbenv -> do
        !result <- playOneBlock currHeader plData pdbenv
        return $! Save currHeader result
  where
    mb = if isGenesisBlock then Nothing else Just (bHeight, bParent)
    bHeight = _blockHeight currHeader
    bParent = _blockParent currHeader
    isGenesisBlock = isGenesisBlockHeader currHeader

validateSilenceDates :: MonadThrow m => BlockHeader -> PayloadData -> m ()
validateSilenceDates bh plData = case txSilenceEndDate (_blockChainwebVersion bh) of
    Just end | end > blockTime && not isGenesisBlock && not payloadIsEmpty ->
        throwM . BlockValidationFailure . A.toJSON $ ObjectEncoded bh
    _ -> pure ()
  where
    blockTime = _bct $ _blockCreationTime bh
    payloadIsEmpty = V.null $ _payloadDataTransactions plData
    isGenesisBlock = isGenesisBlockHeader bh

execTransactions
    :: Maybe BlockHash
    -> Miner
    -> Vector ChainwebTransaction
    -> PactDbEnv'
    -> PactServiceM cas Transactions
execTransactions nonGenesisParentHash miner ctxs (PactDbEnv' pactdbenv) = do
    T2 coinOut mc <- runCoinbase nonGenesisParentHash pactdbenv miner
    txOuts <- applyPactCmds isGenesis pactdbenv ctxs miner mc
    return $! Transactions (paired txOuts) coinOut
  where
    !isGenesis = isNothing nonGenesisParentHash
    cmdBSToTx = toTransactionBytes
      . fmap (T.decodeUtf8 . SB.fromShort . payloadBytes)
    paired = V.zipWith (curry $ first cmdBSToTx) ctxs


runCoinbase
    :: Maybe BlockHash
    -> P.PactDbEnv p
    -> Miner
    -> PactServiceM cas (T2 HashCommandResult ModuleCache)
runCoinbase Nothing _ _ = return $ T2 noCoinbase mempty
runCoinbase (Just parentHash) dbEnv miner = do
    psEnv <- ask

    let !pd = _psPublicData psEnv
        !logger = _cpeLogger . _psCheckpointEnv $ psEnv
        !bh = BlockHeight $ P._pdBlockHeight pd
        !rs = _psMinerRewards psEnv

    reward <- liftIO $! minerReward rs bh
    T2 cr mc <- liftIO $! applyCoinbase logger dbEnv miner reward pd parentHash
    return $! T2 (toHashCommandResult cr) mc

-- | Apply multiple Pact commands, incrementing the transaction Id for each.
-- The output vector is in the same order as the input (i.e. you can zip it
-- with the inputs.)
applyPactCmds
    :: Bool
    -> P.PactDbEnv p
    -> Vector ChainwebTransaction
    -> Miner
    -> ModuleCache
    -> PactServiceM cas (Vector HashCommandResult)
applyPactCmds isGenesis env cmds miner mc =
    V.fromList . ($ []) . sfst <$> V.foldM f (T2 id mc) cmds
  where
    f  (T2 dl mcache) cmd = applyPactCmd isGenesis env cmd miner mcache dl

-- | Apply a single Pact command
applyPactCmd
    :: Bool
    -> P.PactDbEnv p
    -> ChainwebTransaction
    -> Miner
    -> ModuleCache
    -> ([HashCommandResult] -> [HashCommandResult])  -- ^ difference list
    -> PactServiceM cas (T2 ([HashCommandResult] -> [HashCommandResult]) ModuleCache)
applyPactCmd isGenesis dbEnv cmdIn miner mcache dl = do
    psEnv <- ask
    let !logger   = _cpeLogger . _psCheckpointEnv $ psEnv
        !pd       = _psPublicData psEnv
        !spv      = _psSpvSupport psEnv
        pactHash  = view P.cmdHash cmdIn

    T2 !result mcache' <- liftIO $ if isGenesis
        then applyGenesisCmd logger dbEnv pd spv (payloadObj <$> cmdIn)
        else applyCmd logger dbEnv miner (_psGasModel psEnv) pd spv cmdIn mcache

    cp <- getCheckpointer
    -- mark the tx as processed at the checkpointer.
    liftIO $ _cpRegisterProcessedTx cp pactHash
    let !res = toHashCommandResult result
    pure $! T2 (dl . (res :)) mcache'

toHashCommandResult :: P.CommandResult [P.TxLog A.Value] -> HashCommandResult
toHashCommandResult = over (P.crLogs . _Just) $ P.pactHash . encodeToByteString

transactionsFromPayload :: PayloadData -> IO (Vector ChainwebTransaction)
transactionsFromPayload plData = do
    unless (null theLefts) $ do
        throwM $ TransactionDecodeFailure $ "Failed to decode pact transactions: "
            <> (T.intercalate ". " $ T.pack <$> theLefts)
    return $! V.fromList (rights eithers)
  where
    toCWTransaction bs = codecDecode chainwebPayloadCodec bs
    !transSeq = _payloadDataTransactions plData
    !transList = toList transSeq
    !bytes = _transactionBytes <$!> transList
    !eithers = toCWTransaction <$!> bytes
    theLefts = lefts eithers

execLookupPactTxs
    :: PayloadCas cas
    => Maybe (T2 BlockHeight BlockHash)
    -> Vector P.PactHash
    -> PactServiceM cas (Vector (Maybe (T2 BlockHeight BlockHash)))
execLookupPactTxs restorePoint txs
    | V.null txs = return mempty
    | otherwise = go
  where
    getRestorePoint = case restorePoint of
        Nothing -> fmap (\bh -> T2 (_blockHeight bh) (_blockHash bh))
            <$> findLatestValidBlock
        x -> return x

    go = do
        cp <- getCheckpointer
        mrp <- getRestorePoint
        case mrp of
            Nothing -> return mempty      -- can't look up anything at genesis
            Just (T2 lh lha) ->
                withCheckpointerRewind (Just (lh + 1, lha)) "lookupPactTxs" $ \_ ->
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

-- | temporary gas model without sizing
tableGasModelNoSize :: GasCostConfig -> GasModel
tableGasModelNoSize gasConfig =
  let run name ga = case ga of
        GSelect mColumns -> case mColumns of
          Nothing -> 1
          Just [] -> 1
          Just cs -> _gasCostConfig_selectColumnCost gasConfig * (fromIntegral (length cs))
        GSortFieldLookup n ->
          fromIntegral n * _gasCostConfig_sortFactor gasConfig
        GConcatenation i j ->
          fromIntegral (i + j) * _gasCostConfig_concatenationFactor gasConfig
        GUnreduced ts -> case Map.lookup name (_gasCostConfig_primTable gasConfig) of
          Just g -> g ts
          Nothing -> error $ "Unknown primitive \"" <> T.unpack name <> "\" in determining cost of GUnreduced"
        GPostRead r -> case r of
          ReadData cols -> _gasCostConfig_readColumnCost gasConfig * fromIntegral (Map.size (_objectMap cols))
          ReadKey _rowKey -> _gasCostConfig_readColumnCost gasConfig
          ReadTxId -> _gasCostConfig_readColumnCost gasConfig
          ReadModule _moduleName _mCode ->  _gasCostConfig_readColumnCost gasConfig
          ReadInterface _moduleName _mCode ->  _gasCostConfig_readColumnCost gasConfig
          ReadNamespace _ns ->  _gasCostConfig_readColumnCost gasConfig
          ReadKeySet _ksName _ks ->  _gasCostConfig_readColumnCost gasConfig
          ReadYield (Yield _obj _) -> _gasCostConfig_readColumnCost gasConfig * fromIntegral (Map.size (_objectMap _obj))
        GWrite _w -> 1 {- case w of
          WriteData _type key obj ->
            (memoryCost key (_gasCostConfig_writeBytesCost gasConfig))
            + (memoryCost obj (_gasCostConfig_writeBytesCost gasConfig))
          WriteTable tableName -> (memoryCost tableName (_gasCostConfig_writeBytesCost gasConfig))
          WriteModule _modName _mCode ->
            (memoryCost _modName (_gasCostConfig_writeBytesCost gasConfig))
            + (memoryCost _mCode (_gasCostConfig_writeBytesCost gasConfig))
          WriteInterface _modName _mCode ->
            (memoryCost _modName (_gasCostConfig_writeBytesCost gasConfig))
            + (memoryCost _mCode (_gasCostConfig_writeBytesCost gasConfig))
          WriteNamespace ns -> (memoryCost ns (_gasCostConfig_writeBytesCost gasConfig))
          WriteKeySet ksName ks ->
            (memoryCost ksName (_gasCostConfig_writeBytesCost gasConfig))
            + (memoryCost ks (_gasCostConfig_writeBytesCost gasConfig))
          WriteYield obj -> (memoryCost obj (_gasCostConfig_writeBytesCost gasConfig)) -}
        GModuleMember _module -> _gasCostConfig_moduleMemberCost gasConfig
        GModuleDecl _moduleName _mCode -> (_gasCostConfig_moduleCost gasConfig)
        GUse _moduleName _mHash -> (_gasCostConfig_useModuleCost gasConfig)
          -- The above seems somewhat suspect (perhaps cost should scale with the module?)
        GInterfaceDecl _interfaceName _iCode -> (_gasCostConfig_interfaceCost gasConfig)
        GUserApp t -> case t of
          Defpact -> (_gasCostConfig_defPactCost gasConfig) * _gasCostConfig_functionApplicationCost gasConfig
          _ -> _gasCostConfig_functionApplicationCost gasConfig
  in GasModel
      { gasModelName = "table"
      , gasModelDesc = "table-based cost model"
      , runGasModel = run
      }
