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
import Data.DList (DList(..))
import qualified Data.DList as DL
import Data.Either
import Data.Foldable (foldl', toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
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

import qualified Pact.Gas as P
import Pact.Gas.Table
import qualified Pact.Interpreter as P
import qualified Pact.Parse as P
import qualified Pact.Types.ChainMeta as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.PactValue as P
import qualified Pact.Types.Runtime as P

------------------------------------------------------------------------------
-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeader, genesisBlockPayload)
import Chainweb.BlockHeaderDB
import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.Logger
import qualified Chainweb.Mempool.Mempool as Mempool
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
import Chainweb.Version (ChainwebVersion(..), HasChainwebVersion(..), HasChainId(..), txEnabledDate)
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
    -> BlockHeaderDb
    -> PayloadDb cas
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> IO ()
initPactService ver cid chainwebLogger reqQ mempoolAccess bhDb pdb dbDir nodeid resetDb =
    initPactService' ver cid chainwebLogger bhDb pdb dbDir nodeid resetDb go
  where
    go = initialPayloadState ver cid >> serviceRequests mempoolAccess reqQ

initPactService'
    :: Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> ChainId
    -> logger
    -> BlockHeaderDb
    -> PayloadDb cas
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> PactServiceM cas a
    -> IO a
initPactService' ver cid chainwebLogger bhDb pdb dbDir nodeid doResetDb act = do
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
          gasModel = tableGasModel defaultGasConfig
      let !pse = PactServiceEnv Nothing checkpointEnv def pdb bhDb gasModel rs
      evalStateT (runReaderT act pse) (PactServiceState Nothing mempty)
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
initialPayloadState v@Testnet03 cid =
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
            PreInsertCheckMsg (PreInsertCheckReq txs resultVar) -> do

                -- FIXME what can we use as parent hash here?
                -- PublicData has "", but that's not a valid BlockHash.
                --
                -- For now grab the last header from the block hash, but that
                -- can be @Nothing@.
                -- Or should we use the consensus state?
                --
                cp <- view $ psCheckpointEnv . cpeCheckpointer
                latestHash <- liftIO (_cpGetLatestBlock cp) >>= \case
                    Nothing -> fmap _blockHash $ genesisBlockHeader
                        <$> view chainwebVersion
                        <*> view chainId
                    Just (_, x) -> return x

                tryOne "execPreInsertCheckReq" resultVar $
                    execPreInsertCheckReq latestHash txs
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
    rewindTo Nothing target
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
    :: Miner
    -> PactDbEnv'
    -> BlockHash
    -> Vector (T2 ChainwebTransaction Uniqueness)
    -> PactServiceM cas (Vector BuyGasValidation)
attemptBuyGas miner (PactDbEnv' dbEnv) parentHash txs = do
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
        pd <- set P.pdPublicMeta pm <$!> view psPublicData
        spv <- pactSPV <$> view psBlockHeaderDb <*> pure parentHash
        return $! TransactionEnv P.Transactional db l pd spv nid gp rk gl
      where
        !pm = publicMetaOf cmd
        !nid = networkIdOf cmd
        !rk = P.cmdToRequestKey cmd

    runBuyGas
        :: P.PactDbEnv a
        -> ModuleCache
        -> T2 ChainwebTransaction Uniqueness
        -> PactServiceM cas (T2 ModuleCache BuyGasValidation)
    runBuyGas _ mcache (T2 tx Duplicate) = return $ T2 mcache (T3 tx Duplicate BuyGasFailed)
    runBuyGas db mcache (T2 tx Unique) = do
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
            Left _ ->
              return (T2 mcache (T3 tx Unique BuyGasFailed))  -- TODO throw InsertError instead
            Right t ->
              return $! T2 (_txCache t) (T3 tx Unique BuyGasPassed)

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
    -> IO (Vector (Either Mempool.InsertError ()))
validateChainwebTxs cp blockOriginationTime bh txs doBuyGas
    | bh == 0 = pure $! V.replicate (V.length txs) (Right ())
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
    validate
        :: T3 ChainwebTransaction Uniqueness AbleToBuyGas
        -> IO (Either Mempool.InsertError ())
    validate (T3 _ Duplicate _) = pure (Left Mempool.InsertErrorDuplicate)
    validate (T3 _ _ BuyGasFailed) = pure (Left Mempool.InsertErrorNoGas)
    validate (T3 tx Unique BuyGasPassed) =
      return $!
      bool (Left Mempool.InsertErrorInvalidTime) (Right ()) $
      checkTimes tx

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
          runDebitGas txs = fst <$!> runPactServiceM psState psEnv runGas
            where
              runGas = attemptBuyGas miner pdbenv (_blockHash parentHeader) txs
          validate bhi _bha txs = do
            -- note that here we previously were doing a validation
            -- that target == cpGetLatestBlock
            -- which we determined was unnecessary and was a db hit
            --
            -- TODO: propagate the underlying error type?
            V.map (either (const False) (const True))
                <$> validateChainwebTxs cp creationTime bhi txs runDebitGas
      liftIO $! fmap Discard $!
        mpaGetBlock mpAccess validate bHeight pHash parentHeader

    doNewBlock newTrans pdbenv = do
        logInfo $ "execNewBlock, about to get call processFork: "
                <> " (parent height = " <> sshow pHeight <> ")"
                <> " (parent hash = " <> sshow pHash <> ")"

        -- locally run 'execTransactions' with updated blockheight data
        -- Reject bad coinbase transactions in new block.
        results <- withBlockData parentHeader $
            execTransactions (Just pHash) miner newTrans (EnforceCoinbaseFailure True) pdbenv

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
        -- Reject bad coinbase txs in genesis.
        results <- execTransactions Nothing miner newTrans (EnforceCoinbaseFailure True) pdbenv
        return $! Discard (toPayloadWithOutputs miner results)

execLocal
    :: PayloadCas cas
    => ChainwebTransaction
    -> PactServiceM cas HashCommandResult
execLocal cmd = withDiscardedBatch $ do
    cp <- getCheckpointer
    mbLatestBlock <- liftIO $ _cpGetLatestBlock cp
    (bhe, bhash) <- case mbLatestBlock of
                       Nothing -> throwM NoBlockValidatedYet
                       (Just !p) -> return p
    let target = Just (succ bhe, bhash)
    bhDb <- asks _psBlockHeaderDb
    -- NOTE: On local calls, there might be code which needs the results of
    -- (chain-data). In such a case, the function `withBlockData` provides the
    -- necessary information for this call to return sensible values.
    parentHeader <- liftIO $! lookupM bhDb bhash
    let spv = pactSPV bhDb bhash
    withCheckpointer target "execLocal" $ \(PactDbEnv' pdbenv) -> do
        PactServiceEnv{..} <- ask
        mc <- use psInitCache
        r <- withBlockData parentHeader $
             liftIO $ applyLocal (_cpeLogger _psCheckpointEnv) pdbenv
                _psPublicData spv cmd mc
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
withBlockData bh action = locally psPublicData go action
  where
    (BlockHeight !bhe) = _blockHeight bh
    (BlockHash !ph) = _blockParent bh
    (BlockCreationTime (Time (TimeSpan (Micros !bt)))) =
      _blockCreationTime bh

    go t = t
      { P._pdBlockHeight = succ bhe
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
    oks <- liftIO (
        fmap (either (const False) (const True)) <$>
           validateChainwebTxs cp creationTime
               (_blockHeight currHeader) trans skipDebitGas)
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
      -- reject bad coinbase in genesis
      then execTransactions Nothing m txs (EnforceCoinbaseFailure True) pdbenv
      else do
        bhDb <- asks _psBlockHeaderDb
        ph <- liftIO $! lookupM bhDb (_blockParent currHeader)
        -- allow bad coinbase in validate
        withBlockData ph $! execTransactions (Just bParent) m txs (EnforceCoinbaseFailure False) pdbenv

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
    validateTxEnabled currHeader plData
    withBatch $ withCheckpointerRewind mb "execValidateBlock" $ \pdbenv -> do
        !result <- playOneBlock currHeader plData pdbenv
        return $! Save currHeader result
  where
    mb = if isGenesisBlock then Nothing else Just (bHeight, bParent)
    bHeight = _blockHeight currHeader
    bParent = _blockParent currHeader
    isGenesisBlock = isGenesisBlockHeader currHeader

validateTxEnabled :: MonadThrow m => BlockHeader -> PayloadData -> m ()
validateTxEnabled bh plData = case txEnabledDate (_blockChainwebVersion bh) of
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
    -> EnforceCoinbaseFailure
    -> PactDbEnv'
    -> PactServiceM cas Transactions
execTransactions nonGenesisParentHash miner ctxs enfCBFail (PactDbEnv' pactdbenv) = do
    mc <- use psInitCache
    ver <- asks _chainwebVersion
    cid <- asks _chainId
    let genBlockHash = _blockHash $ genesisBlockHeader ver cid
        parentHash = fromMaybe genBlockHash nonGenesisParentHash

    coinOut <- runCoinbase nonGenesisParentHash pactdbenv miner enfCBFail mc
    txOuts <- applyPactCmds isGenesis pactdbenv parentHash ctxs miner mc

    return $! Transactions (paired txOuts) coinOut
  where
    !isGenesis = isNothing nonGenesisParentHash
    cmdBSToTx = toTransactionBytes
      . fmap (T.decodeUtf8 . SB.fromShort . payloadBytes)
    paired outs = V.zipWith (curry $ first cmdBSToTx) ctxs outs


runCoinbase
    :: Maybe BlockHash
    -> P.PactDbEnv p
    -> Miner
    -> EnforceCoinbaseFailure
    -> ModuleCache
    -> PactServiceM cas HashCommandResult
runCoinbase Nothing _ _ _ _ = return noCoinbase
runCoinbase (Just parentHash) dbEnv miner enfCBFail mc = do
    psEnv <- ask

    let !pd = _psPublicData psEnv
        !logger = _cpeLogger . _psCheckpointEnv $ psEnv
        !bh = BlockHeight $ P._pdBlockHeight pd
        !rs = _psMinerRewards psEnv

    reward <- liftIO $! minerReward rs bh
    cr <- liftIO $! applyCoinbase logger dbEnv miner reward pd parentHash enfCBFail mc
    return $! toHashCommandResult cr

-- | Apply multiple Pact commands, incrementing the transaction Id for each.
-- The output vector is in the same order as the input (i.e. you can zip it
-- with the inputs.)
applyPactCmds
    :: Bool
    -> P.PactDbEnv p
    -> BlockHash
    -> Vector ChainwebTransaction
    -> Miner
    -> ModuleCache
    -> PactServiceM cas (Vector HashCommandResult)
applyPactCmds isGenesis env parentHash cmds miner mc =
    V.fromList . toList . sfst <$> V.foldM f (T2 mempty mc) cmds
  where
    f  (T2 dl mcache) cmd = applyPactCmd isGenesis env parentHash cmd miner mcache dl

-- | Apply a single Pact command
applyPactCmd
    :: Bool
    -> P.PactDbEnv p
    -> BlockHash
    -> ChainwebTransaction
    -> Miner
    -> ModuleCache
    -> DList HashCommandResult
    -> PactServiceM cas (T2 (DList HashCommandResult) ModuleCache)
applyPactCmd isGenesis dbEnv parentHash cmdIn miner mcache dl = do
    psEnv <- ask
    let !logger   = _cpeLogger . _psCheckpointEnv $ psEnv
        !pd       = _psPublicData psEnv
        !spv      = pactSPV (_psBlockHeaderDb psEnv) parentHash
        pactHash  = P._cmdHash cmdIn

    T2 result mcache' <- liftIO $ if isGenesis
        then applyGenesisCmd logger dbEnv pd spv (payloadObj <$> cmdIn)
        else applyCmd logger dbEnv miner (_psGasModel psEnv) pd spv cmdIn mcache

    when isGenesis $
      psInitCache <>= mcache'

    cp <- getCheckpointer
    -- mark the tx as processed at the checkpointer.
    liftIO $ _cpRegisterProcessedTx cp pactHash
    let !res = toHashCommandResult result
    pure $! T2 (DL.snoc dl res) mcache'

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

execPreInsertCheckReq
    :: PayloadCas cas
    => BlockHash
    -> Vector ChainwebTransaction
    -> PactServiceM cas (Vector (Either Mempool.InsertError ()))
execPreInsertCheckReq parentHash txs = do
    cp <- getCheckpointer
    b <- liftIO $ _cpGetLatestBlock cp
    case b of
        Nothing -> return $! V.map (const (Right ())) txs
        Just (h, ha) ->
            withCheckpointer (Just (h+1, ha)) "execPreInsertCheckReq" $ \pdb -> do
                now <- liftIO getCurrentTimeIntegral
                psEnv <- ask
                psState <- get
                liftIO (Discard <$>
                        validateChainwebTxs cp (BlockCreationTime now) (h + 1) txs
                              (runGas pdb psEnv psState))
  where
    runGas pdb psEnv psState ts =
        fst <$!> runPactServiceM psState psEnv (attemptBuyGas noMiner pdb parentHash ts)

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
