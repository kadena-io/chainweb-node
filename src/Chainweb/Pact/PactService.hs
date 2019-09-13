{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Aeson as A
-- import Data.ByteString.Base64.URL as Base64
import Data.Bifoldable (bitraverse_)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as SB
import Data.Decimal
import Data.Default (def)
import Data.Either
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.Directory
import System.LogLevel

import Prelude hiding (lookup)

------------------------------------------------------------------------------
-- external pact modules

import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.Parse as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.PactValue as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.SPV as P

------------------------------------------------------------------------------
-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import qualified Chainweb.BlockHeader.Genesis.DevelopmentPayload as DN
import qualified Chainweb.BlockHeader.Genesis.TestnetPayload as PN
import qualified Chainweb.BlockHeader.Genesis.FastTimedCPMPayload as TN
import Chainweb.BlockHeaderDB
import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.NodeId
import Chainweb.Pact.Backend.RelationalCheckpointer (initRelationalCheckpointer)
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Service.PactQueue (getNextRequest)
import Chainweb.Pact.Service.Types
import Chainweb.Pact.SPV
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV.CreateProof (createTransactionOutputProof)
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.TreeDB (collectForkBlocks, lookup, lookupM)
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..))
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
    -> TQueue RequestMsg
    -> MemPoolAccess
    -> MVar (CutDb cas)
    -> BlockHeaderDb
    -> PayloadDb cas
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> IO ()
initPactService ver cid chainwebLogger reqQ mempoolAccess cdbv bhDb pdb dbDir
                nodeid resetDb =
    initPactService' ver cid chainwebLogger (pactSPV cdbv) bhDb pdb dbDir
                     nodeid resetDb $ do
        initialPayloadState ver cid
        serviceRequests mempoolAccess reqQ

initPactService'
    :: Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> ChainId
    -> logger
    -> (P.Logger -> P.SPVSupport)
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
        "opening sqlitedb named " <> (T.pack sqlitefile)

    withSQLiteConnection sqlitefile chainwebPragmas False $ \sqlenv -> do
      checkpointEnv <- initRelationalCheckpointer
                           initBlockState sqlenv logger gasEnv

      let !rs = readRewards ver
      let !pse = PactServiceEnv Nothing checkpointEnv (spv logger) def pdb
                                bhDb rs
      evalStateT (runReaderT act pse) (PactServiceState Nothing)
  where
    loggers = pactLoggers chainwebLogger
    logger = P.newLogger loggers $ P.LogName ("PactService" <> show cid)
    gasEnv = P.GasEnv 0 0.0 (P.constGasModel 1)

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
initialPayloadState v@TimedCPM{} cid = initializeCoinContract v cid TN.payloadBlock
initialPayloadState v@FastTimedCPM{} cid = initializeCoinContract v cid TN.payloadBlock
initialPayloadState v@Development cid = initializeCoinContract v cid DN.payloadBlock
initialPayloadState v@Testnet02 cid = initializeCoinContract v cid PN.payloadBlock

initializeCoinContract
    :: forall cas. PayloadCas cas
    => ChainwebVersion
    -> ChainId
    -> PayloadWithOutputs
    -> PactServiceM cas ()
initializeCoinContract v cid pwo = do
    cp <- view (psCheckpointEnv . cpeCheckpointer)
    genesisExists <- liftIO $ _cpLookupBlockInCheckpointer cp (0, ghash)
    unless genesisExists $ do
        txs <- execValidateBlock genesisHeader inputPayloadData
        bitraverse_ throwM pure $ validateHashes txs genesisHeader
  where
    ghash :: BlockHash
    ghash = _blockHash genesisHeader

    inputPayloadData :: PayloadData
    inputPayloadData = payloadWithOutputsToPayloadData pwo

    genesisHeader :: BlockHeader
    genesisHeader = genesisBlockHeader v cid

-- | Loop forever, serving Pact execution requests and reponses from the queues
serviceRequests
    :: PayloadCas cas
    => MemPoolAccess
    -> TQueue RequestMsg
    -> PactServiceM cas ()
serviceRequests memPoolAccess reqQ = do
    logInfo "Starting service"
    go `finally` logInfo "Stopping service"
  where
    go = do
        logDebug $ "serviceRequests: wait"
        msg <- liftIO $ getNextRequest reqQ
        logDebug $ "serviceRequests: " <> sshow msg
        case msg of
            CloseMsg -> return ()
            LocalMsg LocalReq{..} -> do
                r <- try $ execLocal _localRequest
                case r of
                  Left (SomeException e) -> liftIO $ putMVar _localResultVar $ toPactInternalError e
                  Right r' -> liftIO $ putMVar _localResultVar $ Right r'
                go
            NewBlockMsg NewBlockReq {..} -> do
                txs <- try $ execNewBlock memPoolAccess _newBlockHeader _newMiner
                case txs of
                  Left (SomeException e) -> do
                    logError (show e)
                    liftIO $ putMVar _newResultVar $ toPactInternalError e
                  Right r -> liftIO $ putMVar _newResultVar $ Right r
                go
            ValidateBlockMsg ValidateBlockReq {..} -> do
                txs <- try $ execValidateBlock _valBlockHeader _valPayloadData
                case txs of
                  Left (SomeException e) -> do
                    logError (show e)
                    liftIO $ putMVar _valResultVar $ toPactInternalError e
                  Right r -> liftIO $ putMVar _valResultVar $ validateHashes r _valBlockHeader
                go
            SpvMsg (SpvReq ph bh mv) -> do
                idx <- try $ execSpv undefined undefined undefined bh undefined ph
                case idx of
                  Left (SomeException e) -> do
                    logError (show e)
                    liftIO $ putMVar mv $ toPactInternalError e
                  Right r -> liftIO $ putMVar mv $ Right r
    toPactInternalError e = Left $ PactInternalError $ T.pack $ show e


toTransactionBytes :: P.Command ByteString -> Transaction
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
    :: PayloadWithOutputs
    -> BlockHeader
    -> Either PactException PayloadWithOutputs
validateHashes pwo bHeader =
    let newHash = _payloadWithOutputsPayloadHash pwo
        prevHash = _blockPayloadHash bHeader
    in if newHash == prevHash
        then Right pwo
        else Left $ BlockValidationFailure $ toS $
            "Hash from Pact execution: " ++ show newHash ++
            " does not match the previously stored hash: " ++ show prevHash ++
            ". Payload with outputs: " ++ show pwo


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
    checkPointer <- view (psCheckpointEnv . cpeCheckpointer)
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
    cenv <- restoreCheckpointer target caller
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
    checkPointer <- view (psCheckpointEnv . cpeCheckpointer)
    liftIO $! finalize checkPointer


_liftCPErr :: Either String a -> PactServiceM cas a
_liftCPErr = either internalError' return

validateChainwebTxsPreBlock
    :: PactDbEnv'
    -> Checkpointer
    -> BlockHeight
    -> BlockHash
    -> Vector ChainwebTransaction
    -> IO (Vector Bool)
validateChainwebTxsPreBlock dbEnv cp bh hash txs = do
    lb <- _cpGetLatestBlock cp
    when (Just (pred bh, hash) /= lb) $
        fail "internal error: restore point is wrong, refusing to validate."
    V.mapM checkOne txs
  where
    checkAccount tx = do
      let !pm = P._pMeta $ payloadObj $ P._cmdPayload tx
      let sender = P._pmSender pm
          (P.GasLimit (P.ParsedInteger limit)) = P._pmGasLimit pm
          (P.GasPrice (P.ParsedDecimal price)) = P._pmGasPrice pm
          limitInCoin = price * fromIntegral limit
      m <- readCoinAccount dbEnv sender
      case m of
        Nothing -> return True
        Just (T2 b _g) -> return $! b >= limitInCoin

    checkOne tx = do
        let pactHash = view P.cmdHash tx
        mb <- _cpLookupProcessedTx cp pactHash
        if mb == Nothing
        then checkAccount tx
        else return False

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
-- in 500k steps
--
minerReward
    :: forall cas
    . BlockHeight -> PactServiceM cas P.ParsedDecimal
minerReward bh = do
    m <- view $ psMinerRewards . at (roundBy bh 500000)
    case m of
      Nothing -> internalError
          $ "block height outside of admissible range: "
          <> sshow bh
      Just r -> return r
{-# INLINABLE minerReward #-}

-- | Note: The BlockHeader param here is the PARENT HEADER of the new
-- block-to-be
--
execNewBlock
    :: PayloadCas cas
    => MemPoolAccess
    -> BlockHeader
    -> Miner
    -> PactServiceM cas PayloadWithOutputs
execNewBlock mpAccess parentHeader miner = withDiscardedBatch $ do
    withCheckpointerRewind (Just (bHeight, pHash)) "execNewBlock" $ \pdbenv -> do
        logInfo $ "execNewBlock, about to get call processFork: "
                <> " (parent height = " <> sshow pHeight <> ")"
                <> " (parent hash = " <> sshow pHash <> ")"
        liftIO $ mpaProcessFork mpAccess parentHeader
        liftIO $ mpaSetLastHeader mpAccess parentHeader
        cp <- view (psCheckpointEnv . cpeCheckpointer)
        let validate = validateChainwebTxsPreBlock pdbenv cp
        newTrans <- liftIO $
            mpaGetBlock mpAccess validate bHeight pHash parentHeader
        -- locally run 'execTransactions' with updated blockheight data
        results <- withBlockData parentHeader $
            execTransactions (Just pHash) miner newTrans pdbenv
        return $! Discard (toPayloadWithOutputs miner results)
  where
    pHeight = _blockHeight parentHeader
    pHash = _blockHash parentHeader
    bHeight = succ pHeight


withBatch :: PactServiceM cas a -> PactServiceM cas a
withBatch act = mask $ \r -> do
    cp <- view (psCheckpointEnv . cpeCheckpointer)
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
        cp <- view (psCheckpointEnv . cpeCheckpointer)
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
    bh <- use psStateValidated >>= \v -> case v of
        Nothing -> throwM NoBlockValidatedYet
        (Just !p) -> return p

    let target = Just (succ $ _blockHeight bh, _blockHash bh)
    withCheckpointerRewind target "execLocal" $ \(PactDbEnv' pdbenv) -> do
        PactServiceEnv{..} <- ask
        r <- liftIO $ applyLocal (_cpeLogger _psCheckpointEnv) pdbenv
                _psPublicData _psSpvSupport (fmap payloadObj cmd)
        return $! Discard (toHashCommandResult r)

-- | Rewinding to given blockheader, lookup tx hash in
-- pact tx index and construct an SPV output proof from the
-- resulting data.
--
execSpv
    :: PayloadCas cas
    => CutDb cas
    -> BlockHeaderDb
    -> PayloadDb cas
    -> BlockHeader
        -- ^ given by most current consensus
    -> ChainId
        -- ^ target chain id
    -> P.PactHash
        -- ^ hash of tx to lookup index
    -> PactServiceM cas Base64TxOutputProof
execSpv cdb bdb pdb bh tid ph =
    rewindTo bh' $ \_ -> do
      cp <- view $ psCheckpointEnv . cpeCheckpointer
      m <- liftIO $ _cpLookupProcessedTx cp $ ph
      case m of
        Nothing -> internalError
          $ "Transaction hash not found: "
          <> sshow ph
        Just (!bhe, _) -> do
          idx <- liftIO $ getTxIdx bdb pdb bhe ph
          case idx of
            Left e -> internalError'
              $ "Transaction index not found: "
              <> sshow e
            Right i -> do
              sid <- view
                 $ psPublicData
                 . P.pdPublicMeta
                 . P.pmChainId
                 . to (read . T.unpack . P._chainId)

              _p <- liftIO $ createTransactionOutputProof cdb tid sid bhe i
              -- let p' = Base64TxOutputProof $ Base64.encode p
              return $ error "TODO: base64-encoded tx output proof"
  where
    !bh' = Just (_blockHeight bh, _blockHash bh)

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

-- | Execute a block.
--
playOneBlock
    :: BlockHeader
    -> PayloadData
    -> PactDbEnv'
    -> PactServiceM cas PayloadWithOutputs
playOneBlock currHeader plData pdbenv = do
    miner <- decodeStrictOrThrow' (_minerData $ _payloadDataMiner plData)
    trans <- liftIO $ transactionsFromPayload plData
    !results <- go miner trans
    psStateValidated .= Just currHeader
    return $! toPayloadWithOutputs miner results
  where
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
rewindTo mb = do
    cp <- view (psCheckpointEnv . cpeCheckpointer)
    maybe rewindGenesis (doRewind cp) mb
  where
    rewindGenesis = return ()
    doRewind cp (newH, parentHash) = do
        payloadDb <- asks _psPdb
        mbLastBlock <- liftIO $ _cpGetLatestBlock cp
        lastHeightAndHash <- maybe failNonGenesisOnEmptyDb return mbLastBlock
        bhDb <- asks _psBlockHeaderDb
        playFork cp bhDb payloadDb newH parentHash lastHeightAndHash

    failNonGenesisOnEmptyDb = fail "impossible: playing non-genesis block to empty DB"

    playFork cp bhdb payloadDb newH parentHash (lastBlockHeight, lastHash) = do
        parentHeader <- liftIO $ lookupM bhdb parentHash
        lastHeader <- findValidParent lastBlockHeight lastHash

        (!_, _, newBlocks) <-
            liftIO $ collectForkBlocks bhdb lastHeader parentHeader
        -- play fork blocks
        V.mapM_ (fastForward payloadDb) newBlocks
      where
        findValidParent height hash = liftIO (lookup bhdb hash) >>= \case
            Just x -> return x
            Nothing -> do
                logInfo $ "exception during rewind to "
                    <> sshow (newH, parentHash) <> ". Failed to look last hash "
                    <> sshow (height, hash) <> " in block header db. Continuing with parent."
                liftIO (_cpGetBlockParent cp (height, hash)) >>= \case
                    Nothing -> throwM $ BlockValidationFailure
                        $ "exception during rewind: missing block parent of last hash " <> sshow (height, hash)
                    Just predHash -> findValidParent (pred height) predHash

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
execValidateBlock currHeader plData =
    -- TODO: are we actually validating the output hash here?
    withBatch $ withCheckpointerRewind mb "execValidateBlock" $ \pdbenv -> do
        !result <- playOneBlock currHeader plData pdbenv
        return $! Save currHeader result
  where
    mb = if isGenesisBlock then Nothing else Just (bHeight, bParent)
    bHeight = _blockHeight currHeader
    bParent = _blockParent currHeader
    isGenesisBlock = isGenesisBlockHeader currHeader


execTransactions
    :: Maybe BlockHash
    -> Miner
    -> Vector ChainwebTransaction
    -> PactDbEnv'
    -> PactServiceM cas Transactions
execTransactions nonGenesisParentHash miner ctxs (PactDbEnv' pactdbenv) = do
    coinOut <- runCoinbase nonGenesisParentHash pactdbenv miner
    txOuts <- applyPactCmds isGenesis pactdbenv ctxs miner
    return $! Transactions (paired txOuts) coinOut
  where
    !isGenesis = isNothing nonGenesisParentHash
    cmdBSToTx = toTransactionBytes . fmap (SB.fromShort . payloadBytes)
    paired = V.zipWith (curry $ first cmdBSToTx) ctxs


runCoinbase
    :: Maybe BlockHash
    -> P.PactDbEnv p
    -> Miner
    -> PactServiceM cas HashCommandResult
runCoinbase Nothing _ _ = return noCoinbase
runCoinbase (Just parentHash) dbEnv miner = do
    psEnv <- ask

    let !pd = _psPublicData psEnv
        !logger = _cpeLogger . _psCheckpointEnv $ psEnv
        !bh = BlockHeight $ P._pdBlockHeight pd

    reward <- minerReward bh
    cr <- liftIO $! applyCoinbase logger dbEnv miner reward pd parentHash
    return $! toHashCommandResult cr

-- | Apply multiple Pact commands, incrementing the transaction Id for each
applyPactCmds
    :: Bool
    -> P.PactDbEnv p
    -> Vector ChainwebTransaction
    -> Miner
    -> PactServiceM cas (Vector HashCommandResult)
applyPactCmds isGenesis env cmds miner =
    V.fromList . sfst <$> V.foldM f mempty cmds
  where
    f  (T2 v mcache) cmd = applyPactCmd isGenesis env cmd miner mcache v

-- | Apply a single Pact command
applyPactCmd
    :: Bool
    -> P.PactDbEnv p
    -> ChainwebTransaction
    -> Miner
    -> ModuleCache
    -> [HashCommandResult]
    -> PactServiceM cas (T2 [HashCommandResult] ModuleCache)
applyPactCmd isGenesis dbEnv cmdIn miner mcache v = do
    psEnv <- ask
    let !logger   = _cpeLogger . _psCheckpointEnv $ psEnv
        !gasModel = P._geGasModel . _cpeGasEnv . _psCheckpointEnv $ psEnv
        !pd       = _psPublicData psEnv
        !spv      = _psSpvSupport psEnv
        pactHash  = view P.cmdHash cmdIn

    -- cvt from Command PayloadWithTexts to Command ((Payload PublicMeta ParsedCode)
    let !cmd = payloadObj <$> cmdIn
    T2 !result mcache' <- liftIO $ if isGenesis
        then applyGenesisCmd logger dbEnv pd spv cmd
        else applyCmd logger dbEnv miner gasModel pd spv cmd mcache

    cp <- view (psCheckpointEnv . cpeCheckpointer)
    -- mark the tx as processed at the checkpointer.
    liftIO $ _cpRegisterProcessedTx cp pactHash
    let !res = toHashCommandResult result
    pure $! T2 (res : v) mcache'

toHashCommandResult :: P.CommandResult [P.TxLog A.Value] -> HashCommandResult
toHashCommandResult = over (P.crLogs . _Just) f
  where
    f !x = let !out = P.pactHash $ encodeToByteString x
           in out

transactionsFromPayload :: PayloadData -> IO (Vector ChainwebTransaction)
transactionsFromPayload plData = do
    let !transSeq = _payloadDataTransactions plData
    let !transList = toList transSeq
    let !bytes = _transactionBytes <$!> transList
    let !eithers = toCWTransaction <$!> bytes
    -- Note: if any transactions fail to convert, the final validation hash
    -- will fail to match the one computed during newBlock
    let theRights = rights eithers
    return $! V.fromList theRights
  where
    toCWTransaction bs = codecDecode chainwebPayloadCodec bs
