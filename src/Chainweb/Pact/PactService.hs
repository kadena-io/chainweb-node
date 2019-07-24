{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
      -- * For Side-tooling
    , execNewGenesisBlock
    , initPactService'
    ) where

------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (catches, Handler(..), finally, try)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Aeson as A
import Data.Bifoldable (bitraverse_)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Either
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import qualified Data.Sequence as Seq
import Data.String.Conv (toS)
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.Directory
import System.LogLevel

------------------------------------------------------------------------------
-- external pact modules
import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.SPV as P

------------------------------------------------------------------------------
-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
    (BlockHeader(..), BlockHeight(..), isGenesisBlockHeader)
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeader.Genesis.Testnet00Payload (payloadBlock)
import Chainweb.BlockHeaderDB
import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.CutDB
import Chainweb.Logger
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
import Chainweb.Transaction
import Chainweb.TreeDB (TreeDbException(..), collectForkBlocks, lookupM)
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
        nodeid resetDb go
  where
    go = do
        initialPayloadState ver cid mempoolAccess
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
initPactService' ver cid chainwebLogger spv bhDb pdb dbDir nodeid resetDb act = do
    let loggers = pactLoggers chainwebLogger
    let logger = P.newLogger loggers $ P.LogName ("PactService" <> show cid)
    let gasEnv = P.GasEnv 0 0.0 (P.constGasModel 1)
    let blockstate = BlockState 0 Nothing 0 M.empty
    let getsqliteDir = case dbDir of
          Nothing -> getXdgDirectory XdgData
            $ "chainweb-node/" <> sshow ver <> maybe mempty (("/" <>) . T.unpack . toText) nodeid <> "/sqlite"
          Just d -> return (d <> "sqlite")

    sqlitedir <- getsqliteDir

    when resetDb $ do
      exist <- doesDirectoryExist sqlitedir
      when exist $ removeDirectoryRecursive sqlitedir

    createDirectoryIfMissing True sqlitedir

    logFunctionText chainwebLogger Info $ "opened sqlitedb for " <> sshow cid <> " in directory " <> sshow sqlitedir

    let sqlitefile = sqlitedir <> "/" <> "pactservice-sqlite-chain" <> T.unpack (chainIdToText  cid) <> ".sqlite"

    logFunctionText chainwebLogger Info $ "opening sqlitedb named " <> (T.pack sqlitefile)

    withSQLiteConnection sqlitefile fastNoJournalPragmas False $ \sqlenv -> do

      checkpointEnv <- initRelationalCheckpointer blockstate sqlenv logger gasEnv

      let !pd = P.PublicData def def def
      let !pse = PactServiceEnv Nothing checkpointEnv (spv logger) pd pdb bhDb

      evalStateT (runReaderT act pse) (PactServiceState Nothing)

initialPayloadState
    :: PayloadCas cas
    => ChainwebVersion
    -> ChainId
    -> MemPoolAccess
    -> PactServiceM cas ()
initialPayloadState Test{} _ _ = pure ()
initialPayloadState TimedConsensus{} _ _ = pure ()
initialPayloadState PowConsensus{} _ _ = pure ()
initialPayloadState v@TimedCPM{} cid mpa = initializeCoinContract v cid mpa
initialPayloadState v@Development cid mpa = initializeCoinContract v cid mpa
initialPayloadState v@Testnet00 cid mpa = initializeCoinContract v cid mpa
initialPayloadState v@Testnet01 cid mpa = initializeCoinContract v cid mpa
initialPayloadState v@Testnet02 cid mpa = initializeCoinContract v cid mpa

initializeCoinContract
    :: PayloadCas cas
    => ChainwebVersion
    -> ChainId
    -> MemPoolAccess
    -> PactServiceM cas ()
initializeCoinContract v cid mpa = do
    cp <- view (psCheckpointEnv . cpeCheckpointer)
    genesisExists <- liftIO $ lookupBlockInCheckpointer cp (0, ghash)
    when (not genesisExists) createCoinContract

  where
    ghash = _blockHash genesisHeader
    createCoinContract = do
        txs <- execValidateBlock mpa genesisHeader inputPayloadData
        bitraverse_ throwM pure $ validateHashes txs genesisHeader

    PayloadWithOutputs{..} = payloadBlock
    inputPayloadData = PayloadData (fmap fst _payloadWithOutputsTransactions)
                       _payloadWithOutputsMiner
                       _payloadWithOutputsPayloadHash
                       _payloadWithOutputsTransactionsHash
                       _payloadWithOutputsOutputsHash
    genesisHeader = genesisBlockHeader v cid

-- | Forever loop serving Pact ececution requests and reponses from the queues
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
                txs <- try $ execValidateBlock memPoolAccess _valBlockHeader
                                 _valPayloadData
                case txs of
                  Left (SomeException e) -> do
                    logError (show e)
                    liftIO $ putMVar _valResultVar $ toPactInternalError e
                  Right r -> liftIO $ putMVar _valResultVar $ validateHashes r _valBlockHeader
                go
    toPactInternalError e = Left $ PactInternalError $ T.pack $ show e


toTransactionBytes :: P.Command ByteString -> Transaction
toTransactionBytes cwTrans =
    let plBytes = encodeToByteString cwTrans
    in Transaction { _transactionBytes = plBytes }


toOutputBytes :: HashCommandResult -> TransactionOutput
toOutputBytes cr =
    let outBytes = A.encode cr
    in TransactionOutput { _transactionOutputBytes = toS outBytes }


toPayloadWithOutputs :: MinerInfo -> Transactions -> PayloadWithOutputs
toPayloadWithOutputs mi ts =
    let oldSeq = Seq.fromList $ V.toList $ _transactionPairs ts
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
            " does not match the previously stored hash: " ++ show prevHash


restoreCheckpointer
    :: PayloadCas cas
    => Maybe (BlockHeight,BlockHash)
    -> PactServiceM cas PactDbEnv'
restoreCheckpointer maybeBB = do
    checkPointer <- view (psCheckpointEnv . cpeCheckpointer)
    logInfo $ "restoring " <> sshow maybeBB
    env <- liftIO $ restore checkPointer maybeBB
    return env


discardCheckpointer :: PayloadCas cas => PactServiceM cas ()
discardCheckpointer = finalizeCheckpointer $ \checkPointer -> discard checkPointer


finalizeCheckpointer :: (Checkpointer -> IO ()) -> PactServiceM cas ()
finalizeCheckpointer finalize = do
    checkPointer <- view (psCheckpointEnv . cpeCheckpointer)
    liftIO $! finalize checkPointer


_liftCPErr :: Either String a -> PactServiceM cas a
_liftCPErr = either internalError' return


-- | Note: The BlockHeader param here is the PARENT HEADER of the new
-- block-to-be
execNewBlock
    :: PayloadCas cas
    => MemPoolAccess
    -> BlockHeader
    -> MinerInfo
    -> PactServiceM cas PayloadWithOutputs
execNewBlock mpAccess parentHeader miner = do
    let pHeight@(BlockHeight bh) = _blockHeight parentHeader
        pHash = _blockHash parentHeader
        bHeight = succ pHeight

    -- TODO: shouldn't we process fork on validate?
    logInfo $ "execNewBlock, about to get call processFork: "
           <> " (parent height = " <> sshow pHeight <> ")"
           <> " (parent hash = " <> sshow pHash <> ")"
    liftIO $ mpaProcessFork mpAccess parentHeader

    newTrans <- liftIO $! mpaGetBlock mpAccess bHeight pHash parentHeader
    let rewindPoint = Just (bHeight, pHash)

    -- TODO: should we work towards uncommenting this? Currently, cutdb traffic
    -- moves the pactdb "current block" cursor too often for us to be sure that
    -- the miner and consensus are going to be in sync for newBlock. Changes to
    -- consensus or to checkpoint API may allow us to re-enable this check
    --
    -- cp <- view (psCheckpointEnv . cpeCheckpointer)
    -- latest <- liftIO $ getLatestBlock cp
    -- when (latest /= Just (pHeight, pHash)) $
    --     throwM $ PactServiceIllegalRewind rewindPoint latest

    -- rewind should usually be trivial / no-op
    rewindTo mpAccess rewindPoint $ \pdbenv -> do
        -- locally run 'execTransactions' with updated blockheight data
        results <- locally (psPublicData . P.pdBlockHeight) (const (succ bh)) $
                   execTransactions (Just pHash) miner newTrans pdbenv
        discardCheckpointer
        return $! toPayloadWithOutputs miner results


-- | only for use in generating genesis blocks in tools
execNewGenesisBlock
    :: PayloadCas cas
    => MinerInfo
    -> Vector ChainwebTransaction
    -> PactServiceM cas PayloadWithOutputs
execNewGenesisBlock miner newTrans = do
    pdbenv <- restoreCheckpointer Nothing
    results <- execTransactions Nothing miner newTrans pdbenv
    discardCheckpointer
    return $! toPayloadWithOutputs miner results


execLocal
    :: PayloadCas cas
    => ChainwebTransaction
    -> PactServiceM cas HashCommandResult
execLocal cmd = do
    bh <- use psStateValidated >>= \v -> case v of
        Nothing -> throwM NoBlockValidatedYet
        (Just !p) -> return p

    !pdbst <- restoreCheckpointer $ Just (succ $ _blockHeight bh, _blockHash bh)

    case pdbst of
        PactDbEnv' pactdbenv -> do
            PactServiceEnv{..} <- ask
            r <- liftIO $ applyLocal (_cpeLogger _psCheckpointEnv) pactdbenv
                 _psPublicData _psSpvSupport (fmap payloadObj cmd)
            discardCheckpointer
            return $! toHashCommandResult r

logg :: String -> String -> PactServiceM cas ()
logg level msg = view (psCheckpointEnv . cpeLogger)
  >>= \l -> liftIO $ P.logLog l level msg

logInfo :: String -> PactServiceM cas ()
logInfo = logg "INFO"

logError :: String -> PactServiceM cas ()
logError = logg "ERROR"

logDebug :: String -> PactServiceM cas ()
logDebug = logg "DEBUG"

playOneBlock
    :: MemPoolAccess
    -> BlockHeader
    -> PayloadData
    -> PactDbEnv'
    -> PactServiceM cas PayloadWithOutputs
playOneBlock mpAccess currHeader plData pdbenv = do
    -- precondition: restore has been called already
    --
    -- TODO: check precondition?
    miner <- decodeStrictOrThrow' (_minerData $ _payloadDataMiner plData)
    trans <- liftIO $ transactionsFromPayload plData
    !results <- locally (psPublicData . P.pdBlockHeight) (const bh) $
                execTransactions pBlock miner trans pdbenv
    finalizeCheckpointer (flip save bHash)   -- caller must restore again
    psStateValidated .= Just currHeader
    liftIO $ mpaSetLastHeader mpAccess currHeader
    return $! toPayloadWithOutputs miner results

  where
    (BlockHeight bh) = _blockHeight currHeader
    bHash = _blockHash currHeader
    bParent = _blockParent currHeader
    isGenesisBlock = isGenesisBlockHeader currHeader
    pBlock = if isGenesisBlock then Nothing else Just bParent

rewindTo
    :: forall a cas . PayloadCas cas
    => MemPoolAccess
    -> Maybe (BlockHeight, ParentHash)
    -> (PactDbEnv' -> PactServiceM cas a)
    -> PactServiceM cas a
rewindTo mpAccess mb act = do
    cp <- view (psCheckpointEnv . cpeCheckpointer)
    withRewind cp $ maybe rewindGenesis (doRewind cp) mb
  where
    rewindGenesis = restoreCheckpointer Nothing >>= act
    doRewind cp (newH, parentHash) = do
        payloadDb <- asks _psPdb
        mbLastBlock <- liftIO $ getLatestBlock cp
        lastHeightAndHash <- maybe failNonGenesisOnEmptyDb return mbLastBlock
        bhDb <- asks _psBlockHeaderDb
        playFork cp bhDb payloadDb newH parentHash lastHeightAndHash

    failNonGenesisOnEmptyDb = fail "impossible: playing non-genesis block to empty DB"

    withRewind :: forall z c . PayloadCas c
               => Checkpointer -> PactServiceM c z -> PactServiceM c z
    withRewind cp m = do
        e <- ask
        s <- get
        (a, s') <- liftIO $ withAtomicRewind cp $ runStateT (runReaderT m e) s
        put $! s'
        return $! a

    playFork cp bhdb payloadDb newH parentHash (lastBlockHeight, lastHash) =
      flip catches exHandlers $ do
          parentHeader <- liftIO $ lookupM bhdb parentHash
          lastHeader <- liftIO $ lookupM bhdb lastHash
          (!_, _, newBlocks) <-
              liftIO $ collectForkBlocks bhdb lastHeader parentHeader
          -- play fork blocks
          V.mapM_ (fastForward payloadDb) newBlocks
          -- play new block
          restoreCheckpointer (Just (newH, parentHash)) >>= act
      where
        exHandlers = [Handler handleTreeDbFailure, Handler handlePayloadFailure]
        handleEx :: forall e . Exception e => e -> PactServiceM cas a
        handleEx e =
              (liftIO $ getBlockParent cp (lastBlockHeight, lastHash)) >>= \case
                Nothing -> throwM e
                Just hash -> playFork cp bhdb payloadDb newH parentHash
                                      (pred lastBlockHeight, hash)
        handleTreeDbFailure e@(_ :: TreeDbException BlockHeaderDb) = handleEx e
        handlePayloadFailure e@(_ :: PayloadNotFoundException) = handleEx e

    fastForward :: forall c . PayloadCas c
                => PayloadDb c -> BlockHeader -> PactServiceM c ()
    fastForward payloadDb block = do
        let h = _blockHeight block
        let ph = _blockParent block
        pdbenv <- restoreCheckpointer (Just (h, ph))
        let bpHash = _blockPayloadHash block
        payload <- liftIO (payloadWithOutputsToPayloadData <$>
                                casLookupM payloadDb bpHash)
        void $ playOneBlock mpAccess block payload pdbenv
        -- double check output hash here?

-- | Validate a mined block. Execute the transactions in Pact again as
-- validation. Note: The BlockHeader here is the header of the block being
-- validated
execValidateBlock
    :: PayloadCas cas
    => MemPoolAccess
    -> BlockHeader
    -> PayloadData
    -> PactServiceM cas PayloadWithOutputs
execValidateBlock mpAccess currHeader plData =
    -- TODO: are we actually validating the output hash here?
    rewindTo mpAccess mb $ playOneBlock mpAccess currHeader plData

  where
    mb = if isGenesisBlock then Nothing else Just (bHeight, bParent)
    bHeight = _blockHeight currHeader
    bParent = _blockParent currHeader
    isGenesisBlock = isGenesisBlockHeader currHeader


execTransactions
    :: Maybe BlockHash
    -> MinerInfo
    -> Vector ChainwebTransaction
    -> PactDbEnv'
    -> PactServiceM cas Transactions
execTransactions nonGenesisParentHash miner ctxs (PactDbEnv' pactdbenv) = do
    !coinOut <- runCoinbase nonGenesisParentHash pactdbenv miner
    !txOuts <- applyPactCmds isGenesis pactdbenv ctxs miner
    return $! Transactions (paired txOuts) coinOut
  where
    !isGenesis = isNothing nonGenesisParentHash
    cmdBSToTx = toTransactionBytes . fmap payloadBytes
    paired = V.zipWith (curry $ first cmdBSToTx) ctxs


runCoinbase
    :: Maybe BlockHash
    -> P.PactDbEnv p
    -> MinerInfo
    -> PactServiceM cas HashCommandResult
runCoinbase Nothing _ _ = return noCoinbase
runCoinbase (Just parentHash) dbEnv mi@MinerInfo{..} = do
  psEnv <- ask

  let reward = 42.0 -- TODO. Not dispatching on chainweb version yet as E's PR will have PublicData
      pd = _psPublicData psEnv
      logger = _cpeLogger . _psCheckpointEnv $ psEnv

  cr <- liftIO $! applyCoinbase logger dbEnv mi reward pd parentHash
  return $! toHashCommandResult cr


-- | Apply multiple Pact commands, incrementing the transaction Id for each
applyPactCmds
    :: Bool
    -> P.PactDbEnv p
    -> Vector ChainwebTransaction
    -> MinerInfo
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
    -> MinerInfo
    -> ModuleCache
    -> [HashCommandResult]
    -> PactServiceM cas (T2 [HashCommandResult] ModuleCache)
applyPactCmd isGenesis dbEnv cmdIn miner mcache v = do
    psEnv <- ask
    let !logger   = _cpeLogger . _psCheckpointEnv $ psEnv
        !gasModel = P._geGasModel . _cpeGasEnv . _psCheckpointEnv $ psEnv
        !pd       = _psPublicData psEnv
        !spv      = _psSpvSupport psEnv

    -- cvt from Command PayloadWithTexts to Command ((Payload PublicMeta ParsedCode)
    let !cmd = payloadObj <$> cmdIn
    T2 result mcache' <- liftIO $ if isGenesis
        then applyGenesisCmd logger dbEnv pd spv cmd
        else applyCmd logger dbEnv miner gasModel pd spv cmd mcache

    pure $! T2 (toHashCommandResult result : v) mcache'

toHashCommandResult :: P.CommandResult [P.TxLog A.Value] -> HashCommandResult
toHashCommandResult = over (P.crLogs . _Just) (P.pactHash . encodeToByteString)

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
