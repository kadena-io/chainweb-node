{-# language
    DeriveAnyClass
    , DerivingStrategies
    , FlexibleContexts
    , ImportQualifiedPost
    , LambdaCase
    , NumericUnderscores
    , OverloadedStrings
    , PackageImports
    , RankNTypes
    , RecordWildCards
    , ScopedTypeVariables
    , TypeApplications
#-}

module Chainweb.Test.Pact5.Utils
    ( initCheckpointer
    , pactTxFrom4To5
    , getTestLogLevel
    , testLogFn
    , insertMempool
    , lookupMempool
    , withTestCutDb
    , withTempSQLiteResource
    , withInMemSQLiteResource
    , withTestRocksDb
    , withPactQueue
    , withMempool
    , withRunPactService
    , testRocksDb
    , withTestBlockHeaderDb
    )
    where

import Chainweb.Cut
import Chainweb.Chainweb (validatingMempoolConfig)
import "pact" Pact.Types.Command qualified as Pact4
import "pact" Pact.Types.Hash qualified as Pact4
import Chainweb.BlockCreationTime (BlockCreationTime(..))
import Chainweb.BlockHash (BlockHash)
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB (BlockHeaderDb, initBlockHeaderDb, closeBlockHeaderDb)
import Chainweb.BlockHeaderDB qualified as BlockHeaderDB
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.ChainValue (ChainValue(..), ChainValueCasLookup, chainLookupM)
import Chainweb.Cut
import Chainweb.Cut hiding (cutFetchTimeout)
import Chainweb.Cut.Create (InvalidSolvedHeader(..), WorkHeader(..), SolvedWork(..), extendCut, getCutExtension, newWorkHeaderPure)
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI.Server
import Chainweb.Graph
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.InMem
import Chainweb.Mempool.Mempool (InsertType (..), LookupResult(..), MempoolBackend (..), TransactionHash(..))
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Backend.Utils (openSQLiteConnection, closeSQLiteConnection, chainwebPragmas)
import Chainweb.Pact.PactService
import Chainweb.Pact.PactService.Pact4.ExecBlock ()
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Types
import Chainweb.Pact.Types (SQLiteEnv)
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Pact5.Transaction qualified as Pact5
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Test.Sync.WebBlockHeaderStore
import Chainweb.Time
import Chainweb.TreeDB (MaxRank(..))
import Chainweb.Utils
import Chainweb.Utils.Serialization (runGetS, runPutS)
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb)
import Chainweb.WebPactExecutionService
import Control.Concurrent hiding (throwTo)
import Control.Concurrent.Async
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.STM as STM
import Control.Exception (AsyncException (..), finally, throwTo)
import Control.Lens hiding (elements, only)
import Control.Monad
import Control.Monad.Catch hiding (finally)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (ResourceT, allocate, runResourceT)
import Control.Monad.Trans.Resource qualified as Resource
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Decimal (Decimal)
import Data.Foldable
import Data.Function
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.LogMessage
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.TaskMap
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Database.RocksDB.Internal qualified as R
import GHC.Stack
import GHC.Stack (HasCallStack)
import Network.HTTP.Client qualified as HTTP
import Numeric.Natural
import Pact.Core.Capabilities
import Pact.Core.ChainData hiding (ChainId, _chainId)
import Pact.Core.Command.Types
import Pact.Core.Command.Types qualified as Pact5
import Pact.Core.Gas.Types
import Pact.Core.Hash qualified as Pact5
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.JSON.Encode qualified as J
import Pact.Types.Gas qualified as Pact4
import PredicateTransformers as PT
import Streaming.Prelude qualified as S
import System.Environment (lookupEnv)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.LogLevel
import System.Random (randomIO)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Text.Printf (printf)

data Fixture = Fixture
    { _fixtureCutDb :: CutDb RocksDbTable
    , _fixturePayloadDb :: PayloadDb RocksDbTable
    , _fixtureWebBlockHeaderDb :: WebBlockHeaderDb
    , _fixtureMempools :: ChainMap (MempoolBackend Pact4.UnparsedTransaction)
    , _fixturePactQueues :: ChainMap PactQueue
    }

mkFixture :: ChainwebVersion -> PactServiceConfig -> RocksDb -> ResourceT IO Fixture
mkFixture v pactServiceConfig baseRdb = do
    logger <- liftIO getTestLogger
    (payloadDb, webBlockHeaderDb) <- withBlockDbs v baseRdb
    perChain <- iforM (HashSet.toMap (chainIds v)) $ \chain () -> do
        pactQueue <- liftIO $ newPactQueue 2_000
        mempool <- withMempool v chain pactQueue
        withRunPactService logger v chain pactQueue mempool webBlockHeaderDb payloadDb pactServiceConfig
        return (mempool, pactQueue)
    let webPact = mkWebPactExecutionService $ HashMap.map (mkPactExecutionService . snd) perChain
    cutDb <- withTestCutDb logger baseRdb v webBlockHeaderDb webPact

    return $ Fixture
        { _fixtureCutDb = cutDb
        , _fixturePayloadDb = payloadDb
        , _fixtureWebBlockHeaderDb = webBlockHeaderDb
        , _fixtureMempools = OnChains $ fst <$> perChain
        , _fixturePactQueues = OnChains $ snd <$> perChain
        }

advanceAllChains :: ()
    => ChainwebVersion
    -> Fixture
    -> ChainMap (BlockHeader -> PactQueue -> MempoolBackend Pact4.UnparsedTransaction -> IO PayloadWithOutputs)
    -> IO (ChainMap (Vector (CommandResult Pact5.Hash Text)))
advanceAllChains v Fixture{..} blocks = do
    latestCut :: HashMap ChainId BlockHeader  <- liftIO $ _fixtureCutDb ^. cut . to _cutMap
    commandResults <- forConcurrently (HashSet.toList (chainIds v)) $ \cid -> do
        let ph = latestCut ^?! ix cid
        creationTime <- getCurrentTimeIntegral
        let pactQueue = _fixturePactQueues ^?! atChain cid
        let mempool = _fixtureMempools ^?! atChain cid

        return (cid, commandResults)
        {-
        ph <- getParentTestBlockDb _fixtureCutDb cid
        creationTime <- getCurrentTimeIntegral
        let pactQueue = _fixturePactQueues ^?! atChain cid
        let mempool = _fixtureMempools ^?! atChain cid
        let makeEmptyBlock p _ _ = do
                bip <- throwIfNotPact5 =<< throwIfNoHistory =<< newBlock noMiner NewBlockEmpty (ParentHeader p) pactQueue
                return $! finalizeBlock bip
        payload <- fromMaybe makeEmptyBlock (blocks ^? atChain cid) ph pactQueue mempool
        added <- addTestBlockDb _fixtureCutDb (succ $ _blockHeight ph) (Nonce 0) (\_ _ -> creationTime) cid payload
        when (not added) $ error "failed to mine block"
        ph' <- getParentTestBlockDb _fixtureCutDb cid
        payload' <- validateBlock ph' (CheckablePayloadWithOutputs payload) pactQueue
        assertEqual "payloads must not be altered by validateBlock" payload payload'
        commandResults :: Vector (CommandResult Pact5.Hash Text) <- forM (_payloadWithOutputsTransactions payload') (decodeOrThrow' . LBS.fromStrict . _transactionOutputBytes . snd)
        return (cid, commandResults)
        -}
    return (onChains commandResults)

{-
-- this mines a block on *all chains*. if you don't specify a payload on a chain,
-- it adds empty blocks!
advanceAllChains :: ()
    => Fixture
    -> ChainMap (BlockHeader -> PactQueue -> MempoolBackend Pact4.UnparsedTransaction -> IO PayloadWithOutputs)
    -> IO (ChainMap (Vector (CommandResult Pact5.Hash Text)))
advanceAllChains Fixture{..} blocks = do
    commandResults <-
        forConcurrently (HashSet.toList (chainIds v)) $ \c -> do
            ph <- getParentTestBlockDb _fixtureBlockDb c
            creationTime <- getCurrentTimeIntegral
            let pactQueue = _fixturePactQueues ^?! atChain c
            let mempool = _fixtureMempools ^?! atChain c
            let makeEmptyBlock p _ _ = do
                    bip <- throwIfNotPact5 =<< throwIfNoHistory =<<
                        newBlock noMiner NewBlockEmpty (ParentHeader p) pactQueue
                    return $! finalizeBlock bip

            payload <- fromMaybe makeEmptyBlock (blocks ^? atChain cid) ph pactQueue mempool
            added <- addTestBlockDb _fixtureBlockDb
                (succ $ _blockHeight ph)
                (Nonce 0)
                (\_ _ -> creationTime)
                c
                payload
            when (not added) $
                error "failed to mine block"
            ph' <- getParentTestBlockDb _fixtureBlockDb c
            payload' <- validateBlock ph' (CheckablePayloadWithOutputs payload) pactQueue
            assertEqual "payloads must not be altered by validateBlock" payload payload'
            commandResults :: Vector (CommandResult Pact5.Hash Text)
                <- forM
                    (_payloadWithOutputsTransactions payload')
                    (decodeOrThrow' . LBS.fromStrict . _transactionOutputBytes . snd)
            -- assert on the command results
            return (c, commandResults)

    return (onChains commandResults)
-}

withBlockDbs :: ChainwebVersion -> RocksDb -> ResourceT IO (PayloadDb RocksDbTable, WebBlockHeaderDb)
withBlockDbs v rdb = do
    testRdb <- liftIO $ testRocksDb "withBlockDbs" rdb
    webBlockHeaderDb <- liftIO $ initWebBlockHeaderDb testRdb v
    let payloadDb = newPayloadDb testRdb
    liftIO $ initializePayloadDb v payloadDb
    return (payloadDb, webBlockHeaderDb)

testBlockHeaderDb
    :: RocksDb
    -> BlockHeader
    -> IO BlockHeaderDb
testBlockHeaderDb rdb h = do
    rdb' <- testRocksDb "withTestBlockHeaderDb" rdb
    initBlockHeaderDb (BlockHeaderDB.Configuration h rdb')

withTestBlockHeaderDb
    :: RocksDb
    -> BlockHeader
    -> ResourceT IO BlockHeaderDb
withTestBlockHeaderDb rdb h =
    snd <$> allocate (testBlockHeaderDb rdb h) closeBlockHeaderDb

testRocksDb
    :: ByteString -- ^ Prefix
    -> RocksDb
    -> IO RocksDb
testRocksDb l r = do
    prefix <- (<>) l . sshow <$> (randomIO @Word64)
    return r { _rocksDbNamespace = prefix }

withTestRocksDb :: ResourceT IO RocksDb
withTestRocksDb = view _2 . snd <$> allocate create destroy
    where
        create = do
            sysdir <- getCanonicalTemporaryDirectory
            dir <- createTempDirectory sysdir "chainweb-rocksdb-tmp"
            opts@(R.Options' opts_ptr _ _) <- R.mkOpts modernDefaultOptions
            rocks <- openRocksDb dir opts_ptr
            return (dir, rocks, opts)

        destroy (dir, rocks, opts) =
            closeRocksDb rocks `finally`
                R.freeOpts opts `finally`
                destroyRocksDb dir

-- | Internal. See https://www.sqlite.org/c3ref/open.html
withSQLiteResource
    :: String
    -> ResourceT IO SQLiteEnv
withSQLiteResource file = snd <$> allocate
    (openSQLiteConnection file chainwebPragmas)
    closeSQLiteConnection

-- | Open a temporary file-backed SQLite database.
withTempSQLiteResource :: ResourceT IO SQLiteEnv
withTempSQLiteResource = withSQLiteResource ""

-- | Open a temporary in-memory SQLite database.
withInMemSQLiteResource :: ResourceT IO SQLiteEnv
withInMemSQLiteResource = withSQLiteResource ":memory:"

withTestCutDb :: (Logger logger)
    => logger
    -> RocksDb
    -> ChainwebVersion
    -> WebBlockHeaderDb
    -> WebPactExecutionService
    -> ResourceT IO (CutDb RocksDbTable)
withTestCutDb logger rdb v webBlockHeaderDb webPact = snd <$> allocate create destroy
    where
        create :: IO (CutDb RocksDbTable)
        create = do
            rocks <- testRocksDb "withTestCutDb" rdb
            let payloadDb = newPayloadDb rocks
            let cutHashesDb = cutHashesTable rocks
            initializePayloadDb v payloadDb
            httpManager <- HTTP.newManager HTTP.defaultManagerSettings

            headerStore <- newWebBlockHeaderStore httpManager webBlockHeaderDb (logFunction logger)
            payloadStore <- newWebPayloadStore httpManager webPact payloadDb (logFunction logger)
            let cutHashesStore = cutHashesTable rocks

            let cutFetchTimeout = 3_000_000
            cutDb <- startCutDb (defaultCutDbParams v cutFetchTimeout) (logFunction logger) headerStore payloadStore cutHashesStore
            mine defaultMiner webPact cutDb (genesisCut v)
            return cutDb

        destroy :: CutDb RocksDbTable -> IO ()
        destroy = stopCutDb

-- | Build a linear chainweb (no forks, assuming single threaded use of the
-- cutDb). No POW or poison delay is applied. Block times are real times.
--
mine :: (HasCallStack, CanReadablePayloadCas tbl)
    => Miner
        -- ^ The miner. For testing you may use 'defaultMiner' or 'noMiner'.
    -> WebPactExecutionService
        -- ^ only the new-block generator is used. For testing you may use
        -- 'fakePact'.
    -> CutDb tbl
    -> Cut
    -> IO (Cut, ChainId, PayloadWithOutputs)
mine miner pact cutDb c = do

    -- Pick a chain that isn't blocked. With that mining is guaranteed to
    -- succeed if
    --
    -- - there are no other writers to the cut db,
    -- - the chainweb is in a consistent state,
    -- - the pact execution service is synced with the cutdb, and
    -- - the transaction generator produces valid blocks.
    cid <- getRandomUnblockedChain c

    tryMineForChain miner pact cutDb c cid >>= \case
        Left _ -> throwM $ InternalInvariantViolation
            "Failed to create new cut. This is a bug in Chainweb.Test.CutDB or one of it's users"
        Right x -> do
            void $ awaitCut cutDb $ ((<=) `on` _cutHeight) (view _1 x)
            return x

-- | Atomically await for a 'CutDb' instance to synchronize cuts according to some
-- predicate for a given 'Cut' and the results of '_cutStm'.
awaitCut
    :: CutDb tbl
    -> (Cut -> Bool)
    -> IO Cut
awaitCut cdb k = atomically $ do
    c <- _cutStm cdb
    STM.check $ k c
    pure c

testMineWithPayloadHash
    :: forall cid hdb
    . HasChainId cid
    => ChainValueCasLookup hdb BlockHeader
    => hdb
    -> Nonce
    -> Time Micros
    -> BlockPayloadHash
    -> cid
    -> Cut
    -> IO (Either MineFailure (T2 BlockHeader Cut))
testMineWithPayloadHash db n t ph cid c = try
    $ createNewCut (chainLookupM db) n t ph cid c

-- | Create a new block. Only produces a new cut but doesn't insert it into the
-- chain database.
--
-- The creation time isn't checked.
--
createNewCut
    :: (HasCallStack, MonadCatch m, HasChainId cid)
    => (ChainValue BlockHash -> m BlockHeader)
    -> Nonce
    -> Time Micros
    -> BlockPayloadHash
    -> cid
    -> Cut
    -> m (T2 BlockHeader Cut)
createNewCut hdb n t pay i c = do
    extension <- fromMaybeM BadAdjacents $ getCutExtension c i
    work <- newWorkHeaderPure hdb (BlockCreationTime t) extension pay
    (h, mc') <- extendCut c pay (solveWork work n t)
        `catch` \(InvalidSolvedHeader _ msg) -> throwM $ InvalidHeader msg
    c' <- fromMaybeM BadAdjacents mc'
    return $ T2 h c'

-- | Solve Work. Doesn't check that the nonce and the time are valid.
--
solveWork :: HasCallStack => WorkHeader -> Nonce -> Time Micros -> SolvedWork
solveWork w n t =
    case runGetS decodeBlockHeaderWithoutHash $ SBS.fromShort $ _workHeaderBytes w of
        Nothing -> error "Chainwb.Test.Cut.solveWork: Invalid work header bytes"
        Just hdr -> SolvedWork
            $ fromJuste
            $ runGetS decodeBlockHeaderWithoutHash
            $ runPutS
            $ encodeBlockHeaderWithoutHash
                -- After injecting the nonce and the creation time will have to do a
                -- serialization roundtrip to update the Merkle hash.
                --
                -- A "real" miner would inject the nonce and time without first
                -- decoding the header and would hand over the header in serialized
                -- form.

            $ set blockCreationTime (BlockCreationTime t)
            $ set blockNonce n
            $ hdr
-- | Return a random chain id from a cut that is not blocked.
--
getRandomUnblockedChain :: Cut -> IO ChainId
getRandomUnblockedChain c = do
    shuffled <- generate $ shuffle $ toList $ _cutMap c
    S.each shuffled
        & S.filter isUnblocked
        & S.map _blockChainId
        & S.head_
        & fmap fromJuste
  where
    isUnblocked h =
        let bh = _blockHeight h
            cid = _blockChainId h
        in all (>= bh) $ fmap _blockHeight $ toList $ cutAdjs c cid

-- | Build a linear chainweb (no forks). No POW or poison delay is applied.
-- Block times are real times.
--
tryMineForChain
    :: forall tbl. (HasCallStack, CanReadablePayloadCas tbl)
    => Miner
        -- ^ The miner. For testing you may use 'defaultMiner'.
    -> WebPactExecutionService
    -> CutDb tbl
    -> Cut
    -> ChainId
    -> IO (Either MineFailure (Cut, ChainId, PayloadWithOutputs))
tryMineForChain miner webPact cutDb c cid = do
    newBlock <- throwIfNoHistory =<< _webPactNewBlock webPact cid miner NewBlockFill parent
    let outputs = newBlockToPayloadWithOutputs newBlock
    let payloadHash = _payloadWithOutputsPayloadHash outputs
    t <- getCurrentTimeIntegral
    x <- testMineWithPayloadHash wdb (Nonce 0) t payloadHash cid c
    case x of
        Right (T2 h c') -> do
            addCutHashes cutDb (cutToCutHashes Nothing c')
                { _cutHashesHeaders = HashMap.singleton (_blockHash h) h
                , _cutHashesPayloads = HashMap.singleton (_blockPayloadHash h) (payloadWithOutputsToPayloadData outputs)
                }
            return $ Right (c', cid, outputs)
        Left e -> return $ Left e
    where
        parent = ParentHeader $ c ^?! ixg cid -- parent to mine on
        wdb = view cutDbWebBlockHeaderDb cutDb

-- | picks a random block header from a web chain. The result header is
-- guaranteed to not be a genesis header.
--
-- The web chain must contain at least one block that isn't a genesis block.
--
randomBlockHeader
    :: HasCallStack
    => CutDb tbl
    -> IO BlockHeader
randomBlockHeader cutDb = do
    curCut <- _cut cutDb
    allBlockHeaders <- webEntries (view cutDbWebBlockHeaderDb cutDb) $ \s -> s
        & S.filter (checkHeight curCut)
        & S.toList_
    generate $ elements allBlockHeaders
    where
        chainHeight curCut cid = _blockHeight (curCut ^?! ixg (_chainId cid))
        checkHeight curCut x = (_blockHeight x /= 0) && (_blockHeight x <= chainHeight curCut x)

-- | Picks a random transaction from a chain web, making sure that the
-- transaction isn't ahead of the longest cut.
--
randomTransaction
    :: HasCallStack
    => CanReadablePayloadCas tbl
    => CutDb tbl
    -> IO (BlockHeader, Int, Transaction, TransactionOutput)
randomTransaction cutDb = do
    bh <- randomBlockHeader cutDb
    Just pd <- lookupPayloadDataWithHeight payloadDb (Just $ _blockHeight bh) (_blockPayloadHash bh)
    let pay = BlockPayload
            { _blockPayloadTransactionsHash = _payloadDataTransactionsHash pd
            , _blockPayloadOutputsHash = _payloadDataOutputsHash pd
            , _blockPayloadPayloadHash = _payloadDataPayloadHash pd
            }

    Just btxs <-
        tableLookup
            (_newTransactionDbBlockTransactionsTbl $ _transactionDb payloadDb)
            (_blockHeight bh, _blockPayloadTransactionsHash pay)
    txIx <- generate $ choose (0, length (_blockTransactions btxs) - 1)
    Just outs <-
        tableLookup
            (_newBlockOutputsTbl $ _payloadCacheBlockOutputs $ _payloadCache payloadDb)
            (_blockHeight bh, _blockPayloadOutputsHash pay)
    return
        ( bh
        , txIx
        , _blockTransactions btxs Vector.! txIx
        , _blockOutputs outs Vector.! txIx
        )
    where
        payloadDb = view cutDbPayloadDb cutDb

data MineFailure
    = InvalidHeader T.Text
        -- ^ The header is invalid, e.g. because of a bad nonce or creation time.
    | MissingParentHeader
        -- ^ A parent header is missing in the chain db
    | BadAdjacents
        -- ^ This could mean that the chain is blocked.
    deriving stock (Show)
    deriving anyclass (Exception)

withPactQueue :: ResourceT IO PactQueue
withPactQueue = do
    liftIO (newPactQueue 2_000)

withMempool :: ()
    => ChainwebVersion
    -> ChainId
    -> PactQueue
    -> ResourceT IO (MempoolBackend Pact4.UnparsedTransaction)
withMempool v cid pactQueue = do
    pactExecutionServiceVar <- liftIO $ newMVar (mkPactExecutionService pactQueue)
    let mempoolCfg = validatingMempoolConfig cid v (Pact4.GasLimit 150_000) (Pact4.GasPrice 1e-8) pactExecutionServiceVar
    liftIO $ startInMemoryMempoolTest mempoolCfg

withRunPactService :: (Logger logger)
    => logger
    -> ChainwebVersion
    -> ChainId
    -> PactQueue
    -> MempoolBackend Pact4.UnparsedTransaction
    -> WebBlockHeaderDb
    -> PayloadDb RocksDbTable
    -> PactServiceConfig
    -> ResourceT IO ()
withRunPactService logger v cid pactQueue mempool webBlockHeaderDb payloadDb pactServiceConfig = do
    sqlite <- withTempSQLiteResource
    blockHeaderDb <- liftIO $ getWebBlockHeaderDb webBlockHeaderDb cid
    mempoolConsensus <- liftIO $ mkMempoolConsensus mempool blockHeaderDb (Just payloadDb) --bhdb (Just (_bdbPayloadDb tdb))
    let mempoolAccess = pactMemPoolAccess mempoolConsensus logger

    void $ Resource.allocate
        (forkIO $ runPactService v cid logger Nothing pactQueue mempoolAccess blockHeaderDb payloadDb sqlite pactServiceConfig) --bhdb (_bdbPayloadDb tdb) sqlite pactServiceConfig)
        (\tid -> throwTo tid ThreadKilled)

-- | Insert a 'Pact5.Transaction' into the mempool. The mempool currently operates by default on
--   'Pact4.UnparsedTransaction's, so the txs have to be converted.
insertMempool :: MempoolBackend Pact4.UnparsedTransaction -> InsertType -> [Pact5.Transaction] -> IO ()
insertMempool mp insertType txs = do
    let unparsedTxs :: [Pact4.UnparsedTransaction]
        unparsedTxs = flip map txs $ \tx ->
            case codecDecode Pact4.rawCommandCodec (codecEncode Pact5.payloadCodec tx) of
                Left err -> error err
                Right a -> a
    mempoolInsert mp insertType $ Vector.fromList unparsedTxs

-- | Looks up transactions in the mempool. Returns a set which indicates pending membership of the mempool.
lookupMempool :: MempoolBackend Pact4.UnparsedTransaction -> Vector Pact5.Hash -> IO (HashSet Pact5.Hash)
lookupMempool mp hashes = do
    results <- mempoolLookup mp $ Vector.map (TransactionHash . Pact5.unHash) hashes
    return $ HashSet.fromList $ Vector.toList $ flip Vector.mapMaybe results $ \case
        Missing -> Nothing
        Pending tx -> Just $ Pact5.Hash $ Pact4.unHash $ Pact4.toUntypedHash $ Pact4._cmdHash tx

-- | Initializes a checkpointer for a given chain.
initCheckpointer :: ChainwebVersion -> ChainId -> SQLiteEnv -> IO (Checkpointer GenericLogger)
initCheckpointer v cid sql = do
    logLevel <- getTestLogLevel
    initRelationalCheckpointer defaultModuleCacheLimit sql DoNotPersistIntraBlockWrites (genericLogger logLevel (testLogFn logLevel)) v cid

-- | Convert a 'Pact4.Transaction' to a 'Pact5.Transaction'.
pactTxFrom4To5 :: Pact4.Transaction -> Pact5.Transaction
pactTxFrom4To5 tx =
    let
        e = do
            let json = J.encode (fmap (Text.decodeUtf8 . SBS.fromShort . Pact4.payloadBytes) tx)
            cmdWithPayload <- Aeson.eitherDecode @(Pact5.Command Text) json
            Pact5.parseCommand cmdWithPayload
    in
    case e of
        Left err -> error err
        Right cmds -> cmds

-- | Users of this test suite can set the log level they desire by setting the "CHAINWEB_TEST_LOG_LEVEL"
--   environment variable.
getTestLogLevel :: IO LogLevel
getTestLogLevel = do
    let parseLogLevel txt = case T.toUpper txt of
            "DEBUG" -> Debug
            "INFO" -> Info
            "WARN" -> Warn
            "ERROR" -> Error
            _ -> Error
    fromMaybe Error . fmap (parseLogLevel . T.pack) <$> lookupEnv "CHAINWEB_TEST_LOG_LEVEL"

-- | Generally, we want tests to throw an exception on an Error log, but we don't want
--   to throw an exception on any other level of log.
testLogFn :: LogLevel -> Text -> IO ()
testLogFn ll msg = case ll of
    Error -> do
        error (Text.unpack msg)
    _ -> do
        Text.putStrLn msg

getTestLogger :: IO GenericLogger
getTestLogger = do
    logLevel <- getTestLogLevel
    return $ genericLogger logLevel (testLogFn logLevel)