{-# language
    BangPatterns
    , DataKinds
    , DeriveAnyClass
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
    , TemplateHaskell
    , TypeApplications
#-}

module Chainweb.Test.Pact5.CutFixture
    ( Fixture(..)
    , mkFixture
    , fixtureCutDb
    , fixturePayloadDb
    , fixtureWebBlockHeaderDb
    , fixtureWebPactExecutionService
    , fixtureMempools
    , fixturePactQueues
    , advanceAllChains
    , withTestCutDb
    )
    where

import Chainweb.BlockCreationTime (BlockCreationTime(..))
import Chainweb.BlockHash (BlockHash)
import Chainweb.BlockHeader hiding (blockCreationTime, blockNonce)
import Chainweb.BlockHeader.Internal (blockCreationTime, blockNonce)
import Chainweb.ChainId
import Chainweb.ChainValue (ChainValue(..), ChainValueCasLookup, chainLookupM)
import Chainweb.Cut
import Chainweb.Cut.Create (InvalidSolvedHeader(..), WorkHeader(..), SolvedWork(..), extendCut, getCutExtension, newWorkHeaderPure)
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Mempool.Mempool (MempoolBackend)
import Chainweb.Miner.Pact
import Chainweb.Pact.PactService.Pact4.ExecBlock ()
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Types
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Storage.Table.RocksDB
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Test.Pact5.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Serialization (runGetS, runPutS)
import Chainweb.Version
import Chainweb.Version.Utils
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService
import Control.Concurrent.STM as STM
import Control.Lens hiding (elements, only)
import Control.Monad
import Control.Monad.Catch hiding (finally)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Foldable
import Data.Function
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Stack
import Network.HTTP.Client qualified as HTTP
import Pact.Core.Command.Types
import Pact.Core.Hash qualified as Pact5
import Streaming.Prelude qualified as S
import Test.QuickCheck
import Test.Tasty.HUnit (assertBool)

data Fixture = Fixture
    { _fixtureCutDb :: CutDb RocksDbTable
    , _fixturePayloadDb :: PayloadDb RocksDbTable
    , _fixtureWebBlockHeaderDb :: WebBlockHeaderDb
    , _fixtureWebPactExecutionService :: WebPactExecutionService
    , _fixtureMempools :: ChainMap (MempoolBackend Pact4.UnparsedTransaction)
    , _fixturePactQueues :: ChainMap PactQueue
    }
makeLenses ''Fixture

mkFixture :: ChainwebVersion -> PactServiceConfig -> RocksDb -> ResourceT IO Fixture
mkFixture v pactServiceConfig baseRdb = do
    logger <- liftIO getTestLogger
    (payloadDb, webBHDb) <- withBlockDbs v baseRdb
    perChain <- iforM (HashSet.toMap (chainIds v)) $ \chain () -> do
        pactQueue <- liftIO $ newPactQueue 2_000
        mempool <- withMempool v chain pactQueue
        withRunPactService logger v chain pactQueue mempool webBHDb payloadDb pactServiceConfig
        return (mempool, pactQueue)
    let webPact = mkWebPactExecutionService $ HashMap.map (mkPactExecutionService . snd) perChain
    cutDb <- withTestCutDb logger baseRdb v webBHDb webPact

    let fixture = Fixture
            { _fixtureCutDb = cutDb
            , _fixturePayloadDb = payloadDb
            , _fixtureWebBlockHeaderDb = webBHDb
            , _fixtureWebPactExecutionService = webPact
            , _fixtureMempools = OnChains $ fst <$> perChain
            , _fixturePactQueues = OnChains $ snd <$> perChain
            }
    _ <- liftIO $ advanceAllChains v fixture
    return fixture

advanceAllChains :: (HasCallStack)
    => ChainwebVersion
    -> Fixture
    -> IO (ChainMap (Vector (CommandResult Pact5.Hash Text)))
advanceAllChains v Fixture{..} = do
    latestCut <- liftIO $ _fixtureCutDb ^. cut
    let blockHeights = fmap (view blockHeight) $ latestCut ^. cutMap
    let latestBlockHeight = maximum blockHeights
    assertBool "all block heights in the latest cut must be the same" $
        all (== latestBlockHeight) blockHeights

    (_finalCut, perChainCommandResults) <- foldM
        (\ (prevCut, !acc) cid -> do
            putStrLn $ "accumulator: " ++ show acc
            (newCut, _minedChain, pwo) <- mine noMiner NewBlockFill _fixtureWebPactExecutionService _fixtureCutDb prevCut

            putStrLn $ "PAYLOAD HASH: " <> sshow (_payloadWithOutputsPayloadHash pwo)

            commandResults <- forM (_payloadWithOutputsTransactions pwo) $ \(_, txOut) -> do
                decodeOrThrow' $ LBS.fromStrict $ _transactionOutputBytes txOut

            addNewPayload _fixturePayloadDb latestBlockHeight pwo

            return $ (newCut, (cid, commandResults) : acc)
        )
        (latestCut, [])
        (HashSet.toList (chainIdsAt v (latestBlockHeight + 1)))

    return (onChains perChainCommandResults)

withTestCutDb :: (Logger logger)
    => logger
    -> RocksDb
    -> ChainwebVersion
    -> WebBlockHeaderDb
    -> WebPactExecutionService
    -> ResourceT IO (CutDb RocksDbTable)
withTestCutDb logger rdb v webBHDb webPact = snd <$> allocate create destroy
    where
        create :: IO (CutDb RocksDbTable)
        create = do
            rocks <- testRocksDb "withTestCutDb" rdb
            let payloadDb = newPayloadDb rocks
            --let cutHashesDb = cutHashesTable rocks
            initializePayloadDb v payloadDb
            httpManager <- HTTP.newManager HTTP.defaultManagerSettings

            headerStore <- newWebBlockHeaderStore httpManager webBHDb (logFunction logger)
            payloadStore <- newWebPayloadStore httpManager webPact payloadDb (logFunction logger)
            let cutHashesStore = cutHashesTable rocks

            let cutFetchTimeout = 3_000_000
            cutDb <- startCutDb (defaultCutDbParams v cutFetchTimeout) (logFunction logger) headerStore payloadStore cutHashesStore
            _ <- mine defaultMiner NewBlockEmpty webPact cutDb (genesisCut v)
            return cutDb

        destroy :: CutDb RocksDbTable -> IO ()
        destroy = stopCutDb

-- | Build a linear chainweb (no forks, assuming single threaded use of the
-- cutDb). No POW or poison delay is applied. Block times are real times.
mine :: (HasCallStack, CanReadablePayloadCas tbl)
    => Miner
        -- ^ The miner. For testing you may use 'defaultMiner' or 'noMiner'.
    -> NewBlockFill
    -> WebPactExecutionService
        -- ^ only the new-block generator is used. For testing you may use
        -- 'fakePact'.
    -> CutDb tbl
    -> Cut
    -> IO (Cut, ChainId, PayloadWithOutputs)
mine miner newBlockStrat pact cutDb c = do

    -- Pick a chain that isn't blocked. With that mining is guaranteed to
    -- succeed if
    --
    -- - there are no other writers to the cut db,
    -- - the chainweb is in a consistent state,
    -- - the pact execution service is synced with the cutdb, and
    -- - the transaction generator produces valid blocks.
    cid <- getRandomUnblockedChain c

    tryMineForChain miner newBlockStrat pact cutDb c cid >>= \case
        Left _ -> throwM $ InternalInvariantViolation
            "Failed to create new cut. This is a bug in Chainweb.Test.Pact5.CutFixture or one of its users"
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
    :: forall cid hdb. (HasChainId cid, ChainValueCasLookup hdb BlockHeader)
    => hdb
    -> Nonce
    -> Time Micros
    -> BlockPayloadHash
    -> cid
    -> Cut
    -> IO (Either MineFailure (T2 BlockHeader Cut))
testMineWithPayloadHash db n t ph cid c = try $ createNewCut (chainLookupM db) n t ph cid c

-- | Create a new block. Only produces a new cut but doesn't insert it into the
-- chain database.
--
-- The creation time isn't checked.
--
createNewCut
    :: (HasCallStack, MonadCatch m, MonadIO m, HasChainId cid)
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
        & S.map (view blockChainId)
        & S.head_
        & fmap fromJuste
    where
        isUnblocked h =
            let bh = view blockHeight h
                cid = view blockChainId h
            in all (>= bh) $ fmap (view blockHeight) $ toList $ cutAdjs c cid

-- | Build a linear chainweb (no forks). No POW or poison delay is applied.
-- Block times are real times.
--
tryMineForChain
    :: forall tbl. (HasCallStack, CanReadablePayloadCas tbl)
    => Miner
        -- ^ The miner. For testing you may use 'defaultMiner'.
    -> NewBlockFill
    -> WebPactExecutionService
    -> CutDb tbl
    -> Cut
    -> ChainId
    -> IO (Either MineFailure (Cut, ChainId, PayloadWithOutputs))
tryMineForChain miner newBlockStrat webPact cutDb c cid = do
    newBlock <- throwIfNoHistory =<< _webPactNewBlock webPact cid miner newBlockStrat parent
    let outputs = newBlockToPayloadWithOutputs newBlock
    let payloadHash = _payloadWithOutputsPayloadHash outputs
    t <- getCurrentTimeIntegral
    x <- testMineWithPayloadHash wdb (Nonce 0) t payloadHash cid c
    case x of
        Right (T2 h c') -> do
            addCutHashes cutDb (cutToCutHashes Nothing c')
                { _cutHashesHeaders = HashMap.singleton (view blockHash h) h
                , _cutHashesPayloads = HashMap.singleton (view blockPayloadHash h) (payloadWithOutputsToPayloadData outputs)
                }
            return $ Right (c', cid, outputs)
        Left e -> return $ Left e
    where
        parent = ParentHeader $ c ^?! ixg cid -- parent to mine on
        wdb = view cutDbWebBlockHeaderDb cutDb

data MineFailure
    = InvalidHeader Text
        -- ^ The header is invalid, e.g. because of a bad nonce or creation time.
    | MissingParentHeader
        -- ^ A parent header is missing in the chain db
    | BadAdjacents
        -- ^ This could mean that the chain is blocked.
    deriving stock (Show)
    deriving anyclass (Exception)