{-# language
    BangPatterns
    , ConstraintKinds
    , DataKinds
    , DeriveAnyClass
    , DerivingStrategies
    , FlexibleContexts
    , FlexibleInstances
    , ImplicitParams
    , ImportQualifiedPost
    , LambdaCase
    , MultiParamTypeClasses
    , NumericUnderscores
    , OverloadedStrings
    , PackageImports
    , RankNTypes
    , RecordWildCards
    , ScopedTypeVariables
    , TemplateHaskell
    , TupleSections
    , TypeApplications
    , ViewPatterns
#-}

-- | A fixture which provides access to the internals of a running node, with
-- multiple chains. Usually, you initialize it with `mkFixture`, insert
-- transactions into the mempool as desired, and use `advanceAllChains` to
-- trigger mining on all chains at once.
module Chainweb.Test.Pact5.CutFixture
    ( Fixture(..)
    , HasFixture(..)
    , mkFixture
    , fixtureCutDb
    , fixturePayloadDb
    , fixtureWebBlockHeaderDb
    , fixtureWebPactExecutionService
    , fixtureMempools
    , fixturePactQueues
    , advanceAllChains
    , advanceAllChains_
    , advanceToForkHeight
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
import Chainweb.Storage.Table (Casify)
import Chainweb.Storage.Table.RocksDB
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Test.Pact5.Utils
import Chainweb.Test.Utils
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
import Data.Function
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import GHC.Stack
import Network.HTTP.Client qualified as HTTP

data Fixture = Fixture
    { _fixtureCutDb :: CutDb RocksDbTable
    , _fixturePayloadDb :: PayloadDb RocksDbTable
    , _fixtureWebBlockHeaderDb :: WebBlockHeaderDb
    , _fixtureWebPactExecutionService :: WebPactExecutionService
    , _fixtureMempools :: ChainMap (MempoolBackend Pact4.UnparsedTransaction)
    , _fixturePactQueues :: ChainMap PactQueue
    }
makeLenses ''Fixture

class HasFixture a where
    cutFixture :: a -> IO Fixture
instance HasFixture Fixture where
    cutFixture = return
instance HasFixture a => HasFixture (IO a) where
    cutFixture = (>>= cutFixture)

mkFixture :: ChainwebVersion -> PactServiceConfig -> RocksDb -> ResourceT IO Fixture
mkFixture v pactServiceConfig baseRdb = do
    logger <- liftIO getTestLogger
    testRdb <- liftIO $ testRocksDb "withBlockDbs" baseRdb
    (payloadDb, webBHDb) <- withBlockDbs v testRdb
    perChain <- iforM (HashSet.toMap (chainIds v)) $ \chain () -> do
        pactQueue <- liftIO $ newPactQueue 2_000
        mempool <- withMempool v chain pactQueue
        withRunPactService logger v chain pactQueue mempool webBHDb payloadDb pactServiceConfig
        return (mempool, pactQueue)
    let webPact = mkWebPactExecutionService $ HashMap.map (mkPactExecutionService . snd) perChain
    let cutHashesStore = cutHashesTable testRdb
    cutDb <- withTestCutDb logger v webBHDb payloadDb cutHashesStore webPact

    let fixture = Fixture
            { _fixtureCutDb = cutDb
            , _fixturePayloadDb = payloadDb
            , _fixtureWebBlockHeaderDb = webBHDb
            , _fixtureWebPactExecutionService = webPact
            , _fixtureMempools = OnChains $ fst <$> perChain
            , _fixturePactQueues = OnChains $ snd <$> perChain
            }
    -- we create the first block to avoid rejecting txs based on genesis
    -- block creation time being from the past
    _ <- liftIO $ advanceAllChains fixture
    return fixture

-- | Advance all chains by one block, filling that block with whatever is in
-- their mempools at the time.
--
advanceAllChains
    :: (HasCallStack, HasFixture a)
    => a
    -> IO (Cut, ChainMap (Vector TestPact5CommandResult))
advanceAllChains fx = do
    Fixture{..} <- cutFixture fx
    let v = _chainwebVersion _fixtureCutDb
    latestCut <- liftIO $ _fixtureCutDb ^. cut
    let blockHeights = fmap (view blockHeight) $ latestCut ^. cutMap
    let latestBlockHeight = maximum blockHeights

    -- TODO: rejig this to do parallel mining.
    (finalCut, perChainCommandResults) <- foldM
        (\ (prevCut, !acc) cid -> do
            (newCut, _minedChain, pwo) <-
                mine cid noMiner NewBlockFill _fixtureWebPactExecutionService _fixtureCutDb prevCut
            commandResults <- forM (_payloadWithOutputsTransactions pwo) $ \(_, txOut) -> do
                decodeOrThrow' $ LBS.fromStrict $ _transactionOutputBytes txOut

            addNewPayload _fixturePayloadDb latestBlockHeight pwo

            return $ (newCut, (cid, commandResults) : acc)
        )
        (latestCut, [])
        (HashSet.toList (chainIdsAt v (latestBlockHeight + 1)))

    return (finalCut, onChains perChainCommandResults)

advanceAllChains_
    :: (HasCallStack, HasFixture a)
    => a
    -> IO ()
advanceAllChains_ = void . advanceAllChains

-- Advance to the forkheight of a fork.
--
-- Throws an 'error' if the fork is not found in the ChainwebVersion '_versionForks', or if
-- the fork height is ever 'ForkNever' or 'ForkGenesis' on all chains.
--
-- Does nothing if you are already at or past the the forkheight.
advanceToForkHeight :: (HasCallStack, HasFixture fx) => fx -> Fork -> IO ()
advanceToForkHeight fx fork = do
    Fixture{..} <- cutFixture fx
    let v = _chainwebVersion _fixtureCutDb
    latestCut <- liftIO $ _fixtureCutDb ^. cut
    let latestBlockHeight = latestCut ^. cutMaxHeight

    let targetHeight = fromMaybe (error "advanceToForkHeight: no fork found") $
            maximumOf (versionForks . ix fork . folded . _ForkAtBlockHeight) v

    when (targetHeight > latestBlockHeight) $ do
        replicateM_ (int (targetHeight - latestBlockHeight)) $ advanceAllChains_ fx

withTestCutDb :: (Logger logger)
    => logger
    -> ChainwebVersion
    -> WebBlockHeaderDb
    -> PayloadDb RocksDbTable
    -> Casify RocksDbTable CutHashes
    -> WebPactExecutionService
    -> ResourceT IO (CutDb RocksDbTable)
withTestCutDb logger v webBHDb payloadDb cutHashesStore webPact = snd <$> allocate create destroy
    where
        create :: IO (CutDb RocksDbTable)
        create = do
            initializePayloadDb v payloadDb
            httpManager <- HTTP.newManager HTTP.defaultManagerSettings

            headerStore <- newWebBlockHeaderStore httpManager webBHDb (logFunction logger)
            payloadStore <- newWebPayloadStore httpManager webPact payloadDb (logFunction logger)

            let cutFetchTimeout = 3_000_000
            cutDb <- startCutDb (defaultCutDbParams v cutFetchTimeout) (logFunction logger) headerStore payloadStore cutHashesStore
            return cutDb

        destroy :: CutDb RocksDbTable -> IO ()
        destroy = stopCutDb

-- | Build a linear chainweb (no forks, assuming single threaded use of the
-- cutDb). No POW or poison delay is applied. Block times are real times.
mine
    :: (HasCallStack, CanReadablePayloadCas tbl)
    => ChainId
    -> Miner
        -- ^ The miner. For testing you may use 'defaultMiner' or 'noMiner'.
    -> NewBlockFill
    -> WebPactExecutionService
        -- ^ only the new-block generator is used. For testing you may use
        -- 'fakePact'.
    -> CutDb tbl
    -> Cut
    -> IO (Cut, ChainId, PayloadWithOutputs)
mine cid miner newBlockStrat pact cutDb c = do
    tryMineForChain miner newBlockStrat pact cutDb c cid >>= \case
        Left _ -> throwM $ InternalInvariantViolation
            $ "Failed to create new cut on chain " <> toText cid <> "."
            <> "This is a bug in Chainweb.Test.Pact5.CutFixture or one of its users; check that this chain's adjacent chains aren't too far behind."
            <> "\nCut: \n"
            <> Text.unlines (cutToTextShort c)
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
