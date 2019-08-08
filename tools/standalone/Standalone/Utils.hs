{-# language LambdaCase #-}
{-# language TupleSections #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}

module Standalone.Utils where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM as STM
import Control.Error.Util (note)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Bifunctor (first)
import Data.CAS.RocksDB
import Data.Default
import Data.Foldable
import Data.Function
import Data.IVar
import Data.LogMessage
import Data.PQueue
import Data.Reflection
import Data.TaskMap
import Data.Tuple.Strict
import Data.Word
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM

import GHC.Stack

import Numeric.Natural

import qualified Network.HTTP.Client as HTTP

import P2P.TaskQueue

import qualified Streaming.Prelude as S

import System.Random

import Test.QuickCheck

-- pact imports
import Pact.Gas
import Pact.Types.ChainMeta
import Pact.Types.ChainId
import Pact.Types.Logger
import Pact.Types.Gas
import Pact.Types.SPV

-- chainweb imports
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Types
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Difficulty
import Chainweb.NodeId
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.SPV
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Sync.WebBlockHeaderStore.Types
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebBlockHeaderDB.Types
import Chainweb.WebPactExecutionService

withAll :: ChainwebVersion -> ([SQLiteEnv] -> IO c) -> IO c
withAll vv f = foldl' (\soFar _ -> with soFar) f (chainIds vv) []
  where
    with :: ([SQLiteEnv] -> IO c) -> [SQLiteEnv] -> IO c
    with g envs =  withTempSQLiteConnection chainwebPragmas $ \s -> g (s : envs)

-- | A test PactExecutionService for a chainweb
--
testWebPactExecutionService
    :: PayloadCas cas
    => ChainwebVersion
    -> Maybe (MVar (CutDb cas))
    -> IO WebBlockHeaderDb
    -> IO (PayloadDb cas)
    -> (Chainweb.Version.ChainId -> MemPoolAccess)
       -- ^ transaction generator
    -> [SQLiteEnv]
    -> IO WebPactExecutionService
testWebPactExecutionService v cutDB webdbIO pdbIO mempoolAccess sqlenvs
    = fmap mkWebPactExecutionService
    $ fmap HM.fromList
    $ traverse mkPact
    $ zip sqlenvs
    $ toList
    $ chainIds v
  where
    mkPact (sqlenv, c) = do
        webdb <- webdbIO
        let bhdbs = _webBlockHeaderDb webdb
        let bhdb = fromJuste $ HM.lookup c bhdbs
        let bhdbIO = return bhdb
        (c,) <$> testPactExecutionService v c cutDB bhdbIO pdbIO (mempoolAccess c) sqlenv

-- | A test PactExecutionService for a single chain
--
testPactExecutionService
    :: PayloadCas cas
    => ChainwebVersion
    -> Chainweb.Version.ChainId
    -> Maybe (MVar (CutDb cas))
    -> IO BlockHeaderDb
    -> IO (PayloadDb cas)
    -> MemPoolAccess
       -- ^ transaction generator
    -> SQLiteEnv
    -> IO PactExecutionService
testPactExecutionService v cid cutDB bhdbIO pdbIO mempoolAccess sqlenv = do
    bhdb <- bhdbIO
    pdb <- pdbIO
    ctx <- testPactCtxSQLite v cid cutDB bhdb pdb sqlenv
    return $ PactExecutionService
        { _pactNewBlock = \m p ->
            evalPactServiceM ctx $ execNewBlock mempoolAccess p m
        , _pactValidateBlock = \h d ->
            evalPactServiceM ctx $ execValidateBlock mempoolAccess h d
        , _pactLocal = error
            "Chainweb.Test.Pact.Utils.testPactExecutionService._pactLocal: not implemented"
        }

testPactCtxSQLite
  :: PayloadCas cas
  => ChainwebVersion
  -> Chainweb.Version.ChainId
  -> Maybe (MVar (CutDb cas))
  -> BlockHeaderDb
  -> PayloadDb cas
  -> SQLiteEnv
  -> IO (TestPactCtx cas)
testPactCtxSQLite v cid cdbv bhdb pdb sqlenv = do
    cpe <- initRelationalCheckpointer initBlockState sqlenv logger gasEnv
    ctx <- TestPactCtx
      <$> newMVar (PactServiceState Nothing)
      <*> pure (PactServiceEnv Nothing cpe spv pd pdb bhdb)
    evalPactServiceM ctx (initialPayloadState v cid mempty)
    return ctx
  where
    loggers = pactTestLogger False
    logger = newLogger loggers $ LogName ("PactService" ++ show cid)
    gasEnv = GasEnv 0 0 (constGasModel 0)
    spv = maybe noSPVSupport (\cdb -> pactSPV cdb logger) cdbv
    pd = def & pdPublicMeta . pmChainId .~ (ChainId $ chainIdToText cid)

data TestPactCtx cas = TestPactCtx
    { _testPactCtxState :: !(MVar PactServiceState)
    , _testPactCtxEnv :: !(PactServiceEnv cas)
    }

evalPactServiceM :: TestPactCtx cas -> PactServiceM cas a -> IO a
evalPactServiceM ctx pact = modifyMVar (_testPactCtxState ctx) $ \s -> do
    (a,s') <- runStateT (runReaderT pact (_testPactCtxEnv ctx)) s
    return (s',a)

pactTestLogger :: Bool -> Loggers
pactTestLogger showAll = initLoggers putStrLn f def
  where
    f _ b "ERROR" d = doLog error b "ERROR" d
    f _ b "DEBUG" d | not showAll = doLog (\_ -> return ()) b "DEBUG" d
    f _ b "INFO" d | not showAll = doLog (\_ -> return ()) b "INFO" d
    f _ b "DDL" d | not showAll = doLog (\_ -> return ()) b "DDL" d
    f a b c d = doLog a b c d

withTestCutDb
    :: forall a
    . HasCallStack
    => RocksDb
    -> ChainwebVersion
        -- ^ the chainweb version
    -> Int
        -- ^ number of blocks in the chainweb in addition to the
        -- genesis blocks. If Nothing, number of blocks is unbounded.
    -> (WebBlockHeaderDb -> PayloadDb RocksDbCas -> IO WebPactExecutionService)
        -- ^ a pact execution service.
        --
        -- When transaction don't matter you can use 'fakePact' from this module.
        --
        -- The function "testWebPactExecutionService" provides an pact execution
        -- service that can be given a transaction generator, that allows to
        -- create blocks with a well-defined set of test transactions.
        --
    -> LogFunction
    -> (CutDb RocksDbCas -> IO a)
        -- ^ a logg function (use @\_ _ -> return ()@ turn of logging)
    -> IO a
withTestCutDb rdb v n pactIO logfun f = do
    rocksDb <- testRocksDb "withTestCutDb" rdb
    let payloadDb = newPayloadDb rocksDb
        cutHashesDb = cutHashesTable rocksDb
    initializePayloadDb v payloadDb
    webDb <- initWebBlockHeaderDb rocksDb v
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    pact <- pactIO webDb payloadDb
    withLocalWebBlockHeaderStore mgr webDb $ \headerStore ->
        withLocalPayloadStore mgr payloadDb pact $ \payloadStore ->
            withCutDb (defaultCutDbConfig v) logfun headerStore payloadStore cutHashesDb $ \cutDb -> do
                foldM_ (\c _ -> view _1 <$> mine defaultMiner pact cutDb c) (genesisCut v) [0..n]
                f cutDb

testRocksDb
    :: B.ByteString
    -> RocksDb
    -> IO RocksDb
testRocksDb l = rocksDbNamespace (const prefix)
  where
    prefix = (<>) l . sshow <$> (randomIO @Word64)

withLocalWebBlockHeaderStore
    :: HTTP.Manager
    -> WebBlockHeaderDb
    -> (WebBlockHeaderStore -> IO a)
    -> IO a
withLocalWebBlockHeaderStore mgr webDb inner = withNoopQueueServer $ \queue -> do
    mem <- new
    inner $ WebBlockHeaderStore webDb mem queue (\_ _ -> return ()) mgr

withLocalPayloadStore
    :: HTTP.Manager
    -> PayloadDb cas
    -> WebPactExecutionService
    -> (WebBlockPayloadStore cas -> IO a)
    -> IO a
withLocalPayloadStore mgr payloadDb pact inner = withNoopQueueServer $ \queue -> do
    mem <- new
    inner $ WebBlockPayloadStore payloadDb mem queue (\_ _ -> return ()) mgr pact

-- | Build a linear chainweb (no forks, assuming single threaded use of the
-- cutDb). No POW or poison delay is applied. Block times are real times.
--
mine
    :: HasCallStack
    => PayloadCas cas
    => MinerInfo
        -- ^ The miner. For testing you may use 'defaultMiner'.
    -> WebPactExecutionService
        -- ^ only the new-block generator is used. For testing you may use
        -- 'fakePact'.
    -> CutDb cas
    -> Cut
    -> IO (Cut, Chainweb.Version.ChainId, PayloadWithOutputs)
mine miner pact cutDb c = do

    -- Pick a chain that isn't blocked. With that mining is guaranteed to
    -- succeed if
    --
    -- * there are no other writers to the cut db,
    -- * the chainweb is in a consistent state,
    -- * the pact execution service is synced with the cutdb, and
    -- * the transaction generator produces valid blocks.
    cid <- getRandomUnblockedChain c

    tryMineForChain miner pact cutDb c cid >>= \case
        Left _ -> throwM $ InternalInvariantViolation
            "Failed to create new cut. This is a bug in Test.Chainweb.CutDB or one of it's users"
        Right x -> do
            void $ awaitCut cutDb $ ((<=) `on` _cutHeight) (view _1 x)
            return x


withNoopQueueServer :: (PQueue (Task env a) -> IO b) -> IO b
withNoopQueueServer a = do
    q <- newEmptyPQueue
    let failTask = do
            task <- pQueueRemove q
            putIVar (_taskResult task) $ Left $ []
    withAsync (forever failTask) $ const $ a q


-- | Return a random chain id from a cut that is not blocked.
--
getRandomUnblockedChain :: Cut -> IO Chainweb.Version.ChainId
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
    :: forall cas
    . HasCallStack
    => PayloadCas cas
    => MinerInfo
        -- ^ The miner. For testing you may use 'defaultMiner'.
        -- miner.
    -> WebPactExecutionService
        -- ^ only the new-block generator is used. For testing you may use
        -- 'fakePact'.
    -> CutDb cas
    -> Cut
    -> Chainweb.Version.ChainId
    -> IO (Either MineFailure (Cut, Chainweb.Version.ChainId, PayloadWithOutputs))
tryMineForChain miner webPact cutDb c cid = do
    outputs <- _webPactNewBlock webPact miner parent
    let payloadHash = _payloadWithOutputsPayloadHash outputs
    t <- getCurrentTimeIntegral
    x <- testMineWithPayloadHash (Nonce 0) target t payloadHash (NodeId 0) cid c
    case x of
        Right (T2 h c') -> do
            validate h outputs
            addCutHashes cutDb (cutToCutHashes Nothing c')
            return $ Right (c', cid, outputs)
        Left e -> return $ Left e
  where
    parent = c ^?! ixg cid -- parent to mine on
    target = _blockTarget parent -- No difficulty adjustment

    payloadDb = view cutDbPayloadCas cutDb
    webDb = view cutDbWebBlockHeaderDb cutDb
    pact = _webPactExecutionService webPact

    validate h outputs = do
        let pd = payloadWithOutputsToPayloadData outputs
        void $ _pactValidateBlock pact h pd
        addNewPayload payloadDb outputs
        give webDb (insertWebBlockHeaderDb h)

data MineFailure = BadNonce | BadAdjacents


-- | Atomically await for a 'CutDb' instance to synchronize cuts according to some
-- predicate for a given 'Cut' and the results of '_cutStm'.
--
awaitCut
    :: CutDb cas
    -> (Cut -> Bool)
    -> IO Cut
awaitCut cdb k = atomically $ do
  c <- _cutStm cdb
  STM.check $ k c
  pure c

-- | Extend the cut db until either a cut that meets some condition is
-- encountered or the given number of cuts is mined. In the former case just the
-- cut that fullfills the condition is returned. In the latter case 'Nothing' is
-- returned.
--
-- Note that the this function may skip over some cuts when waiting for a cut that satisfies the predicate.
-- So, for instance, instead of checking for a particular cut height, one should
-- check for a cut height that is larger or equal than the expected height.
--
extendAwait
    :: PayloadCas cas
    => CutDb cas
    -> WebPactExecutionService
    -> Natural
    -> (Cut -> Bool)
    -> IO (Maybe Cut)
extendAwait cdb pact i p = race gen (awaitCut cdb p) >>= \case
    Left _ -> return Nothing
    Right c -> return (Just c)
  where
    gen = void
        $ S.foldM_ checkCut (return 0) return
        $ S.map (view (_1 . cutHeight))
        $ extendTestCutDb cdb pact i

    checkCut prev cur = do
        unless (prev < cur) $ throwM $ InternalInvariantViolation $ unexpectedMsg
            "New cut is not larger than the previous one. This is bug in Chainweb.Test.CutDB"
            (Expected prev)
            (Actual cur)
        return cur

testMineWithPayloadHash
    :: forall cid
    . HasChainId cid
    => Nonce
    -> HashTarget
    -> Time Micros
    -> BlockPayloadHash
    -> NodeId
    -> cid
    -> Cut
    -> IO (Either MineFailure (T2 BlockHeader Cut))
testMineWithPayloadHash n target t payloadHash nid i c =
    forM (createNewCut n target t payloadHash nid i c) return


-- | Adds the requested number of new blocks to the given 'CutDb'.
--
-- It is assumed that the 'WebPactExecutionService' is synced with the 'CutDb'.
-- This can be done by calling 'syncPact'. The 'WebPactExecutionService' that
-- was used to generate the given CutDb is already synced.
--
-- If the 'WebPactExecutionService' is not synced with the 'CutDb', this
-- function will result in an exception @PactInternalError
-- "InMemoryCheckpointer: Restore not found"@.
--
extendTestCutDb
    :: PayloadCas cas
    => CutDb cas
    -> WebPactExecutionService
    -> Natural
    -> S.Stream (S.Of (Cut, Chainweb.Version.ChainId, PayloadWithOutputs)) IO ()
extendTestCutDb cutDb pact n = S.scanM
    (\(c, _, _) _ -> mine defaultMiner pact cutDb c)
    (mine defaultMiner pact cutDb =<< _cut cutDb)
    return
    (S.each [0..n-1])



-- | Create a new block. Only produces a new cut but doesn't insert it into the
-- chain database.
--
createNewCut
    :: HasCallStack
    => HasChainId cid
    => Nonce
    -> HashTarget
    -> Time Micros
    -> BlockPayloadHash
    -> NodeId
    -> cid
    -> Cut
    -> Either MineFailure (T2 BlockHeader Cut)
createNewCut n target t pay nid i c = do
    h <- note BadAdjacents $ newHeader . BlockHashRecord <$> newAdjHashes
    unless (checkTarget target $ _blockPow h) $ Left BadNonce
    c' <- first (\e -> error $ "Chainweb.Cut.createNewCut: " <> sshow e)
        $ monotonicCutExtension c h
    return $ T2 h c'
  where
    cid = Chainweb.Version._chainId i

    -- | The parent block to mine on.
    --
    p :: BlockHeader
    p = c ^?! ixg cid

    newHeader :: BlockHashRecord -> BlockHeader
    newHeader as = newBlockHeader (nodeIdFromNodeId nid cid) as pay n target t p

    -- | Try to get all adjacent hashes dependencies.
    --
    newAdjHashes :: Maybe (HM.HashMap Chainweb.Version.ChainId BlockHash)
    newAdjHashes = iforM (_getBlockHashRecord $ _blockAdjacentHashes p) $ \xcid _ ->
        c ^?! ixg xcid . to (tryAdj (_blockHeight p))

    tryAdj :: BlockHeight -> BlockHeader -> Maybe BlockHash
    tryAdj h b
        | _blockHeight b == h = Just $! _blockHash b
        | _blockHeight b == h + 1 = Just $! _blockParent b
        | otherwise = Nothing
