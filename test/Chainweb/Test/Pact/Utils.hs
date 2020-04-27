{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
-- |
-- Module: Chainweb.Test.Pact.Utils
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution via (inprocess) API  in Chainweb
module Chainweb.Test.Pact.Utils
( -- * Test key data
  SimpleKeyPair
, sender00
, sender01
, testKeyPairs
, mkKeySetData
-- * 'PactValue' helpers
, pInteger
, pString
, pDecimal
, pBool
-- * Capability helpers
, mkCapability
, mkTransferCap
, gasCap
, mkCoinCap
-- * Command builder
, defaultCmd
, mkCmd
, buildCmd
, buildCwCmd
, mkExec'
, mkExec
, mkCont
, mkContMsg
, ContMsg (..)
, mkSigner
, mkSigner'
, CmdBuilder
, cbSigners
, cbRPC
, cbNonce
, cbNetworkId
, cbChainId
, cbSender
, cbGasLimit
, cbGasPrice
, cbTTL
, cbCreationTime
, CmdSigner
, csSigner
, csPrivKey
-- * Pact Service creation
, withPactTestBlockDb
, withWebPactExecutionService
, withPactCtxSQLite
, WithPactCtxSQLite
-- * Other service creation
, initializeSQLite
, freeSQLiteResource
, withTestBlockDbTest
, defaultPactServiceConfig
, withMVarResource
, withTime
, withPayloadDb
, withBlockHeaderDb
, withTemporaryDir
-- * Mempool utils
, delegateMemPoolAccess
, withDelegateMempool
, setMempool
-- * Block formation
, runCut
, Noncer
, zeroNoncer
-- * miscellaneous
, toTxCreationTime
, dummyLogger
, pactTestLogger
, epochCreationTime
, someTestVersionHeader
, someBlockHeader
, testPactFilesDir

) where

import Control.Arrow ((&&&))
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens (view, _3, makeLenses)
import Control.Monad
import Control.Monad.Catch

import Data.Aeson (Value(..), object, (.=))
import Data.CAS.HashMap hiding (toList)
import Data.CAS.RocksDB
import Data.Decimal
import Data.Default (def)
import Data.Foldable
import Data.IORef
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Tuple.Strict
import Data.String

import GHC.Generics

import System.Directory
import System.IO.Extra
import System.LogLevel

import Test.Tasty

-- internal pact modules

import Pact.ApiReq (ApiKeyPair(..), mkKeyPairs)
import Pact.Gas
import Pact.Types.Capability
import qualified Pact.Types.ChainId as P
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Exp
import Pact.Types.Gas
import Pact.Types.Logger
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.RPC
import Pact.Types.Runtime (PactId)
import Pact.Types.SPV
import Pact.Types.SQLite
import Pact.Types.Util (parseB16TextOnly)

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.RelationalCheckpointer
    (initRelationalCheckpointer')
import Chainweb.Pact.Backend.SQLite.DirectV2
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..), chainIds, someChainId)
import qualified Chainweb.Version as Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

-- ----------------------------------------------------------------------- --
-- Keys

type SimpleKeyPair = (Text,Text)

-- | Legacy; better to use 'CmdSigner'/'CmdBuilder'.
testKeyPairs :: SimpleKeyPair -> Maybe [SigCapability] -> IO [SomeKeyPairCaps]
testKeyPairs skp capsm = do
  kp <- toApiKp $ mkSigner' skp (fromMaybe [] capsm)
  mkKeyPairs [kp]

testPactFilesDir :: FilePath
testPactFilesDir = "test/pact/"

sender00 :: SimpleKeyPair
sender00 = ("368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
           ,"251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898")

sender01 :: SimpleKeyPair
sender01 = ("6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"
           ,"2beae45b29e850e6b1882ae245b0bab7d0689ebdd0cd777d4314d24d7024b4f7")


-- | Make trivial keyset data
mkKeySetData :: Text  -> [SimpleKeyPair] -> Value
mkKeySetData name keys = object [ name .= map fst keys ]


-- ----------------------------------------------------------------------- --
-- PactValue helpers

-- | Make PactValue from 'Integral'
pInteger :: Integer -> PactValue
pInteger = PLiteral . LInteger

-- | Make PactValue from text
pString :: Text -> PactValue
pString = PLiteral . LString

-- | Make PactValue from decimal
pDecimal :: Decimal -> PactValue
pDecimal = PLiteral . LDecimal

-- | Make PactValue from boolean
pBool :: Bool -> PactValue
pBool = PLiteral . LBool


-- ----------------------------------------------------------------------- --
-- Capability helpers

-- | Cap smart constructor.
mkCapability :: ModuleName -> Text -> [PactValue] -> SigCapability
mkCapability mn cap args = SigCapability (QualifiedName mn cap def) args

-- | Convenience to make caps like TRANSFER, GAS etc.
mkCoinCap :: Text -> [PactValue] -> SigCapability
mkCoinCap n = mkCapability "coin" n

mkTransferCap :: Text -> Text -> Decimal -> SigCapability
mkTransferCap sender receiver amount = mkCoinCap "TRANSFER"
  [ pString sender, pString receiver, pDecimal amount ]

gasCap :: SigCapability
gasCap = mkCoinCap "GAS" []



-- ----------------------------------------------------------------------- --
-- CmdBuilder and friends


-- | Pair a 'Signer' with private key.
data CmdSigner = CmdSigner
  { _csSigner :: !Signer
  , _csPrivKey :: !Text
  } deriving (Eq,Show,Ord,Generic)

-- | Make ED25519 signer.
mkSigner :: Text -> Text -> [SigCapability] -> CmdSigner
mkSigner pubKey privKey caps = CmdSigner
  { _csSigner = signer
  , _csPrivKey = privKey }
  where
    signer = Signer
      { _siScheme = Nothing
      , _siPubKey = pubKey
      , _siAddress = Nothing
      , _siCapList = caps }

mkSigner' :: SimpleKeyPair -> [SigCapability] -> CmdSigner
mkSigner' (pub,priv) = mkSigner pub priv

-- | Chainweb-oriented command builder.
data CmdBuilder = CmdBuilder
  { _cbSigners :: ![CmdSigner]
  , _cbRPC :: !(PactRPC Text)
  , _cbNonce :: !Text
  , _cbNetworkId :: !(Maybe ChainwebVersion)
  , _cbChainId :: !ChainId
  , _cbSender :: !Text
  , _cbGasLimit :: !GasLimit
  , _cbGasPrice :: !GasPrice
  , _cbTTL :: !TTLSeconds
  , _cbCreationTime :: !TxCreationTime
  } deriving (Eq,Show,Generic)

-- | Make code-only Exec PactRPC
mkExec' :: Text -> PactRPC Text
mkExec' ecode = mkExec ecode Null

-- | Make Exec PactRPC
mkExec :: Text -> Value -> PactRPC Text
mkExec ecode edata = Exec $ ExecMsg ecode edata

mkCont :: ContMsg -> PactRPC Text
mkCont = Continuation

mkContMsg :: PactId -> Int -> ContMsg
mkContMsg pid step = ContMsg
  { _cmPactId = pid
  , _cmStep = step
  , _cmRollback = False
  , _cmData = Null
  , _cmProof = Nothing }

-- | Default builder.
defaultCmd :: CmdBuilder
defaultCmd = CmdBuilder
  { _cbSigners = []
  , _cbRPC = mkExec' "1"
  , _cbNonce = "nonce"
  , _cbNetworkId = Nothing
  , _cbChainId = unsafeChainId 0
  , _cbSender = "sender00"
  , _cbGasLimit = 10_000
  , _cbGasPrice = 0.000_1
  , _cbTTL = 300 -- 5 minutes
  , _cbCreationTime = 0 -- epoch
  }

-- | Default builder with nonce and RPC
mkCmd :: Text -> PactRPC Text -> CmdBuilder
mkCmd nonce rpc = defaultCmd
  { _cbRPC = rpc
  , _cbNonce = nonce
  }

-- | Main builder command.
buildCmd :: CmdBuilder -> IO (Command (Payload PublicMeta ParsedCode))
buildCmd CmdBuilder{..} = do
  akps <- mapM toApiKp _cbSigners
  kps <- mkKeyPairs akps
  cmd <- mkCommand kps pm _cbNonce nid _cbRPC
  case verifyCommand cmd of
    ProcSucc r -> return r
    ProcFail e -> throwM $ userError $ "buildCmd failed: " ++ e
  where
    nid = fmap (P.NetworkId . sshow) _cbNetworkId
    cid = fromString $ show (chainIdInt _cbChainId :: Int)
    pm = PublicMeta cid _cbSender _cbGasLimit _cbGasPrice _cbTTL _cbCreationTime

dieL :: MonadThrow m => [Char] -> Either [Char] a -> m a
dieL msg = either (\s -> throwM $ userError $ msg ++ ": " ++ s) return

toApiKp :: MonadThrow m => CmdSigner -> m ApiKeyPair
toApiKp (CmdSigner Signer{..} privKey) = do
  sk <- dieL "private key" $ parseB16TextOnly privKey
  pk <- dieL "public key" $ parseB16TextOnly _siPubKey
  return $!
    ApiKeyPair (PrivBS sk) (Just (PubBS pk)) _siAddress _siScheme (Just _siCapList)

-- | 'buildCmd' variant for 'ChainwebTransaction'
buildCwCmd :: CmdBuilder -> IO ChainwebTransaction
buildCwCmd = fmap (fmap mkPayloadWithText) . buildCmd


-- ----------------------------------------------------------------------- --
-- Service creation utilities


pactTestLogger :: Bool -> Loggers
pactTestLogger showAll = initLoggers putStrLn f def
  where
    f _ b "ERROR" d = doLog (\_ -> return ()) b "ERROR" d
    f _ b "DEBUG" d | not showAll = doLog (\_ -> return ()) b "DEBUG" d
    f _ b "INFO" d | not showAll = doLog (\_ -> return ()) b "INFO" d
    f _ b "DDL" d | not showAll = doLog (\_ -> return ()) b "DDL" d
    f a b c d = doLog a b c d

-- | Test Pact Execution Context for running inside 'PactServiceM'.
-- Only used internally.
data TestPactCtx cas = TestPactCtx
    { _testPactCtxState :: !(MVar PactServiceState)
    , _testPactCtxEnv :: !(PactServiceEnv cas)
    }


evalPactServiceM_ :: TestPactCtx cas -> PactServiceM cas a -> IO a
evalPactServiceM_ ctx pact = modifyMVar (_testPactCtxState ctx) $ \s -> do
    T2 a s' <- runPactServiceM s (_testPactCtxEnv ctx) pact
    return (s',a)

destroyTestPactCtx :: TestPactCtx cas -> IO ()
destroyTestPactCtx = void . takeMVar . _testPactCtxState

-- | setup TestPactCtx, internal function.
-- Use 'withPactCtxSQLite' in tests.
testPactCtxSQLite
  :: PayloadCasLookup cas
  => ChainwebVersion
  -> Version.ChainId
  -> BlockHeaderDb
  -> PayloadDb cas
  -> SQLiteEnv
  -> PactServiceConfig
  -> IO (TestPactCtx cas,PactDbEnv')
testPactCtxSQLite v cid bhdb pdb sqlenv config = do
    (dbSt,cpe) <- initRelationalCheckpointer' initBlockState sqlenv logger v
    let rs = readRewards v
        t0 = BlockCreationTime $ Time (TimeSpan (Micros 0))
    !ctx <- TestPactCtx
      <$!> newMVar (PactServiceState Nothing mempty 0 t0 Nothing noSPVSupport)
      <*> pure (pactServiceEnv cpe rs)
    evalPactServiceM_ ctx (initialPayloadState dummyLogger v cid)
    return (ctx,dbSt)
  where
    loggers = pactTestLogger False -- toggle verbose pact test logging
    logger = newLogger loggers $ LogName ("PactService" ++ show cid)
    pactServiceEnv cpe rs = PactServiceEnv
        { _psMempoolAccess = Nothing
        , _psCheckpointEnv = cpe
        , _psPdb = pdb
        , _psBlockHeaderDb = bhdb
        , _psGasModel = constGasModel 0
        , _psMinerRewards = rs
        , _psReorgLimit = fromIntegral $ _pactReorgLimit config
        , _psOnFatalError = defaultOnFatalError mempty
        , _psVersion = v
        , _psValidateHashesOnReplay = _pactRevalidate config
        , _psAllowReadsInLocal = _pactAllowReadsInLocal config
        }


-- | A test PactExecutionService for a chainweb (ie pact services
-- for all chainweb chains).
withWebPactExecutionService
    :: ChainwebVersion
    -> TestBlockDb
    -> MemPoolAccess
    -> (WebPactExecutionService -> IO a)
    -> IO a
withWebPactExecutionService v bdb mempoolAccess act =
  withDbs $ \sqlenvs -> do
    pacts <- fmap (mkWebPactExecutionService . HM.fromList)
           $ traverse mkPact
           $ zip sqlenvs
           $ toList
           $ chainIds v
    act pacts
  where
    withDbs f = foldl' (\soFar _ -> withDb soFar) f (chainIds v) []
    withDb g envs =  withTempSQLiteConnection chainwebPragmas $ \s -> g (s : envs)
    mkPact (sqlenv, c) = do
        bhdb <- getBlockHeaderDb c bdb
        (ctx,_) <- testPactCtxSQLite v c bhdb (_bdbPayloadDb bdb) sqlenv defaultPactServiceConfig
        return $ (c,) $ PactExecutionService
          { _pactNewBlock = \m p -> evalPactServiceM_ ctx $ execNewBlock mempoolAccess p m
          , _pactValidateBlock = \h d ->
              evalPactServiceM_ ctx $ execValidateBlock h d
          , _pactLocal = error
              "Chainweb.Test.Pact.Utils.testPactExecutionService._pactLocal: not implemented"
          , _pactLookup = error
              "Chainweb.Test.Pact.Utils.testPactExecutionService._pactLookup: not implemented"
          , _pactPreInsertCheck = error
              "Chainweb.Test.Pact.Utils.testPactExecutionService._pactPreInsertCheck: not implemented"
          }


-- | Noncer for 'runCut'
type Noncer = ChainId -> IO Nonce

zeroNoncer :: Noncer
zeroNoncer = const (return $ Nonce 0)

-- | Populate blocks for every chain of the current cut. Uses provided pact
-- service to produce a new block, add it to dbs, etc.
runCut :: ChainwebVersion -> TestBlockDb -> WebPactExecutionService -> GenBlockTime -> Noncer -> IO ()
runCut v bdb pact genTime noncer =
  forM_ (chainIds v) $ \cid -> do
    ph <- ParentHeader <$> getParentTestBlockDb bdb cid
    pout <- _webPactNewBlock pact noMiner ph
    n <- noncer cid
    addTestBlockDb bdb n genTime cid pout
    h <- getParentTestBlockDb bdb cid
    void $ _webPactValidateBlock pact h (payloadWithOutputsToPayloadData pout)

initializeSQLite :: IO (IO (), SQLiteEnv)
initializeSQLite = do
      (file, del) <- newTempFile
      e <- open2 file
      case e of
        Left (_err, _msg) ->
          internalError "initializeSQLite: A connection could not be opened."
        Right r ->  return (del, SQLiteEnv r (SQLiteConfig file chainwebPragmas))

freeSQLiteResource :: (IO (), SQLiteEnv) -> IO ()
freeSQLiteResource (del,sqlenv) = do
  void $ close_v2 $ _sConn sqlenv
  del

-- | Run in 'PactServiceM' with direct db access.
type WithPactCtxSQLite cas = forall a . (PactDbEnv' -> PactServiceM cas a) -> IO a

-- | Used to run 'PactServiceM' functions directly on a database (ie not use checkpointer).
withPactCtxSQLite
  :: PayloadCasLookup cas
  => ChainwebVersion
  -> IO BlockHeaderDb
  -> IO (PayloadDb cas)
  -> PactServiceConfig
  -> (WithPactCtxSQLite cas -> TestTree)
  -> TestTree
withPactCtxSQLite v bhdbIO pdbIO config f =
  withResource
    initializeSQLite
    freeSQLiteResource $ \io ->
      withResource (start io) (destroy io) $ \ctxIO -> f $ \toPact -> do
          (ctx, dbSt) <- ctxIO
          evalPactServiceM_ ctx (toPact dbSt)
  where
    destroy = const (destroyTestPactCtx . fst)
    start ios = do
        let cid = someChainId v
        bhdb <- bhdbIO
        pdb <- pdbIO
        (_,s) <- ios
        testPactCtxSQLite v cid bhdb pdb s config

withMVarResource :: a -> (IO (MVar a) -> TestTree) -> TestTree
withMVarResource value = withResource (newMVar value) mempty

withTime :: (IO (Time Micros) -> TestTree) -> TestTree
withTime = withResource getCurrentTimeIntegral mempty

toTxCreationTime :: Integral a => Time a -> TxCreationTime
toTxCreationTime (Time timespan) = TxCreationTime $ fromIntegral $ timeSpanToSeconds timespan

withPayloadDb :: (IO (PayloadDb HashMapCas) -> TestTree) -> TestTree
withPayloadDb = withResource newPayloadDb mempty


-- | 'MemPoolAccess' that delegates all calls to the contents of provided `IORef`.
delegateMemPoolAccess :: IORef MemPoolAccess -> MemPoolAccess
delegateMemPoolAccess r = MemPoolAccess
  { mpaGetBlock = \a b c d -> call mpaGetBlock $ \f -> f a b c d
  , mpaSetLastHeader = \a -> call mpaSetLastHeader ($ a)
  , mpaProcessFork = \a -> call mpaProcessFork ($ a)
  , mpaBadlistTx = \a -> call mpaBadlistTx ($ a)
  }
  where
    call :: (MemPoolAccess -> f) -> (f -> IO a) -> IO a
    call f g = readIORef r >>= g . f

-- | use a "delegate" which you can dynamically reset/modify.
-- Returns the updateable 'IORef MemPoolAccess` for use
-- in tests, and the plain 'MemPoolAccess' for initializing
-- PactService etc.
withDelegateMempool
  :: (IO (IORef MemPoolAccess, MemPoolAccess) -> TestTree)
  -> TestTree
withDelegateMempool = withResource start mempty
  where
    start = (id &&& delegateMemPoolAccess) <$> newIORef mempty

-- | Set test mempool using IORef.
setMempool :: IO (IORef MemPoolAccess) -> MemPoolAccess -> IO ()
setMempool refIO mp = refIO >>= flip writeIORef mp

withBlockHeaderDb
    :: IO RocksDb
    -> BlockHeader
    -> (IO BlockHeaderDb -> TestTree)
    -> TestTree
withBlockHeaderDb iordb b = withResource start stop
  where
    start = do
        rdb <- testRocksDb "withBlockHeaderDb" =<< iordb
        testBlockHeaderDb rdb b
    stop = closeBlockHeaderDb

withTemporaryDir :: (IO FilePath -> TestTree) -> TestTree
withTemporaryDir = withResource (fst <$> newTempDir) removeDirectoryRecursive

withTestBlockDbTest
  :: ChainwebVersion -> (IO TestBlockDb -> TestTree) -> TestTree
withTestBlockDbTest v a =
  withRocksResource $ \rdb ->
  withResource (start rdb) mempty a
  where
    start r = r >>= mkTestBlockDb v

-- | Single-chain Pact via service queue.
withPactTestBlockDb
    :: ChainwebVersion
    -> ChainId
    -> LogLevel
    -> (IO MemPoolAccess)
    -> PactServiceConfig
    -> (IO (PactQueue,TestBlockDb) -> TestTree)
    -> TestTree
withPactTestBlockDb version cid logLevel mempoolIO pactConfig f =
  withTemporaryDir $ \iodir ->
  withTestBlockDbTest version $ \bdbio ->
  withResource (startPact bdbio iodir) stopPact $ f . fmap (view _3)
  where
    startPact bdbio iodir = do
        reqQ <- atomically $ newTBQueue 2000
        dir <- iodir
        bdb <- bdbio
        mempool <- mempoolIO
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb bdb) cid
        let pdb = _bdbPayloadDb bdb
        sqlEnv <- startSqliteDb version cid logger (Just dir) Nothing False
        a <- async $
             initPactService version cid logger reqQ mempool bhdb pdb sqlEnv pactConfig
        return (a, sqlEnv, (reqQ,bdb))

    stopPact (a, sqlEnv, _) = cancel a >> stopSqliteDb sqlEnv

    logger = genericLogger logLevel T.putStrLn

dummyLogger :: GenericLogger
dummyLogger = genericLogger Quiet T.putStrLn

someTestVersion :: ChainwebVersion
someTestVersion = FastTimedCPM peterson

someTestVersionHeader :: BlockHeader
someTestVersionHeader = someBlockHeader someTestVersion 10

epochCreationTime :: BlockCreationTime
epochCreationTime = BlockCreationTime epoch

-- | The runtime is linear in the requested height. This can be slow if a large
-- block height is requested for a chainweb version that simulates realtime
-- mining. It is fast enough for testing purposes with "fast" mining chainweb
-- versions like 'someTestVersion' for block heights up to, say, 1000.
--
someBlockHeader :: ChainwebVersion -> BlockHeight -> BlockHeader
someBlockHeader v 0 = genesisBlockHeader v (unsafeChainId 0)
someBlockHeader v h = (!! (int h - 1))
    $ testBlockHeaders
    $ ParentHeader
    $ genesisBlockHeader v (unsafeChainId 0)

makeLenses ''CmdBuilder
makeLenses ''CmdSigner
