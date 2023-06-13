{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}


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
, sender00Ks
, allocation00KeyPair
, testKeyPairs
, mkKeySetData
-- * 'PactValue' helpers
, pInteger
, pString
, pDecimal
, pBool
, pList
, pKeySet
, pObject
-- * event helpers
, mkEvent
, mkTransferEvent
, mkTransferXChainEvent
, mkTransferXChainRecdEvent
, mkXYieldEvent
, mkXResumeEvent
-- * Capability helpers
, mkCapability
, mkTransferCap
, mkGasCap
, mkCoinCap
, mkXChainTransferCap
-- * Command builder
, defaultCmd
, mkCmd
, buildRawCmd
, buildCwCmd
, buildTextCmd
, mkExec'
, mkExec
, mkCont
, mkContMsg
, ContMsg (..)
, mkSigner
, mkSigner'
, CmdBuilder(..)
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
, freeGasModel
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
, setOneShotMempool
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
import Control.Lens (view, _3, makeLenses)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Aeson (Value(..), object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as BS
import Data.Decimal
import Data.Default (def)
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.String
import qualified Data.Vector as V

import GHC.Generics

import System.Directory
import System.IO.Temp (createTempDirectory)
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
import Pact.Types.Hash
import Pact.Types.Logger
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.RPC
import Pact.Types.Runtime (PactEvent(..))
import Pact.Types.SPV
import Pact.Types.Term
import Pact.Types.SQLite
import Pact.Types.Util (parseB16TextOnly)

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.BlockHeight
import Chainweb.ChainId
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
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Utils
import Chainweb.Test.Utils.BlockHeader
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..), chainIds)
import qualified Chainweb.Version as Version
import Chainweb.Version.Utils (someChainId)
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Chainweb.Storage.Table.HashMap hiding (toList)
import Chainweb.Storage.Table.RocksDB

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


allocation00KeyPair :: SimpleKeyPair
allocation00KeyPair =
    ( "d82d0dcde9825505d86afb6dcc10411d6b67a429a79e21bda4bb119bf28ab871"
    , "c63cd081b64ae9a7f8296f11c34ae08ba8e1f8c84df6209e5dee44fa04bcb9f5"
    )


-- | Make trivial keyset data
mkKeySetData :: Text  -> [SimpleKeyPair] -> Value
mkKeySetData name keys = object [ name .= map fst keys ]

sender00Ks :: KeySet
sender00Ks = mkKeySet
    ["368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"]
    "keys-all"

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

pList :: [PactValue] -> PactValue
pList = PList . V.fromList

pKeySet :: KeySet -> PactValue
pKeySet = PGuard . GKeySet

pObject :: [(FieldKey,PactValue)] -> PactValue
pObject = PObject . ObjectMap . M.fromList

mkEvent
    :: MonadThrow m
    => Text
    -- ^ name
    -> [PactValue]
    -- ^ params
    -> ModuleName
    -> Text
    -- ^ module hash
    -> m PactEvent
mkEvent n params m mh = do
  mh' <- decodeB64UrlNoPaddingText mh
  return $ PactEvent n params m (ModuleHash (Hash $ BS.toShort mh'))

mkTransferEvent
    :: MonadThrow m
    => Text
    -- ^ sender
    -> Text
    -- ^ receiver
    -> Decimal
    -- ^ amount
    -> ModuleName
    -> Text
    -- ^ module hash
    -> m PactEvent
mkTransferEvent sender receiver amount m mh =
  mkEvent "TRANSFER" [pString sender,pString receiver,pDecimal amount] m mh

mkTransferXChainEvent
    :: MonadThrow m
    => Text
    -- ^ sender
    -> Text
    -- ^ receiver
    -> Decimal
    -- ^ amount
    -> ModuleName
    -> Text
    -- ^ module hash
    -> Text
    -- ^ target chain id
    -> m PactEvent
mkTransferXChainEvent sender receiver amount m mh tid
    = mkEvent "TRANSFER_XCHAIN" args m mh
  where
    args =
      [ pString sender
      , pString receiver
      , pDecimal amount
      , pString tid
      ]

mkTransferXChainRecdEvent
    :: MonadThrow m
    => Text
    -- ^ sender
    -> Text
    -- ^ receiver
    -> Decimal
    -- ^ amount
    -> ModuleName
    -> Text
    -- ^ module hash
    -> Text
    -- ^ source chain id
    -> m PactEvent
mkTransferXChainRecdEvent sender receiver amount m mh sid
    = mkEvent "TRANSFER_XCHAIN_RECD" args m mh
  where
    args =
      [ pString sender
      , pString receiver
      , pDecimal amount
      , pString sid
      ]

mkXYieldEvent
    :: MonadThrow m
    => Text
    -- ^ sender
    -> Text
    -- ^ receiver
    -> Decimal
    -- ^ amount
    -> KeySet
    -- ^ receiver guard
    -> ModuleName
    -> Text
    -- ^ module hash
    -> Text
    -- ^ target chain id
    -> Text
    -- ^ source chain id
    -> m PactEvent
mkXYieldEvent sender receiver amount ks mn mh tid sid
    = mkEvent "X_YIELD" args mn mh
  where
    args =
      [ pString tid
      , pString "coin.transfer-crosschain"
      , pList
        [ pString sender
        , pString receiver
        , pKeySet ks
        , pString sid
        , pDecimal amount
        ]
      ]

mkXResumeEvent
    :: MonadThrow m
    => Text
    -- ^ sender
    -> Text
    -- ^ receiver
    -> Decimal
    -- ^ amount
    -> KeySet
    -- ^ receiver guard
    -> ModuleName
    -> Text
    -- ^ module hash
    -> Text
    -- ^ target chain id
    -> Text
    -- ^ source chain id
    -> m PactEvent
mkXResumeEvent sender receiver amount ks mn mh tid sid
    = mkEvent "X_RESUME" args mn mh
  where
    args =
      [ pString tid
      , pString "coin.transfer-crosschain"
      , pList
        [ pString sender
        , pString receiver
        , pKeySet ks
        , pString sid
        , pDecimal amount
        ]
      ]

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

mkXChainTransferCap :: Text -> Text -> Decimal -> Text -> SigCapability
mkXChainTransferCap sender receiver amount cid = mkCoinCap "TRANSFER_XCHAIN"
  [ pString sender
  , pString receiver
  , pDecimal amount
  , pString cid
  ]

mkGasCap :: SigCapability
mkGasCap = mkCoinCap "GAS" []



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

-- | Build parsed + verified Pact command
--
buildCwCmd :: (MonadThrow m, MonadIO m) => CmdBuilder -> m ChainwebTransaction
buildCwCmd cmd = buildRawCmd cmd >>= \c -> case verifyCommand c of
    ProcSucc r -> return $ fmap (mkPayloadWithText c) r
    ProcFail e -> throwM $ userError $ "buildCmd failed: " ++ e



-- | Build unparsed, unverified command
--
buildTextCmd :: CmdBuilder -> IO (Command Text)
buildTextCmd = fmap (fmap T.decodeUtf8) . buildRawCmd

-- | Build a raw bytestring command
--
buildRawCmd :: (MonadThrow m, MonadIO m) => CmdBuilder -> m (Command ByteString)
buildRawCmd CmdBuilder{..} = do
    akps <- mapM toApiKp _cbSigners
    kps <- liftIO $ mkKeyPairs akps
    liftIO $ mkCommand kps pm _cbNonce nid _cbRPC
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

-- ----------------------------------------------------------------------- --
-- Service creation utilities


pactTestLogger :: (String -> IO ()) -> Bool -> Loggers
pactTestLogger backend showAll = initLoggers backend f def
  where
    f _ b "ERROR" d = doLog (\_ -> return ()) b "ERROR" d
    f _ b "DEBUG" d | not showAll = doLog (\_ -> return ()) b "DEBUG" d
    f _ b "INFO" d | not showAll = doLog (\_ -> return ()) b "INFO" d
    f _ b "DDL" d | not showAll = doLog (\_ -> return ()) b "DDL" d
    f a b c d = doLog a b c d

-- | Test Pact Execution Context for running inside 'PactServiceM'.
-- Only used internally.
data TestPactCtx tbl = TestPactCtx
    { _testPactCtxState :: !(MVar PactServiceState)
    , _testPactCtxEnv :: !(PactServiceEnv tbl)
    }


evalPactServiceM_ :: TestPactCtx tbl -> PactServiceM tbl a -> IO a
evalPactServiceM_ ctx pact = modifyMVar (_testPactCtxState ctx) $ \s -> do
    T2 a s' <- runPactServiceM s (_testPactCtxEnv ctx) pact
    return (s',a)

destroyTestPactCtx :: TestPactCtx tbl -> IO ()
destroyTestPactCtx = void . takeMVar . _testPactCtxState

-- | setup TestPactCtx, internal function.
-- Use 'withPactCtxSQLite' in tests.
testPactCtxSQLite
  :: CanReadablePayloadCas tbl
  => (String -> IO ())
    -- ^ logging backend
  -> ChainwebVersion
  -> Version.ChainId
  -> BlockHeaderDb
  -> PayloadDb tbl
  -> SQLiteEnv
  -> PactServiceConfig
  -> (TxContext -> GasModel)
  -> IO (TestPactCtx tbl, PactDbEnv')
testPactCtxSQLite logBackend v cid bhdb pdb sqlenv conf gasmodel = do
    (dbSt,cpe) <- initRelationalCheckpointer' initialBlockState sqlenv cpLogger v cid
    let rs = readRewards
        ph = ParentHeader $ genesisBlockHeader v cid
    !ctx <- TestPactCtx
      <$!> newMVar (PactServiceState Nothing mempty ph noSPVSupport)
      <*> pure (pactServiceEnv cpe rs)
    evalPactServiceM_ ctx (initialPayloadState dummyLogger mempty v cid)
    return (ctx, PactDbEnv' dbSt)
  where
    initialBlockState = initBlockState defaultModuleCacheLimit $ Version.genesisHeight v cid
    loggers = pactTestLogger logBackend False -- toggle verbose pact test logging
    cpLogger = newLogger loggers $ LogName ("Checkpointer" ++ show cid)
    pactServiceEnv cpe rs = PactServiceEnv
        { _psMempoolAccess = Nothing
        , _psCheckpointEnv = cpe
        , _psPdb = pdb
        , _psBlockHeaderDb = bhdb
        , _psGasModel = gasmodel
        , _psMinerRewards = rs
        , _psReorgLimit = fromIntegral $ _pactReorgLimit conf
        , _psLocalRewindDepthLimit = fromIntegral $ _pactLocalRewindDepthLimit conf
        , _psOnFatalError = defaultOnFatalError mempty
        , _psVersion = v
        , _psValidateHashesOnReplay = _pactRevalidate conf
        , _psAllowReadsInLocal = _pactAllowReadsInLocal conf
        , _psIsBatch = False
        , _psCheckpointerDepth = 0
        , _psLogger = newLogger loggers $ LogName ("PactService" ++ show cid)
        , _psGasLogger = Nothing
        , _psLoggers = loggers
        , _psBlockGasLimit = _pactBlockGasLimit conf
        , _psChainId = cid
        }

freeGasModel :: TxContext -> GasModel
freeGasModel = const $ constGasModel 0


-- | A queue-less WebPactExecutionService (for all chains)
-- with direct chain access map for local.
withWebPactExecutionService
    :: (String -> IO ())
        -- ^ logging backend
    -> ChainwebVersion
    -> PactServiceConfig
    -> TestBlockDb
    -> MemPoolAccess
    -> (TxContext -> GasModel)
    -> ((WebPactExecutionService,HM.HashMap ChainId PactExecutionService) -> IO a)
    -> IO a
withWebPactExecutionService logBackend v pactConfig bdb mempoolAccess gasmodel act =
  withDbs $ \sqlenvs -> do
    pacts <- fmap HM.fromList
           $ traverse mkPact
           $ zip sqlenvs
           $ toList
           $ chainIds v
    act (mkWebPactExecutionService pacts, pacts)
  where
    withDbs f = foldl' (\soFar _ -> withDb soFar) f (chainIds v) []
    withDb g envs = withTempSQLiteConnection chainwebPragmas $ \s -> g (s : envs)

    mkPact (sqlenv, c) = do
        bhdb <- getBlockHeaderDb c bdb
        (ctx,_) <- testPactCtxSQLite logBackend v c bhdb (_bdbPayloadDb bdb) sqlenv pactConfig gasmodel
        return $ (c,) $ PactExecutionService
          { _pactNewBlock = \m p ->
              evalPactServiceM_ ctx $ execNewBlock mempoolAccess p m
          , _pactValidateBlock = \h d ->
              evalPactServiceM_ ctx $ execValidateBlock mempoolAccess h d
          , _pactLocal = \pf sv rd cmd ->
              evalPactServiceM_ ctx $ Right <$> execLocal cmd pf sv rd
          , _pactLookup = \rp hashes ->
              evalPactServiceM_ ctx $ Right <$> execLookupPactTxs rp hashes
          , _pactPreInsertCheck = \_ txs ->
              evalPactServiceM_ ctx $ (Right . V.map (() <$)) <$> execPreInsertCheckReq txs
          , _pactBlockTxHistory = \h d ->
              evalPactServiceM_ ctx $ Right <$> execBlockTxHistory h d
          , _pactHistoricalLookup = \h d k ->
              evalPactServiceM_ ctx $ Right <$> execHistoricalLookup h d k
          , _pactSyncToBlock = \h ->
              evalPactServiceM_ ctx $ execSyncToBlock h
          }


-- | Noncer for 'runCut'
type Noncer = ChainId -> IO Nonce

zeroNoncer :: Noncer
zeroNoncer = const (return $ Nonce 0)

-- | Populate blocks for every chain of the current cut. Uses provided pact
-- service to produce a new block, add it to dbs, etc.
runCut
    :: ChainwebVersion
    -> TestBlockDb
    -> WebPactExecutionService
    -> GenBlockTime
    -> Noncer
    -> Miner
    -> IO ()
runCut v bdb pact genTime noncer miner =
  forM_ (chainIds v) $ \cid -> do
    ph <- ParentHeader <$> getParentTestBlockDb bdb cid
    pout <- _webPactNewBlock pact miner ph
    n <- noncer cid
    addTestBlockDb bdb n genTime cid pout
    h <- getParentTestBlockDb bdb cid
    void $ _webPactValidateBlock pact h (payloadWithOutputsToPayloadData pout)

initializeSQLite :: IO SQLiteEnv
initializeSQLite = open2 file >>= \case
    Left (_err, _msg) ->
        internalError "initializeSQLite: A connection could not be opened."
    Right r ->  return (SQLiteEnv r (SQLiteConfig file chainwebPragmas))
  where
    file = "" {- temporary sqlitedb -}

freeSQLiteResource :: SQLiteEnv -> IO ()
freeSQLiteResource sqlenv = void $ close_v2 $ _sConn sqlenv

-- | Run in 'PactServiceM' with direct db access.
type WithPactCtxSQLite tbl = forall a . (PactDbEnv' -> PactServiceM tbl a) -> IO a

-- | Used to run 'PactServiceM' functions directly on a database (ie not use checkpointer).
withPactCtxSQLite
  :: CanReadablePayloadCas tbl
  => ChainwebVersion
  -> IO BlockHeaderDb
  -> IO (PayloadDb tbl)
  -> PactServiceConfig
  -> (WithPactCtxSQLite tbl -> TestTree)
  -> TestTree
withPactCtxSQLite v bhdbIO pdbIO conf f =
  withResource
    initializeSQLite
    freeSQLiteResource $ \io ->
      withResource (start io) destroy $ \ctxIO -> f $ \toPact -> do
          (ctx, dbSt) <- ctxIO
          evalPactServiceM_ ctx (toPact dbSt)
  where
    destroy = destroyTestPactCtx . fst
    start ios = do
        let cid = someChainId v
        bhdb <- bhdbIO
        pdb <- pdbIO
        s <- ios
        testPactCtxSQLite (\_ -> return ()) v cid bhdb pdb s conf freeGasModel

toTxCreationTime :: Integral a => Time a -> TxCreationTime
toTxCreationTime (Time timespan) = TxCreationTime $ fromIntegral $ timeSpanToSeconds timespan

withPayloadDb :: (IO (PayloadDb HashMapTable) -> TestTree) -> TestTree
withPayloadDb = withResource newPayloadDb mempty


-- | 'MemPoolAccess' that delegates all calls to the contents of provided `IORef`.
delegateMemPoolAccess :: IORef MemPoolAccess -> MemPoolAccess
delegateMemPoolAccess r = MemPoolAccess
  { mpaGetBlock = \a b c d e -> call mpaGetBlock $ \f -> f a b c d e
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
setMempool :: MonadIO m => IO (IORef MemPoolAccess) -> MemPoolAccess -> m ()
setMempool refIO mp = liftIO (refIO >>= flip writeIORef mp)

-- | Set test mempool wrapped with a "one shot" 'mpaGetBlock' adapter.
setOneShotMempool :: MonadIO m => IO (IORef MemPoolAccess) -> MemPoolAccess -> m ()
setOneShotMempool mpRefIO mp = do
  oneShot <- liftIO $ newIORef False
  setMempool mpRefIO $ mp
    { mpaGetBlock = \g v i a e -> readIORef oneShot >>= \case
        False -> writeIORef oneShot True >> mpaGetBlock mp g v i a e
        True -> mempty
    }


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
withTemporaryDir = withResource
    (getTemporaryDirectory >>= \d -> createTempDirectory d "test-pact")
    removeDirectoryRecursive

withTestBlockDbTest
    :: ChainwebVersion
    -> RocksDb
    -> (IO TestBlockDb -> TestTree)
    -> TestTree
withTestBlockDbTest v rdb = withResource (mkTestBlockDb v rdb) mempty

-- | Single-chain Pact via service queue.
withPactTestBlockDb
    :: ChainwebVersion
    -> ChainId
    -> RocksDb
    -> (IO MemPoolAccess)
    -> PactServiceConfig
    -> (IO (PactQueue,TestBlockDb) -> TestTree)
    -> TestTree
withPactTestBlockDb version cid rdb mempoolIO pactConfig f =
  withTemporaryDir $ \iodir ->
  withTestBlockDbTest version rdb $ \bdbio ->
  withResource (startPact bdbio iodir) stopPact $ f . fmap (view _3)
  where
    startPact bdbio iodir = do
        reqQ <- newPactQueue 2000
        dir <- iodir
        bdb <- bdbio
        mempool <- mempoolIO
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb bdb) cid
        let pdb = _bdbPayloadDb bdb
        sqlEnv <- startSqliteDb cid logger dir False
        a <- async $ runForever (\_ _ -> return ()) "Chainweb.Test.Pact.Utils.withPactTestBlockDb" $
            runPactService version cid logger reqQ mempool bhdb pdb sqlEnv pactConfig
        return (a, sqlEnv, (reqQ,bdb))

    stopPact (a, sqlEnv, _) = cancel a >> stopSqliteDb sqlEnv

    -- Ideally, we should throw 'error' when the logger is invoked, because
    -- error logs should not happen in production and should always be resolved.
    -- Unfortunately, that's not yet always the case. So we just drop the
    -- message.
    --
    logger = genericLogger Error (\_ -> return ())

dummyLogger :: GenericLogger
dummyLogger = genericLogger Error (error . T.unpack)

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
