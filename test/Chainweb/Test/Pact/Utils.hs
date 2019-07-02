{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module: Chainweb.Test.PactInProcApi
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution via (inprocess) API  in Chainweb
module Chainweb.Test.Pact.Utils
( -- * test data
  someED25519Pair
, testPactFilesDir
, testKeyPairs
, adminData
  -- * helper functions
, getByteString
, mergeObjects
, formatB16PubKey
, goldenTestTransactions
, mkPactTestTransactions
, pactTestLogger
-- * Test Pact Execution Environment
, TestPactCtx(..)
, PactTransaction(..)
, testPactCtx
, destroyTestPactCtx
, evalPactServiceM
, withPactCtx
, withPactCtxSQLite
, testWebPactExecutionService
, testPactExecutionService
, initializeSQLite
, freeSQLiteResource
, testPactCtxSQLite
) where

import Control.Concurrent.MVar
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.State.Strict
import Control.Monad.Trans.Reader

import Data.Aeson (Value(..), object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Default (def)
import Data.Foldable
import Data.Functor (void)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Vector (Vector)


import System.IO.Extra

import Test.Tasty

-- internal pact modules

import Pact.ApiReq (ApiKeyPair(..), mkKeyPairs)
import Pact.Types.ChainId
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Logger
import Pact.Types.RPC (ExecMsg(..), PactRPC(Exec))
import Pact.Types.SPV (noSPVSupport)
import Pact.Types.SQLite hiding (fastNoJournalPragmas)
import Pact.Types.Util (toB16Text)

-- internal modules

import Chainweb.BlockHeaderDB.Types
import Chainweb.ChainId (chainIdToText)
import Chainweb.CutDB
import Chainweb.Pact.Backend.InMemoryCheckpointer (initInMemoryCheckpointEnv)
import Chainweb.Pact.Backend.RelationalCheckpointer
    (initRelationalCheckpointer, initRelationalCheckpointer')
import Chainweb.Pact.Backend.SQLite.DirectV2
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.InternalTypes
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.Types (internalError)
import Chainweb.Pact.SPV
import Chainweb.Payload.PayloadStore
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..), chainIds, someChainId)
import qualified Chainweb.Version as V
import Chainweb.WebBlockHeaderDB.Types
import Chainweb.WebPactExecutionService

import Pact.Gas
import Pact.Types.Gas


testKeyPairs :: IO [SomeKeyPair]
testKeyPairs = do
    let (pub, priv, addr, scheme) = someED25519Pair
        apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme)
    mkKeyPairs [apiKP]

testPactFilesDir :: FilePath
testPactFilesDir = "test/pact/"

-- | note this is "sender00"'s key
someED25519Pair :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
someED25519Pair =
    ( PubBS $ getByteString
        "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
    , PrivBS $ getByteString
        "251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898"
    , "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
    , ED25519
    )

-- ----------------------------------------------------------------------- --
-- helper logic

getByteString :: ByteString -> ByteString
getByteString = fst . B16.decode

formatB16PubKey :: SomeKeyPair -> Text
formatB16PubKey = toB16Text . formatPublicKey

-- | Merge a list of JSON Objects together. Note: this will yield an empty
-- object in the case that there are no objects in the list of values.
--
mergeObjects :: [Value] -> Value
mergeObjects = Object . HM.unions . foldr unwrap []
  where
    unwrap (Object o) = (:) o
    unwrap _ = id

adminData :: IO (Maybe Value)
adminData = fmap k testKeyPairs
  where
    k ks = Just $ object [ "test-admin-keyset" .= fmap formatB16PubKey ks ]

-- | Shim for 'PactExec' and 'PactInProcApi' tests
goldenTestTransactions :: Vector PactTransaction -> IO (Vector ChainwebTransaction)
goldenTestTransactions txs = do
    ks <- testKeyPairs

    mkPactTestTransactions "sender00" "0" ks "1" 100 0.0001 txs

-- Make pact transactions specifying sender, chain id of the signer,
-- signer keys, nonce, gas rate, gas limit, and the transactions
-- (with data) to execute.
--
mkPactTestTransactions
    :: Text
    -- ^ sender
    -> ChainId
    -- ^ chain id of execution
    -> [SomeKeyPair]
    -- ^ signer keys
    -> Text
    -- ^ nonce
    -> GasLimit
    -- ^ starting gas
    -> GasPrice
    -- ^ gas rate
    -> Vector PactTransaction
    -- ^ the pact transactions with data to run
    -> IO (Vector ChainwebTransaction)
mkPactTestTransactions sender cid ks nonce gas gasrate txs =
    traverse go txs
  where
    go (PactTransaction c d) = do
      let dd = mergeObjects (toList d)
          pm = PublicMeta cid sender gas gasrate
          msg = Exec (ExecMsg c dd)

      cmd <- mkCommand ks pm nonce msg
      case verifyCommand cmd of
        ProcSucc t -> return $ fmap (k t) cmd
        ProcFail e -> throwM $ userError e

    k t bs = PayloadWithText bs (_cmdPayload t)


pactTestLogger :: Bool -> Loggers
pactTestLogger showAll = initLoggers putStrLn f def
  where
    f _ b "ERROR" d = doLog error b "ERROR" d
    f _ b "DEBUG" d | not showAll = doLog (\_ -> return ()) b "DEBUG" d
    f _ b "INFO" d | not showAll = doLog (\_ -> return ()) b "INFO" d
    f _ b "DDL" d | not showAll = doLog (\_ -> return ()) b "DDL" d
    f a b c d = doLog a b c d

-- -------------------------------------------------------------------------- --
-- Test Pact Execution Context

data TestPactCtx cas = TestPactCtx
    { _testPactCtxState :: !(MVar PactServiceState)
    , _testPactCtxEnv :: !(PactServiceEnv cas)
    }

data PactTransaction = PactTransaction
  { _pactCode :: Text
  , _pactData :: Maybe Value
  } deriving (Eq, Show)


evalPactServiceM :: TestPactCtx cas -> PactServiceM cas a -> IO a
evalPactServiceM ctx pact = modifyMVar (_testPactCtxState ctx) $ \s -> do
    (a,s') <- runStateT (runReaderT pact (_testPactCtxEnv ctx)) s
    return (s',a)

destroyTestPactCtx :: TestPactCtx cas -> IO ()
destroyTestPactCtx = void . takeMVar . _testPactCtxState

testPactCtx
    :: PayloadCas cas
    => ChainwebVersion
    -> V.ChainId
    -> Maybe (MVar (CutDb cas))
    -> BlockHeaderDb
    -> PayloadDb cas
    -> IO (TestPactCtx cas)
testPactCtx v cid cdbv bhdb pdb = do
    cpe <- initInMemoryCheckpointEnv loggers logger gasEnv
    ctx <- TestPactCtx
        <$> newMVar (PactServiceState Nothing)
        <*> pure (PactServiceEnv Nothing cpe spv pd pdb bhdb)
    evalPactServiceM ctx (initialPayloadState v cid noopMemPoolAccess)
    return ctx
  where
    loggers = pactTestLogger False
    logger = newLogger loggers $ LogName "PactService"
    gasEnv = GasEnv 0 0 (constGasModel 0)
    spv = maybe noSPVSupport (\cdb -> pactSPV cdb logger) cdbv
    pd = def & pdPublicMeta . pmChainId .~ (ChainId $ chainIdToText cid)

testPactCtxSQLite
  :: PayloadCas cas
  => ChainwebVersion
  -> V.ChainId
  -> Maybe (MVar (CutDb cas))
  -> BlockHeaderDb
  -> PayloadDb cas
  -> SQLiteEnv
  -> IO (TestPactCtx cas)
testPactCtxSQLite v cid cdbv bhdb pdb sqlenv = do
    cpe <- initRelationalCheckpointer blockstate sqlenv logger gasEnv
    ctx <- TestPactCtx
      <$> newMVar (PactServiceState Nothing)
      <*> pure (PactServiceEnv Nothing cpe spv pd pdb bhdb)
    evalPactServiceM ctx (initialPayloadState v cid noopMemPoolAccess)
    return ctx
  where
    loggers = pactTestLogger False
    logger = newLogger loggers $ LogName ("PactService" ++ show cid)
    gasEnv = GasEnv 0 0 (constGasModel 0)
    spv = maybe noSPVSupport (\cdb -> pactSPV cdb logger) cdbv
    pd = def & pdPublicMeta . pmChainId .~ (ChainId $ chainIdToText cid)
    blockstate = BlockState 0 Nothing (BlockVersion 0 0) M.empty

-- | A test PactExecutionService for a single chain
--
testPactExecutionService
    :: PayloadCas cas
    => ChainwebVersion
    -> V.ChainId
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

-- | A test PactExecutionService for a chainweb
--
testWebPactExecutionService
    :: PayloadCas cas
    => ChainwebVersion
    -> Maybe (MVar (CutDb cas))
    -> IO WebBlockHeaderDb
    -> IO (PayloadDb cas)
    -> (V.ChainId -> MemPoolAccess)
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

-- | This enforces that only a single test can use the pact context at a time.
-- It's up to the user to ensure that tests are scheduled in the right order.
--
withPactCtx
    :: PayloadCas cas
    => ChainwebVersion
    -> Maybe (MVar (CutDb cas))
    -> IO BlockHeaderDb
    -> IO (PayloadDb cas)
    -> ((forall a . PactServiceM cas a -> IO a) -> TestTree)
    -> TestTree
withPactCtx v cutDB bhdbIO pdbIO f =
    withResource start destroyTestPactCtx $
    \ctxIO -> f $ \pact -> do
        ctx <- ctxIO
        evalPactServiceM ctx pact
  where
    start = do
        bhdb <- bhdbIO
        pdb <- pdbIO
        testPactCtx v (someChainId v) cutDB bhdb pdb

initializeSQLite :: IO (IO (), SQLiteEnv)
initializeSQLite = do
      (file, del) <- newTempFile
      e <- open2 file
      case e of
        Left (_err, _msg) ->
          internalError "initializeSQLite: A connection could not be opened."
        Right r ->  return $ (del, SQLiteEnv r (SQLiteConfig file fastNoJournalPragmas))

freeSQLiteResource :: (IO (), SQLiteEnv) -> IO ()
freeSQLiteResource (del,sqlenv) = do
  void $ close_v2 $ _sConn sqlenv
  del

withPactCtxSQLite
  :: PayloadCas cas
  => ChainwebVersion
  -> Maybe (MVar (CutDb cas))
  -> IO BlockHeaderDb
  -> IO (PayloadDb cas)
  -> ((forall a . (PactDbEnv' -> PactServiceM cas a) -> IO a) -> TestTree)
  -> TestTree
withPactCtxSQLite v cutDB bhdbIO pdbIO f =
  withResource
    initializeSQLite
    freeSQLiteResource $ \io -> do
      withResource (start io cutDB) (destroy io) $ \ctxIO -> f $ \toPact -> do
          (ctx, dbSt) <- ctxIO
          evalPactServiceM ctx (toPact dbSt)
  where
    destroy = const (destroyTestPactCtx . fst)
    start ios cdbv = do
      let loggers = pactTestLogger False
          logger = newLogger loggers $ LogName "PactService"
          gasEnv = GasEnv 0 0 (constGasModel 0)
          spv = maybe noSPVSupport (\cdb -> pactSPV cdb logger) cdbv
          cid = someChainId v
          pd = def & pdPublicMeta . pmChainId .~ (ChainId $ chainIdToText cid)
          blockstate = BlockState 0 Nothing (BlockVersion 0 0) M.empty
      bhdb <- bhdbIO
      pdb <- pdbIO
      (_,s) <- ios
      (dbSt, cpe) <- initRelationalCheckpointer' blockstate s logger gasEnv
      ctx <- TestPactCtx
        <$> newMVar (PactServiceState Nothing)
        <*> pure (PactServiceEnv Nothing cpe spv pd pdb bhdb)
      evalPactServiceM ctx (initialPayloadState v cid noopMemPoolAccess)
      return (ctx, dbSt)
