{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
-- |
-- Module: Chainweb.Test.Pact.Utils
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution via (inprocess) API  in Chainweb
module Chainweb.Test.Pact.Utils
( -- * Exceptions
  PactTestFailure(..)
, ChainwebKeyPair
  -- * test data
, sender00KeyPair
, sender01KeyPair
, allocation00KeyPair
, allocation01KeyPair
, allocation02KeyPair
, allocation02KeyPair'
, testPactFilesDir
, testKeyPairs
, adminData
  -- * helper functions
, getByteString
, mergeObjects
, formatB16PubKey
, goldenTestTransactions
, mkTestExecTransactions
, mkTestContTransaction
, mkCoinSig
, pactTestLogger
, withMVarResource
, withTime
, mkKeyset
, stockKey
, toTxCreationTime
, withPayloadDb
, withBlockHeaderDb
, withTemporaryDir
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
, withPact
-- * miscellaneous
, ChainwebNetwork(..)
) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State.Strict
import Control.Monad.Trans.Reader

import Data.Aeson (Value(..), object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Default (def)
import Data.FileEmbed
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.CAS.HashMap hiding (toList)
import Data.CAS.RocksDB
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Text.Encoding

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Yaml as Y

import Servant.Client

import System.Directory
import System.IO.Extra
import System.LogLevel

import Test.Tasty

-- internal pact modules

import Pact.ApiReq (ApiKeyPair(..), mkKeyPairs)
import Pact.Gas
import Pact.Parse
import Pact.Types.Capability
import Pact.Types.ChainId
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Gas
import Pact.Types.Logger
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.RPC
import Pact.Types.Runtime (PactId)
import Pact.Types.SPV
import Pact.Types.SQLite
import Pact.Types.Util (toB16Text)

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.ChainId (chainIdToText)
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.InMemoryCheckpointer (initInMemoryCheckpointEnv)
import Chainweb.Pact.Backend.RelationalCheckpointer
    (initRelationalCheckpointer, initRelationalCheckpointer')
import Chainweb.Pact.Backend.SQLite.DirectV2
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.PactInProcApi (pactQueueSize)
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types (internalError)
import Chainweb.Pact.SPV
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Payload.PayloadStore
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..), chainIds, someChainId)
import qualified Chainweb.Version as Version
import Chainweb.WebBlockHeaderDB.Types
import Chainweb.WebPactExecutionService
import Chainweb.Test.Utils

-- ----------------------------------------------------------------------- --
-- Test Exceptions

data PactTestFailure
    = PollingFailure String
    | SendFailure String
    | LocalFailure String
    | SpvFailure String
    deriving Show

instance Exception PactTestFailure

-- ----------------------------------------------------------------------- --
-- Keys

type ChainwebKeyPair
    = (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)

testKeyPairs :: ChainwebKeyPair -> Maybe [SigCapability] -> IO [SomeKeyPairCaps]
testKeyPairs (pub, priv, addr, scheme) clist =
    mkKeyPairs [ApiKeyPair priv (Just pub) (Just addr) (Just scheme) clist]

testPactFilesDir :: FilePath
testPactFilesDir = "test/pact/"

sender00KeyPair :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
sender00KeyPair =
    ( PubBS $ getByteString
        "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"

    , PrivBS $ getByteString
        "251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898"
    , "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
    , ED25519
    )

sender01KeyPair :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
sender01KeyPair =
    ( PubBS $ getByteString
        "6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"
    , PrivBS $ getByteString
        "2beae45b29e850e6b1882ae245b0bab7d0689ebdd0cd777d4314d24d7024b4f7"
    , "6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"
    , ED25519
    )

allocation00KeyPair :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
allocation00KeyPair =
    ( PubBS $ getByteString
        "d82d0dcde9825505d86afb6dcc10411d6b67a429a79e21bda4bb119bf28ab871"
    , PrivBS $ getByteString
        "c63cd081b64ae9a7f8296f11c34ae08ba8e1f8c84df6209e5dee44fa04bcb9f5"
    , "d82d0dcde9825505d86afb6dcc10411d6b67a429a79e21bda4bb119bf28ab871"
    , ED25519
    )

allocation01KeyPair :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
allocation01KeyPair =
    ( PubBS $ getByteString
        "b4c8a3ea91d3146b0560994740f0e3eed91c59d2eeca1dc99f0c2872845c294d"
    , PrivBS $ getByteString
        "5dbbbd8b765b7d0cf8426d6992924b057c70a2138ecd4cf60cfcde643f304ea9"
    , "b4c8a3ea91d3146b0560994740f0e3eed91c59d2eeca1dc99f0c2872845c294d"
    , ED25519
    )

allocation02KeyPair :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
allocation02KeyPair =
    ( PubBS $ getByteString
        "e9e4e71bd063dcf7e06bd5b1a16688897d15ca8bd2e509c453c616219c186cc5"
    , PrivBS $ getByteString
        "45f026b7a6bb278ed4099136c13e842cdd80138ab7c5acd4a1f0e6c97d1d1e3c"
    , "e9e4e71bd063dcf7e06bd5b1a16688897d15ca8bd2e509c453c616219c186cc5"
    , ED25519
    )

allocation02KeyPair' :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
allocation02KeyPair' =
    ( PubBS $ getByteString
        "0c8212a903f6442c84acd0069acc263c69434b5af37b2997b16d6348b53fcd0a"
    , PrivBS $ getByteString
        "2f75b5d875dd7bf07cc1a6973232a9e53dc1d4ffde2bab0bbace65cd87e87f53"
    , "0c8212a903f6442c84acd0069acc263c69434b5af37b2997b16d6348b53fcd0a"
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
adminData = fmap k $ testKeyPairs sender00KeyPair Nothing
  where
    k ks = Just $ object
        [ "test-admin-keyset" .= fmap (formatB16PubKey . fst) ks
        ]

-- | Shim for 'PactExec' and 'PactInProcApi' tests
goldenTestTransactions
    :: Vector PactTransaction -> IO (Vector ChainwebTransaction)
goldenTestTransactions txs = do
    ks <- testKeyPairs sender00KeyPair Nothing
    mkTestExecTransactions "sender00" "0" ks "1" 10000 0.01 1000000 0 txs

-- Make pact 'ExecMsg' transactions specifying sender, chain id of the signer,
-- signer keys, nonce, gas rate, gas limit, and the transactions
-- (with data) to execute.
--
mkTestExecTransactions
    :: Text
      -- ^ sender
    -> ChainId
      -- ^ chain id of execution
    -> [SomeKeyPairCaps]
      -- ^ signer keys
    -> Text
      -- ^ nonce
    -> GasLimit
      -- ^ starting gas
    -> GasPrice
      -- ^ gas rate
    -> TTLSeconds
      -- ^ time in seconds until expiry (from offset)
    -> TxCreationTime
      -- ^ time in seconds until creation (from offset)
    -> Vector PactTransaction
      -- ^ the pact transactions with data to run
    -> IO (Vector ChainwebTransaction)
mkTestExecTransactions sender cid ks nonce0 gas gasrate ttl ct txs = do
    fmap snd $ foldM go (0 :: Int, mempty) txs
  where
    go (!n,acc) (PactTransaction c d) = do
      let dd = mergeObjects (toList d)
          pm = PublicMeta cid sender gas gasrate ttl ct
          msg = Exec (ExecMsg c dd)

      let nonce = nonce0 <> sshow n
      cmd <- mkCommand ks pm nonce Nothing msg
      case verifyCommand cmd of
        ProcSucc t ->
          let
            -- r = fmap (k t) $ SB.toShort <$> cmd
            r = mkPayloadWithText <$> t
            -- order matters for these tests
          in return $ (succ n, Vector.snoc acc r)
        ProcFail e -> throwM $ userError e

    -- k t bs = PayloadWithText bs (_cmdPayload t)

-- | Make pact 'ContMsg' transactions, specifying sender, chain id of the signer,
-- signer keys, nonce, gas rate, gas limit, cont step, pact id, rollback,
-- proof etc.
--
mkTestContTransaction
    :: Text
      -- ^ sender
    -> ChainId
      -- ^ chain id of execution
    -> [SomeKeyPairCaps]
      -- ^ signer keys
    -> Text
      -- ^ nonce
    -> GasLimit
      -- ^ starting gas
    -> GasPrice
      -- ^ gas rate
    -> Int
      -- ^ continuation step
    -> PactId
      -- ^ pact id
    -> Bool
      -- ^ rollback?
    -> Maybe ContProof
      -- ^ SPV proof
    -> TTLSeconds
      -- ^ time in seconds until expiry (from offset)
    -> TxCreationTime
      -- ^ time in seconds until creation (from offset)
    -> Value
    -> IO (Vector ChainwebTransaction)
mkTestContTransaction sender cid ks nonce gas rate step pid rollback proof ttl ct d = do
    let pm = PublicMeta cid sender gas rate ttl ct
        msg :: PactRPC ContMsg =
          Continuation (ContMsg pid step rollback d proof)

    cmd <- mkCommand ks pm nonce Nothing msg
    case verifyCommand cmd of
      -- ProcSucc t -> return $ Vector.singleton $ fmap (k t) (SB.toShort <$> cmd)
      ProcSucc t -> return $ Vector.singleton $ mkPayloadWithText <$> t
      ProcFail e -> throwM $ userError e
  where
    -- k t bs = PayloadWithText bs (_cmdPayload t)

pactTestLogger :: Bool -> Loggers
pactTestLogger showAll = initLoggers putStrLn f def
  where
    f _ b "ERROR" d = doLog error b "ERROR" d
    f _ b "DEBUG" d | not showAll = doLog (\_ -> return ()) b "DEBUG" d
    f _ b "INFO" d | not showAll = doLog (\_ -> return ()) b "INFO" d
    f _ b "DDL" d | not showAll = doLog (\_ -> return ()) b "DDL" d
    f a b c d = doLog a b c d

mkCoinSig :: Text -> [PactValue] -> SigCapability
mkCoinSig n ps = SigCapability (QualifiedName (ModuleName "coin" Nothing) n def) ps

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
    -> Version.ChainId
    -> Maybe (MVar (CutDb cas))
    -> BlockHeaderDb
    -> PayloadDb cas
    -> IO (TestPactCtx cas)
testPactCtx v cid cdbv bhdb pdb = do
    cpe <- initInMemoryCheckpointEnv loggers logger
    let rs = readRewards v
    ctx <- TestPactCtx
        <$> newMVar (PactServiceState Nothing)
        <*> pure (PactServiceEnv Nothing cpe spv pd pdb bhdb rs (constGasModel 0))
    evalPactServiceM ctx (initialPayloadState v cid)
    return ctx
  where
    loggers = pactTestLogger False -- toggle verbose pact test logging
    logger = newLogger loggers $ LogName "PactService"
    spv = maybe noSPVSupport (\cdb -> pactSPV cid cdb) cdbv
    pd = def & pdPublicMeta . pmChainId .~ (ChainId $ chainIdToText cid)

testPactCtxSQLite
  :: PayloadCas cas
  => ChainwebVersion
  -> Version.ChainId
  -> Maybe (MVar (CutDb cas))
  -> BlockHeaderDb
  -> PayloadDb cas
  -> SQLiteEnv
  -> IO (TestPactCtx cas)
testPactCtxSQLite v cid cdbv bhdb pdb sqlenv = do
    cpe <- initRelationalCheckpointer initBlockState sqlenv logger
    let rs = readRewards v
    ctx <- TestPactCtx
      <$> newMVar (PactServiceState Nothing)
      <*> pure (PactServiceEnv Nothing cpe spv pd pdb bhdb rs (constGasModel 0))
    evalPactServiceM ctx (initialPayloadState v cid)
    return ctx
  where
    loggers = pactTestLogger False -- toggle verbose pact test logging
    logger = newLogger loggers $ LogName ("PactService" ++ show cid)
    spv = maybe noSPVSupport (\cdb -> pactSPV cid cdb) cdbv
    pd = def & pdPublicMeta . pmChainId .~ (ChainId $ chainIdToText cid)

-- | A test PactExecutionService for a single chain
--
testPactExecutionService
    :: PayloadCas cas
    => ChainwebVersion
    -> Version.ChainId
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
        { _pactNewBlock = \m p t -> evalPactServiceM ctx $ execNewBlock mempoolAccess p m t
        , _pactValidateBlock = \h d ->
            evalPactServiceM ctx $ execValidateBlock h d
        , _pactLocal = error
            "Chainweb.Test.Pact.Utils.testPactExecutionService._pactLocal: not implemented"
        , _pactLookup = error
            "Chainweb.Test.Pact.Utils.testPactExecutionService._pactLookup: not implemented"
        }

-- | A test PactExecutionService for a chainweb
--
testWebPactExecutionService
    :: PayloadCas cas
    => ChainwebVersion
    -> Maybe (MVar (CutDb cas))
    -> IO WebBlockHeaderDb
    -> IO (PayloadDb cas)
    -> (Version.ChainId -> MemPoolAccess)
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
        Right r ->  return $ (del, SQLiteEnv r (SQLiteConfig file chainwebPragmas))

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
          spv = maybe noSPVSupport (\cdb -> pactSPV cid cdb) cdbv
          cid = someChainId v
          pd = def & pdPublicMeta . pmChainId .~ (ChainId $ chainIdToText cid)
      bhdb <- bhdbIO
      pdb <- pdbIO
      (_,s) <- ios
      (dbSt, cpe) <- initRelationalCheckpointer' initBlockState s logger
      let rs = readRewards v
      !ctx <- TestPactCtx
        <$!> newMVar (PactServiceState Nothing)
        <*> pure (PactServiceEnv Nothing cpe spv pd pdb bhdb rs (constGasModel 0))
      evalPactServiceM ctx (initialPayloadState v cid)
      return (ctx, dbSt)

withMVarResource :: a -> (IO (MVar a) -> TestTree) -> TestTree
withMVarResource value = withResource (newMVar value) (const $ return ())

withTime :: (IO (Time Integer) -> TestTree) -> TestTree
withTime = withResource getCurrentTimeIntegral (const (return ()))

mkKeyset :: Text -> [PublicKeyBS] -> Value
mkKeyset p ks = object
  [ "pred" .= p
  , "keys" .= ks
  ]

stockKeyFile :: ByteString
stockKeyFile = $(embedFile "pact/genesis/testnet/keys.yaml")

-- | Convenient access to predefined testnet sender accounts
stockKey :: Text -> IO ApiKeyPair
stockKey s = do
  let Right (Y.Object o) = Y.decodeEither' stockKeyFile
      Just (Y.Object kp) = HM.lookup s o
      Just (String pub) = HM.lookup "public" kp
      Just (String priv) = HM.lookup "secret" kp
      mkKeyBS = decodeKey . encodeUtf8
  return $ ApiKeyPair (PrivBS $ mkKeyBS priv) (Just $ PubBS $ mkKeyBS pub) Nothing (Just ED25519) Nothing

decodeKey :: ByteString -> ByteString
decodeKey = fst . B16.decode

toTxCreationTime :: Time Integer -> TxCreationTime
toTxCreationTime (Time timespan) = case timeSpanToSeconds timespan of
          Seconds s -> TxCreationTime $ ParsedInteger s

withPayloadDb :: (IO (PayloadDb HashMapCas) -> TestTree) -> TestTree
withPayloadDb = withResource newPayloadDb (\_ -> return ())

withBlockHeaderDb
    :: IO RocksDb
    -> BlockHeader
    -> (IO BlockHeaderDb -> TestTree)
    -> TestTree
withBlockHeaderDb iordb b = withResource start stop
  where
    start = do
        rdb <- iordb
        testBlockHeaderDb rdb b
    stop = closeBlockHeaderDb

withTemporaryDir :: (IO FilePath -> TestTree) -> TestTree
withTemporaryDir = withResource (fst <$> newTempDir) removeDirectoryRecursive

withPact
    :: ChainwebVersion
    -> LogLevel
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> MemPoolAccess
    -> IO FilePath
    -> (IO PactQueue -> TestTree)
    -> TestTree
withPact version logLevel iopdb iobhdb mempool iodir f =
    withResource startPact stopPact $ f . fmap snd
  where
    startPact = do
        mv <- newEmptyMVar
        reqQ <- atomically $ newTBQueue pactQueueSize
        pdb <- iopdb
        bhdb <- iobhdb
        dir <- iodir
        a <- async $ initPactService version cid logger reqQ mempool mv
                                     bhdb pdb (Just dir) Nothing False
        return (a, reqQ)

    stopPact (a, _) = cancel a

    logger = genericLogger logLevel T.putStrLn
    cid = someChainId version

newtype ChainwebNetwork = ChainwebNetwork { _getClientEnv :: ClientEnv }
