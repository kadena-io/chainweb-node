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

  -- * helper functions
, getByteString
, formatB16PubKey
, mkPactTestTransactions
, mkPactTransaction
, pactTestLogger

-- * Test Pact Execution Environment
, TestPactCtx(..)
, testPactCtx
, destroyTestPactCtx
, evalPactServiceM
, withPactCtx
, testWebPactExecutionService
, testPactExecutionService
) where

import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.State.Strict
import Control.Monad.Trans.Reader

import Data.Aeson (Value(..), object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import Data.Default (def)
import Data.Foldable
import Data.Functor (void)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Time.Clock
import Data.Vector (Vector)

import Test.Tasty

-- internal pact modules

import Pact.ApiReq (ApiKeyPair(..), mkKeyPairs)
import Pact.Parse (ParsedDecimal(..), ParsedInteger(..))
import Pact.Types.API
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Logger
import Pact.Types.RPC (ExecMsg(..), PactRPC(Exec))
import Pact.Types.Util (toB16Text)

-- internal modules

import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..), someChainId, chainIds)
import qualified Chainweb.Version as V
import Chainweb.WebPactExecutionService

import Pact.Gas
import Pact.Interpreter
import Pact.Types.Gas
import qualified Pact.Types.Runtime as P
import Pact.Types.Server

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

------------------------------------------------------------------------------
-- helper logic
------------------------------------------------------------------------------

getByteString :: ByteString -> ByteString
getByteString = fst . B16.decode

formatB16PubKey :: SomeKeyPair -> Text
formatB16PubKey = toB16Text . formatPublicKey

mkPactTestTransactions :: Vector String -> IO (Vector ChainwebTransaction)
mkPactTestTransactions cmdStrs = do
    kps <- testKeyPairs
    let theData = object ["test-admin-keyset" .= fmap formatB16PubKey kps]
    -- using 1 as the nonce here so the hashes match for the same commands (for testing only)
    traverse (mkPactTransaction kps theData "1") cmdStrs

mkPactTransaction
  :: [SomeKeyPair]
  -> Value
  -> Text
  -> String
  -> IO ChainwebTransaction
mkPactTransaction keyPairs theData nonce theCode = do
    let pubMeta = PublicMeta "0" "sender00" (ParsedInteger 100) (ParsedDecimal 0.0001)
    cmdBS <- mkCommand keyPairs pubMeta nonce $
        Exec (ExecMsg (pack theCode) theData)
    case verifyCommand cmdBS of
        ProcSucc cmd -> return $ (\bs -> PayloadWithText bs (_cmdPayload cmd)) <$> cmdBS
        ProcFail err -> throwM . userError $ err


-- | testKeyPairs >>= _mkPactTransaction' Null "(+ 1 2")
_mkPactTransaction'
  :: Value
  -> String
  -> [SomeKeyPair]
  -> IO ()
_mkPactTransaction' theData theCode kps = do
  nonce <- pack . show <$> getCurrentTime
  t <- fmap (decodeUtf8 . payloadBytes) <$> mkPactTransaction kps theData nonce theCode
  BS.putStrLn $ encodeToByteString $ SubmitBatch [t]

pactTestLogger :: Loggers
pactTestLogger = initLoggers putStrLn f def
  where
    f _ b "ERROR" d = doLog error b "ERROR" d
    f _ b "DEBUG" d = doLog (\_ -> return ()) b "DEBUG" d
    f _ b "DDL" d = doLog (\_ -> return ()) b "DDL" d
    f a b c d = doLog a b c d

-- -------------------------------------------------------------------------- --
-- Test Pact Execution Context

data TestPactCtx = TestPactCtx
    { _testPactCtxState :: !(MVar PactServiceState)
    , _testPactCtxEnv :: !PactServiceEnv
    }

evalPactServiceM :: TestPactCtx -> PactServiceM a -> IO a
evalPactServiceM ctx pact = modifyMVar (_testPactCtxState ctx) $ \s -> do
    (a,s') <- runStateT (runReaderT pact (_testPactCtxEnv ctx)) s
    return (s',a)

destroyTestPactCtx :: TestPactCtx -> IO ()
destroyTestPactCtx = void . takeMVar . _testPactCtxState

testPactCtx :: ChainwebVersion -> V.ChainId -> IO TestPactCtx
testPactCtx v cid = do
    dbSt <- initializeState
    checkpointEnv <- initInMemoryCheckpointEnv cmdConfig logger gasEnv
    void $ saveInitial (_cpeCheckpointer checkpointEnv) dbSt
    ctx <- TestPactCtx
        <$> newMVar (PactServiceState dbSt Nothing)
        <*> pure (PactServiceEnv Nothing checkpointEnv P.noSPVSupport def)
    evalPactServiceM ctx (initialPayloadState v cid)
    return ctx
  where
    loggers = pactTestLogger
    logger = newLogger pactTestLogger $ LogName "PactService"
    cmdConfig = toCommandConfig $ pactDbConfig v
    gasLimit = fromMaybe 0 (_ccGasLimit cmdConfig)
    gasRate = fromMaybe 0 (_ccGasRate cmdConfig)
    gasEnv = GasEnv (fromIntegral gasLimit) 0.0 (constGasModel (fromIntegral gasRate))

    initializeState = case _ccSqlite cmdConfig of
        Nothing -> do
            env <- mkPureEnv loggers
            mkPureState env cmdConfig
        Just sqlc -> do
            env <- mkSQLiteEnv logger False sqlc loggers
            mkSQLiteState env cmdConfig

-- | A test PactExecutionService for a single chain
--
testPactExecutionService
    :: ChainwebVersion
    -> V.ChainId
    -> MemPoolAccess
       -- ^ transaction generator
    -> IO PactExecutionService
testPactExecutionService v cid mempoolAccess = do
    ctx <- testPactCtx v cid
    return $ PactExecutionService
        { _pactNewBlock = \m p ->
            evalPactServiceM ctx $ execNewBlock mempoolAccess p m
        , _pactValidateBlock = \h d ->
            evalPactServiceM ctx $ execValidateBlock False h d
        , _pactLocal = error
            "Chainweb.Test.Pact.Utils.testPactExecutionService._pactLocal: not implemented"
        }

-- | A test PactExecutionService for a chainweb
--
testWebPactExecutionService
    :: ChainwebVersion
    -> (V.ChainId -> MemPoolAccess)
       -- ^ transaction generator
    -> IO WebPactExecutionService
testWebPactExecutionService v mempoolAccess
    = fmap mkWebPactExecutionService
    $ fmap HM.fromList
    $ traverse
        (\c -> (c,) <$> testPactExecutionService v c (mempoolAccess c))
    $ toList
    $ chainIds v

-- | This enforces that only a single test can use the pact context at a time.
-- It's up to the user to ensure that tests are scheduled in the right order.
--
withPactCtx :: ChainwebVersion -> ((forall a . PactServiceM a -> IO a) -> TestTree) -> TestTree
withPactCtx v f
    = withResource start destroyTestPactCtx $ \ctxIO -> f $ \pact -> do
        ctx <- ctxIO
        evalPactServiceM ctx pact
  where
    start = testPactCtx v (someChainId v)
