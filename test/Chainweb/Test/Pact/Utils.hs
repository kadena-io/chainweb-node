{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
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
, testWebPactExecutionService
, testPactExecutionService
) where

import Control.Concurrent.MVar
import Control.Exception.Safe (tryAny)
import Control.Lens hiding ((.=))
import qualified Control.Lens as L
import Control.Monad.Catch
import Control.Monad.State.Strict
import Control.Monad.Trans.Reader

import Data.Aeson (Value(..), object, (.=))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import Data.Default (def)
import Data.Foldable
import Data.Functor (void)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Time.Clock
import Data.Vector (Vector)
import qualified Data.Vector as Vector

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

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId (chainIdToText)
import Chainweb.CutDB
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService
import Chainweb.Pact.Backend.InMemoryCheckpointer (initInMemoryCheckpointEnv)
import Chainweb.Pact.Backend.MemoryDb (mkPureState)
import Chainweb.Pact.SPV
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Pact.Utils
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..), someChainId, chainIds)
import qualified Chainweb.Version as V
import Chainweb.WebPactExecutionService

import Pact.Gas
import Pact.Interpreter
import Pact.Types.Gas
import Pact.Types.Runtime hiding (Object(..))
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

-- | Merge a list of JSON Objects together. Note: this will yield an empty
-- object in the case that there are no objects in the list of values.
--
mergeObjects :: [Value] -> Value
mergeObjects = Object . HM.unions . foldr unwrap []
  where
    unwrap (Object o) = (:) o
    unwrap _ = id

-- | Shim for 'PactExec' and 'PactInProcApi' tests
goldenTestTransactions :: Vector PactTransaction -> IO (Vector ChainwebTransaction)
goldenTestTransactions txs = do
    ks <- testKeyPairs
    mkPactTestTransactions "sender00" "0" ks "1" (ParsedInteger 100) (ParsedDecimal 0.0001) txs

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
    -> ParsedInteger
    -- ^ starting gas
    -> ParsedDecimal
    -- ^ gas rate
    -> Vector PactTransaction
    -- ^ the pact transactions with data to run
    -> IO (Vector ChainwebTransaction)
mkPactTestTransactions sender cid keys nonce gas gasrate txs =
    traverse go txs
  where
    go (PactTransaction c d) = do
      let dd = mergeObjects (toList d)
          pm = PublicMeta cid sender gas gasrate
          msg = Exec (ExecMsg c dd)

      cmd <- mkCommand keys pm nonce msg
      case verifyCommand cmd of
        ProcSucc t -> return $ fmap (k t) cmd
        ProcFail e -> throwM $ userError e

    k t bs = PayloadWithText bs (_cmdPayload t)


pactTestLogger :: Bool -> Loggers
pactTestLogger showAll = initLoggers putStrLn f def
  where
    f _ b "ERROR" d = doLog error b "ERROR" d
    f _ b "DEBUG" d | not showAll = doLog (\_ -> return ()) b "DEBUG" d
    f _ b "INFO" d | not showAll = doLog (\_ -> return ()) b "DEBUG" d
    f _ b "DDL" d | not showAll = doLog (\_ -> return ()) b "DDL" d
    f a b c d = doLog a b c d

-- -------------------------------------------------------------------------- --
-- Test Pact Execution Context

data TestPactCtx = TestPactCtx
    { _testPactCtxState :: !(MVar PactServiceState)
    , _testPactCtxEnv :: !PactServiceEnv
    }

data PactTransaction = PactTransaction
  { _pactCode :: Text
  , _pactData :: Maybe Value
  } deriving (Eq, Show)


evalPactServiceM :: TestPactCtx -> PactServiceM a -> IO a
evalPactServiceM ctx pact = modifyMVar (_testPactCtxState ctx) $ \s -> do
    (a,s') <- runStateT (runReaderT pact (_testPactCtxEnv ctx)) s
    return (s',a)

destroyTestPactCtx :: TestPactCtx -> IO ()
destroyTestPactCtx = void . takeMVar . _testPactCtxState

testPactCtx
    :: ChainwebVersion
    -> V.ChainId
    -> Maybe (MVar (CutDb cas))
    -> IO TestPactCtx
testPactCtx v cid cdbv = do
    cpe <- initInMemoryCheckpointEnv logger gasEnv
    env <- mkPureEnv loggers
    dbSt <- mkPureState env
    void $ saveInitial (_cpeCheckpointer cpe) dbSt
    ctx <- TestPactCtx
        <$> newMVar (PactServiceState dbSt Nothing)
        <*> pure (PactServiceEnv Nothing cpe spv pd)
    evalPactServiceM ctx (initialPayloadState v cid)
    return ctx
  where
    loggers = pactTestLogger False
    logger = newLogger loggers $ LogName "PactService"
    gasEnv = GasEnv 0 0 (constGasModel 0)
    spv = maybe noSPV pactSPV cdbv
    pd = def & pdPublicMeta . pmChainId .~ (ChainId $ chainIdToText cid)


-- | A test PactExecutionService for a single chain
--
testPactExecutionService
    :: ChainwebVersion
    -> V.ChainId
    -> Maybe (MVar (CutDb cas))
    -> MemPoolAccess
       -- ^ transaction generator
    -> IO PactExecutionService
testPactExecutionService v cid cutDB mempoolAccess = do
    ctx <- testPactCtx v cid cutDB
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
    -> Maybe (MVar (CutDb cas))
    -> (V.ChainId -> MemPoolAccess)
       -- ^ transaction generator
    -> IO WebPactExecutionService
testWebPactExecutionService v cutDB mempoolAccess
    = fmap mkWebPactExecutionService
    $ fmap HM.fromList
    $ traverse
        (\c -> (c,) <$> testPactExecutionService v c cutDB (mempoolAccess c))
    $ toList
    $ chainIds v

-- | This enforces that only a single test can use the pact context at a time.
-- It's up to the user to ensure that tests are scheduled in the right order.
--
withPactCtx
    :: ChainwebVersion
    -> Maybe (MVar (CutDb cas))
    -> ((forall a . PactServiceM a -> IO a) -> TestTree)
    -> TestTree
withPactCtx v cutDB f
    = withResource (start cutDB) destroyTestPactCtx $ \ctxIO -> f $ \pact -> do
        ctx <- ctxIO
        evalPactServiceM ctx pact
  where
    start = testPactCtx v (someChainId v)
