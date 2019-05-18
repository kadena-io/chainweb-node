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
, singletonOf
, formatB16PubKey
, mkPactTestTransactions
, mkPactTestTransactions'
, mkPactTransaction
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

-- | Merge a list of JSON Objects together. This will defer to 'Null' if any other values
-- are found. Beware.
mergeObjects :: [Value] -> Value
mergeObjects = Object . HM.unions . foldr unwrap []
  where
    unwrap (Object o) = (:) o
    unwrap _ = id

-- | Lift a Maybe into a singleton List
singletonOf :: Maybe a -> [a]
singletonOf = toList

mkPactTestTransactions :: Vector String -> IO (Vector ChainwebTransaction)
mkPactTestTransactions cmdStrs = do
    kps <- testKeyPairs
    let theData = object ["test-admin-keyset" .= fmap formatB16PubKey kps]
    -- using 1 as the nonce here so the hashes match for the same commands (for testing only)
    traverse (mkPactTransaction kps theData "1") cmdStrs

mkPactTestTransactions'
    :: Text
    -> ChainId
    -> Vector PactTransaction
    -> IO (Vector ChainwebTransaction)
mkPactTestTransactions' sender cid txs =
    testKeyPairs >>= forM txs . go
  where
    -- merge tx data and create pact command
    go ks (PactTransaction c d) =
      let d' = mergeObjects $ [keys0] <> singletonOf d
      in mkTx ks d' c

    -- create public test admin keys from test keyset
    keys0 =
      let
        ks = KeySet
          [ "6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7" ]
          (Name "keys-all" def)
      in object ["sender01-keys" .= ks]

    mkTx ks d c = do
      let pm = PublicMeta cid sender (ParsedInteger 100) (ParsedDecimal 0.0001)
      cmd <- mkCommand ks pm "1" $ Exec (ExecMsg c d)
      case verifyCommand cmd of
        ProcSucc t -> return $
          fmap (\bs -> PayloadWithText bs (_cmdPayload t)) cmd
        ProcFail e -> throwM . userError $ e


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
            evalPactServiceM ctx $ testExecNewBlock mempoolAccess p m
        , _pactValidateBlock = \h d ->
            evalPactServiceM ctx $ testExecValidateBlock h d
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


-- | Note: The BlockHeader param here is the PARENT HEADER of the new block-to-be
testExecNewBlock
    :: MemPoolAccess
    -> BlockHeader
    -> MinerInfo
    -> PactServiceM PayloadWithOutputs
testExecNewBlock mpa h m = do
    let bhe@(BlockHeight bh) = _blockHeight h
        bha = _blockHash h

    tx <- liftIO $! mpa bhe bha

    restoreCheckpointer $ Just (bhe, bha)

    -- locally run 'execTransactions' with updated blockheight data
    r <- locally (psPublicData . pdBlockHeight) (\_ -> bh) $
      testExecTransactions (Just bha) m tx

    discardCheckpointer

    return $! toPayloadWithOutputs m r

testExecValidateBlock
    :: BlockHeader
    -- ^ current header
    -> PayloadData
    -- ^ payload data for miner
    -> PactServiceM PayloadWithOutputs
testExecValidateBlock ch d = do
    miner <- decodeStrictOrThrow (_minerData $ _payloadDataMiner d)

    let bhe@(BlockHeight bh) = _blockHeight ch
        bpa = _blockParent ch
        bha = _blockHash ch

    t <- liftIO $ transactionsFromPayload d
    restoreCheckpointer $ Just (pred bhe, bpa)

    rs <- locally (psPublicData . pdBlockHeight) (\_ -> bh) $
      testExecTransactions (Just bpa) miner t

    finalizeCheckpointer $ \cp s -> save cp bhe bha s
    psStateValidated L..= Just ch
    return $! toPayloadWithOutputs miner rs

testExecTransactions
    :: Maybe BlockHash
    -> MinerInfo
    -> Vector ChainwebTransaction
    -> PactServiceM Transactions
testExecTransactions bha m txs = do
    st0 <- use psStateDb

    dbe <- liftIO . toEnv' $ _pdbsDbEnv st0
    coin <- runCoinbase bha dbe m
    txos <- testApplyCmds dbe txs

    penv <- liftIO $ toEnvPersist' dbe

    let st1 = PactDbState penv
        outs = Vector.zipWith k txs txos

    psStateDb L..= st1

    return (Transactions outs coin)
  where
    k = curry $ first (toTransactionBytes . fmap payloadBytes)

testApplyCmds
    :: Env'
    -> Vector (Command PayloadWithText)
    -> PactServiceM (Vector FullLogTxOutput)
testApplyCmds = Vector.mapM . testApplyCmd

testApplyCmd
    :: Env'
    -> Command (PayloadWithText)
    -> PactServiceM FullLogTxOutput
testApplyCmd (Env' dbe) txs = do
    env <- ask

    let l = env ^. psCheckpointEnv . cpeLogger
        pd = env ^. psPublicData
        spv = env ^. psSpvSupport
        cmd = fmap payloadObj txs

    (r, logs) <- liftIO $ applyTestCmd l pd spv cmd
    pure $ FullLogTxOutput (_crResult r) logs
  where
    applyTestCmd l pd spv cmd = do

      let pd' = set pdPublicMeta (publicMetaOf cmd) pd
      let cenv = CommandEnv Nothing Transactional dbe l freeGasEnv pd'
          rk = cmdToRequestKey cmd
          pst0 = set (evalCapabilities . capGranted) [mkMagicCap "FUND_TX", mkMagicCap "COINBASE"] def

      resultE <- tryAny $! runPayload cenv pst0 cmd spv []
      case resultE of
        Left e -> jsonErrorResult cenv rk e [] (Gas 0) $
          "test tx failure for request key while running genesis"
        Right r -> do
          logDebugRequestKey l rk "successful test tx for request key"
          pure r

    mkMagicCap c = UserCapability (ModuleName "coin" Nothing) (DefName c) []
