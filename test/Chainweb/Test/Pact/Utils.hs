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
( -- * Exceptions
  PactTestFailure(..)
  -- * test data
, ChainwebKeyPair
, SimpleKeyPair
, sender00KeyPair
, sender00
, sender01KeyPair
, sender01
, allocation00KeyPair
, allocation01KeyPair
, allocation02KeyPair
, allocation02KeyPair'
, testPactFilesDir
, testKeyPairs
, mkKeySetData
  -- * helper functions
, mergeObjects
, formatB16PubKey
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
-- * 'PactValue' helpers
, pInteger
, pString
, pDecimal
, pBool
-- * Capability helpers
, mkCapability
, mkTransferCap
, gasCap
-- * Command builder
, defaultCmd
, mkCmd
, buildCmd
, buildCwCmd
, mkExec'
, mkExec
, mkCont
, mkContMsg
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
-- * Test Pact Execution Environment
, TestPactCtx(..)
, PactTransaction(..)
, testPactCtx
, destroyTestPactCtx
, evalPactServiceM_
, withPactCtx
, withPactCtxSQLite
, testWebPactExecutionService
, testPactExecutionService
, initializeSQLite
, freeSQLiteResource
, testPactCtxSQLite
, withPact
, withTestBlockDbTest
, withPactTestBlockDb
, WithPactCtxSQLite
, defaultPactServiceConfig
-- * Mempool utils
, delegateMemPoolAccess
, withDelegateMempool
, setMempool
-- * Block formation
, runCut
, Noncer
, zeroNoncer
-- * miscellaneous
, ChainwebNetwork(..)
, dummyLogger
, epochCreationTime
, someTestVersionHeader
, someBlockHeader
) where

import Control.Arrow ((&&&))
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens (view, _3, makeLenses)
import Control.Monad
import Control.Monad.Catch

import Data.Aeson (Value(..), object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.CAS.HashMap hiding (toList)
import Data.CAS.RocksDB
import Data.Decimal
import Data.Default (def)
import Data.FileEmbed
import Data.Foldable
import Data.IORef
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text.IO as T
import Data.Tuple.Strict
import Data.String
import Data.Word

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Yaml as Y
import GHC.Generics
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
import Pact.Types.Util (toB16Text,parseB16TextOnly)

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut.Test
import Chainweb.Cut.TestBlockDb
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.InMemoryCheckpointer (initInMemoryCheckpointEnv)
import Chainweb.Pact.Backend.RelationalCheckpointer
    (initRelationalCheckpointer, initRelationalCheckpointer')
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

type SimpleKeyPair = (Text,Text)

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

sender00 :: SimpleKeyPair
sender00 = ("368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
           ,"251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898")

sender01KeyPair :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
sender01KeyPair =
    ( PubBS $ getByteString
        "6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"
    , PrivBS $ getByteString
        "2beae45b29e850e6b1882ae245b0bab7d0689ebdd0cd777d4314d24d7024b4f7"
    , "6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"
    , ED25519
    )

sender01 :: SimpleKeyPair
sender01 = ("6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"
           ,"2beae45b29e850e6b1882ae245b0bab7d0689ebdd0cd777d4314d24d7024b4f7")

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

-- | Make trivial keyset data
mkKeySetData :: Text  -> [SimpleKeyPair] -> Value
mkKeySetData name keys = object [ name .= map fst keys ]


-- Make pact 'ExecMsg' transactions specifying sender, chain id of the signer,
-- signer keys, nonce, gas rate, gas limit, and the transactions
-- (with data) to execute.
--
mkTestExecTransactions
    :: Text
      -- ^ sender
    -> P.ChainId
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
mkTestExecTransactions sender cid ks nonce0 gas gasrate ttl ct txs =
    snd <$> foldM go (0 :: Int, mempty) txs
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
          in return (succ n, Vector.snoc acc r)
        ProcFail e -> throwM $ userError e

    -- k t bs = PayloadWithText bs (_cmdPayload t)

-- | Make pact 'ContMsg' transactions, specifying sender, chain id of the signer,
-- signer keys, nonce, gas rate, gas limit, cont step, pact id, rollback,
-- proof etc.
--
mkTestContTransaction
    :: Text
      -- ^ sender
    -> P.ChainId
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
      ProcSucc t -> return $ Vector.singleton $ mkPayloadWithText <$> t
      ProcFail e -> throwM $ userError e

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

-- | Cap smart constructor.
mkCapability :: ModuleName -> Text -> [PactValue] -> SigCapability
mkCapability mn cap args = SigCapability (QualifiedName mn cap def) args

mkTransferCap :: Text -> Text -> Decimal -> SigCapability
mkTransferCap sender receiver amount = mkCapability "coin" "TRANSFER"
  [ pString sender, pString receiver, pDecimal amount ]

gasCap :: SigCapability
gasCap = mkCapability "coin" "GAS" []

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
    dieL msg = either (\s -> throwM $ userError $ msg ++ ": " ++ s) return
    toApiKp (CmdSigner Signer{..} privKey) = do
      sk <- dieL "private key" $ parseB16TextOnly privKey
      pk <- dieL "public key" $ parseB16TextOnly _siPubKey
      return $!
        ApiKeyPair (PrivBS sk) (Just (PubBS pk)) _siAddress _siScheme (Just _siCapList)

-- | 'buildCmd' variant for 'ChainwebTransaction'
buildCwCmd :: CmdBuilder -> IO ChainwebTransaction
buildCwCmd = fmap (fmap mkPayloadWithText) . buildCmd


pactTestLogger :: Bool -> Loggers
pactTestLogger showAll = initLoggers putStrLn f def
  where
    f _ b "ERROR" d = doLog (\_ -> return ()) b "ERROR" d
    f _ b "DEBUG" d | not showAll = doLog (\_ -> return ()) b "DEBUG" d
    f _ b "INFO" d | not showAll = doLog (\_ -> return ()) b "INFO" d
    f _ b "DDL" d | not showAll = doLog (\_ -> return ()) b "DDL" d
    f a b c d = doLog a b c d

mkCoinSig :: Text -> [PactValue] -> SigCapability
mkCoinSig n = SigCapability (QualifiedName (ModuleName "coin" Nothing) n def)

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

evalPactServiceM_ :: TestPactCtx cas -> PactServiceM cas a -> IO a
evalPactServiceM_ ctx pact = modifyMVar (_testPactCtxState ctx) $ \s -> do
    T2 a s' <- runPactServiceM s (_testPactCtxEnv ctx) pact
    return (s',a)

destroyTestPactCtx :: TestPactCtx cas -> IO ()
destroyTestPactCtx = void . takeMVar . _testPactCtxState

testPactCtx
    :: PayloadCas cas
    => ChainwebVersion
    -> Version.ChainId
    -> BlockHeaderDb
    -> PayloadDb cas
    -> IO (TestPactCtx cas)
testPactCtx v cid bhdb pdb = do
    cpe <- initInMemoryCheckpointEnv loggers logger
    let rs = readRewards v
        t0 = BlockCreationTime $ Time (TimeSpan (Micros 0))
    ctx <- TestPactCtx
        <$> newMVar (PactServiceState Nothing mempty 0 t0 Nothing noSPVSupport)
        <*> pure (pactServiceEnv cpe rs)
    evalPactServiceM_ ctx (initialPayloadState dummyLogger v cid)
    return ctx
  where
    loggers = pactTestLogger False -- toggle verbose pact test logging
    logger = newLogger loggers $ LogName "PactService"
    pactServiceEnv cpe rs = PactServiceEnv
        { _psMempoolAccess = Nothing
        , _psCheckpointEnv = cpe
        , _psPdb = pdb
        , _psBlockHeaderDb = bhdb
        , _psGasModel = constGasModel 0
        , _psMinerRewards = rs
        , _psReorgLimit = defaultReorgLimit
        , _psOnFatalError = defaultOnFatalError mempty
        , _psVersion = v
        , _psValidateHashesOnReplay = True
        , _psAllowReadsInLocal = False
        }

testPactCtxSQLite
  :: PayloadCas cas
  => ChainwebVersion
  -> Version.ChainId
  -> BlockHeaderDb
  -> PayloadDb cas
  -> SQLiteEnv
  -> IO (TestPactCtx cas)
testPactCtxSQLite v cid bhdb pdb sqlenv = do
    cpe <- initRelationalCheckpointer initBlockState sqlenv logger
    let rs = readRewards v
        t0 = BlockCreationTime $ Time (TimeSpan (Micros 0))
    ctx <- TestPactCtx
      <$> newMVar (PactServiceState Nothing mempty 0 t0 Nothing noSPVSupport)
      <*> pure (pactServiceEnv cpe rs)
    evalPactServiceM_ ctx (initialPayloadState dummyLogger v cid)
    return ctx
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
        , _psReorgLimit = defaultReorgLimit
        , _psOnFatalError = defaultOnFatalError mempty
        , _psVersion = v
        , _psValidateHashesOnReplay = True
        , _psAllowReadsInLocal = False
        }


-- | A test PactExecutionService for a single chain
--
testPactExecutionService
    :: PayloadCas cas
    => ChainwebVersion
    -> Version.ChainId
    -> IO BlockHeaderDb
    -> IO (PayloadDb cas)
    -> MemPoolAccess
       -- ^ transaction generator
    -> SQLiteEnv
    -> IO PactExecutionService
testPactExecutionService v cid bhdbIO pdbIO mempoolAccess sqlenv = do
    bhdb <- bhdbIO
    pdb <- pdbIO
    ctx <- testPactCtxSQLite v cid bhdb pdb sqlenv
    return $ PactExecutionService
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

-- | A test PactExecutionService for a chainweb
--
testWebPactExecutionService
    :: PayloadCas cas
    => ChainwebVersion
    -> IO WebBlockHeaderDb
    -> IO (PayloadDb cas)
    -> (Version.ChainId -> MemPoolAccess)
       -- ^ transaction generator
    -> [SQLiteEnv]
    -> IO WebPactExecutionService
testWebPactExecutionService v webdbIO pdbIO mempoolAccess sqlenvs
    = fmap mkWebPactExecutionService
    $ fmap HM.fromList
    $ traverse mkPact
    $ zip sqlenvs
    $ toList
    $ chainIds v
  where
    mkPact (sqlenv, c) = do
        webdb <- webdbIO
        let bhdbs = view webBlockHeaderDb webdb
        let bhdb = fromJuste $ HM.lookup c bhdbs
        let bhdbIO = return bhdb
        (c,) <$> testPactExecutionService v c bhdbIO pdbIO (mempoolAccess c) sqlenv

type Noncer = ChainId -> IO Nonce

zeroNoncer :: Noncer
zeroNoncer = const (return $ Nonce 0)

-- | Populate blocks for every chain of the current cut. Uses provided pact
-- service to produce a new block, add it
runCut :: ChainwebVersion -> TestBlockDb -> WebPactExecutionService -> GenBlockTime -> Noncer -> IO ()
runCut v bdb pact genTime noncer =
  forM_ (chainIds v) $ \cid -> do
    ph <- ParentHeader <$> getParentTestBlockDb bdb cid
    pout <- _webPactNewBlock pact noMiner ph
    n <- noncer cid
    addTestBlockDb bdb n genTime cid pout
    h <- getParentTestBlockDb bdb cid
    void $ _webPactValidateBlock pact h (payloadWithOutputsToPayloadData pout)

-- | This enforces that only a single test can use the pact context at a time.
-- It's up to the user to ensure that tests are scheduled in the right order.
--
withPactCtx
    :: PayloadCas cas
    => ChainwebVersion
    -> IO BlockHeaderDb
    -> IO (PayloadDb cas)
    -> ((forall a . PactServiceM cas a -> IO a) -> TestTree)
    -> TestTree
withPactCtx v bhdbIO pdbIO f =
    withResource start destroyTestPactCtx $
    \ctxIO -> f $ \pact -> do
        ctx <- ctxIO
        evalPactServiceM_ ctx pact
  where
    start = do
        bhdb <- bhdbIO
        pdb <- pdbIO
        testPactCtx v (someChainId v) bhdb pdb

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

type WithPactCtxSQLite cas = forall a . (PactDbEnv' -> PactServiceM cas a) -> IO a


withPactCtxSQLite
  :: PayloadCas cas
  => ChainwebVersion
  -> IO BlockHeaderDb
  -> IO (PayloadDb cas)
  -> Maybe GasModel
  -> PactServiceConfig
  -> (WithPactCtxSQLite cas -> TestTree)
  -> TestTree
withPactCtxSQLite v bhdbIO pdbIO gasModel config f =
  withResource
    initializeSQLite
    freeSQLiteResource $ \io ->
      withResource (start io) (destroy io) $ \ctxIO -> f $ \toPact -> do
          (ctx, dbSt) <- ctxIO
          evalPactServiceM_ ctx (toPact dbSt)
  where
    destroy = const (destroyTestPactCtx . fst)
    start ios = do
        let loggers = pactTestLogger False
            logger = newLogger loggers $ LogName "PactService"
            cid = someChainId v

        bhdb <- bhdbIO
        pdb <- pdbIO
        (_,s) <- ios
        (dbSt, cpe) <- initRelationalCheckpointer' initBlockState s logger
        let rs = readRewards v
            t0 = BlockCreationTime $ Time (TimeSpan (Micros 0))
            gm = fromMaybe (constGasModel 0) gasModel
        !ctx <- TestPactCtx
          <$!> newMVar (PactServiceState Nothing mempty 0 t0 Nothing noSPVSupport)
          <*> pure (pactServiceEnv cpe pdb bhdb gm rs)
        evalPactServiceM_ ctx (initialPayloadState dummyLogger v cid)
        return (ctx, dbSt)
      where
        pactServiceEnv cpe pdb bhdb gm rs = PactServiceEnv
            { _psMempoolAccess = Nothing
            , _psCheckpointEnv = cpe
            , _psPdb = pdb
            , _psBlockHeaderDb = bhdb
            , _psGasModel = gm
            , _psMinerRewards = rs
            , _psReorgLimit = fromIntegral $ _pactReorgLimit config
            , _psOnFatalError = defaultOnFatalError mempty
            , _psVersion = v
            , _psValidateHashesOnReplay = _pactRevalidate config
            , _psAllowReadsInLocal = _pactAllowReadsInLocal config
            }

withMVarResource :: a -> (IO (MVar a) -> TestTree) -> TestTree
withMVarResource value = withResource (newMVar value) (const $ return ())

withTime :: (IO (Time Micros) -> TestTree) -> TestTree
withTime = withResource getCurrentTimeIntegral (const (return ()))

mkKeyset :: Text -> [PublicKeyBS] -> Value
mkKeyset p ks = object
  [ "pred" .= p
  , "keys" .= ks
  ]

stockKeyFile :: ByteString
stockKeyFile = $(embedFile "pact/genesis/devnet/keys.yaml")

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

toTxCreationTime :: Integral a => Time a -> TxCreationTime
toTxCreationTime (Time timespan) = case timeSpanToSeconds timespan of
          Seconds s -> TxCreationTime $ ParsedInteger s

withPayloadDb :: (IO (PayloadDb HashMapCas) -> TestTree) -> TestTree
withPayloadDb = withResource newPayloadDb (\_ -> return ())


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

-- | use a "delegate" which you can dynamically reset/modify
withDelegateMempool
  :: (IO (IORef MemPoolAccess, MemPoolAccess) -> TestTree)
  -> TestTree
withDelegateMempool = withResource start (const mempty)
  where
    start = (id &&& delegateMemPoolAccess) <$> newIORef mempty

-- | Set test mempool
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
  withResource (start rdb) (const $ return ()) a
  where
    start r = r >>= mkTestBlockDb v


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

withPact
    :: ChainwebVersion
    -> LogLevel
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> MemPoolAccess
    -> IO FilePath
    -> Word64
    -> (IO PactQueue -> TestTree)
    -> TestTree
withPact version logLevel iopdb iobhdb mempool iodir deepForkLimit f =
    withResource startPact stopPact $ f . fmap (view _3)
  where
    startPact = do
        reqQ <- atomically $ newTBQueue 2000
        pdb <- iopdb
        bhdb <- iobhdb
        dir <- iodir
        sqlEnv <- startSqliteDb version cid logger (Just dir) Nothing False
        let pactConfig = defaultPactServiceConfig { _pactReorgLimit = fromIntegral deepForkLimit }

        a <- async $
             initPactService version cid logger reqQ mempool bhdb pdb sqlEnv pactConfig
        return (a, sqlEnv, reqQ)

    stopPact (a, sqlEnv, _) = cancel a >> stopSqliteDb sqlEnv

    logger = genericLogger logLevel T.putStrLn
    cid = someChainId version

newtype ChainwebNetwork = ChainwebNetwork { _getClientEnv :: ClientEnv }

dummyLogger :: GenericLogger
dummyLogger = genericLogger Quiet T.putStrLn

someTestVersion :: ChainwebVersion
someTestVersion = FastTimedCPM peterson

someTestVersionHeader :: BlockHeader
someTestVersionHeader = someBlockHeader someTestVersion 10

epochCreationTime :: BlockCreationTime
epochCreationTime = BlockCreationTime epoch

-- | The runtime is linear in the requested height. This can is slow if a large
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
