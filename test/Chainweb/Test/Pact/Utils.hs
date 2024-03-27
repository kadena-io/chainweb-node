{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

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
, sender02WebAuthn
, sender02WebAuthnPrefixed
, sender03WebAuthn
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
, buildCwCmd
, buildTextCmd
, mkExec'
, mkExec
, mkCont
, mkContMsg
, ContMsg (..)
, mkEd25519Signer
, mkEd25519Signer'
, mkWebAuthnSigner
, mkWebAuthnSigner'
, CmdBuilder(..)
, cbSigners
, cbVerifiers
, cbRPC
, cbNonce
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
, withPactTestBlockDb'
, withWebPactExecutionService
, withPactCtxSQLite
, WithPactCtxSQLite
-- * Other service creation
, initializeSQLite
, freeSQLiteResource
, freeGasModel
, testPactServiceConfig
, withBlockHeaderDb
, withTemporaryDir
, withSqliteDb
-- * Mempool utils
, delegateMemPoolAccess
, withDelegateMempool
, setMempool
, setOneShotMempool
-- * Block formation
, runCut
, Noncer
, zeroNoncer
-- * Pact State
, compact
, compactUntilAvailable
, PactRow(..)
, getLatestPactState
, getPactUserTables
-- * miscellaneous
, toTxCreationTime
, dummyLogger
, hunitDummyLogger
, pactTestLogger
, someTestVersionHeader
, someBlockHeader
, testPactFilesDir
, getPWOByHeader

) where

import Control.Arrow ((&&&))
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Aeson (Value(..), object, (.=), Key)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as BS
import Data.Decimal
import Data.Default (def)
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List qualified as List
import Data.LogMessage
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.String
import qualified Data.Vector as V

import Database.SQLite3.Direct (Database)

import GHC.Generics
import GHC.IO.Exception(IOException(..))

import Streaming.Prelude qualified as S
import System.Directory
import System.IO.Temp (createTempDirectory)
import qualified System.Logger as YAL
import System.LogLevel

import Test.Tasty

-- internal pact modules

import Pact.ApiReq (ApiKeyPair(..), mkKeyPairs)
import Pact.Gas
import Pact.JSON.Legacy.Value
import Pact.Types.Capability
import qualified Pact.Types.ChainId as P
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Exp
import Pact.Types.Gas
import Pact.Types.Hash
import Pact.Types.KeySet
import qualified Pact.Types.Logger as P
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.RPC
import Pact.Types.Runtime (PactEvent(..))
import Pact.Types.Term
import Pact.Types.Util (parseB16TextOnly)
import Pact.Types.Verifier

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState qualified as PactState
import Chainweb.Pact.Backend.PactState (TableDiffable(..), Table(..), PactRow(..))
import Chainweb.Pact.Backend.RelationalCheckpointer (initRelationalCheckpointer)
import Chainweb.Pact.Backend.SQLite.DirectV2
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils hiding (withSqliteDb)
import Chainweb.Pact.PactService
import Chainweb.Pact.RestAPI.Server (validateCommand)
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Utils
import Chainweb.Test.Utils.BlockHeader
import Chainweb.Test.TestVersions
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..), chainIds)
import qualified Chainweb.Version as Version
import Chainweb.Version.Utils (someChainId)
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Chainweb.Storage.Table.RocksDB

-- ----------------------------------------------------------------------- --
-- Keys

type SimpleKeyPair = (Text,Text)

testPactFilesDir :: FilePath
testPactFilesDir = "test/pact/"

sender00 :: SimpleKeyPair
sender00 = ("368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
           ,"251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898")

sender01 :: SimpleKeyPair
sender01 = ("6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"
           ,"2beae45b29e850e6b1882ae245b0bab7d0689ebdd0cd777d4314d24d7024b4f7")

sender02WebAuthnPrefixed :: SimpleKeyPair
sender02WebAuthnPrefixed =
           ("WEBAUTHN-a4010103272006215820c18831c6f15306d6271e154842906b68f26c1af79b132dde6f6add79710303bf"
           ,"fecd4feb1243d715d095e24713875ca76c476f8672ec487be8e3bc110dd329ab")

sender02WebAuthn :: SimpleKeyPair
sender02WebAuthn =
           ("a4010103272006215820c18831c6f15306d6271e154842906b68f26c1af79b132dde6f6add79710303bf"
           ,"fecd4feb1243d715d095e24713875ca76c476f8672ec487be8e3bc110dd329ab")

sender03WebAuthn :: SimpleKeyPair
sender03WebAuthn =
           ("a4010103272006215820ad72392508272b4c45536976474cdd434e772bfd630738ee9aac7343e7222eb6"
           ,"ebe7d1119a53863fa64be7347d82d9fcc9ebeb8cbbe480f5e8642c5c36831434")

allocation00KeyPair :: SimpleKeyPair
allocation00KeyPair =
    ( "d82d0dcde9825505d86afb6dcc10411d6b67a429a79e21bda4bb119bf28ab871"
    , "c63cd081b64ae9a7f8296f11c34ae08ba8e1f8c84df6209e5dee44fa04bcb9f5"
    )


-- | Make trivial keyset data
mkKeySetData :: Key  -> [SimpleKeyPair] -> Value
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
makeLenses ''CmdSigner

-- | Make ED25519 signer.
mkEd25519Signer :: Text -> Text -> [SigCapability] -> CmdSigner
mkEd25519Signer pubKey privKey caps = CmdSigner
  { _csSigner = signer
  , _csPrivKey = privKey
  }
  where
    signer = Signer
      { _siScheme = Nothing
      , _siPubKey = pubKey
      , _siAddress = Nothing
      , _siCapList = caps }

mkEd25519Signer' :: SimpleKeyPair -> [SigCapability] -> CmdSigner
mkEd25519Signer' (pub,priv) = mkEd25519Signer pub priv

mkWebAuthnSigner :: Text -> Text -> [SigCapability] -> CmdSigner
mkWebAuthnSigner pubKey privKey caps = CmdSigner
  { _csSigner = signer
  , _csPrivKey = privKey
  }
  where
    signer = Signer
      { _siScheme = Just WebAuthn
      , _siPubKey = pubKey
      , _siAddress = Nothing
      , _siCapList = caps }

mkWebAuthnSigner' :: SimpleKeyPair -> [SigCapability] -> CmdSigner
mkWebAuthnSigner' (pub, priv) caps = mkWebAuthnSigner pub priv caps

-- | Chainweb-oriented command builder.
data CmdBuilder = CmdBuilder
  { _cbSigners :: ![CmdSigner]
  , _cbVerifiers :: ![Verifier ParsedVerifierProof]
  , _cbRPC :: !(PactRPC Text)
  , _cbNonce :: !Text
  , _cbChainId :: !ChainId
  , _cbSender :: !Text
  , _cbGasLimit :: !GasLimit
  , _cbGasPrice :: !GasPrice
  , _cbTTL :: !TTLSeconds
  , _cbCreationTime :: !TxCreationTime
  } deriving (Eq,Show,Generic)
makeLenses ''CmdBuilder

-- | Make code-only Exec PactRPC
mkExec' :: Text -> PactRPC Text
mkExec' ecode = mkExec ecode Null

-- | Make Exec PactRPC
mkExec :: Text -> Value -> PactRPC Text
mkExec ecode edata = Exec $ ExecMsg ecode (toLegacyJson edata)

mkCont :: ContMsg -> PactRPC Text
mkCont = Continuation

mkContMsg :: PactId -> Int -> ContMsg
mkContMsg pid step = ContMsg
  { _cmPactId = pid
  , _cmStep = step
  , _cmRollback = False
  , _cmData = toLegacyJson Null
  , _cmProof = Nothing }

-- | Default builder.
defaultCmd :: CmdBuilder
defaultCmd = CmdBuilder
  { _cbSigners = []
  , _cbVerifiers = []
  , _cbRPC = mkExec' "1"
  , _cbNonce = "nonce"
  , _cbChainId = unsafeChainId 0
  , _cbSender = "sender00"
  , _cbGasLimit = 10_000
  , _cbGasPrice = 0.000_1
  , _cbTTL = 300 -- 5 minutes
  , _cbCreationTime = 0 -- epoch
  }

-- | Build parsed + verified Pact command
--
-- TODO: Use the new `assertCommand` function.
buildCwCmd :: (MonadThrow m, MonadIO m) => Text -> ChainwebVersion -> CmdBuilder -> m ChainwebTransaction
buildCwCmd nonce v cmd = buildRawCmd nonce v cmd >>= \(c :: Command ByteString) ->
  case validateCommand v (_cbChainId cmd) (T.decodeUtf8 <$> c) of
    Left err -> throwM $ userError $ "buildCmd failed: " ++ err
    Right cmd' -> return cmd'

-- | Build unparsed, unverified command
--
buildTextCmd :: Text -> ChainwebVersion -> CmdBuilder -> IO (Command Text)
buildTextCmd nonce v = fmap (fmap T.decodeUtf8) . buildRawCmd nonce v

-- | Build a raw bytestring command
--
buildRawCmd :: (MonadThrow m, MonadIO m) => Text -> ChainwebVersion -> CmdBuilder -> m (Command ByteString)
buildRawCmd nonce v (set cbNonce nonce -> CmdBuilder{..}) = do
    kps <- liftIO $ traverse mkDynKeyPairs _cbSigners
    cmd <- liftIO $ mkCommandWithDynKeys kps _cbVerifiers pm _cbNonce (Just nid) _cbRPC
    pure cmd
  where
    nid = P.NetworkId (sshow v)
    cid = fromString $ show (chainIdInt _cbChainId :: Int)
    pm = PublicMeta cid _cbSender _cbGasLimit _cbGasPrice _cbTTL _cbCreationTime

dieL :: MonadThrow m => [Char] -> Either [Char] a -> m a
dieL msg = either (\s -> throwM $ userError $ msg ++ ": " ++ s) return

mkDynKeyPairs :: MonadThrow m => CmdSigner -> m (DynKeyPair, [SigCapability])
mkDynKeyPairs (CmdSigner Signer{..} privKey) =
  case (fromMaybe ED25519 _siScheme, _siPubKey, privKey) of
    (ED25519, pub, priv) -> do
      pub' <- either diePubKey return $ parseEd25519PubKey =<< parseB16TextOnly pub
      priv' <- either diePrivKey return $ parseEd25519SecretKey =<< parseB16TextOnly priv
      return $ (DynEd25519KeyPair (pub', priv'), _siCapList)

    (WebAuthn, pub, priv) -> do
      let (pubKeyStripped, wasPrefixed) = fromMaybe
            (pub, WebAuthnPubKeyBare)
            ((,WebAuthnPubKeyPrefixed) <$> T.stripPrefix webAuthnPrefix pub)
      pubWebAuthn <-
        either diePubKey return (parseWebAuthnPublicKey =<< parseB16TextOnly pubKeyStripped)
      privWebAuthn <-
        either diePrivKey return (parseWebAuthnPrivateKey =<< parseB16TextOnly priv)
      return $ (DynWebAuthnKeyPair wasPrefixed pubWebAuthn privWebAuthn, _siCapList)
  where
    diePubKey str = error $ "pubkey: " <> str
    diePrivKey str = error $ "privkey: " <> str

toApiKp :: MonadThrow m => CmdSigner -> m ApiKeyPair
toApiKp (CmdSigner Signer{..} privKey) = do
  sk <- dieL "private key" $ parseB16TextOnly privKey
  pk <- dieL "public key" $ parseB16TextOnly _siPubKey
  let keyPair = ApiKeyPair (PrivBS sk) (Just (PubBS pk)) _siAddress _siScheme (Just _siCapList)
  return $! keyPair

-- | Legacy; better to use 'CmdSigner'/'CmdBuilder'.
-- if caps are empty, gas cap is implicit. otherwise it must be included
testKeyPairs :: SimpleKeyPair -> Maybe [SigCapability] -> IO [(DynKeyPair, [SigCapability])]
testKeyPairs skp capsm = do
  kp <- toApiKp $ mkEd25519Signer' skp (fromMaybe [] capsm)
  mkKeyPairs [kp]

-- ----------------------------------------------------------------------- --
-- Service creation utilities

pactTestLogger :: (String -> IO ()) -> Bool -> P.Loggers
pactTestLogger backend showAll = P.initLoggers backend f def
  where
    f :: (String -> IO ()) -> P.LogName -> String -> String -> IO ()
    f _ b "ERROR" d = P.doLog (\_ -> return ()) b "ERROR" d
    f _ b "DEBUG" d | not showAll = P.doLog (\_ -> return ()) b "DEBUG" d
    f _ b "INFO" d | not showAll = P.doLog (\_ -> return ()) b "INFO" d
    f _ b "DDL" d | not showAll = P.doLog (\_ -> return ()) b "DDL" d
    f a b c d = P.doLog a b c d

-- | Test Pact Execution Context for running inside 'PactServiceM'.
-- Only used internally.
data TestPactCtx logger tbl = TestPactCtx
    { _testPactCtxState :: !(MVar PactServiceState)
    , _testPactCtxEnv :: !(PactServiceEnv logger tbl)
    }

evalPactServiceM_ :: TestPactCtx logger tbl -> PactServiceM logger tbl a -> IO a
evalPactServiceM_ ctx pact = modifyMVar (_testPactCtxState ctx) $ \s -> do
    T2 a s' <- runPactServiceM s (_testPactCtxEnv ctx) pact
    return (s',a)

destroyTestPactCtx :: TestPactCtx logger tbl -> IO ()
destroyTestPactCtx = void . takeMVar . _testPactCtxState

-- | setup TestPactCtx, internal function.
-- Use 'withPactCtxSQLite' in tests.
testPactCtxSQLite
  :: forall logger tbl. (Logger logger, CanReadablePayloadCas tbl)
  => logger
  -> ChainwebVersion
  -> Version.ChainId
  -> BlockHeaderDb
  -> PayloadDb tbl
  -> SQLiteEnv
  -> PactServiceConfig
  -> (TxContext -> GasModel)
  -> IO (TestPactCtx logger tbl)
testPactCtxSQLite logger v cid bhdb pdb sqlenv conf gasmodel = do
    cp <- initRelationalCheckpointer defaultModuleCacheLimit sqlenv DoNotPersistIntraBlockWrites cpLogger v cid
    let rs = readRewards
    !ctx <- TestPactCtx
      <$!> newMVar (PactServiceState mempty)
      <*> pure (mkPactServiceEnv cp rs)
    evalPactServiceM_ ctx (initialPayloadState mempty v cid)
    return ctx
  where
    cpLogger = addLabel ("chain-id", chainIdToText cid) $ addLabel ("sub-component", "checkpointer") $ logger
    mkPactServiceEnv :: Checkpointer logger -> MinerRewards -> PactServiceEnv logger tbl
    mkPactServiceEnv cp rs = PactServiceEnv
        { _psMempoolAccess = Nothing
        , _psCheckpointer = cp
        , _psPdb = pdb
        , _psBlockHeaderDb = bhdb
        , _psGasModel = gasmodel
        , _psMinerRewards = rs
        , _psReorgLimit = _pactReorgLimit conf
        , _psPreInsertCheckTimeout = _pactPreInsertCheckTimeout conf
        , _psOnFatalError = defaultOnFatalError mempty
        , _psVersion = v
        , _psAllowReadsInLocal = _pactAllowReadsInLocal conf
        , _psLogger = addLabel ("chain-id", chainIdToText cid) $ addLabel ("component", "pact") $ _cpLogger $ _cpReadCp cp
        , _psGasLogger = do
            guard (_pactLogGas conf)
            return
                $ addLabel ("chain-id", chainIdToText cid)
                $ addLabel ("component", "pact")
                $ addLabel ("sub-component", "gas")
                $ _cpLogger $ _cpReadCp cp

        , _psBlockGasLimit = _pactBlockGasLimit conf
        , _psEnableLocalTimeout = False
        }

freeGasModel :: TxContext -> GasModel
freeGasModel = const $ constGasModel 0

-- | A queue-less WebPactExecutionService (for all chains)
-- with direct chain access map for local.
withWebPactExecutionService
    :: (Logger logger)
    => logger
    -> ChainwebVersion
    -> PactServiceConfig
    -> TestBlockDb
    -> MemPoolAccess
    -> (TxContext -> GasModel)
    -> ((WebPactExecutionService,HM.HashMap ChainId (SQLiteEnv, PactExecutionService)) -> IO a)
    -> IO a
withWebPactExecutionService logger v pactConfig bdb mempoolAccess gasmodel act =
  withDbs $ \sqlenvs -> do
    pacts <- fmap HM.fromList
           $ traverse (\(dbEnv, cid) -> (cid,) . (dbEnv,) <$> mkPact dbEnv cid)
           $ zip sqlenvs
           $ toList
           $ chainIds v
    act (mkWebPactExecutionService (snd <$> pacts), pacts)
  where
    withDbs f = foldl' (\soFar _ -> withDb soFar) f (chainIds v) []
    withDb g envs = withTempSQLiteConnection chainwebPragmas $ \s -> g (s : envs)

    mkPact :: SQLiteEnv -> ChainId -> IO PactExecutionService
    mkPact sqlenv c = do
        bhdb <- getBlockHeaderDb c bdb
        ctx <- testPactCtxSQLite logger v c bhdb (_bdbPayloadDb bdb) sqlenv pactConfig gasmodel
        return $ PactExecutionService
          { _pactNewBlock = \_ m fill ->
              evalPactServiceM_ ctx $ NewBlockInProgress <$> execNewBlock mempoolAccess m fill
          , _pactContinueBlock = \_ bip ->
              evalPactServiceM_ ctx $ execContinueBlock mempoolAccess bip
          , _pactValidateBlock = \h d ->
              evalPactServiceM_ ctx $ fst <$> execValidateBlock mempoolAccess h d
          , _pactLocal = \pf sv rd cmd ->
              evalPactServiceM_ ctx $ execLocal cmd pf sv rd
          , _pactLookup = \_cid cd hashes ->
              evalPactServiceM_ ctx $ execLookupPactTxs cd hashes
          , _pactPreInsertCheck = \_ txs ->
              evalPactServiceM_ ctx $ V.map (() <$) <$> execPreInsertCheckReq txs
          , _pactBlockTxHistory = \h d ->
              evalPactServiceM_ ctx $ execBlockTxHistory h d
          , _pactHistoricalLookup = \h d k ->
              evalPactServiceM_ ctx $ execHistoricalLookup h d k
          , _pactSyncToBlock = \h ->
              evalPactServiceM_ ctx $ execSyncToBlock h
          , _pactReadOnlyReplay = \l u ->
              evalPactServiceM_ ctx $ execReadOnlyReplay l u
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
    newBlock <- _webPactNewBlock pact cid miner NewBlockFill
    let ph = newBlockParentHeader newBlock
    let pout = newBlockToPayloadWithOutputs newBlock
    n <- noncer cid

    -- skip this chain if mining fails and retry with the next chain.
    whenM (addTestBlockDb bdb (succ $ _blockHeight $ _parentHeader ph) n genTime cid pout) $ do
        h <- getParentTestBlockDb bdb cid
        void $ _webPactValidateBlock pact h (CheckablePayloadWithOutputs pout)

initializeSQLite :: IO SQLiteEnv
initializeSQLite = open2 file >>= \case
    Left (_err, _msg) ->
        internalError "initializeSQLite: A connection could not be opened."
    Right r -> return r
  where
    file = "" {- temporary sqlitedb -}

freeSQLiteResource :: SQLiteEnv -> IO ()
freeSQLiteResource sqlenv = void $ close_v2 sqlenv

type WithPactCtxSQLite logger tbl = forall a . PactServiceM logger tbl a -> IO a

-- | Used to run 'PactServiceM' functions directly on a database (ie not use checkpointer).
withPactCtxSQLite
  :: (Logger logger, CanReadablePayloadCas tbl)
  => logger
  -> ChainwebVersion
  -> IO BlockHeaderDb
  -> IO (PayloadDb tbl)
  -> PactServiceConfig
  -> (WithPactCtxSQLite logger tbl -> TestTree)
  -> TestTree
withPactCtxSQLite logger v bhdbIO pdbIO conf f =
  withResource
    initializeSQLite
    freeSQLiteResource $ \io ->
      withResource (start io) destroy $ \ctxIO -> f $ \toPact -> do
          ctx <- ctxIO
          evalPactServiceM_ ctx toPact
  where
    destroy = destroyTestPactCtx
    cid = someChainId v
    start ios = do
        bhdb <- bhdbIO
        pdb <- pdbIO
        s <- ios
        testPactCtxSQLite logger v cid bhdb pdb s conf freeGasModel

toTxCreationTime :: Integral a => Time a -> TxCreationTime
toTxCreationTime (Time timespan) = TxCreationTime $ fromIntegral $ timeSpanToSeconds timespan

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
withDelegateMempool = withResource' start
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

-- | Single-chain Pact via service queue.
--
--   The difference between this and 'withPactTestBlockDb' is that,
--   this function takes a `SQLiteEnv` resource which it then exposes
--   to the test function.
--
--   TODO: Consolidate these two functions.
withPactTestBlockDb'
    :: ChainwebVersion
    -> ChainId
    -> RocksDb
    -> IO SQLiteEnv
    -> IO MemPoolAccess
    -> PactServiceConfig
    -> (IO (SQLiteEnv,PactQueue,TestBlockDb) -> TestTree)
    -> TestTree
withPactTestBlockDb' version cid rdb sqlEnvIO mempoolIO pactConfig f =
  withResource' (mkTestBlockDb version rdb) $ \bdbio ->
  withResource (startPact bdbio) stopPact $ f . fmap (view _2)
  where
    startPact bdbio = do
        reqQ <- newPactQueue 2000
        bdb <- bdbio
        sqlEnv <- sqlEnvIO
        mempool <- mempoolIO
        bhdb <- getWebBlockHeaderDb (_bdbWebBlockHeaderDb bdb) cid
        let pdb = _bdbPayloadDb bdb
        a <- async $ runForever (\_ _ -> return ()) "Chainweb.Test.Pact.Utils.withPactTestBlockDb" $
            runPactService version cid logger reqQ mempool bhdb pdb sqlEnv pactConfig
        return (a, (sqlEnv,reqQ,bdb))

    stopPact (a, _) = cancel a

    -- Ideally, we should throw 'error' when the logger is invoked, because
    -- error logs should not happen in production and should always be resolved.
    -- Unfortunately, that's not yet always the case. So we just drop the
    -- message.
    --
    logger = genericLogger Error (\_ -> return ())

withSqliteDb :: ()
  => ChainId
  -> IO FilePath
  -> (IO SQLiteEnv -> TestTree)
  -> TestTree
withSqliteDb cid iodir s = withResource start stop s
  where
    start = do
      dir <- iodir
      startSqliteDb cid logger dir False

    stop env = do
      stopSqliteDb env

   -- Ideally, we should throw 'error' when the logger is invoked, because
    -- error logs should not happen in production and should always be resolved.
    -- Unfortunately, that's not yet always the case. So we just drop the
    -- message.
    --
    logger = genericLogger Error (\_ -> return ())

-- | Single-chain Pact via service queue.
withPactTestBlockDb
    :: ChainwebVersion
    -> ChainId
    -> RocksDb
    -> IO MemPoolAccess
    -> PactServiceConfig
    -> (IO (SQLiteEnv,PactQueue,TestBlockDb) -> TestTree)
    -> TestTree
withPactTestBlockDb version cid rdb mempoolIO pactConfig f =
  withTemporaryDir $ \iodir ->
  withResource' (mkTestBlockDb version rdb) $ \bdbio ->
  withResource (startPact bdbio iodir) stopPact $ f . fmap (view _2)
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
        return (a, (sqlEnv,reqQ,bdb))

    stopPact (a, (sqlEnv, _, _)) = cancel a >> stopSqliteDb sqlEnv

    -- Ideally, we should throw 'error' when the logger is invoked, because
    -- error logs should not happen in production and should always be resolved.
    -- Unfortunately, that's not yet always the case. So we just drop the
    -- message.
    --
    logger = genericLogger Error (\_ -> return ())

dummyLogger :: GenericLogger
dummyLogger = genericLogger Error (error . T.unpack)

hunitDummyLogger :: (String -> IO ()) -> GenericLogger
hunitDummyLogger f = genericLogger Error (f . T.unpack)

someTestVersion :: ChainwebVersion
someTestVersion = instantCpmTestVersion petersonChainGraph

someTestVersionHeader :: BlockHeader
someTestVersionHeader = someBlockHeader someTestVersion 10

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

-- | Get all pact user tables.
--
--   Note: This consumes a stream. If you are writing a test
--   with very large pact states (think: Gigabytes), use
--   the streaming version of this function from
--   'Chainweb.Pact.Backend.PactState'.
getPactUserTables :: Database -> IO (Map Text [PactRow])
getPactUserTables db = do
  S.foldM_
    (\m tbl -> pure (M.insert tbl.name tbl.rows m))
    (pure M.empty)
    pure
    (PactState.getPactTables db)

-- | Get active/latest pact state.
--
--   Note: This consumes a stream. If you are writing a test
--   with very large pact states (think: Gigabytes), use
--   the streaming version of this function from
--   'Chainweb.Pact.Backend.PactState'.
getLatestPactState :: Database -> IO (Map Text (Map ByteString ByteString))
getLatestPactState db = do
  S.foldM_
    (\m td -> pure (M.insert td.name td.rows m))
    (pure M.empty)
    pure
    (PactState.getLatestPactStateDiffable db)

locateTarget :: ()
  => SQLiteEnv
  -> C.TargetBlockHeight
  -> IO BlockHeight
locateTarget db = \case
  C.Target height -> do
    PactState.ensureBlockHeightExists db height
    pure height
  C.LatestUnsafe -> do
    PactState.getLatestBlockHeight db
  C.LatestSafe -> do
    latest <- PactState.getLatestBlockHeight db
    earliest <- PactState.getEarliestBlockHeight db

    let safeDepth = 1_000

    when (latest - earliest < safeDepth) $ do
      error "not enough history for Compaction.LatestSafe"

    pure (latest - safeDepth)

-- | Compaction utility for testing.
--   Most of the time the flags will be ['C.NoVacuum']
compact :: ()
  => LogLevel
  -> [C.CompactFlag]
  -> SQLiteEnv
  -> C.TargetBlockHeight
  -> IO ()
compact logLevel cFlags db target = do
  C.withDefaultLogger logLevel $ \logger -> do
    height <- locateTarget db target
    void $ C.compact height logger db cFlags

-- | Compaction function that retries until the database is available.
compactUntilAvailable
  :: C.TargetBlockHeight
  -> YAL.Logger SomeLogMessage
  -> SQLiteEnv
  -> [C.CompactFlag]
  -> IO ()
compactUntilAvailable target logger db flags = do
  height <- locateTarget db target
  go height
  where
    go h = do
      r <- try (C.compact h logger db flags)
      case r of
        Right _ -> pure ()
        Left err
          | C.CompactExceptionDb e <- err
          , Just ioErr <- fromException e
            -- someone, somewhere, is calling "show" on an exception
          , "ErrorBusy" `List.isInfixOf` ioe_description ioErr
          -> putStrLn "Retrying compaction" >> go h
          | otherwise -> throwM err

getPWOByHeader :: BlockHeader -> TestBlockDb -> IO PayloadWithOutputs
getPWOByHeader h (TestBlockDb _ pdb _) =
  lookupPayloadWithHeight pdb (Just $ _blockHeight h) (_blockPayloadHash h) >>= \case
    Nothing -> throwM $ userError "getPWOByHeader: payload not found"
    Just pwo -> return pwo
