{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Chainweb.Pact.RestAPI.Server
( PactServerData(..)
, PactServerData_
, PactCmdLog(..)
, SomePactServerData(..)
, somePactServerData
, pactServer
, sendHandler
, pollHandler
, listenHandler
, localHandler
, spvHandler
, somePactServer
, somePactServers
, newPactServer
, validateCommand

, sendReq
, pollReq
, listenReq
, localReq
, preflightReq
, spv2Req
, spvReq
, ethSpvReq
) where

import Control.Applicative
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Reader hiding (local)
import Control.Monad.State.Strict
import Control.Monad.Trans.Except (ExceptT, runExceptT, except)

import Data.Aeson as Aeson
import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Short as SB
import Data.Default (def)
import Data.Foldable
import Data.Function
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Singletons
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Vector (Vector)
import qualified Data.Vector as V

import Ethereum.Block
import Ethereum.Header
import Ethereum.Misc (bytes)
import Ethereum.Receipt
import Ethereum.Receipt.ReceiptProof
import Ethereum.RLP (putRlpByteString)

import GHC.Generics
import GHC.Stack

import Numeric.Natural

import Prelude hiding (init, lookup)

import Servant

import qualified Streaming.Prelude as S

import System.LogLevel

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Cut
import qualified Chainweb.CutDB as CutDB
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Mempool.Mempool
    (InsertError(..), InsertType(..), MempoolBackend(..), TransactionHash(..), requestKeyToTransactionHash)
import Chainweb.Pact.RestAPI
import Chainweb.Pact.RestAPI.EthSpv
import Chainweb.Pact.RestAPI.SPV
import Chainweb.Pact.Service.Types
import Chainweb.Pact.SPV
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.SPV (SpvException(..))
import Chainweb.SPV.CreateProof
import Chainweb.SPV.EventProof
import Chainweb.SPV.OutputProof
import Chainweb.SPV.PayloadProof
import Chainweb.Transaction
import qualified Chainweb.TreeDB as TreeDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Pact.Validations (assertCommand)
import Chainweb.Version.Guards (isWebAuthnPrefixLegal, pactParserVersion, validPPKSchemes)
import Chainweb.WebPactExecutionService

import qualified Pact.JSON.Encode as J
import qualified Pact.Parse as Pact
import Pact.Types.API
import qualified Pact.Types.ChainId as Pact
import Pact.Types.Command
import Pact.Types.Hash (Hash(..))
import qualified Pact.Types.Hash as Pact
import Pact.Types.PactError (PactError(..), PactErrorType(..))
import Pact.Types.Pretty (pretty)
import Control.Error (note, rights)
import qualified Web.DeepRoute as DR
import Network.HTTP.Types (badRequest400, methodPost, ok200, gatewayTimeout504)
import qualified Web.DeepRoute.Wai as DR
import Web.DeepRoute (errorWithStatus)
import Web.DeepRoute.Wai (responseJSON)
import Web.DeepRoute.Client
import Network.HTTP.Client (RequestBody(RequestBodyLBS))

-- -------------------------------------------------------------------------- --

data PactServerData logger tbl = PactServerData
    { _pactServerDataCutDb :: !(CutDB.CutDb tbl)
    , _pactServerDataMempool :: !(MempoolBackend ChainwebTransaction)
    , _pactServerDataLogger :: !logger
    , _pactServerDataPact :: !PactExecutionService
    , _pactServerChainId :: !ChainId
    }

instance HasChainId (PactServerData logger tbl) where
    _chainId = _pactServerChainId

newtype PactServerData_ (v :: ChainwebVersionT) (c :: ChainIdT) logger tbl
    = PactServerData_ { _unPactServerData :: PactServerData logger tbl }

data SomePactServerData = forall v c logger tbl
    . (KnownChainwebVersionSymbol v,
       KnownChainIdSymbol c,
       CanReadablePayloadCas tbl,
       Logger logger)
    => SomePactServerData (PactServerData_ v c logger tbl)


somePactServerData
    :: CanReadablePayloadCas tbl
    => Logger logger
    => ChainwebVersion
    -> ChainId
    -> PactServerData logger tbl
    -> SomePactServerData
somePactServerData v cid db =
    case someChainwebVersionVal v of
      (SomeChainwebVersionT (Proxy :: Proxy vt)) ->
          case someChainIdVal cid of
              (SomeChainIdT (Proxy :: Proxy cidt)) ->
                  SomePactServerData (PactServerData_ @vt @cidt db)

somePactServer :: SomePactServerData -> SomeServer
somePactServer (SomePactServerData (db :: PactServerData_ v c logger tbl))
    = SomeServer (Proxy @(PactServiceApi v c)) (pactServer @v @c $ _unPactServerData db)


pactServer
    :: forall v c tbl logger
     . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => CanReadablePayloadCas tbl
    => Logger logger
    => PactServerData logger tbl
    -> Server (PactServiceApi v c)
pactServer d =
    pactApiHandlers
        :<|> pactSpvHandler
        :<|> ethSpvHandler
        :<|> pactSpv2Handler
  where
    cid = FromSing (SChainId :: Sing c)
    mempool = _pactServerDataMempool d
    logger = _pactServerDataLogger d
    pact = _pactServerDataPact d
    cdb = _pactServerDataCutDb d
    v = _chainwebVersion cdb

    pactApiHandlers
      = sendHandler logger v cid mempool
      :<|> pollHandler logger cdb cid pact mempool
      :<|> listenHandler logger cdb cid pact mempool
      :<|> localHandler logger v cid pact

    pactSpvHandler = spvHandler logger cdb cid
    pactSpv2Handler = spv2Handler logger cdb cid

sendReq :: ChainwebVersion -> ChainId -> SubmitBatch -> ApiRequest (Either AesonException RequestKeys)
sendReq v cid batch = mkApiRequest
  methodPost
  (traverse jsonBody)
  ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "pact" /@ "api" /@ "v1" /@ "send")
  & requestBody .~ RequestBodyLBS (J.encode batch)

pollReq :: ChainwebVersion -> ChainId -> Maybe ConfirmationDepth -> Poll -> ApiRequest (Either AesonException PollResponses)
pollReq v cid confirmationDepth poll = mkApiRequest
  methodPost
  (traverse jsonBody)
  ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "pact" /@ "api" /@ "v1" /@ "poll")
  & requestQuery .~ [("confirmationDepth", toQueryParam <$> confirmationDepth)]
  & requestBody .~ RequestBodyLBS (J.encode poll)

listenReq :: ChainwebVersion -> ChainId -> ListenerRequest -> ApiRequest (Either AesonException RequestKeys)
listenReq v cid listenerRequest = mkApiRequest
  methodPost
  (traverse jsonBody)
  ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "pact" /@ "api" /@ "v1" /@ "listen")
  & requestBody .~ RequestBodyLBS (J.encode listenerRequest)

localReq
  :: ChainwebVersion -> ChainId
  -> Maybe LocalPreflightSimulation -> Maybe LocalSignatureVerification -> Maybe RewindDepth
  -> Command Text
  -> ApiRequest (Either AesonException LocalResult)
localReq v cid preflight signatureVerification rewindDepth localCommand = mkApiRequest
  methodPost
  (traverse jsonBody)
  ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "pact" /@ "api" /@ "v1" /@ "local")
  & requestQuery .~
    [ ("preflight", toQueryParam <$> preflight)
    , ("signatureVerification", toQueryParam <$> signatureVerification)
    , ("rewindDepth", toQueryParam <$> rewindDepth)
    ]
  & requestBody .~ RequestBodyLBS (J.encode localCommand)

preflightReq
  :: ChainwebVersion -> ChainId
  -> Maybe LocalSignatureVerification -> Maybe RewindDepth
  -> Command Text
  -> ApiRequest (Either AesonException PreflightResult)
preflightReq v cid signatureVerification rewindDepth command = mkApiRequest
  methodPost
  (traverse jsonBody)
  ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "pact" /@ "api" /@ "v1" /@ "local")
  & requestQuery .~
    [ ("preflight", Just $ toQueryParam True)
    , ("signatureVerification", toQueryParam <$> signatureVerification)
    , ("rewindDepth", toQueryParam <$> rewindDepth)
    ]
  & requestBody .~ RequestBodyLBS (J.encode command)


spvReq
  :: ChainwebVersion -> ChainId
  -> SpvRequest
  -> ApiRequest (Either AesonException TransactionOutputProofB64)
spvReq v cid req = mkApiRequest
  methodPost
  (traverse jsonBody)
  ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "pact" /@ "spv")
  & requestBody .~ RequestBodyLBS (J.encode req)

spv2Req
  :: ChainwebVersion -> ChainId
  -> Spv2Request
  -> ApiRequest (Either AesonException SomePayloadProof)
spv2Req v cid req = mkApiRequest
  methodPost
  (traverse jsonBody)
  ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "pact" /@ "spv2")
  & requestBody .~ RequestBodyLBS (encode req)

ethSpvReq
  :: ChainwebVersion -> ChainId
  -> EthSpvRequest
  -> ApiRequest (Either AesonException EthSpvResponse)
ethSpvReq v cid req = mkApiRequest
  methodPost
  (traverse jsonBody)
  ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "pact" /@ "spv" /@ "eth")
  & requestBody .~ RequestBodyLBS (encode req)

newPactServer
  :: CanReadablePayloadCas tbl
  => Logger logger
  => ChainwebVersion
  -> DR.Route (PactServerData logger tbl -> Application)
newPactServer v = fold
  [ DR.seg "api" $ fold

    [ DR.seg "v1" $ fold

      [ DR.seg "send" $
        DR.endpoint methodPost "application/json" $ \PactServerData{..} req resp -> do
          submitBatch <- DR.requestFromJSON req
          rks <- sendHandler _pactServerDataLogger v _pactServerChainId _pactServerDataMempool submitBatch
          resp $ responsePactJSON ok200 [] rks

      , DR.seg "poll" $
        DR.endpoint methodPost "application/json" $ \PactServerData{..} req resp -> do
          poll <- DR.requestFromJSON req
          confirmationDepth <- DR.getParams req (DR.queryParamMaybe "confirmationDepth")
          responses <-
            pollHandler _pactServerDataLogger _pactServerDataCutDb _pactServerChainId _pactServerDataPact _pactServerDataMempool confirmationDepth poll
          resp $ responsePactJSON ok200 [] responses

      , DR.seg "listen" $
        DR.endpoint methodPost "application/json" $ \PactServerData{..} req resp -> do
          listenerRequest <- DR.requestFromJSON req
          response <- listenHandler _pactServerDataLogger _pactServerDataCutDb _pactServerChainId _pactServerDataPact _pactServerDataMempool listenerRequest
          resp $ responsePactJSON ok200 [] response

      , DR.seg "local" $
        DR.endpoint methodPost "application/json" $ \PactServerData{..} req resp -> do
          localCommand <- DR.requestFromJSON req
          (preflight, signatureVerification, rewindDepth) <- DR.getParams req $ (,,)
            <$> DR.queryParamMaybe "preflight"
            <*> DR.queryParamMaybe "signatureVerification"
            <*> DR.queryParamMaybe "rewindDepth"
          response <- localHandler
            _pactServerDataLogger v _pactServerChainId _pactServerDataPact
            preflight signatureVerification rewindDepth
            localCommand
          resp $ responsePactJSON ok200 [] response
      ]
    ]

  , DR.seg "spv" $ fold

    [ DR.seg "eth" $
      DR.endpoint methodPost "application/json" $ \_ req resp -> do
        ethSpvRequest <- DR.requestFromJSON req
        resp . responseJSON ok200 [] =<<
          ethSpvHandler ethSpvRequest

    , DR.endpoint methodPost "application/json" $ \PactServerData{..} req resp -> do
        spvRequest <- DR.requestFromJSON req
        resp . responseJSON ok200 [] =<<
          spvHandler _pactServerDataLogger _pactServerDataCutDb _pactServerChainId spvRequest

    ]

  , DR.seg "spv2" $
    DR.endpoint methodPost "application/json" $ \PactServerData{..} req resp -> do
      spv2Request <- DR.requestFromJSON req
      resp . responseJSON ok200 [] =<<
        spv2Handler _pactServerDataLogger _pactServerDataCutDb _pactServerChainId spv2Request

  ]

somePactServers
    :: CanReadablePayloadCas tbl
    => Logger logger
    => ChainwebVersion
    -> [(ChainId, PactServerData logger tbl)]
    -> SomeServer
somePactServers v =
    mconcat . fmap (somePactServer . uncurry (somePactServerData v))

data PactCmdLog
    = PactCmdLogSend (NonEmpty (Command Text))
    | PactCmdLogPoll (NonEmpty Text)
    | PactCmdLogListen Text
    | PactCmdLogLocal (Command Text)
    | PactCmdLogSpv Text
    deriving (Show, Generic, NFData)

instance ToJSON PactCmdLog where
    toJSON (PactCmdLogSend x) = object
        [ "tag" .= ("PactCmdLogSend" :: T.Text)
        , "contents" .= fmap J.toJsonViaEncode x
        ]
    toJSON (PactCmdLogPoll x) = object
        [ "tag" .= ("PactCmdLogPoll" :: T.Text)
        , "contents" .= x
        ]
    toJSON (PactCmdLogListen x) = object
        [ "tag" .= ("PactCmdLogListen" :: T.Text)
        , "contents" .= x
        ]
    toJSON (PactCmdLogLocal x) = object
        [ "tag" .= ("PactCmdLogLocal" :: T.Text)
        , "contents" .= J.toJsonViaEncode x
        ]
    toJSON (PactCmdLogSpv x) = object
        [ "tag" .= ("PactCmdLogSpv" :: T.Text)
        , "contents" .= x
        ]
    {-# INLINEABLE toJSON #-}

-- -------------------------------------------------------------------------- --
-- Send Handler

sendHandler
    :: (Logger logger, MonadIO m)
    => logger
    -> ChainwebVersion
    -> ChainId
    -> MempoolBackend ChainwebTransaction
    -> SubmitBatch
    -> m RequestKeys
sendHandler logger v cid mempool (SubmitBatch cmds) = liftIO $ do
    logg Info (PactCmdLogSend cmds)
    case traverse (validateCommand v cid) cmds of
       Right enriched -> do
           let txs = V.fromList $ NEL.toList enriched
           -- If any of the txs in the batch fail validation, we reject them all.
           mempoolInsertCheck mempool txs >>= checkResult
           mempoolInsert mempool UncheckedInsert txs
           return $! RequestKeys $ NEL.map cmdToRequestKey enriched
       Left err -> errorWithStatus badRequest400 $ "Validation failed: " <> T.pack err
  where
    logg = logFunctionJson (setComponent "send-handler" logger)

    toPactHash :: TransactionHash -> Pact.TypedHash h
    toPactHash (TransactionHash h) = Pact.TypedHash h

    checkResult :: Either (T2 TransactionHash InsertError) () -> IO ()
    checkResult (Right _) = pure ()
    checkResult (Left (T2 hash insErr)) = do
      let msg = fold [ "Validation failed for hash "
            , sshow $ toPactHash hash
            , ": "
            , sshow insErr
            ]
      errorWithStatus badRequest400 msg


-- -------------------------------------------------------------------------- --
-- Poll Handler

pollHandler
    :: MonadIO m
    => HasCallStack
    => CanReadablePayloadCas tbl
    => Logger logger
    => logger
    -> CutDB.CutDb tbl
    -> ChainId
    -> PactExecutionService
    -> MempoolBackend ChainwebTransaction
    -> Maybe ConfirmationDepth
    -> Poll
    -> m PollResponses
pollHandler logger cdb cid pact mem confDepth (Poll request) = liftIO $ do
    traverse_ validateRequestKey request

    logg Info $ PactCmdLogPoll $ fmap requestKeyToB16Text request
    PollResponses <$!> internalPoll pdb bdb mem pact confDepth request
  where
    pdb = view CutDB.cutDbPayloadDb cdb
    bdb = fromJuste $ preview (CutDB.cutDbBlockHeaderDb cid) cdb
    logg = logFunctionJson (setComponent "poll-handler" logger)

-- -------------------------------------------------------------------------- --
-- Listen Handler

listenHandler
    :: MonadIO m
    => CanReadablePayloadCas tbl
    => Logger logger
    => logger
    -> CutDB.CutDb tbl
    -> ChainId
    -> PactExecutionService
    -> MempoolBackend ChainwebTransaction
    -> ListenerRequest
    -> m ListenResponse
listenHandler logger cdb cid pact mem (ListenerRequest key) = liftIO $ do
    validateRequestKey key

    logg Info $ PactCmdLogListen $ requestKeyToB16Text key
    registerDelay defaultTimeout >>= runListen
  where
    pdb = view CutDB.cutDbPayloadDb cdb
    bdb = fromJuste $ preview (CutDB.cutDbBlockHeaderDb cid) cdb
    logg = logFunctionJson (setComponent "listen-handler" logger)
    runListen :: TVar Bool -> IO ListenResponse
    runListen timedOut = do
      startCut <- CutDB._cut cdb
      case HM.lookup cid (_cutMap startCut) of
        Nothing -> pure $! ListenTimeout defaultTimeout
        Just bh -> poll bh
      where
        go :: BlockHeader -> IO ListenResponse
        go !prevBlock = do
          m <- waitForNewBlock prevBlock
          case m of
            Nothing -> pure $! ListenTimeout defaultTimeout
            Just block -> poll block

        poll :: BlockHeader -> IO ListenResponse
        poll bh = do
          hm <- internalPoll pdb bdb mem pact Nothing (pure key)
          if HM.null hm
          then go bh
          else pure $! ListenResponse $ snd $ head $ HM.toList hm

        waitForNewBlock :: BlockHeader -> IO (Maybe BlockHeader)
        waitForNewBlock lastBlockHeader = atomically $ do
          isTimedOut <- readTVar timedOut
          if isTimedOut
          then do
            pure Nothing
          else do
            Just <$!> CutDB.awaitNewBlockStm cdb cid lastBlockHeader

    -- TODO: make configurable
    defaultTimeout = 180 * 1000000 -- two minutes

-- -------------------------------------------------------------------------- --
-- Local Handler

localHandler
    :: MonadIO m
    => Logger logger
    => logger
    -> ChainwebVersion
    -> ChainId
    -> PactExecutionService
    -> Maybe LocalPreflightSimulation
      -- ^ Preflight flag
    -> Maybe LocalSignatureVerification
      -- ^ No sig verification flag
    -> Maybe RewindDepth
      -- ^ Rewind depth
    -> Command Text
    -> m LocalResult
localHandler logger v cid pact preflight sigVerify rewindDepth cmd = liftIO $ do
    logg Info $ PactCmdLogLocal cmd
    cmd' <- case validatedCommand of
      Right c -> return c
      Left err ->
        errorWithStatus badRequest400 $ "Validation failed: " <> T.pack err

    r <- try $ _pactLocal pact preflight sigVerify rewindDepth cmd'
    case r of
      Left (err :: PactException)  -> errorWithStatus badRequest400 $
        ("Execution failed: " <> T.pack (show err))
      Right (Left (TxTimeout h)) ->
        errorWithStatus gatewayTimeout504 $
          "Local request timed out: " <> sshow h
      Right (Right (LocalPreflightResult (PreflightValidationFailure e))) -> do
        errorWithStatus badRequest400 $
          ("Metadata validation failed: " <> decodeUtf8 (BSL.toStrict (Aeson.encode e)))
      Right (Right lr) -> return $! lr
  where
    logg = logFunctionJson (setComponent "local-handler" logger)

    validatedCommand
      | Just NoVerify <- sigVerify = do
          --
          -- desnote(emily): This workflow is 'Pact.Types.Command.verifyCommand'
          -- lite - only decode and parse the pact command, no sig checking.
          -- We at least check the consistency of the payload hash. Further
          -- down in the 'execLocal' code, 'noSigVerify' triggers a nop on
          -- checking again if 'preflight' is set.
          --
          let payloadBS = encodeUtf8 (_cmdPayload cmd)

          void $ Pact.verifyHash @'Pact.Blake2b_256 (_cmdHash cmd) payloadBS
          decoded <- eitherDecodeStrict' payloadBS
          payloadParsed <- traverse Pact.parsePact decoded

          let cmd' = cmd { _cmdPayload = (payloadBS, payloadParsed) }
          pure $ mkPayloadWithText cmd'
      | otherwise = validateCommand v cid cmd

-- -------------------------------------------------------------------------- --
-- Cross Chain SPV Handler

spvHandler
    :: forall tbl l m
    . ( Logger l
      , CanReadablePayloadCas tbl
      , MonadIO m
      , MonadThrow m
      )
    => l
    -> CutDB.CutDb tbl
        -- ^ cut db
    -> ChainId
        -- ^ the chain id of the source chain id used in the
        -- execution of a cross-chain-transfer.
    -> SpvRequest
        -- ^ Contains the (pact) chain id of the target chain id used in the
        -- 'target-chain' field of a cross-chain-transfer.
        -- Also contains the request key of of the cross-chain transfer
        -- tx request.
    -> m TransactionOutputProofB64
spvHandler l cdb cid (SpvRequest rk (Pact.ChainId ptid)) = do
    validateRequestKey rk

    liftIO $! logg (sshow ph)

    T2 bhe _bha <- liftIO (try $ _pactLookup pe cid Nothing (pure ph)) >>= \case
      Left (e :: PactException) ->
        liftIO $ DR.errorWithStatus badRequest400 $ "Internal error: transaction hash lookup failed: " <> sshow e
      Right v -> case HM.lookup ph v of
        Nothing -> liftIO $ DR.errorWithStatus badRequest400 $ "Transaction hash not found: " <> sshow ph
        Just t -> return t

    idx <- liftIO (getTxIdx bdb pdb bhe ph) >>= \case
      Left e -> liftIO $ DR.errorWithStatus badRequest400
        $ "Internal error: Index lookup for hash failed: "
        <> sshow e
      Right i -> return i

    tid <- chainIdFromText ptid
    p <- liftIO (try $ createTransactionOutputProof cdb tid cid bhe idx) >>= \case
      Left e@SpvExceptionTargetNotReachable{} ->
        liftIO $ DR.errorWithStatus badRequest400 $ "SPV target not reachable: " <> _spvExceptionMsg e
      Left e@SpvExceptionVerificationFailed{} ->
        liftIO $ DR.errorWithStatus badRequest400 $ "SPV verification failed: " <> _spvExceptionMsg e
      Left e ->
        liftIO $ DR.errorWithStatus badRequest400 $ "Internal error: SPV verification failed: " <> _spvExceptionMsg e
      Right q -> return q

    return $! b64 p
  where
    pe = _webPactExecutionService $ view CutDB.cutDbPactService cdb
    ph = Pact.fromUntypedHash $ unRequestKey rk
    bdb = fromJuste $ preview (CutDB.cutDbBlockHeaderDb cid) cdb
    pdb = view CutDB.cutDbPayloadDb cdb
    b64 = TransactionOutputProofB64
      . encodeB64UrlNoPaddingText
      . BSL8.toStrict
      . Aeson.encode

    logg = logFunctionJson (setComponent "spv-handler" l) Info
      . PactCmdLogSpv

-- -------------------------------------------------------------------------- --
-- SPV2 Handler

spv2Handler
    :: forall m tbl l
    . ( Logger l
      , CanReadablePayloadCas tbl
      , MonadIO m
      )
    => l
    -> CutDB.CutDb tbl
        -- ^ CutDb contains the cut, payload, and block db
    -> ChainId
        -- ^ ChainId of the target
    -> Spv2Request
        -- ^ Contains the (pact) chain id of the target chain id used in the
        -- 'target-chain' field of a cross-chain-transfer.
        -- Also contains the request key of of the cross-chain transfer
        -- tx request.
    -> m SomePayloadProof
spv2Handler l cdb cid r = case _spvSubjectIdType sid of
    SpvSubjectResult
        |  _spv2ReqAlgorithm r /= SpvSHA512t_256 ->
            liftIO $ DR.errorWithStatus badRequest400 $ "Algorithm " <> sshow r <> " is not supported with SPV result proofs."
        | otherwise -> proof createOutputProofDb
    SpvSubjectEvents
        | cid /= _spvSubjectIdChain sid ->
            liftIO $ DR.errorWithStatus badRequest400 "Cross chain SPV proofs for are not supported for Pact events"
        | otherwise -> case _spv2ReqAlgorithm r of
            SpvSHA512t_256 -> proof createEventsProofDb
            SpvKeccak_256 -> proof createEventsProofDbKeccak256
  where
    proof
        :: forall a
        . MerkleHashAlgorithm a
        => MerkleHashAlgorithmName a
        => (BlockHeaderDb -> PayloadDb tbl -> Natural -> BlockHash -> RequestKey -> IO (PayloadProof a))
        -> m SomePayloadProof
    proof f = SomePayloadProof <$> do
        validateRequestKey rk
        liftIO $! logg (sshow ph)
        T2 bhe bha <- liftIO (try $ _pactLookup pe cid Nothing (pure ph)) >>= \case
            Left (e :: PactException) ->
                liftIO $ DR.errorWithStatus badRequest400 $ "Internal error: transaction hash lookup failed: " <> sshow e
            Right v -> case HM.lookup ph v of
                Nothing -> liftIO $ DR.errorWithStatus badRequest400 $ "Transaction hash not found: " <> sshow ph
                Just t -> return t

        let confDepth = fromMaybe (diameter (chainGraphAt cdb bhe)) $ _spv2ReqMinimalProofDepth r

        liftIO (tryAllSynchronous $ f bdb pdb confDepth bha rk) >>= \case
            Left e -> liftIO $ DR.errorWithStatus badRequest400 $ "SPV proof creation failed:" <> sshow e
            Right q -> return q

    sid = _spv2ReqSubjectIdentifier r

    rk = _spvSubjectIdReqKey sid
    pe = _webPactExecutionService $ view CutDB.cutDbPactService cdb
    ph = Pact.fromUntypedHash $ unRequestKey rk
    bdb = fromJuste $ preview (CutDB.cutDbBlockHeaderDb cid) cdb
    pdb = view CutDB.cutDbPayloadDb cdb

    logg = logFunctionJson (setComponent "spv-handler" l) Info
      . PactCmdLogSpv

-- -------------------------------------------------------------------------- --
-- Eth SPV Handler

ethSpvHandler
    :: MonadIO m
    => EthSpvRequest
    -> m EthSpvResponse
ethSpvHandler req = liftIO $ do

    -- find block with transaction
    (block, rest) <- case evalState start Nothing of
        Left () -> liftIO $ DR.errorWithStatus badRequest400 $ "the transaction " <> sshow tx <> " is not contained in any of the provided blocks"
        Right x -> return x

    -- select and order set of receipts in the block
    --
    -- How big can blocks be? Should we create an index instead?
    --
    rcs <- forM (_rpcBlockTransactions block) $ \t -> do
        case L.find (\r -> _rpcReceiptTransactionHash r == t) receipts of
            Nothing -> liftIO $ DR.errorWithStatus badRequest400 $ "missing receipt for tx " <> sshow t
            Just x -> return x

    -- select and order set of extra headers and create proof
    case rpcReceiptProof (_rpcBlockHeader block) (hdrs block rest) rcs (TransactionIndex 28) of
        Left e -> liftIO $ DR.errorWithStatus badRequest400 $ "failed to create proof: " <> sshow e
        Right proof -> return $ EthSpvResponse $
            encodeB64UrlNoPaddingText (putRlpByteString proof)

  where
    receipts = _ethSpvReqReceipts req
    blocks = _ethSpvReqBlocks req
    orderedBlocks = L.sortOn (_hdrNumber . _rpcBlockHeader) blocks
    tx = _ethSpvReqTransactionHash req

    start = S.each orderedBlocks
        & S.dropWhile (notElem tx . _rpcBlockTransactions)
        & S.next

    -- filter sequence consecutive headers
    hdrs block rest = flip evalState (Just $ _rpcBlockHash block) $ rest
        & S.filterM (\b -> do
            c <- get
            if Just (bytes $ _hdrParentHash $ _rpcBlockHeader b) == (bytes <$> c)
                then True <$ put (Just $ _rpcBlockHash b)
                else return False
        )
        & S.map _rpcBlockHeader
        & S.toList_

-- --------------------------------------------------------------------------- --
-- Poll Helper

internalPoll
    :: CanReadablePayloadCas tbl
    => PayloadDb tbl
    -> BlockHeaderDb
    -> MempoolBackend ChainwebTransaction
    -> PactExecutionService
    -> Maybe ConfirmationDepth
    -> NonEmpty RequestKey
    -> IO (HashMap RequestKey (CommandResult Hash))
internalPoll pdb bhdb mempool pactEx confDepth requestKeys0 = do
    -- get leaf block header for our chain from current best cut
    results0 <- _pactLookup pactEx cid confDepth requestKeys
        -- TODO: are we sure that all of these are raised locally. This will cause the
        -- server to shut down the connection without returning a result to the user.
    let results1 = V.map (\rk -> (rk, HM.lookup (Pact.fromUntypedHash $ unRequestKey rk) results0)) requestKeysV
    let (present0, missing) = V.unstablePartition (isJust . snd) results1
    let present = V.map (second fromJuste) present0
    badlisted <- V.toList <$> checkBadList (V.map fst missing)
    vs <- mapM lookup present
    let good = rights $ V.toList vs
    return $! HM.fromList (good ++ badlisted)
  where
    cid = _chainId bhdb
    !requestKeysV = V.fromList $ NEL.toList requestKeys0
    !requestKeys = V.map (Pact.fromUntypedHash . unRequestKey) requestKeysV

    lookup
        :: (RequestKey, T2 BlockHeight BlockHash)
        -> IO (Either String (RequestKey, CommandResult Hash))
    lookup (key, T2 _ ha) = fmap (key,) <$> lookupRequestKey key ha

    -- TODO: group by block for performance (not very important right now)
    lookupRequestKey key bHash = runExceptT $ do
        let keyHash = unRequestKey key
        let pactHash = Pact.fromUntypedHash keyHash
        let matchingHash = (== pactHash) . _cmdHash . fst
        blockHeader <- liftIO $ TreeDB.lookupM bhdb bHash
        let payloadHash = _blockPayloadHash blockHeader
        (_payloadWithOutputsTransactions -> txsBs) <- barf "tablelookupFailed" =<< liftIO (lookupPayloadWithHeight pdb (Just $ _blockHeight blockHeader) payloadHash)
        !txs <- mapM fromTx txsBs
        case find matchingHash txs of
            Just (_cmd, TransactionOutput output) -> do
                out <- barf "decodeStrict' output" $! decodeStrict' output
                when (_crReqKey out /= key) $
                    fail "internal error: Transaction output doesn't match its hash!"
                enrichCR blockHeader out
            Nothing -> throwError $ "Request key not found: " <> sshow keyHash

    fromTx :: (Transaction, TransactionOutput) -> ExceptT String IO (Command Text, TransactionOutput)
    fromTx (!tx, !out) = do
        !tx' <- except $ toPactTx tx
        return (tx', out)

    checkBadList :: Vector RequestKey -> IO (Vector (RequestKey, CommandResult Hash))
    checkBadList rkeys = do
        let !hashes = V.map requestKeyToTransactionHash rkeys
        out <- mempoolCheckBadList mempool hashes
        let bad = V.map (RequestKey . Hash . unTransactionHash . fst) $
                  V.filter snd $ V.zip hashes out
        return $! V.map hashIsOnBadList bad

    hashIsOnBadList :: RequestKey -> (RequestKey, CommandResult Hash)
    hashIsOnBadList rk =
        let res = PactResult (Left err)
            err = PactError TxFailure def [] doc
            doc = pretty (T.pack $ show InsertErrorBadlisted)
            !cr = CommandResult rk Nothing res 0 Nothing Nothing Nothing []
        in (rk, cr)

    enrichCR :: BlockHeader -> CommandResult Hash -> ExceptT String IO (CommandResult Hash)
    enrichCR bh = return . set crMetaData
      (Just $ object
       [ "blockHeight" .= _blockHeight bh
       , "blockTime" .= _blockCreationTime bh
       , "blockHash" .= _blockHash bh
       , "prevBlockHash" .= _blockParent bh
       ])

-- -------------------------------------------------------------------------- --
-- Misc Utils

barf :: Monad m => e -> Maybe a -> ExceptT e m a
barf e = maybe (throwError e) return

toPactTx :: Transaction -> Either String (Command Text)
toPactTx (Transaction b) = note "toPactTx failure" (decodeStrict' b)

-- TODO: all of the functions in this module can instead grab the current block height from consensus
-- and pass it here to get a better estimate of what behavior is correct.
validateCommand :: ChainwebVersion -> ChainId -> Command Text -> Either String ChainwebTransaction
validateCommand v cid (fmap encodeUtf8 -> cmdBs) = case parsedCmd of
  Right (commandParsed :: ChainwebTransaction) ->
    if assertCommand
         commandParsed
         (validPPKSchemes v cid bh)
         (isWebAuthnPrefixLegal v cid bh)
    then Right commandParsed
    else Left "Command failed validation"
  Left e -> Left $ "Pact parsing error: " ++ e
  where
    bh = maxBound :: BlockHeight
    decodeAndParse bs =
        traverse (parsePact (pactParserVersion v cid bh)) =<< Aeson.eitherDecodeStrict' bs
    parsedCmd = mkPayloadWithText <$>
        cmdPayload (\bs -> (bs,) <$> decodeAndParse bs) cmdBs

-- | Validate the length of the request key's underlying hash.
--
validateRequestKey :: MonadIO m => RequestKey -> m ()
validateRequestKey (RequestKey h'@(Hash h))
    | keyLength == blakeHashLength = return ()
    | otherwise = liftIO $ DR.errorWithStatus badRequest400
        ( "Request Key "
        <> Pact.hashToText h'
        <> " has incorrect hash of length "
        <> sshow keyLength
        )
  where
    -- length of the encoded request key hash
    --
    keyLength = SB.length h

    -- Blake hash length = 32 - the length of a
    -- Blake2b_256 hash
    --
    blakeHashLength :: Int
    blakeHashLength = Pact.hashLength Pact.Blake2b_256
{-# INLINE validateRequestKey #-}
