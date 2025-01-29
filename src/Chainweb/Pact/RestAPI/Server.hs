{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
, sendHandler
, pollHandler
, listenHandler
, localHandler
, spvHandler
, somePactServer
, somePactServers
, validateCommand
, validatePact5Command
) where

import Control.Applicative
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Except (ExceptT, runExceptT, except)

import Data.Aeson as Aeson
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Short as SB
import Data.Either (partitionEithers, isRight)
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
import Ethereum.Header hiding (blockHash)
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
    (InsertError(..), InsertType(..), MempoolBackend(..), TransactionHash(..), pact5RequestKeyToTransactionHash)
import Chainweb.Pact.RestAPI
import Chainweb.Pact.RestAPI.EthSpv
import Chainweb.Pact.RestAPI.SPV
import Chainweb.Pact.Types
import Chainweb.Pact4.SPV qualified as Pact4
import Pact.Types.ChainMeta qualified as Pact4
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.SPV (SpvException(..))
import Chainweb.SPV.CreateProof
import Chainweb.SPV.EventProof
import Chainweb.SPV.OutputProof
import Chainweb.SPV.PayloadProof
import qualified Chainweb.Pact4.Transaction as Pact4 hiding (parsePact)
import qualified Chainweb.TreeDB as TreeDB
import Chainweb.Utils
import Chainweb.Version
import qualified Chainweb.Pact4.Validations as Pact4
import Chainweb.Version.Guards (isWebAuthnPrefixLegal, validPPKSchemes)
import Chainweb.WebPactExecutionService

import qualified Pact.JSON.Encode as J
import qualified Pact.Parse as Pact4
import qualified Pact.Types.API as Pact4
import qualified Pact.Types.ChainId as Pact4
import qualified Pact.Types.Command as Pact4
import qualified Pact.Types.Hash as Pact4

import qualified Pact.Core.Command.Types as Pact5
import qualified Pact.Core.Pretty as Pact5
import qualified Chainweb.Pact5.Transaction as Pact5
import qualified Chainweb.Pact5.Types as Pact5
import qualified Chainweb.Pact5.Validations as Pact5
import Data.Coerce
import qualified Pact.Core.Command.Server as Pact5
import qualified Pact.Core.Errors as Pact5
import qualified Pact.Core.Hash as Pact5
import qualified Pact.Core.Gas as Pact5

-- -------------------------------------------------------------------------- --

data PactServerData logger tbl = PactServerData
    { _pactServerDataCutDb :: !(CutDB.CutDb tbl)
    , _pactServerDataMempool :: !(MempoolBackend Pact4.UnparsedTransaction)
    , _pactServerDataLogger :: !logger
    , _pactServerDataPact :: !PactExecutionService
    }

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

    pactApiHandlers
      = sendHandler logger mempool
      :<|> pollHandler logger cdb cid pact mempool
      :<|> listenHandler logger cdb cid pact mempool
      :<|> localHandler logger pact

    pactSpvHandler = spvHandler logger cdb cid
    pactSpv2Handler = spv2Handler logger cdb cid

somePactServer :: SomePactServerData -> SomeServer
somePactServer (SomePactServerData (db :: PactServerData_ v c logger tbl))
    = SomeServer (Proxy @(PactServiceApi v c)) (pactServer @v @c $ _unPactServerData db)


somePactServers
    :: CanReadablePayloadCas tbl
    => Logger logger
    => ChainwebVersion
    -> [(ChainId, PactServerData logger tbl)]
    -> SomeServer
somePactServers v =
    mconcat . fmap (somePactServer . uncurry (somePactServerData v))

data PactCmdLog
    = PactCmdLogSend (NonEmpty (Pact4.Command Text))
    | PactCmdLogPoll (NonEmpty Text)
    | PactCmdLogListen Text
    | PactCmdLogLocal (Pact4.Command Text)
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

-- TODO: convert to Pact 5
sendHandler
    :: Logger logger
    => logger
    -> MempoolBackend Pact4.UnparsedTransaction
    -> Pact4.SubmitBatch
    -> Handler Pact4.RequestKeys
sendHandler logger mempool (Pact4.SubmitBatch cmds) = Handler $ do
    liftIO $ logg Info (PactCmdLogSend cmds)
    let cmdPayloads :: Either String (NonEmpty (Pact4.Command (ByteString, Pact4.Payload Pact4.PublicMeta Text)))
        cmdPayloads = traverse (traverse (\t -> (encodeUtf8 t,) <$> eitherDecodeStrictText t)) cmds
    case cmdPayloads of
      Right (fmap Pact4.mkPayloadWithText -> cmdsWithParsedPayloads) -> do
          let cmdsWithParsedPayloadsV = V.fromList $ NEL.toList cmdsWithParsedPayloads
          -- If any of the txs in the batch fail validation, we reject them all.
          liftIO (mempoolInsertCheckVerbose mempool cmdsWithParsedPayloadsV) >>= checkResult
          liftIO (mempoolInsert mempool UncheckedInsert cmdsWithParsedPayloadsV)
          return $! Pact4.RequestKeys $ NEL.map Pact4.cmdToRequestKey cmdsWithParsedPayloads
      Left err -> failWith $ "reading JSON for transaction failed: " <> T.pack err
  where
    failWith :: Text -> ExceptT ServerError IO a
    failWith err = do
      liftIO $ logFunctionText logger Info err
      throwError $ setErrText err err400

    logg = logFunctionJson (setComponent "send-handler" logger)

    checkResult :: Vector (T2 TransactionHash (Either InsertError Pact4.UnparsedTransaction)) -> ExceptT ServerError IO ()
    checkResult vec
        | V.null vec = return ()
        | otherwise = do
            let errors = flip mapMaybe (L.zip [0..] (V.toList vec)) $ \(i, T2 txHash e) -> case e of
                    Left err -> Just $ "Transaction " <> sshow txHash <> " at index " <> sshow @Word i <> " failed with: " <> sshow err
                    Right _ -> Nothing
            if null errors
            then do
                return ()
            else do
                let err = "One or more transactions were invalid: " <> T.intercalate ", " errors
                failWith err

-- -------------------------------------------------------------------------- --
-- Poll Handler

-- TODO: convert to Pact 5?
pollHandler
    :: (HasCallStack, CanReadablePayloadCas tbl, Logger logger)
    => logger
    -> CutDB.CutDb tbl
    -> ChainId
    -> PactExecutionService
    -> MempoolBackend Pact4.UnparsedTransaction
    -> Maybe ConfirmationDepth
    -> Pact5.PollRequest
    -> Handler Pact5.PollResponse
pollHandler logger cdb cid pact mem confDepth (Pact5.PollRequest request) = do
    liftIO $! logg Info $ PactCmdLogPoll $ fmap Pact5.requestKeyToB64Text request
    Pact5.PollResponse <$!> liftIO (internalPoll logger pdb bdb mem pact confDepth request)
  where
    pdb = view CutDB.cutDbPayloadDb cdb
    bdb = fromJuste $ preview (CutDB.cutDbBlockHeaderDb cid) cdb
    logg = logFunctionJson (setComponent "poll-handler" logger)

-- -------------------------------------------------------------------------- --
-- Listen Handler

-- TODO: convert to Pact 5?
listenHandler
    :: (CanReadablePayloadCas tbl, Logger logger)
    => logger
    -> CutDB.CutDb tbl
    -> ChainId
    -> PactExecutionService
    -> MempoolBackend Pact4.UnparsedTransaction
    -> Pact5.ListenRequest
    -> Handler Pact5.ListenResponse
listenHandler logger cdb cid pact mem (Pact5.ListenRequest key) = do
    liftIO $ logg Info $ PactCmdLogListen $ Pact5.requestKeyToB64Text key
    liftIO (registerDelay defaultTimeout) >>= runListen
  where
    pdb = view CutDB.cutDbPayloadDb cdb
    bdb = fromJuste $ preview (CutDB.cutDbBlockHeaderDb cid) cdb
    logg = logFunctionJson (setComponent "listen-handler" logger)
    runListen :: TVar Bool -> Handler Pact5.ListenResponse
    runListen timedOut = do
      startCut <- liftIO $ CutDB._cut cdb
      case HM.lookup cid (_cutMap startCut) of
        Nothing -> throwError err504
        Just bh -> poll bh
      where
        go :: BlockHeader -> Handler Pact5.ListenResponse
        go !prevBlock = do
          m <- liftIO $ waitForNewBlock prevBlock
          case m of
            Nothing -> throwError err504
            Just block -> poll block

        poll :: BlockHeader -> Handler Pact5.ListenResponse
        poll bh = do
          hm <- liftIO $ internalPoll logger pdb bdb mem pact Nothing (pure key)
          if HM.null hm
          then go bh
          else pure $! Pact5.ListenResponse $ snd $ unsafeHead "Chainweb.Pact.RestAPI.Server.listenHandler.poll" $ HM.toList hm

        waitForNewBlock :: BlockHeader -> IO (Maybe BlockHeader)
        waitForNewBlock lastBlockHeader = atomically $ do
          isTimedOut <- readTVar timedOut
          if isTimedOut
          then do
            pure Nothing
          else do
            Just <$!> CutDB.awaitNewBlockStm cdb cid (view blockHash lastBlockHeader)

    -- TODO: make configurable
    defaultTimeout = 180 * 1000000 -- two minutes

-- -------------------------------------------------------------------------- --
-- Local Handler

-- TODO: convert to Pact 5?
localHandler
    :: Logger logger
    => logger
    -> PactExecutionService
    -> Maybe LocalPreflightSimulation
      -- ^ Preflight flag
    -> Maybe LocalSignatureVerification
      -- ^ No sig verification flag
    -> Maybe RewindDepth
      -- ^ Rewind depth
    -> Pact4.Command Text
    -> Handler LocalResult
localHandler logger pact preflight sigVerify rewindDepth cmd = do
    liftIO $ logg Info $ PactCmdLogLocal cmd
    cmd' <- case validatedCommand of
      Right c -> return c
      Left err ->
        throwError $ setErrText ("Validation failed: " <> T.pack err) err400

    r <- liftIO $ try $ _pactLocal pact preflight sigVerify rewindDepth cmd'
    case r of
      Left (err :: PactException)  -> throwError $ setErrText
        ("Execution failed: " <> T.pack (show err)) err400
      Right (preview _MetadataValidationFailure -> Just e) -> do
        throwError $ setErrText
          ("Metadata validation failed: " <> decodeUtf8 (BSL.toStrict (Aeson.encode e))) err400
      Right lr -> return $! lr
  where
    logg = logFunctionJson (setComponent "local-handler" logger)

    validatedCommand
      | Just NoVerify <- sigVerify = do
          --
          -- desnote(emily): This workflow is 'Pact.Types.Command.verifyCommand'
          -- lite - only decode the pact command, no sig checking.
          -- We at least check the consistency of the payload hash. Further
          -- down in the 'execLocal' code, 'noSigVerify' triggers a nop on
          -- checking again if 'preflight' is set.
          --
          let payloadBS = encodeUtf8 (Pact4._cmdPayload cmd)

          void $ Pact4.verifyHash @'Pact4.Blake2b_256 (Pact4._cmdHash cmd) payloadBS
          decoded <- eitherDecodeStrict' payloadBS

          let cmd' = cmd { Pact4._cmdPayload = (payloadBS, decoded) }
          pure $ Pact4.mkPayloadWithText cmd'
      | otherwise = Pact4.mkPayloadWithText <$>
        traverse (\bs -> (encodeUtf8 bs,) <$> eitherDecodeStrictText bs) cmd

-- -------------------------------------------------------------------------- --
-- Cross Chain SPV Handler

spvHandler
    :: forall tbl l
    . ( Logger l
      , CanReadablePayloadCas tbl
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
    -> Handler TransactionOutputProofB64
spvHandler l cdb cid (SpvRequest rk (Pact4.ChainId ptid)) = do
    validateRequestKey rk

    liftIO $! logg (sshow ph)

    T2 bhe _bha <- liftIO (try $ _pactLookup pe cid Nothing (pure $ coerce $ Pact4.toUntypedHash ph)) >>= \case
      Left (e :: PactException) ->
        toErr $ "Internal error: transaction hash lookup failed: " <> sshow e
      Right v -> case HM.lookup (coerce $ Pact4.toUntypedHash ph) v of
        Nothing -> toErr $ "Transaction hash not found: " <> sshow ph
        Just t -> return t

    idx <- liftIO (Pact4.getTxIdx bdb pdb bhe ph) >>= \case
      Left e -> toErr
        $ "Internal error: Index lookup for hash failed: "
        <> sshow e
      Right i -> return i

    tid <- chainIdFromText ptid
    p <- liftIO (try $ createTransactionOutputProof cdb tid cid bhe idx) >>= \case
      Left e@SpvExceptionTargetNotReachable{} ->
        toErr $ "SPV target not reachable: " <> _spvExceptionMsg e
      Left e@SpvExceptionVerificationFailed{} ->
        toErr $ "SPV verification failed: " <> _spvExceptionMsg e
      Left e ->
        toErr $ "Internal error: SPV verification failed: " <> _spvExceptionMsg e
      Right q -> return q

    return $! b64 p
  where
    pe = _webPactExecutionService $ view CutDB.cutDbPactService cdb
    ph = Pact4.fromUntypedHash $ Pact4.unRequestKey rk
    bdb = fromJuste $ preview (CutDB.cutDbBlockHeaderDb cid) cdb
    pdb = view CutDB.cutDbPayloadDb cdb
    b64 = TransactionOutputProofB64
      . encodeB64UrlNoPaddingText
      . BSL8.toStrict
      . Aeson.encode

    logg = logFunctionJson (setComponent "spv-handler" l) Info
      . PactCmdLogSpv

    toErr e = throwError $ setErrText e err400

-- -------------------------------------------------------------------------- --
-- SPV2 Handler

spv2Handler
    :: forall tbl l
    . ( Logger l
      , CanReadablePayloadCas tbl
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
    -> Handler SomePayloadProof
spv2Handler l cdb cid r = case _spvSubjectIdType sid of
    SpvSubjectResult
        |  _spv2ReqAlgorithm r /= SpvSHA512t_256 ->
            toErr $ "Algorithm " <> sshow r <> " is not supported with SPV result proofs."
        | otherwise -> proof createOutputProofDb
    SpvSubjectEvents
        | cid /= _spvSubjectIdChain sid ->
            toErr "Cross chain SPV proofs for are not supported for Pact events"
        | otherwise -> case _spv2ReqAlgorithm r of
            SpvSHA512t_256 -> proof createEventsProofDb
            SpvKeccak_256 -> proof createEventsProofDbKeccak256
  where
    proof
        :: forall a
        . MerkleHashAlgorithm a
        => MerkleHashAlgorithmName a
        => (BlockHeaderDb -> PayloadDb tbl -> Natural -> BlockHash -> Pact4.RequestKey -> IO (PayloadProof a))
        -> Handler SomePayloadProof
    proof f = SomePayloadProof <$> do
        validateRequestKey rk
        liftIO $! logg (sshow ph)
        T2 bhe bha <- liftIO (try $ _pactLookup pe cid Nothing (pure $ coerce ph)) >>= \case
            Left (e :: PactException) ->
                toErr $ "Internal error: transaction hash lookup failed: " <> sshow e
            Right v -> case HM.lookup (coerce ph) v of
                Nothing -> toErr $ "Transaction hash not found: " <> sshow ph
                Just t -> return t

        let confDepth = fromMaybe (diameter (chainGraphAt cdb bhe)) $ _spv2ReqMinimalProofDepth r

        liftIO (tryAllSynchronous $ f bdb pdb confDepth bha rk) >>= \case
            Left e -> toErr $ "SPV proof creation failed:" <> sshow e
            Right q -> return q

    sid = _spv2ReqSubjectIdentifier r

    rk = _spvSubjectIdReqKey sid
    pe = _webPactExecutionService $ view CutDB.cutDbPactService cdb
    ph = Pact4.unRequestKey rk
    bdb = fromJuste $ preview (CutDB.cutDbBlockHeaderDb cid) cdb
    pdb = view CutDB.cutDbPayloadDb cdb

    logg = logFunctionJson (setComponent "spv-handler" l) Info
      . PactCmdLogSpv

    toErr e = throwError $ err400 { errBody = e }

-- -------------------------------------------------------------------------- --
-- Eth SPV Handler

ethSpvHandler
    :: EthSpvRequest
    -> Handler EthSpvResponse
ethSpvHandler req = do

    -- find block with transaction
    (block, rest) <- case evalState start Nothing of
        Left () -> toErr $ "the transaction " <> sshow tx <> " is not contained in any of the provided blocks"
        Right x -> return x

    -- select and order set of receipts in the block
    --
    -- How big can blocks be? Should we create an index instead?
    --
    rcs <- forM (_rpcBlockTransactions block) $ \t -> do
        case L.find (\r -> _rpcReceiptTransactionHash r == t) receipts of
            Nothing -> toErr $ "missing receipt for tx " <> sshow t
            Just x -> return x

    -- select and order set of extra headers and create proof
    case rpcReceiptProof (_rpcBlockHeader block) (hdrs block rest) rcs (TransactionIndex 28) of
        Left e -> toErr $ "failed to create proof: " <> sshow e
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

    toErr e = throwError $ err400 { errBody = e }

-- --------------------------------------------------------------------------- --
-- Poll Helper

internalPoll
    :: (CanReadablePayloadCas tbl, Logger logger)
    => logger
    -> PayloadDb tbl
    -> BlockHeaderDb
    -> MempoolBackend Pact4.UnparsedTransaction
    -> PactExecutionService
    -> Maybe ConfirmationDepth
    -> NonEmpty Pact5.RequestKey
    -> IO (HashMap Pact5.RequestKey (Pact5.CommandResult Pact5.Hash Pact5.PactOnChainError))
internalPoll logger pdb bhdb mempool pactEx confDepth requestKeys0 = do
    let dbg txt = logFunctionText logger Debug txt
    -- get leaf block header for our chain from current best cut
    results0 <- _pactLookup pactEx cid confDepth (coerce requestKeys)
    dbg $ "internalPoll.results0: " <> sshow results0
        -- TODO: are we sure that all of these are raised locally. This will cause the
        -- server to shut down the connection without returning a result to the user.
    let results1 = V.map (\rk -> (rk, HM.lookup (coerce $ Pact5.unRequestKey rk) results0)) requestKeysV
    let (present0, missing) = V.unstablePartition (isJust . snd) results1
    let present = V.map (second fromJuste) present0
    badlisted <- V.toList <$> checkBadList (V.map fst missing)
    dbg $ "internalPoll.badlisted: " <> sshow badlisted
    vs <- mapM lookup present
    let (errs, notErrs) = partitionEithers $ V.toList vs
    let good = catMaybes notErrs
    logFunctionJson logger Warn errs
    return $! HM.fromList (good ++ badlisted)
  where
    cid = _chainId bhdb
    !requestKeysV = V.fromList $ NEL.toList requestKeys0
    !requestKeys = V.map Pact5.unRequestKey requestKeysV

    lookup
        :: (Pact5.RequestKey, T2 BlockHeight BlockHash)
        -> IO (Either String (Maybe (Pact5.RequestKey, Pact5.CommandResult Pact5.Hash Pact5.PactOnChainError)))
    lookup (key, T2 _ ha) = (fmap . fmap . fmap) (key,) $ lookupRequestKey key ha

    -- TODO: group by block for performance (not very important right now)
    lookupRequestKey
      :: Pact5.RequestKey
      -> BlockHash
      -> IO (Either String (Maybe (Pact5.CommandResult Pact5.Hash Pact5.PactOnChainError)))
    lookupRequestKey key bHash = runExceptT $ do
        let pactHash = Pact5.unRequestKey key
        let matchingHash = (== pactHash) . Pact5._cmdHash . fst
        blockHeader <- liftIO (TreeDB.lookup bhdb bHash) >>= \case
          Nothing -> throwError $ "missing block header: " <> sshow key
          Just x -> return x

        let payloadHash = view blockPayloadHash blockHeader
        (_payloadWithOutputsTransactions -> txsBs) <-
          barf
            ("payload lookup failed: " <> T.unpack (blockHeaderShortDescription blockHeader)
            <> " w/ payload hash " <> sshow (view blockPayloadHash blockHeader))
          =<< liftIO (lookupPayloadWithHeight pdb (Just $ view blockHeight blockHeader) payloadHash)
        !txs <- mapM fromTx txsBs
        case find matchingHash txs of
            Just (_cmd, TransactionOutput output) -> do
                out <- case eitherDecodeStrict' output of
                    Left err -> throwError $
                      "error decoding tx output for command " <> sshow (Pact5._cmdHash _cmd) <> ": " <> err
                    Right decodedOutput -> return decodedOutput
                when (Pact5._crReqKey out /= key) $
                    throwError "internal error: Transaction output doesn't match its hash!"
                return $ Just $ enrichCR blockHeader out
            Nothing -> return Nothing

    fromTx :: (Transaction, TransactionOutput) -> ExceptT String IO (Pact5.Command Text, TransactionOutput)
    fromTx (Transaction txBytes, !out) = do
        !tx' <- except $ eitherDecodeStrict' txBytes & _Left %~
          (\decodeErr -> "Transaction failed to decode: " <> decodeErr)
        return (tx', out)

    checkBadList :: Vector Pact5.RequestKey -> IO (Vector (Pact5.RequestKey, Pact5.CommandResult Pact5.Hash Pact5.PactOnChainError))
    checkBadList rkeys = do
        let !hashes = V.map pact5RequestKeyToTransactionHash rkeys
        out <- mempoolCheckBadList mempool hashes
        let bad = V.map (Pact5.RequestKey . Pact5.Hash . unTransactionHash . fst) $
                  V.filter snd $ V.zip hashes out
        return $! V.map hashIsOnBadList bad

    hashIsOnBadList :: Pact5.RequestKey -> (Pact5.RequestKey, Pact5.CommandResult Pact5.Hash Pact5.PactOnChainError)
    hashIsOnBadList rk =
        let res = Pact5.PactResultErr err
            err = Pact5.PactOnChainError
              -- the only legal error type, once chainweaver is really gone, we
              -- can use a real error type
              (Pact5.ErrorType "TxFailure")
              (Pact5.mkBoundedText "Transaction is badlisted because it previously failed to validate.")
              (Pact5.LocatedErrorInfo Pact5.TopLevelErrorOrigin Pact5.noInfo)
            !cr = Pact5.CommandResult rk Nothing res (mempty :: Pact5.Gas) Nothing Nothing Nothing []
        in (rk, cr)

    enrichCR :: BlockHeader -> Pact5.CommandResult i e -> Pact5.CommandResult i e
    enrichCR bh = set Pact5.crMetaData
      (Just $ object
       [ "blockHeight" .= view blockHeight bh
       , "blockTime" .= view blockCreationTime bh
       , "blockHash" .= view blockHash bh
       , "prevBlockHash" .= view blockParent bh
       ])

-- -------------------------------------------------------------------------- --
-- Misc Utils

barf :: Monad m => e -> Maybe a -> ExceptT e m a
barf e = maybe (throwError e) return

-- TODO: all of the functions in this module can instead grab the current block height from consensus
-- and pass it here to get a better estimate of what behavior is correct.
validateCommand :: ChainwebVersion -> ChainId -> Pact4.Command Text -> Either Text Pact4.Transaction
validateCommand v cid (fmap encodeUtf8 -> cmdBs) = case parsedCmd of
  Right (commandParsed :: Pact4.Transaction) ->
    case Pact4.assertCommand commandParsed (validPPKSchemes v cid bh) (isWebAuthnPrefixLegal v cid bh) of
      Left err -> Left $ "Command failed validation: " <> Pact4.displayAssertCommandError err
      Right () -> Right commandParsed
  Left e -> Left $ "Pact parsing error: " <> T.pack e
  where
    bh = maxBound :: BlockHeight
    decodeAndParse bs =
        traverse (Pact4.parsePact) =<< Aeson.eitherDecodeStrict' bs
    parsedCmd = Pact4.mkPayloadWithText <$>
        Pact4.cmdPayload (\bs -> (bs,) <$> decodeAndParse bs) cmdBs

-- TODO: all of the functions in this module can instead grab the current block height from consensus
-- and pass it here to get a better estimate of what behavior is correct.
validatePact5Command :: ChainwebVersion -> Pact5.Command Text -> Either String Pact5.Transaction
validatePact5Command _v cmdText = case parsedCmd of
  Right (commandParsed :: Pact5.Transaction) ->
    if isRight (Pact5.assertCommand commandParsed)
    then Right commandParsed
    else Left "Command failed validation"
  Left e -> Left $ "Pact parsing error: " ++ Pact5.renderCompactString e
  where
    parsedCmd = Pact5.parseCommand cmdText

-- | Validate the length of the request key's underlying hash.
--
validateRequestKey :: Pact4.RequestKey -> Handler ()
validateRequestKey (Pact4.RequestKey h'@(Pact4.Hash h))
    | keyLength == blakeHashLength = return ()
    | otherwise = throwError $ setErrText
        ( "Request Key "
        <> Pact4.hashToText h'
        <> " has incorrect hash of length "
        <> sshow keyLength
        ) err400
  where
    -- length of the encoded request key hash
    --
    keyLength = SB.length h

    -- Blake hash length = 32 - the length of a
    -- Blake2b_256 hash
    --
    blakeHashLength :: Int
    blakeHashLength = Pact4.hashLength Pact4.Blake2b_256
{-# INLINE validateRequestKey #-}
