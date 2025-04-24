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
{-# LANGUAGE NumericUnderscores #-}

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
-- , spvHandler
, somePactServer
, somePactServers
, validateCommand
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

import Data.Aeson as Aeson hiding (Success)
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
import Data.Validation
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
    (InsertError(..), InsertType(..), MempoolBackend(..), TransactionHash(..), pactRequestKeyToTransactionHash)
import Chainweb.Pact.RestAPI
import Chainweb.Pact.RestAPI.EthSpv
import Chainweb.Pact.RestAPI.SPV
import Chainweb.Pact.Types
import Chainweb.Pact.SPV qualified as SPV
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.SPV (SpvException(..))
import Chainweb.SPV.CreateProof
import Chainweb.SPV.EventProof
import Chainweb.SPV.OutputProof
import Chainweb.SPV.PayloadProof
import Chainweb.Pact.Transaction qualified as Pact hiding (parsePact)
import Chainweb.TreeDB qualified as TreeDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Pact.Validations qualified as Pact
import Chainweb.Version.Guards (validPPKSchemes)

import qualified Pact.JSON.Encode as J

import qualified Pact.Core.Command.Types as Pact
import qualified Pact.Core.Pretty as Pact
import qualified Chainweb.Pact.Transaction as Pact
import qualified Chainweb.Pact.Types as Pact
import qualified Chainweb.Pact.Validations as Pact
import Data.Coerce
import qualified Pact.Core.Command.Server as Pact
import qualified Pact.Core.Errors as Pact
import qualified Pact.Core.Hash as Pact
import qualified Pact.Core.Gas as Pact
import qualified Pact.Core.Command.Client as Pact
import qualified Pact.Core.ChainData as Pact
import Chainweb.PayloadProvider.Pact (PactPayloadProvider (..))
import Chainweb.PayloadProvider.P2P
import Chainweb.Pact.PactService
import Chainweb.Pact.Backend.Types (Historical(..), throwIfNoHistory)
import qualified Pact.Core.StableEncoding as Pact
import qualified Pact.Core.Info as Pact
import Control.Concurrent (threadDelay)

-- -------------------------------------------------------------------------- --

data PactServerData logger tbl = PactServerData
    { _pactServerDataMempool :: !(MempoolBackend Pact.Transaction)
    , _pactServerDataLogger :: !logger
    , _pactServerDataPact :: !(ServiceEnv tbl)
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
        -- :<|> pactSpvHandler
        -- :<|> ethSpvHandler
        -- :<|> pactSpv2Handler
  where
    mempool = _pactServerDataMempool d
    logger = _pactServerDataLogger d
    pact = _pactServerDataPact d

    pactApiHandlers
      = sendHandler logger mempool
      :<|> pollHandler logger pact mempool
      :<|> listenHandler logger pact mempool
      :<|> localHandler logger pact

    -- pactSpvHandler = spvHandler logger cdb pdb pact cid
    -- pactSpv2Handler = spv2Handler logger cdb pdb pact cid

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
    = PactCmdLogSend (NonEmpty (Pact.Command Text))
    | PactCmdLogPoll (NonEmpty Text)
    | PactCmdLogListen Text
    | PactCmdLogLocal (Pact.Command Text)
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
    -> MempoolBackend Pact.Transaction
    -> Pact.SendRequest
    -> Handler Pact.SendResponse
sendHandler logger mempool (Pact.SendRequest (Pact.SubmitBatch cmds)) = Handler $ do
    liftIO $ logg Info (PactCmdLogSend cmds)
    cmdsWithParsedPayloads <- parsedCommands
    let cmdsWithParsedPayloadsV = V.fromList $ NEL.toList cmdsWithParsedPayloads
    -- If any of the txs in the batch fail validation, we reject them all.
    liftIO (mempoolInsertCheckVerbose mempool cmdsWithParsedPayloadsV) >>= checkResult
    liftIO (mempoolInsert mempool UncheckedInsert cmdsWithParsedPayloadsV)
    return $! Pact.SendResponse $ Pact.RequestKeys $ fmap Pact.cmdToRequestKey cmdsWithParsedPayloads
  where
    convertError = Pact._boundedText . Pact._peMsg . Pact.pactErrorToOnChainError . fmap Pact.spanInfoToLineInfo
    failWith :: Text -> ExceptT ServerError IO a
    failWith err = do
      liftIO $ logFunctionText logger Info err
      throwError $ setErrText err err400

    logg = logFunctionJson (setComponent "send-handler" logger)

    parsedCommands :: ExceptT ServerError IO (NonEmpty Pact.Transaction)
    parsedCommands = do
      let
        parsedWithErrs =
          flip traverse (NEL.zip (NEL.iterate succ (0 :: Word)) cmds) $ \(i, tx) ->
            case Pact.parseCommand tx of
              Left err ->
                Failure $ NEL.singleton $
                  "Transaction at index " <> sshow i <>
                  " has invalid Pact code: " <> convertError err
              Right cmd ->
                pure cmd
      case parsedWithErrs of
        Failure errs ->
          failWith $ "One or more transactions has invalid Pact code: " <> T.intercalate ", " (toList errs)
        Success parsedCmds ->
          return parsedCmds

    checkResult :: Vector (T2 TransactionHash (Either InsertError Pact.Transaction)) -> ExceptT ServerError IO ()
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
    -> ServiceEnv tbl
    -> MempoolBackend Pact.Transaction
    -> Maybe ConfirmationDepth
    -> Pact.PollRequest
    -> Handler Pact.PollResponse
pollHandler logger pact mem confDepth (Pact.PollRequest request) = do
    liftIO $! logg Info $ PactCmdLogPoll $ fmap Pact.requestKeyToB64Text request
    Pact.PollResponse <$!> liftIO (internalPoll logger mem pact confDepth request)
  where
    logg = logFunctionJson (setComponent "poll-handler" logger)

-- -------------------------------------------------------------------------- --
-- Listen Handler

-- TODO: convert to Pact 5?
listenHandler
    :: (CanReadablePayloadCas tbl, Logger logger)
    => logger
    -> ServiceEnv tbl
    -> MempoolBackend Pact.Transaction
    -> Pact.ListenRequest
    -> Handler Pact.ListenResponse
listenHandler logger pact mem (Pact.ListenRequest key) = do
    liftIO $ logg Info $ PactCmdLogListen $ Pact.requestKeyToB64Text key
    liftIO (registerDelay defaultTimeout) >>= runListen
  where
    logg = logFunctionJson (setComponent "listen-handler" logger)
    runListen :: TVar Bool -> Handler Pact.ListenResponse
    runListen timedOut = poll
      where
        onMissing :: Handler Pact.ListenResponse
        onMissing = do
          isTimedOut <- liftIO $ readTVarIO timedOut
          if isTimedOut
          then throwError err504
          else liftIO (threadDelay 1_000_000) >> poll

        poll :: Handler Pact.ListenResponse
        poll = do
          hm <- liftIO $ internalPoll logger mem pact Nothing (pure key)
          if HM.null hm
          then onMissing
          else pure $! Pact.ListenResponse $ snd $ unsafeHead "Chainweb.Pact.RestAPI.Server.listenHandler.poll" $ HM.toList hm

    -- TODO: make configurable
    defaultTimeout = 180 * 1000000 -- two minutes

-- -------------------------------------------------------------------------- --
-- Local Handler

-- TODO: convert to Pact 5?
localHandler
    :: Logger logger
    => CanReadablePayloadCas tbl
    => logger
    -> ServiceEnv tbl
    -> Maybe LocalPreflightSimulation
      -- ^ Preflight flag
    -> Maybe LocalSignatureVerification
      -- ^ No sig verification flag
    -> Maybe RewindDepth
      -- ^ Rewind depth
    -> Pact.Command Text
    -> Handler LocalResult
localHandler logger pact preflight sigVerify rewindDepth cmd = do
    liftIO $ logg Info $ PactCmdLogLocal cmd
    cmd' <- validatedCommand

    r <- liftIO $ execLocal logger pact cmd' preflight sigVerify rewindDepth
    case r of
      Historical (MetadataValidationFailure e) -> do
        throwError $ setErrText
          ("Metadata validation failed: " <> decodeUtf8 (BSL.toStrict (Aeson.encode e))) err400
      Historical lr -> return $! lr
      NoHistory -> throwError $ setErrText
        "No block exists at the given rewind depth" err500
  where
    logg = logFunctionJson (setComponent "local-handler" logger)

    mkParseError err = setErrJSONPact (Pact.pactErrorToOnChainError (Pact.spanInfoToLineInfo <$> err)) err400

    validatedCommand
      | Just NoVerify <- sigVerify = do
          --
          -- desnote(emily): This workflow is 'Pact.Types.Command.verifyCommand'
          -- lite - only decode the pact command, no sig checking.
          -- We at least check the consistency of the payload hash. Further
          -- down in the 'execLocal' code, 'noSigVerify' triggers a nop on
          -- checking again if 'preflight' is set.
          --
          let command' = Pact.parseCommand cmd
          case command' of
            Left err -> throwError $ mkParseError err
            Right cmd' -> pure cmd'
      | otherwise =
        either (throwError . mkParseError) return
          $ Pact.parseCommand cmd

-- -------------------------------------------------------------------------- --
-- Cross Chain SPV Handler

-- spvHandler
--     :: forall tbl l
--     . Logger l
--     => CanReadablePayloadCas tbl
--     => l
--     -> CutDB.CutDb
--         -- ^ cut db
--     -> PayloadDb tbl
--     -> PactLookup
--     -> ChainId
--         -- ^ the chain id of the source chain id used in the
--         -- execution of a cross-chain-transfer.
--     -> SpvRequest
--         -- ^ Contains the (pact) chain id of the target chain id used in the
--         -- 'target-chain' field of a cross-chain-transfer.
--         -- Also contains the request key of of the cross-chain transfer
--         -- tx request.
--     -> Handler TransactionOutputProofB64
-- spvHandler l cdb pdb pe cid (SpvRequest rk (Pact.ChainId ptid)) = do

--     liftIO $! logg (sshow ph)

--     T2 bhe _bha <- liftIO (pe cid Nothing (pure $ Pact.unHash ph)) >>= \v ->
--       -- Left (e :: PactException) ->
--       --   toErr $ "Internal error: transaction hash lookup failed: " <> sshow e
--       case HM.lookup (Pact.unHash ph) v of
--         Nothing -> toErr $ "Transaction hash not found: " <> sshow ph
--         Just t -> return t

--     idx <- undefined >>= \case -- liftIO (Pact.getTxIdx bdb pdb bhe ph) >>= \case
--       Left e -> toErr
--         $ "Internal error: Index lookup for hash failed: "
--         <> sshow e
--       Right i -> return i

--     tid <- chainIdFromText ptid
--     p <- liftIO (try $ createTransactionOutputProof cdb tid cid bhe idx) >>= \case
--       Left e@SpvExceptionTargetNotReachable{} ->
--         toErr $ "SPV target not reachable: " <> _spvExceptionMsg e
--       Left e@SpvExceptionVerificationFailed{} ->
--         toErr $ "SPV verification failed: " <> _spvExceptionMsg e
--       Left e ->
--         toErr $ "Internal error: SPV verification failed: " <> _spvExceptionMsg e
--       Right q -> return q

--     return $! b64 p
--   where
--     ph = Pact.unRequestKey rk
--     bdb = fromJuste $ preview (CutDB.cutDbBlockHeaderDb cid) cdb
--     b64 = TransactionOutputProofB64
--       . encodeB64UrlNoPaddingText
--       . BSL8.toStrict
--       . Aeson.encode

--     logg = logFunctionJson (setComponent "spv-handler" l) Info
--       . PactCmdLogSpv

--     toErr e = throwError $ setErrText e err400

-- -------------------------------------------------------------------------- --
-- SPV2 Handler

-- spv2Handler
--     :: forall tbl l
--     . Logger l
--     => CanReadablePayloadCas tbl
--     => l
--     -> CutDB.CutDb
--         -- ^ CutDb contains the cut, payload, and block db
--     -> PayloadDb tbl
--     -> PactLookup
--     -> ChainId
--         -- ^ ChainId of the target
--     -> Spv2Request
--         -- ^ Contains the (pact) chain id of the target chain id used in the
--         -- 'target-chain' field of a cross-chain-transfer.
--         -- Also contains the request key of of the cross-chain transfer
--         -- tx request.
--     -> Handler SomePayloadProof
-- spv2Handler l cdb pdb pactLookup cid r = case _spvSubjectIdType sid of
--     SpvSubjectResult
--         |  _spv2ReqAlgorithm r /= SpvSHA512t_256 ->
--             toErr $ "Algorithm " <> sshow r <> " is not supported with SPV result proofs."
--         | otherwise -> proof createOutputProofDb
--     SpvSubjectEvents
--         | cid /= _spvSubjectIdChain sid ->
--             toErr "Cross chain SPV proofs for are not supported for Pact events"
--         | otherwise -> case _spv2ReqAlgorithm r of
--             SpvSHA512t_256 -> proof createEventsProofDb
--             SpvKeccak_256 -> proof createEventsProofDbKeccak256
--   where
--     proof
--         :: forall a
--         . MerkleHashAlgorithm a
--         => MerkleHashAlgorithmName a
--         => (BlockHeaderDb -> PayloadDb tbl -> Natural -> BlockHash -> Pact.RequestKey -> IO (PayloadProof a))
--         -> Handler SomePayloadProof
--     proof f = SomePayloadProof <$> do
--         liftIO $! logg (sshow ph)
--         T2 bhe bha <- liftIO (pactLookup Nothing (pure $ coerce ph)) >>=
--             -- Left (e :: PactException) ->
--             --     toErr $ "Internal error: transaction hash lookup failed: " <> sshow e
--             \v -> case HM.lookup (coerce ph) v of
--                 Nothing -> toErr $ "Transaction hash not found: " <> sshow ph
--                 Just t -> return t

--         let confDepth = fromMaybe (diameter (chainGraphAt cdb bhe)) $ _spv2ReqMinimalProofDepth r

--         liftIO (tryAllSynchronous $ f bdb pdb confDepth bha rk) >>= \case
--             Left e -> toErr $ "SPV proof creation failed:" <> sshow e
--             Right q -> return q

--     sid = _spv2ReqSubjectIdentifier r

--     rk = _spvSubjectIdReqKey sid
--     ph = Pact.unRequestKey rk
--     bdb = fromJuste $ preview (CutDB.cutDbBlockHeaderDb cid) cdb

--     logg = logFunctionJson (setComponent "spv-handler" l) Info
--       . PactCmdLogSpv

--     toErr e = throwError $ err400 { errBody = e }

-- -- -------------------------------------------------------------------------- --
-- -- Eth SPV Handler

-- ethSpvHandler
--     :: EthSpvRequest
--     -> Handler EthSpvResponse
-- ethSpvHandler req = do

--     -- find block with transaction
--     (block, rest) <- case evalState start Nothing of
--         Left () -> toErr $ "the transaction " <> sshow tx <> " is not contained in any of the provided blocks"
--         Right x -> return x

--     -- select and order set of receipts in the block
--     --
--     -- How big can blocks be? Should we create an index instead?
--     --
--     rcs <- forM (_rpcBlockTransactions block) $ \t -> do
--         case L.find (\r -> _rpcReceiptTransactionHash r == t) receipts of
--             Nothing -> toErr $ "missing receipt for tx " <> sshow t
--             Just x -> return x

--     -- select and order set of extra headers and create proof
--     case rpcReceiptProof (_rpcBlockHeader block) (hdrs block rest) rcs (TransactionIndex 28) of
--         Left e -> toErr $ "failed to create proof: " <> sshow e
--         Right proof -> return $ EthSpvResponse $
--             encodeB64UrlNoPaddingText (putRlpByteString proof)

--   where
--     receipts = _ethSpvReqReceipts req
--     blocks = _ethSpvReqBlocks req
--     orderedBlocks = L.sortOn (_hdrNumber . _rpcBlockHeader) blocks
--     tx = _ethSpvReqTransactionHash req

--     start = S.each orderedBlocks
--         & S.dropWhile (notElem tx . _rpcBlockTransactions)
--         & S.next

--     -- filter sequence consecutive headers
--     hdrs block rest = flip evalState (Just $ _rpcBlockHash block) $ rest
--         & S.filterM (\b -> do
--             c <- get
--             if Just (bytes $ _hdrParentHash $ _rpcBlockHeader b) == (bytes <$> c)
--                 then True <$ put (Just $ _rpcBlockHash b)
--                 else return False
--         )
--         & S.map _rpcBlockHeader
--         & S.toList_

--     toErr e = throwError $ err400 { errBody = e }

-- --------------------------------------------------------------------------- --
-- Poll Helper

internalPoll
    :: (CanReadablePayloadCas tbl, Logger logger)
    => logger
    -> MempoolBackend Pact.Transaction
    -> ServiceEnv tbl
    -> Maybe ConfirmationDepth
    -> NonEmpty Pact.RequestKey
    -> IO (HashMap Pact.RequestKey (Pact.CommandResult Pact.Hash Pact.PactOnChainError))
internalPoll logger mempool pact confDepth requestKeys0 = do
    let dbg txt = logFunctionText logger Debug txt
    -- get leaf block header for our chain from current best cut
    results0 <- throwIfNoHistory =<< execLookupPactTxs logger pact confDepth (coerce requestKeys)

    dbg $ "internalPoll.results0: " <> sshow results0
        -- TODO: are we sure that all of these are raised locally. This will cause the
        -- server to shut down the connection without returning a result to the user.
    let results1 = V.map (\rk -> (rk, HM.lookup (coerce rk) results0)) requestKeysV
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
    !requestKeysV = V.fromList $ NEL.toList requestKeys0
    !requestKeys = V.map Pact.unRequestKey requestKeysV

    lookup
        :: (Pact.RequestKey, T3 BlockHeight BlockPayloadHash BlockHash)
        -> IO (Either String (Maybe (Pact.RequestKey, Pact.CommandResult Pact.Hash Pact.PactOnChainError)))
    lookup (key, T3 bh bph bhsh) = (fmap . fmap . fmap) (key,) $ lookupRequestKey key bph bhsh bh

    -- TODO: group by block for performance (not very important right now)
    lookupRequestKey
      :: Pact.RequestKey
      -> BlockPayloadHash
      -> BlockHash
      -> BlockHeight
      -> IO (Either String (Maybe (Pact.CommandResult Pact.Hash Pact.PactOnChainError)))
    lookupRequestKey key payloadHash bHash height = runExceptT $ do
        let pactHash = Pact.unRequestKey key
        let matchingHash = (== pactHash) . Pact._cmdHash . fst
        let pdb = _payloadStoreTable (_psPdb pact)

        (_payloadWithOutputsTransactions -> txsBs) <-
          barf
            ("payload lookup failed w/ payload hash: " <> sshow payloadHash)
          =<< liftIO (lookupPayloadWithHeight pdb (Just height) payloadHash)
        !txs <- mapM fromTx txsBs
        case find matchingHash txs of
            Just (_cmd, TransactionOutput output) -> do
                out <- case eitherDecodeStrict' output of
                    Left err -> throwError $
                      "error decoding tx output for command " <> sshow (Pact._cmdHash _cmd) <> ": " <> err
                    Right decodedOutput -> return decodedOutput
                when (Pact._crReqKey out /= key) $
                    throwError "internal error: Transaction output doesn't match its hash!"
                return $ Just $ enrichCR payloadHash bHash height out
            Nothing -> return Nothing

    fromTx :: (Transaction, TransactionOutput) -> ExceptT String IO (Pact.Command Text, TransactionOutput)
    fromTx (Transaction txBytes, !out) = do
        !tx' <- except $ eitherDecodeStrict' txBytes & _Left %~
          (\decodeErr -> "Transaction failed to decode: " <> decodeErr)
        return (tx', out)

    checkBadList :: Vector Pact.RequestKey -> IO (Vector (Pact.RequestKey, Pact.CommandResult Pact.Hash Pact.PactOnChainError))
    checkBadList rkeys = do
        let !hashes = V.map pactRequestKeyToTransactionHash rkeys
        out <- mempoolCheckBadList mempool hashes
        let bad = V.map (Pact.RequestKey . Pact.Hash . unTransactionHash . fst) $
                  V.filter snd $ V.zip hashes out
        return $! V.map hashIsOnBadList bad

    hashIsOnBadList :: Pact.RequestKey -> (Pact.RequestKey, Pact.CommandResult Pact.Hash Pact.PactOnChainError)
    hashIsOnBadList rk =
        let res = Pact.PactResultErr err
            err = Pact.PactOnChainError
              -- the only legal error type, once chainweaver is really gone, we
              -- can use a real error type
              (Pact.ErrorType "EvalError")
              (Pact.mkBoundedText "Transaction is badlisted because it previously failed to validate.")
              (Pact.LocatedErrorInfo Pact.TopLevelErrorOrigin Pact.noInfo)
            !cr = Pact.CommandResult rk Nothing res (mempty :: Pact.Gas) Nothing Nothing Nothing []
        in (rk, cr)

    enrichCR :: BlockPayloadHash -> BlockHash -> BlockHeight -> Pact.CommandResult i e -> Pact.CommandResult i e
    enrichCR bph bhsh bh = set Pact.crMetaData
      (Just $ object
       [ "blockHeight" .= bh
       , "blockHash" .= bhsh
       , "blockPayloadHash" .= bph
       ])

-- -------------------------------------------------------------------------- --
-- Misc Utils

barf :: Monad m => e -> Maybe a -> ExceptT e m a
barf e = maybe (throwError e) return

-- TODO: all of the functions in this module can instead grab the current block height from consensus
-- and pass it here to get a better estimate of what behavior is correct.
validateCommand :: ChainwebVersion -> Pact.Command Text -> Either String Pact.Transaction
validateCommand _v cmdText = case parsedCmd of
  Right (commandParsed :: Pact.Transaction) ->
    if isRight (Pact.assertCommand commandParsed)
    then Right commandParsed
    else Left "Command failed validation"
  Left e -> Left $ "Pact parsing error: " ++ Pact.renderCompactString e
  where
    parsedCmd = Pact.parseCommand cmdText
