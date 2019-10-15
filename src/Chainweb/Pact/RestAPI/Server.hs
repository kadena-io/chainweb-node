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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Pact.RestAPI.Server
( PactServerData
, PactServerData_
, PactCmdLog(..)
, SomePactServerData(..)
, somePactServerData
, pactServer
, somePactServer
, somePactServers
) where


import Control.Applicative
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Lens (view, (^.), (^?!), _head)
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Reader
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe

import Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Short as SB
import Data.CAS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Singletons
import Data.Text (Text)
import Data.Text.Encoding
import Data.Tuple.Strict
import qualified Data.Vector as V

import qualified GHC.Event as Ev
import GHC.Generics

import Prelude hiding (init, lookup)

import Servant

import System.LogLevel

-- internal modules

import Pact.Types.API
import qualified Pact.Types.ChainId as Pact
import Pact.Types.Command
import Pact.Types.Hash (Hash(..))
import qualified Pact.Types.Hash as Pact

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.CutResources
import Chainweb.Cut
import qualified Chainweb.CutDB as CutDB
import Chainweb.Logger
import Chainweb.Mempool.Mempool
    (InsertError, InsertType(..), MempoolBackend(..), TransactionHash(..))
import Chainweb.Pact.RestAPI
import Chainweb.Pact.Service.Types
import Chainweb.Pact.SPV
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.SPV (SpvException(..))
import Chainweb.SPV.CreateProof
import Chainweb.Transaction (ChainwebTransaction, mkPayloadWithText)
import qualified Chainweb.TreeDB as TreeDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService

------------------------------------------------------------------------------
type PactServerData logger cas =
    (CutResources logger cas, ChainResources logger)

newtype PactServerData_ (v :: ChainwebVersionT) (c :: ChainIdT) logger cas
    = PactServerData_ { _unPactServerData :: PactServerData logger cas }

data SomePactServerData = forall v c logger cas
    . (KnownChainwebVersionSymbol v,
       KnownChainIdSymbol c,
       PayloadCas cas,
       Logger logger)
    => SomePactServerData (PactServerData_ v c logger cas)


somePactServerData
    :: PayloadCas cas
    => Logger logger
    => ChainwebVersion
    -> ChainId
    -> PactServerData logger cas
    -> SomePactServerData
somePactServerData v cid db =
    case someChainwebVersionVal v of
      (SomeChainwebVersionT (Proxy :: Proxy vt)) ->
          case someChainIdVal cid of
              (SomeChainIdT (Proxy :: Proxy cidt)) ->
                  SomePactServerData (PactServerData_ @vt @cidt db)


pactServer
    :: forall v c cas logger
     . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PayloadCas cas
    => Logger logger
    => PactServerData logger cas
    -> Server (PactServiceApi v c)
pactServer (cut, chain) =
    pactApiHandlers :<|> pactSpvHandler
  where
    cid = FromSing (SChainId :: Sing c)
    mempool = _chainResMempool chain
    logger = _chainResLogger chain

    pactApiHandlers
      = sendHandler logger mempool
      :<|> pollHandler logger cut cid chain
      :<|> listenHandler logger cut cid chain
      :<|> localHandler logger cut cid chain

    pactSpvHandler = spvHandler logger cut cid chain


somePactServer :: SomePactServerData -> SomeServer
somePactServer (SomePactServerData (db :: PactServerData_ v c logger cas))
    = SomeServer (Proxy @(PactServiceApi v c)) (pactServer @v @c $ _unPactServerData db)


somePactServers
    :: PayloadCas cas
    => Logger logger
    => ChainwebVersion
    -> [(ChainId, PactServerData logger cas)]
    -> SomeServer
somePactServers v =
    mconcat . fmap (somePactServer . uncurry (somePactServerData v))

data PactCmdLog
  = PactCmdLogSend (NonEmpty (Command Text))
  | PactCmdLogPoll (NonEmpty Text)
  | PactCmdLogListen Text
  | PactCmdLogLocal (Command Text)
  | PactCmdLogSpv Text
  deriving (Show, Generic, ToJSON, NFData)


sendHandler
    :: Logger logger
    => logger
    -> MempoolBackend ChainwebTransaction
    -> SubmitBatch
    -> Handler RequestKeys
sendHandler logger mempool (SubmitBatch cmds) = Handler $ do
    liftIO $ logg Info (PactCmdLogSend cmds)
    case traverse validateCommand cmds of
        Right enriched -> do
            let txs = V.fromList $ NEL.toList enriched
            -- If any of the txs in the batch fail validation, we reject them all.
            liftIO (mempoolInsertCheck mempool txs) >>= checkResult
            liftIO (mempoolInsert mempool UncheckedInsert txs)
            return $! RequestKeys $ NEL.map cmdToRequestKey enriched
        Left err -> failWith $ "Validation failed: " <> err
  where
    failWith :: String -> ExceptT ServerError IO a
    failWith err = throwError $ err400 { errBody = BSL8.pack err }

    logg = logFunctionJson (setComponent "send-handler" logger)

    toPactHash :: TransactionHash -> Pact.TypedHash h
    toPactHash (TransactionHash h) = Pact.TypedHash $ SB.fromShort h

    checkResult :: Either (T2 TransactionHash InsertError) () -> ExceptT ServerError IO ()
    checkResult (Right _) = pure ()
    checkResult (Left (T2 hash insErr)) =
        failWith $ concat [ "Validation failed for hash "
                          , show $ toPactHash hash
                          , ": "
                          , show insErr
                          ]

pollHandler
    :: PayloadCas cas
    => Logger logger
    => logger
    -> CutResources logger cas
    -> ChainId
    -> ChainResources logger
    -> Poll
    -> Handler PollResponses
pollHandler logger cutR cid chain (Poll request) = liftIO $ do
    logg Info $ PactCmdLogPoll $ fmap requestKeyToB16Text request
    -- get current best cut
    cut <- CutDB._cut $ _cutResCutDb cutR
    PollResponses <$> internalPoll cutR cid chain cut request
  where
    logg = logFunctionJson (setComponent "poll-handler" logger)

listenHandler
    :: PayloadCas cas
    => Logger logger
    => logger
    -> CutResources logger cas
    -> ChainId
    -> ChainResources logger
    -> ListenerRequest
    -> Handler ListenResponse
listenHandler logger cutR cid chain (ListenerRequest key) = do
    liftIO $ logg Info $ PactCmdLogListen $ requestKeyToB16Text $ key
    liftIO (handleTimeout runListen)
  where
    logg = logFunctionJson (setComponent "listen-handler" logger)
    runListen :: TVar Bool -> IO ListenResponse
    runListen timedOut = go Nothing
      where
        go :: Maybe Cut -> IO ListenResponse
        go !prevCut = do
            m <- waitForNewCut prevCut
            case m of
                Nothing -> return $! ListenTimeout defaultTimeout
                (Just cut) -> poll cut

        poll :: Cut -> IO ListenResponse
        poll cut = do
            hm <- internalPoll cutR cid chain cut (pure key)
            if HM.null hm
              then go (Just cut)
              else return $! ListenResponse $ snd $ head $ HM.toList hm

        waitForNewCut :: Maybe Cut -> IO (Maybe Cut)
        waitForNewCut lastCut = atomically $ do
             -- TODO: we should compute greatest common ancestor here to bound the
             -- search
             t <- readTVar timedOut
             if t
                 then return Nothing
                 else Just <$> do
                     !cut <- CutDB._cutStm $ _cutResCutDb cutR
                     when (lastCut == Just cut) retry
                     return cut

    -- TODO: make configurable
    defaultTimeout = 120 * 1000000      -- two minutes

    handleTimeout m = bracket init cleanup (m . fst)
      where
        init = do
            !tv <- newTVarIO False
            mgr <- Ev.getSystemTimerManager
            !tkey <- Ev.registerTimeout mgr defaultTimeout $
                     atomically $ writeTVar tv True
            return $! (tv, tkey)
        cleanup (_, tkey) = do
            mgr <- Ev.getSystemTimerManager
            Ev.unregisterTimeout mgr tkey

-- TODO: reimplement local in terms of pact execution service
localHandler
    :: Logger logger
    => logger
    -> CutResources logger cas
    -> ChainId
    -> ChainResources logger
    -> Command Text
    -> Handler (CommandResult Hash)
localHandler logger _ _ cr cmd = do
    liftIO $ logg Info $ PactCmdLogLocal cmd
    cmd' <- case validateCommand cmd of
      (Right !c) -> return c
      Left err ->
        throwError $ err400 { errBody = "Validation failed: " <> BSL8.pack err }
    r <- liftIO $ _pactLocal (_chainResPact cr) cmd'
    case r of
      Left err ->
        throwError $ err400 { errBody = "Execution failed: " <> BSL8.pack (show err) }
      (Right !r') -> return r'
  where
    logg = logFunctionJson (setComponent "local-handler" logger)


spvHandler
    :: forall cas l
    . ( Logger l
      , PayloadCas cas
      )
    => l
    -> CutResources l cas
        -- ^ cut resources contain the cut, payload, and
        -- block db
    -> ChainId
        -- ^ the chain id of the source chain id used in the
        -- execution of a cross-chain-transfer.
    -> ChainResources l
        -- ^ chain resources contain a pact service
    -> SpvRequest
        -- ^ Contains the (pact) chain id of the target chain id used in the
        -- 'target-chain' field of a cross-chain-transfer.
        -- Also contains the request key of of the cross-chain transfer
        -- tx request.
    -> Handler TransactionOutputProofB64
spvHandler l cutR cid chainR (SpvRequest rk (Pact.ChainId ptid)) = do
    liftIO $! logg (sshow ph)

    cut <- liftIO $! CutDB._cut cdb
    bh <- liftIO $! lookupCutM cid cut

    T2 bhe _bha <- liftIO (_pactLookup pe (Right bh) (pure ph)) >>= \case
      Left e ->
        toErr $ "Internal error: transaction hash lookup failed: " <> sshow e
      Right v -> case v ^?! _head of
        Nothing -> toErr $ "Transaction hash not found: " <> sshow ph
        Just t -> return t

    idx <- liftIO (getTxIdx bdb pdb bhe ph) >>= \case
      Left e -> toErr
        $ "Internal error: Index lookup for hash failed: "
        <> sshow e
      Right i -> return i

    tid <- chainIdFromText ptid
    p <- liftIO (try $ createTransactionOutputProof cdb tid cid bhe idx) >>= \case
      Left e@SpvExceptionTargetNotReachable{} ->
        toErr $ "SPV target not reachable: " <> spvErrOf e
      Left e@SpvExceptionVerificationFailed{} ->
        toErr $ "SPV verification failed: " <> spvErrOf e
      Left e -> toErr $ "Internal error: SPV verification failed: " <> spvErrOf e
      Right q -> return q

    return $! b64 p
  where
    pe = _chainResPact chainR
    ph = Pact.fromUntypedHash $ unRequestKey rk
    cdb = _cutResCutDb cutR
    bdb = _chainResBlockHeaderDb chainR
    pdb = view CutDB.cutDbPayloadCas cdb
    b64 = TransactionOutputProofB64
      . encodeB64UrlNoPaddingText
      . BSL8.toStrict
      . Aeson.encode

    logg = logFunctionJson (setComponent "spv-handler" l) Info
      . PactCmdLogSpv

    toErr e = throwError $ err400 { errBody = e }

    spvErrOf = BSL8.fromStrict
      . encodeUtf8
      . _spvExceptionMsg

------------------------------------------------------------------------------

internalPoll
    :: PayloadCas cas
    => CutResources logger cas
    -> ChainId
    -> ChainResources logger
    -> Cut
    -> NonEmpty RequestKey
    -> IO (HashMap RequestKey (CommandResult Hash))
internalPoll cutR cid chain cut requestKeys0 = do
    -- get leaf block header for our chain from current best cut
    chainLeaf <- lookupCutM cid cut
    results0 <- _pactLookup pactEx (Right chainLeaf) requestKeys >>= either throwM return
    let results = V.map (\(a, b) -> (a, fromJust b)) $
                  V.filter (isJust . snd) $
                  V.zip requestKeysV results0
    (HM.fromList . catMaybes . V.toList) <$> mapM lookup results
  where
    pactEx = view chainResPact chain
    !requestKeysV = V.fromList $ NEL.toList requestKeys0
    !requestKeys = V.map (Pact.fromUntypedHash . unRequestKey) requestKeysV
    pdb = cutR ^. cutsCutDb . CutDB.cutDbPayloadCas
    bhdb = _chainResBlockHeaderDb chain

    lookup
        :: (RequestKey, T2 BlockHeight BlockHash)
        -> IO (Maybe (RequestKey, CommandResult Hash))
    lookup (key, T2 _ ha) = fmap (key,) <$> lookupRequestKey key ha

    -- TODO: group by block for performance (not very important right now)
    lookupRequestKey key bHash = runMaybeT $ do
        let keyHash = unRequestKey key
        let pactHash = Pact.fromUntypedHash keyHash
        let matchingHash = (== pactHash) . _cmdHash . fst
        blockHeader <- liftIO $ TreeDB.lookupM bhdb bHash
        let payloadHash = _blockPayloadHash blockHeader
        (PayloadWithOutputs txsBs _ _ _ _ _) <- MaybeT $ casLookup pdb payloadHash
        !txs <- mapM fromTx txsBs
        case find matchingHash txs of
            (Just (_cmd, (TransactionOutput output))) -> do
                out <- MaybeT $ return $! decodeStrict' output
                when (_crReqKey out /= key) $
                    fail "internal error: Transaction output doesn't match its hash!"
                return out
            Nothing -> mzero

    fromTx (!tx, !out) = do
        !tx' <- MaybeT (return (toPactTx tx))
        return $! (tx', out)


toPactTx :: Transaction -> Maybe (Command Text)
toPactTx (Transaction b) = decodeStrict' b

validateCommand :: Command Text -> Either String ChainwebTransaction
validateCommand cmdText = case verifyCommand cmdBS of
    ProcSucc cmd -> Right (mkPayloadWithText <$> cmd)
    ProcFail err -> Left err
  where
    cmdBS :: Command ByteString
    cmdBS = encodeUtf8 <$> cmdText
