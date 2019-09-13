{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Pact.RestAPI.Server
( PactServerData
, PactServerData_
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
import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as SB
import Data.CAS
import Data.HashMap.Strict (HashMap)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.Singletons
import Data.Text (Text)
import Data.Text.Encoding
import Data.Witherable (wither)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NEL
import qualified Data.Vector as V
import qualified GHC.Event as Ev

import GHC.Generics

import Prelude hiding (init, lookup)

import Servant

import System.LogLevel

-- internal modules

import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Hash (Hash(..))
import qualified Pact.Types.Hash as H

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.CutResources
import Chainweb.Cut
import qualified Chainweb.CutDB as CutDB
import Chainweb.Logger
import Chainweb.Mempool.Mempool (MempoolBackend(..))
import Chainweb.Pact.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Transaction (ChainwebTransaction, PayloadWithText(..))
import qualified Chainweb.TreeDB as TreeDB
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
    -> Server (PactApi v c)
pactServer (cut, chain) =
    sendHandler logger mempool :<|>
    pollHandler logger cut cid chain :<|>
    listenHandler logger cut cid chain :<|>
    localHandler logger cut cid chain
  where
    cid = FromSing (SChainId :: Sing c)
    mempool = _chainResMempool chain
    logger = _chainResLogger chain


somePactServer :: SomePactServerData -> SomeServer
somePactServer (SomePactServerData (db :: PactServerData_ v c logger cas))
    = SomeServer (Proxy @(PactApi v c)) (pactServer @v @c $ _unPactServerData db)


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
  | PactCmdLogPoll (NonEmpty ByteString)
  | PactCmdLogListen ByteString
  | PactCmdLogLocal (Command Text)
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
        liftIO $ mempoolInsert mempool $! V.fromList $ NEL.toList enriched
        return $! RequestKeys $ NEL.map cmdToRequestKey enriched
      Left err ->
        throwError $ err400 { errBody = "Validation failed: " <> BSL8.pack err }
  where
    logg = logFunctionJson (setComponent "send-handler" logger)

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
    logg Info $ PactCmdLogPoll $ fmap (unHash . unRequestKey) request
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
    liftIO $ logg Info $ PactCmdLogListen $ unHash $ unRequestKey $ key
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


------------------------------------------------------------------------------
internalPoll
    :: PayloadCas cas
    => CutResources logger cas
    -> ChainId
    -> ChainResources logger
    -> Cut
    -> NonEmpty RequestKey
    -> IO (HashMap RequestKey (CommandResult Hash))
internalPoll cutR cid chain cut requestKeys =
    HM.fromList <$> wither lookup (NEL.toList requestKeys)
  where
    lookup :: RequestKey -> IO (Maybe (RequestKey, CommandResult Hash))
    lookup key = fmap (key,) <$> lookupRequestKey cid cut cutR chain key

lookupRequestKey
    :: PayloadCas cas
    => ChainId
    -> Cut
    -> CutResources logger cas
    -> ChainResources logger
    -> RequestKey
    -> IO (Maybe (CommandResult Hash))
lookupRequestKey cid cut cutResources chain key = do
    -- get leaf block header for our chain from current best cut
    chainLeaf <- lookupCutM cid cut

    let leafHeight = _blockHeight chainLeaf
    let minHeight = boundHeight leafHeight

    -- walk backwards from there. Bound the number of blocks searched
    runMaybeT $ lookupRequestKeyInBlock cutResources chain key
                                        minHeight chainLeaf

  where
    boundHeight h | h <= maxHeightDelta = 0
                  | otherwise = h - maxHeightDelta
    maxHeightDelta = 8192       -- TODO: configurable


lookupRequestKeyInBlock
    :: PayloadCas cas
    => CutResources logger cas  -- ^ cut resources
    -> ChainResources logger    -- ^ chain
    -> RequestKey               -- ^ key to search
    -> BlockHeight              -- ^ lowest block to search
    -> BlockHeader              -- ^ search starts here
    -> MaybeT IO (CommandResult Hash)
lookupRequestKeyInBlock cutR chain key minHeight = go
  where
    keyHash :: H.Hash
    keyHash = unRequestKey key

    pdb = cutR ^. cutsCutDb . CutDB.cutDbPayloadCas

    -- TODO: use the transaction index to implement pact tx lookup
    go !blockHeader = lookupInPayload blockHeader

    lookupInPayload blockHeader = do
        let payloadHash = _blockPayloadHash blockHeader
        (PayloadWithOutputs txsBs _ _ _ _ _) <- MaybeT $ casLookup pdb payloadHash
        !txs <- mapM fromTx txsBs

        case find matchingHash txs of
            (Just (_cmd, (TransactionOutput output))) ->
                MaybeT $ return $! decodeStrict' output

            Nothing -> lookupParent blockHeader

    fromTx (!tx, !out) = do
        !tx' <- MaybeT (return (toPactTx tx))
        return $! (tx', out)
    matchingHash (cmd, _) = H.toUntypedHash (_cmdHash cmd) == keyHash

    lookupParent blockHeader = do
        let parentHash = _blockParent blockHeader
        let bdb = _chainResBlockHeaderDb chain
        parentHeader <- liftIO $ TreeDB.lookupM bdb parentHash
        let parentHeight = _blockHeight parentHeader
        if parentHeight <= minHeight
          then empty
          else go parentHeader


toPactTx :: Transaction -> Maybe (Command Text)
toPactTx (Transaction b) = decodeStrict' b

validateCommand :: Command Text -> Either String ChainwebTransaction
validateCommand cmdText = let
  cmdBS = encodeUtf8 <$> cmdText
  in case verifyCommand cmdBS of
  ProcSucc cmd -> return $! (\bs -> PayloadWithText (SB.toShort bs) (_cmdPayload cmd)) <$> cmdBS
  ProcFail err -> Left $ err
