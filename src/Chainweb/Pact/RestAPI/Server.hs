{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Chainweb.Pact.RestAPI.Server where

import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar
import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.CAS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Singletons
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Vector as V
import qualified GHC.Event as Ev
import qualified Pact.Server.ApiServer as P
import Pact.Types.API
import Pact.Types.Command
import Prelude hiding (init, lookup)
import Servant

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.CutResources
import Chainweb.Cut
import qualified Chainweb.CutDB as CutDB
import Chainweb.Mempool.Mempool (MempoolBackend(..))
import Chainweb.Pact.RestAPI
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Transaction (ChainwebTransaction, PayloadWithText(..))
import qualified Chainweb.TreeDB as TreeDB
import Chainweb.Version

------------------------------------------------------------------------------
pactServer
    :: KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PayloadCas cas
    => CutResources logger cas
    -> ChainResources logger
    -> P.ApiEnv
    -> Server (PactApi v c)
pactServer cut chain conf =
    sendHandler mempool :<|>
    pollHandler cut cid chain :<|>
    listenHandler cut cid chain :<|>
    localHandler conf
  where
    cid = FromSing (SChainId :: Sing c)
    mempool = _chainResMempool chain


sendHandler :: MempoolBackend ChainwebTransaction -> SubmitBatch -> Handler NoContent
sendHandler mempool (SubmitBatch cmds) = Handler $ do
  case traverse validateCommand cmds of
    Just enriched -> do
      liftIO $ mempoolInsert mempool $! V.fromList enriched
      return NoContent
    Nothing ->
      throwError $ err400 { errBody = "Validation failed." }


pollHandler
    :: PayloadCas cas
    => CutResources logger cas
    -> ChainId
    -> ChainResources logger
    -> Poll
    -> Handler PollResponses
pollHandler cutR cid chain (Poll request) = liftIO $ do
    -- get current best cut
    cut <- CutDB._cut $ _cutResCutDb cutR
    PollResponses <$> internalPoll cutR cid chain cut request


listenHandler
    :: PayloadCas cas
    => CutResources logger cas
    -> ChainId
    -> ChainResources logger
    -> ListenerRequest
    -> Handler ApiResult
listenHandler cutR cid chain (ListenerRequest key) =
    liftIO $ handleTimeout runListen
  where
    nullResponse = ApiResult (object []) Nothing Nothing

    runListen timedOut = go Nothing
      where
        go !prevCut = do
            m <- waitForNewCut prevCut
            case m of
                Nothing -> return nullResponse      -- timeout
                (Just cut) -> poll cut

        poll cut = do
            hm <- internalPoll cutR cid chain cut [key]
            if HashMap.null hm
              then go (Just cut)
              else return $! snd $ head $ HashMap.toList hm

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


localHandler :: P.ApiEnv -> Command Text -> Handler (CommandSuccess Value)
localHandler conf x = Handler $ runReaderT (P.localHandler x) conf


------------------------------------------------------------------------------
internalPoll
    :: PayloadCas cas
    => CutResources logger cas
    -> ChainId
    -> ChainResources logger
    -> Cut
    -> [RequestKey]
    -> IO (HashMap RequestKey ApiResult)
internalPoll cutR cid chain cut requestKeys =
    toHashMap <$> mapM lookup requestKeys
  where
    lookup :: RequestKey -> IO (Maybe (RequestKey, ApiResult))
    lookup key = fmap (key,) <$> lookupRequestKey cid cut cutR chain key

    toHashMap = HashMap.fromList . catMaybes


lookupRequestKey
    :: PayloadCas cas
    => ChainId
    -> Cut
    -> CutResources logger cas
    -> ChainResources logger
    -> RequestKey
    -> IO (Maybe ApiResult)
lookupRequestKey cid cut cutResources chain key = do
    -- get leaf block header for our chain from current best cut
    chainLeaf <- lookupCutM cid cut

    let leafHeight = _blockHeight chainLeaf
    let minHeight = boundHeight leafHeight

    -- walk backwards from there. Bound the number of blocks searched
    runMaybeT $ lookupRequestKeyInBlock cutResources chain key minHeight chainLeaf

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
    -> MaybeT IO ApiResult
lookupRequestKeyInBlock cutR chain key minHeight = go
  where
    go blockHeader = do
        -- TODO: bloom filter cache
        let payloadHash = _blockPayloadHash blockHeader
        let pdb = cutR ^. cutsCutDb . CutDB.cutDbPayloadCas
        (PayloadWithOutputs txsBs _ _ _) <- MaybeT $ casLookup pdb payloadHash
        txs <- mapM fromTx txsBs

        case find matchingHash txs of
            (Just (_cmd, (TransactionOutput output))) -> do
                -- TODO: ApiResult has untyped fields and none of us is 100%
                -- sure what should go in here
                val <- MaybeT $ return $ decodeStrict output
                return $! ApiResult val Nothing Nothing    -- TODO: what should be here for metadata?

            Nothing -> lookupParent blockHeader

    fromTx (tx, out) = do
        !tx' <- MaybeT (return (toPactTx tx))
        return $! (tx', out)
    matchingHash (cmd, _) = _cmdHash cmd == unRequestKey key

    lookupParent blockHeader = do
        let parentHash = _blockParent blockHeader
        let bdb = _chainResBlockHeaderDb chain
        parentHeader <- liftIO $ TreeDB.lookupM bdb parentHash
        let parentHeight = _blockHeight parentHeader
        if parentHeight <= minHeight
          then MaybeT (return Nothing)
          else go parentHeader


toPactTx :: Transaction -> Maybe (Command Text)
toPactTx (Transaction b) = decodeStrict b


validateCommand :: Command Text -> Maybe (Command PayloadWithText)
validateCommand cmdText = let
  cmdBS = encodeUtf8 <$> cmdText
  in case verifyCommand cmdBS of
  ProcSucc cmd -> Just $ (\bs -> PayloadWithText bs (_cmdPayload cmd)) <$> cmdBS
  ProcFail{} -> Nothing


unimplemented :: Handler a
unimplemented = throwError $ err501 { errBody = "unimplemented" }
