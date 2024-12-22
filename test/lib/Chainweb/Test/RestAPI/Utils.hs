{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Test.RestAPI.Utils
  -- * Debugging
( debug

  -- * Utils
, repeatUntil
, clientErrorStatusCode
, isFailureResponse
, getStatusCode

  -- * Pact client DSL
, PactTestFailure(..)
, PollingExpectation(..)
, local
, localTestToRetry
, spv
, ethSpv
, sending
, polling
, pollingWithDepth
, getCurrentBlockHeight

  -- * Rosetta client DSL
, RosettaTestException(..)
, accountBalance
, blockTransaction
, block
, constructionDerive
, constructionPreprocess
, constructionMetadata
, constructionPayloads
, constructionParse
, constructionCombine
, constructionHash
, constructionSubmit
, mempoolTransaction
, mempool
, networkOptions
, networkList
, networkStatus
) where


import Control.Lens
import Control.Monad.Catch
import Control.Retry

import Data.Foldable (toList)
import Data.Text (Text)
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Network.HTTP.Types.Status (Status(..))

import Rosetta

import Servant.Client

-- internal chainweb modules

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut.CutHashes (_cutHashes, _bhwhHeight)
import Chainweb.CutDB.RestAPI.Client
import Chainweb.Pact.RestAPI.Client
import Chainweb.Pact.RestAPI.EthSpv
import Chainweb.Pact.Types
import Chainweb.Rosetta.RestAPI.Client
import Chainweb.Version
import Chainweb.Test.Utils

-- internal pact modules

import qualified Pact.JSON.Encode as J
import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Hash
import qualified Pact.Core.Command.Server as Pact5
import qualified Pact.Core.Command.Types as Pact5
import qualified Pact.Types.API as Pact4
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL

-- ------------------------------------------------------------------ --
-- Defaults

debug :: String -> IO ()
#if DEBUG_TEST
debug = putStrLn
#else
debug = const $ return ()
#endif

-- ------------------------------------------------------------------ --
-- Pact api client utils w/ retry


data PactTestFailure
    = PollingFailure String
    | SendFailure String
    | LocalFailure String
    | SpvFailure String
    | GetBlockHeightFailure String
    deriving Show

instance Exception PactTestFailure

-- | Retry an IO action until it satisfies a predicate
--
repeatUntil :: (a -> IO Bool) -> IO a -> IO a
repeatUntil test action = retrying testRetryPolicy
    (\_ b -> not <$> test b)
    (const action)


-- | Calls to /local via the pact local api client with retry
--
localWithQueryParams
    :: ChainwebVersion
    -> ChainId
    -> ClientEnv
    -> Maybe LocalPreflightSimulation
    -> Maybe LocalSignatureVerification
    -> Maybe RewindDepth
    -> Command Text
    -> IO LocalResult
localWithQueryParams v sid cenv pf sv rd cmd =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "requesting local cmd for " <> take 19 (show cmd)
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- send a single local request and return the result
      --
      runClientM (pactLocalWithQueryApiClient v sid pf sv rd cmd) cenv >>= \case
        Left e -> throwM $ LocalFailure (show e)
        Right t -> return t
  where
    h _ = Handler $ \case
      LocalFailure _ -> pure True
      _ -> pure False

-- | Calls /local via the pact local api client with preflight
-- turned off. Retries.
--
local
    :: ChainwebVersion
    -> ChainId
    -> ClientEnv
    -> Command Text
    -> IO (CommandResult Hash)
local v sid cenv cmd = do
    Just cr <- preview _LocalResultLegacy <$>
      localWithQueryParams v sid cenv Nothing Nothing Nothing cmd
    Just pact4Cr <- return $
      Aeson.decode (TL.encodeUtf8 $ TL.fromStrict $ J.getJsonText cr)
    pure pact4Cr

localTestToRetry
    :: ChainwebVersion
    -> ChainId
    -> ClientEnv
    -> Command Text
    -> (CommandResult Hash -> Bool)
    -> IO (CommandResult Hash)
localTestToRetry v sid cenv cmd test =
    repeatUntil (return . test) (local v sid cenv cmd)

-- | Request an SPV proof using exponential retry logic
--
spv
    :: ChainwebVersion
    -> ChainId
    -> ClientEnv
    -> SpvRequest
    -> IO TransactionOutputProofB64
spv v sid cenv r =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "requesting spv proof for " <> show r
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- send a single spv request and return the result
      --
      runClientM (pactSpvApiClient v sid r) cenv >>= \case
        Left e -> throwM $ SpvFailure (show e)
        Right t -> return t
  where
    h _ = Handler $ \case
      SpvFailure _ -> return True
      _ -> return False

-- | Request an Eth SPV proof using exponential retry logic
--
ethSpv
    :: ChainwebVersion
    -> ChainId
    -> ClientEnv
    -> EthSpvRequest
    -> IO EthSpvResponse
ethSpv v sid cenv r =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "requesting eth-spv proof for " <> show (_ethSpvReqTransactionHash r)
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- send a single spv request and return the result
      --
      runClientM (ethSpvApiClient v sid r) cenv >>= \case
        Left e -> throwM $ SpvFailure (show e)
        Right t -> return t
  where
    h _ = Handler $ \case
      SpvFailure _ -> return True
      _ -> return False

-- | Send a batch with retry logic waiting for success.
sending
    :: ChainwebVersion
    -> ChainId
    -> ClientEnv
    -> SubmitBatch
    -> IO RequestKeys
sending v sid cenv batch =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "sending requestkeys " <> show (_cmdHash <$> toList ss)
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- Send and return naively
      --
      runClientM (pactSendApiClient v sid batch) cenv >>= \case
        Left e -> throwM $ SendFailure (show e)
        Right rs -> return rs

  where
    ss = _sbCmds batch

    h _ = Handler $ \case
      SendFailure _ -> return True
      _ -> return False

-- | Poll with retry using an exponential backoff
--
data PollingExpectation = ExpectPactError | ExpectPactResult
  deriving Eq

polling
    :: ChainwebVersion
    -> ChainId
    -> ClientEnv
    -> RequestKeys
    -> PollingExpectation
    -> IO Pact4.PollResponses
polling v sid cenv rks pollingExpectation =
  pollingWithDepth v sid cenv rks Nothing pollingExpectation

pollingWithDepth
    :: ChainwebVersion
    -> ChainId
    -> ClientEnv
    -> RequestKeys
    -> Maybe ConfirmationDepth
    -> PollingExpectation
    -> IO Pact4.PollResponses
pollingWithDepth v sid cenv rks confirmationDepth pollingExpectation =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "polling for requestkeys " <> show (toList rs)
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- Run the poll cmd loop and check responses
      -- by making sure results are successful and request keys
      -- are sane

      runClientM (pactPollWithQueryApiClient v sid confirmationDepth $ Pact5.PollRequest rs) cenv >>= \case
        Left e -> throwM $ PollingFailure (show e)
        Right r@(Pact5.PollResponse mp) ->
          if all (go mp) (toList rs)
          then do
            let pact4Resps = HM.fromList $
                  [ (toPact4RequestKey rk, toPact4CommandResult cr) | (rk, cr) <- HM.toList mp ]
            return $ Pact4.PollResponses pact4Resps
          else throwM $ PollingFailure $ T.unpack $ "polling check failed: " <> J.encodeText r
  where
    h _ = Handler $ \case
      PollingFailure _ -> return True
      _ -> return False

    rs = toPact5RequestKey <$> _rkRequestKeys rks

    validate Pact5.PactResultOk{} = pollingExpectation == ExpectPactResult
    validate Pact5.PactResultErr{} = pollingExpectation == ExpectPactError

    go m rk = case m ^. at rk of
      Just cr -> Pact5._crReqKey cr == rk && validate (Pact5._crResult cr)
      Nothing -> False

getCurrentBlockHeight :: ChainwebVersion -> ClientEnv -> ChainId -> IO BlockHeight
getCurrentBlockHeight сv cenv cid =
  runClientM (cutGetClient сv) cenv >>= \case
    Left e -> throwM $ GetBlockHeightFailure $ "Failed to get cuts: " ++ show e
    Right cuts -> return $ fromJust $ _bhwhHeight <$> HM.lookup cid (_cutHashes cuts)

-- ------------------------------------------------------------------ --
-- Rosetta api client utils w/ retry

data RosettaTestException
    = AccountBalanceFailure String
    | BlockTransactionFailure String
    | BlockFailure String
    | ConstructionPreprocessFailure String
    | ConstructionMetadataFailure String
    | ConstructionPayloadsFailure String
    | ConstructionParseFailure String
    | ConstructionCombineFailure String
    | ConstructionHashFailure String
    | ConstructionSubmitFailure String
    | MempoolTransactionFailure String
    | MempoolFailure String
    | NetworkListFailure String
    | NetworkOptionsFailure String
    | NetworkStatusFailure String
    deriving Show

instance Exception RosettaTestException

accountBalance
    :: ChainwebVersion
    -> ClientEnv
    -> AccountBalanceReq
    -> IO AccountBalanceResp
accountBalance v cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting account balance for " <> show req
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaAccountBalanceApiClient v req) cenv >>= \case
      Left e -> throwM $ AccountBalanceFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      AccountBalanceFailure _ -> return True
      _ -> return False

blockTransaction
    :: ChainwebVersion
    -> ClientEnv
    -> BlockTransactionReq
    -> IO BlockTransactionResp
blockTransaction v cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting block transaction for " <> show req
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaBlockTransactionApiClient v req) cenv >>= \case
      Left e -> throwM $ BlockTransactionFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      BlockTransactionFailure _ -> return True
      _ -> return False

block
    :: ChainwebVersion
    -> ClientEnv
    -> BlockReq
    -> IO BlockResp
block v cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting block for " <> show req
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaBlockApiClient v req) cenv >>= \case
      Left e -> throwM $ BlockFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      BlockFailure _ -> return True
      _ -> return False

constructionDerive
    :: ChainwebVersion
    -> ClientEnv
    -> ConstructionDeriveReq
    -> IO ConstructionDeriveResp
constructionDerive v cenv req =
  recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting derive preprocess for " <> (show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaConstructionDeriveApiClient v req) cenv >>= \case
      Left e -> throwM $ ConstructionPreprocessFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      ConstructionPreprocessFailure _ -> return True
      _ -> return False

constructionPreprocess
    :: ChainwebVersion
    -> ClientEnv
    -> ConstructionPreprocessReq
    -> IO ConstructionPreprocessResp
constructionPreprocess v cenv req =
  recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting construction preprocess for " <> (show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaConstructionPreprocessApiClient v req) cenv >>= \case
      Left e -> throwM $ ConstructionPreprocessFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      ConstructionPreprocessFailure _ -> return True
      _ -> return False

constructionMetadata
    :: ChainwebVersion
    -> ClientEnv
    -> ConstructionMetadataReq
    -> IO ConstructionMetadataResp
constructionMetadata v cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting construction metadata for " <> show req
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaConstructionMetadataApiClient v req) cenv >>= \case
      Left e -> throwM $ ConstructionMetadataFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      ConstructionMetadataFailure _ -> return True
      _ -> return False

constructionPayloads
    :: ChainwebVersion
    -> ClientEnv
    -> ConstructionPayloadsReq
    -> IO ConstructionPayloadsResp
constructionPayloads v cenv req =
  recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting construction payloads for " <> (show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaConstructionPayloadsApiClient v req) cenv >>= \case
      Left e -> throwM $ ConstructionPayloadsFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      ConstructionPayloadsFailure _ -> return True
      _ -> return False

constructionParse
    :: ChainwebVersion
    -> ClientEnv
    -> ConstructionParseReq
    -> IO ConstructionParseResp
constructionParse v cenv req =
  recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting construction parse for " <> (show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaConstructionParseApiClient v req) cenv >>= \case
      Left e -> throwM $ ConstructionParseFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      ConstructionParseFailure _ -> return True
      _ -> return False

constructionCombine
    :: ChainwebVersion
    -> ClientEnv
    -> ConstructionCombineReq
    -> IO ConstructionCombineResp
constructionCombine v cenv req =
  recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting construction combine for " <> (show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaConstructionCombineApiClient v req) cenv >>= \case
      Left e -> throwM $ ConstructionCombineFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      ConstructionCombineFailure _ -> return True
      _ -> return False

constructionHash
    :: ChainwebVersion
    -> ClientEnv
    -> ConstructionHashReq
    -> IO TransactionIdResp
constructionHash v cenv req =
  recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting construction hash for " <> (show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaConstructionHashApiClient v req) cenv >>= \case
      Left e -> throwM $ ConstructionHashFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      ConstructionHashFailure _ -> return True
      _ -> return False

constructionSubmit
    :: ChainwebVersion
    -> ClientEnv
    -> ConstructionSubmitReq
    -> IO TransactionIdResp
constructionSubmit v cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting construction submit for " <> show req
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaConstructionSubmitApiClient v req) cenv >>= \case
      Left e -> throwM $ ConstructionSubmitFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      ConstructionSubmitFailure _ -> return True
      _ -> return False

mempoolTransaction
    :: ChainwebVersion
    -> ClientEnv
    -> MempoolTransactionReq
    -> IO MempoolTransactionResp
mempoolTransaction v cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting mempool transaction for " <> show req
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaMempoolTransactionApiClient v req) cenv >>= \case
      Left e -> throwM $ MempoolTransactionFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      MempoolTransactionFailure _ -> return True
      _ -> return False

mempool
    :: ChainwebVersion
    -> ClientEnv
    -> NetworkReq
    -> IO MempoolResp
mempool v cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting mempool for " <> show req
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaMempoolApiClient v req) cenv >>= \case
      Left e -> throwM $ MempoolFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      MempoolFailure _ -> return True
      _ -> return False

networkList
    :: ChainwebVersion
    -> ClientEnv
    -> MetadataReq
    -> IO NetworkListResp
networkList v cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting network list for " <> show req
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaNetworkListApiClient v req) cenv >>= \case
      Left e -> throwM $ NetworkListFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      NetworkListFailure _ -> return True
      _ -> return False

networkOptions
    :: ChainwebVersion
    -> ClientEnv
    -> NetworkReq
    -> IO NetworkOptionsResp
networkOptions v cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting network options for " <> show req
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaNetworkOptionsApiClient v req) cenv >>= \case
      Left e -> throwM $ NetworkOptionsFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      NetworkOptionsFailure _ -> return True
      _ -> return False

networkStatus
    :: ChainwebVersion
    -> ClientEnv
    -> NetworkReq
    -> IO NetworkStatusResp
networkStatus v cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting network status for " <> show req
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaNetworkStatusApiClient v req) cenv >>= \case
      Left e -> throwM $ NetworkStatusFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      NetworkStatusFailure _ -> return True
      _ -> return False

clientErrorStatusCode :: ClientError -> Maybe Int
clientErrorStatusCode = \case
  FailureResponse _ resp -> Just $ getStatusCode resp
  DecodeFailure _ resp -> Just $ getStatusCode resp
  UnsupportedContentType _ resp -> Just $ getStatusCode resp
  InvalidContentTypeHeader resp -> Just $ getStatusCode resp
  ConnectionError _ -> Nothing

isFailureResponse :: ClientError -> Bool
isFailureResponse = \case
  FailureResponse {} -> True
  _ -> False

getStatusCode :: ResponseF a -> Int
getStatusCode resp = statusCode (responseStatusCode resp)
