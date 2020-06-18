{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Test.RestAPI.Utils
( -- * Retry Policy
  testRetryPolicy
  -- * Debugging
, debug
  -- * Pact client DSL
, PactTestFailure(..)
, PollingExpectation(..)
, local
, localTestToRetry
, spv
, sending
, polling
  -- * Rosetta client DSL
, RosettaTestException(..)
, accountBalance
, blockTransaction
, block
, constructionMetadata
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

import Data.Either
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

import Rosetta

import Servant.Client

-- internal chainweb modules

import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Pact.RestAPI.Client
import Chainweb.Pact.Service.Types
import Chainweb.Rosetta.RestAPI.Client
import Chainweb.Utils
import Chainweb.Version

-- internal pact modules

import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Hash

-- ------------------------------------------------------------------ --
-- Defaults

debug :: String -> IO ()
#if DEBUG_TEST
debug = putStrLn
#else
debug = const $ return ()
#endif

v :: ChainwebVersion
v = FastTimedCPM petersonChainGraph

-- | Backoff up to a constant 250ms, limiting to ~40s
-- (actually saw a test have to wait > 22s)
testRetryPolicy :: RetryPolicy
testRetryPolicy = stepped <> limitRetries 150
  where
    stepped = retryPolicy $ \rs -> case rsIterNumber rs of
      0 -> Just 20_000
      1 -> Just 50_000
      2 -> Just 100_000
      _ -> Just 250_000

-- ------------------------------------------------------------------ --
-- Pact api client utils w/ retry


data PactTestFailure
    = PollingFailure String
    | SendFailure String
    | LocalFailure String
    | SpvFailure String
    | SlowChain String
    deriving Show

instance Exception PactTestFailure

-- | Calls to /local via the pact local api client with retry
--
local
    :: ChainId
    -> ClientEnv
    -> Command Text
    -> IO (CommandResult Hash)
local sid cenv cmd =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "requesting local cmd for " <> (take 19 $ show cmd)
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- send a single spv request and return the result
      --
      runClientM (pactLocalApiClient v sid cmd) cenv >>= \case
        Left e -> throwM $ LocalFailure (show e)
        Right t -> return t
  where
    h _ = Handler $ \case
      LocalFailure _ -> return True
      _ -> return False

localTestToRetry
    :: ChainId
    -> ClientEnv
    -> Command Text
    -> (CommandResult Hash -> Bool)
    -> IO (CommandResult Hash)
localTestToRetry sid cenv cmd test = retrying testRetryPolicy check_ (\_ -> go)
  where
    go = local sid cenv cmd
    check_ _ cr = return $ not $ test cr

-- | Request an SPV proof using exponential retry logic
--
spv
    :: ChainId
    -> ClientEnv
    -> SpvRequest
    -> IO TransactionOutputProofB64
spv sid cenv r =
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

-- | Send a batch with retry logic waiting for success.
sending
    :: ChainId
    -> ClientEnv
    -> SubmitBatch
    -> IO RequestKeys
sending sid cenv batch =
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

polling
    :: ChainId
    -> ClientEnv
    -> RequestKeys
    -> PollingExpectation
    -> IO PollResponses
polling sid cenv rks pollingExpectation =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "polling for requestkeys " <> show (toList rs)
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- Run the poll cmd loop and check responses
      -- by making sure results are successful and request keys
      -- are sane

      runClientM (pactPollApiClient v sid $ Poll rs) cenv >>= \case
        Left e -> throwM $ PollingFailure (show e)
        Right r@(PollResponses mp) ->
          if all (go mp) (toList rs)
          then return r
          else throwM $ PollingFailure $ T.unpack $ "polling check failed: " <> encodeToText r
  where
    h _ = Handler $ \case
      PollingFailure _ -> return True
      _ -> return False

    rs = _rkRequestKeys rks

    validate (PactResult a) = case pollingExpectation of
      ExpectPactResult -> isRight a
      ExpectPactError -> isLeft a

    go m rk = case m ^. at rk of
      Just cr ->  _crReqKey cr == rk && validate (_crResult cr)
      Nothing -> False


-- ------------------------------------------------------------------ --
-- Rosetta api client utils w/ retry

data RosettaTestException
    = AccountBalanceFailure String
    | BlockTransactionFailure String
    | BlockFailure String
    | ConstructionMetadataFailure String
    | ConstructionSubmitFailure String
    | MempoolTransactionFailure String
    | MempoolFailure String
    | NetworkListFailure String
    | NetworkOptionsFailure String
    | NetworkStatusFailure String
    deriving Show

instance Exception RosettaTestException

accountBalance
    :: ClientEnv
    -> AccountBalanceReq
    -> IO AccountBalanceResp
accountBalance cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting account balance for " <> (take 10 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaAccountBalanceApiClient v req) cenv >>= \case
      Left e -> throwM $ AccountBalanceFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      AccountBalanceFailure _ -> return True
      _ -> return False

blockTransaction
    :: ClientEnv
    -> BlockTransactionReq
    -> IO BlockTransactionResp
blockTransaction cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting block transaction for " <> (take 10 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaBlockTransactionApiClient v req) cenv >>= \case
      Left e -> throwM $ BlockTransactionFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      BlockTransactionFailure _ -> return True
      _ -> return False

block
    :: ClientEnv
    -> BlockReq
    -> IO BlockResp
block cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting block for " <> (take 10 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaBlockApiClient v req) cenv >>= \case
      Left e -> throwM $ BlockFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      BlockFailure _ -> return True
      _ -> return False

constructionMetadata
    :: ClientEnv
    -> ConstructionMetadataReq
    -> IO ConstructionMetadataResp
constructionMetadata cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting construction metadata for " <> (take 10 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaConstructionMetadataApiClient v req) cenv >>= \case
      Left e -> throwM $ ConstructionMetadataFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      ConstructionMetadataFailure _ -> return True
      _ -> return False

constructionSubmit
    :: ClientEnv
    -> ConstructionSubmitReq
    -> IO ConstructionSubmitResp
constructionSubmit cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting construction submit for " <> (take 10 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaConstructionSubmitApiClient v req) cenv >>= \case
      Left e -> throwM $ ConstructionSubmitFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      ConstructionSubmitFailure _ -> return True
      _ -> return False

mempoolTransaction
    :: ClientEnv
    -> MempoolTransactionReq
    -> IO MempoolTransactionResp
mempoolTransaction cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting mempool transaction for " <> (take 10 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaMempoolTransactionApiClient v req) cenv >>= \case
      Left e -> throwM $ MempoolTransactionFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      MempoolTransactionFailure _ -> return True
      _ -> return False

mempool
    :: ClientEnv
    -> MempoolReq
    -> IO MempoolResp
mempool cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting mempool for " <> (take 10 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaMempoolApiClient v req) cenv >>= \case
      Left e -> throwM $ MempoolFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      MempoolFailure _ -> return True
      _ -> return False

networkList
    :: ClientEnv
    -> MetadataReq
    -> IO NetworkListResp
networkList cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting network list for " <> (take 10 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaNetworkListApiClient v req) cenv >>= \case
      Left e -> throwM $ NetworkListFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      NetworkListFailure _ -> return True
      _ -> return False

networkOptions
    :: ClientEnv
    -> NetworkReq
    -> IO NetworkOptionsResp
networkOptions cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting network options for " <> (take 10 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaNetworkOptionsApiClient v req) cenv >>= \case
      Left e -> throwM $ NetworkOptionsFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      NetworkOptionsFailure _ -> return True
      _ -> return False

networkStatus
    :: ClientEnv
    -> NetworkReq
    -> IO NetworkStatusResp
networkStatus cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting network status for " <> (take 10 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaNetworkStatusApiClient v req) cenv >>= \case
      Left e -> throwM $ NetworkStatusFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      NetworkStatusFailure _ -> return True
      _ -> return False
