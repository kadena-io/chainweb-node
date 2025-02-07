{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
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
, spv
, ethSpv
, sending
, polling
, pollingWithDepth
, getCurrentBlockHeight

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

import Servant.Client

-- internal chainweb modules

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut.CutHashes (_cutHashes, _bhwhHeight)
import Chainweb.CutDB.RestAPI.Client
import Chainweb.Pact.RestAPI.Client
import Chainweb.Pact.RestAPI.EthSpv
import Chainweb.Pact.Types
import Chainweb.Version
import Chainweb.Test.Utils

-- internal pact modules

import qualified Pact.JSON.Encode as J
import qualified Pact.Core.Command.Server as Pact
import qualified Pact.Core.Command.Types as Pact
import qualified Pact.Core.Hash as Pact
import qualified Pact.Core.Command.Client as Pact
import qualified Pact.Core.Errors as Pact

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
    :: HasVersion
    => ChainId
    -> ClientEnv
    -> Maybe LocalPreflightSimulation
    -> Maybe LocalSignatureVerification
    -> Maybe RewindDepth
    -> Pact.Command Text
    -> IO LocalResult
localWithQueryParams sid cenv pf sv rd cmd =
    runClientM (pactLocalApiClient sid pf sv rd cmd) cenv >>= \case
      Left e -> throwM $ LocalFailure (show e)
      Right t -> return t

-- | Calls /local via the pact local api client with preflight
-- turned off. Retries.
--
local
    :: HasVersion
    => ChainId
    -> ClientEnv
    -> Pact.Command Text
    -- TODO: PP. This needs to become a full PactError eventually
    -> IO (Pact.CommandResult Pact.Hash Pact.PactOnChainError)
local sid cenv cmd = do
    Just cr <- preview _LocalResultLegacy <$>
      localWithQueryParams sid cenv Nothing Nothing Nothing cmd
    return cr

-- | Request an SPV proof using exponential retry logic
--
spv
    :: HasVersion
    => ChainId
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
      runClientM (pactSpvApiClient sid r) cenv >>= \case
        Left e -> throwM $ SpvFailure (show e)
        Right t -> return t
  where
    h _ = Handler $ \case
      SpvFailure _ -> return True
      _ -> return False

-- | Request an Eth SPV proof using exponential retry logic
--
ethSpv
    :: HasVersion
    => ChainId
    -> ClientEnv
    -> EthSpvRequest
    -> IO EthSpvResponse
ethSpv sid cenv r =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "requesting eth-spv proof for " <> show (_ethSpvReqTransactionHash r)
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- send a single spv request and return the result
      --
      runClientM (ethSpvApiClient sid r) cenv >>= \case
        Left e -> throwM $ SpvFailure (show e)
        Right t -> return t
  where
    h _ = Handler $ \case
      SpvFailure _ -> return True
      _ -> return False

-- | Send a batch.
sending
    :: HasVersion
    => ChainId
    -> ClientEnv
    -> Pact.SubmitBatch
    -> IO Pact.RequestKeys
sending sid cenv batch =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "sending requestkeys " <> show (Pact._cmdHash <$> toList ss)
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- Send and return naively
      --
      runClientM (pactSendApiClient sid (Pact.SendRequest batch)) cenv >>= \case
        Left e -> throwM $ SendFailure (show e)
        Right (Pact.SendResponse rs) -> return rs

  where
    ss = Pact._sbCmds batch

    h _ = Handler $ \case
      SendFailure _ -> return True
      _ -> return False

-- | Poll with retry using an exponential backoff
--
data PollingExpectation = ExpectPactError | ExpectPactResult
  deriving Eq

polling
    :: HasVersion
    => ChainId
    -> ClientEnv
    -> Pact.RequestKeys
    -> PollingExpectation
    -> IO Pact.PollResponse
polling sid cenv rks pollingExpectation =
  pollingWithDepth sid cenv rks Nothing pollingExpectation

pollingWithDepth
    :: HasVersion
    => ChainId
    -> ClientEnv
    -> Pact.RequestKeys
    -> Maybe ConfirmationDepth
    -> PollingExpectation
    -> IO Pact.PollResponse
pollingWithDepth sid cenv rks confirmationDepth pollingExpectation =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "polling for requestkeys " <> show (toList rs)
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- Run the poll cmd loop and check responses
      -- by making sure results are successful and request keys
      -- are sane

      runClientM (pactPollApiClient sid confirmationDepth $ Pact.PollRequest rs) cenv >>= \case
        Left e -> throwM $ PollingFailure (show e)
        Right r@(Pact.PollResponse mp) ->
          if all (go mp) (toList rs)
          then do
            return $ Pact.PollResponse mp
          else throwM $ PollingFailure $ T.unpack $ "polling check failed: " <> J.encodeText r
  where
    h _ = Handler $ \case
      PollingFailure _ -> return True
      _ -> return False

    rs = Pact._rkRequestKeys rks

    validate Pact.PactResultOk{} = pollingExpectation == ExpectPactResult
    validate Pact.PactResultErr{} = pollingExpectation == ExpectPactError

    go m rk = case m ^. at rk of
      Just cr -> Pact._crReqKey cr == rk && validate (Pact._crResult cr)
      Nothing -> False

getCurrentBlockHeight :: HasVersion => ClientEnv -> ChainId -> IO BlockHeight
getCurrentBlockHeight cenv cid =
  runClientM cutGetClient cenv >>= \case
    Left e -> throwM $ GetBlockHeightFailure $ "Failed to get cuts: " ++ show e
    Right cuts -> return $ fromJust $ _bhwhHeight <$> HM.lookup cid (_cutHashes cuts)

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
