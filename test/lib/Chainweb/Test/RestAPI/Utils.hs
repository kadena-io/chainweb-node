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
