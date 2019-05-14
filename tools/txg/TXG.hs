{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Module: TXG
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- TODO
--

module TXG ( main ) where

import BasePrelude hiding (loop, rotate, timeout, (%))

import Configuration.Utils hiding (Error, Lens', (<.>))

import Control.Concurrent.Async (async, mapConcurrently_)
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Lens hiding (op, (.=), (|>))
import Control.Monad.Except
import Control.Monad.Reader hiding (local)
import Control.Monad.State.Strict

import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NEL
import Data.LogMessage
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Queue.Bounded as BQ
import Data.Sequence.NonEmpty (NESeq(..))
import qualified Data.Sequence.NonEmpty as NES
import Data.Text (Text)
import qualified Data.Text as T

import Fake (fake, generate)

import Network.HTTP.Client hiding (Proxy, host)
import Network.HTTP.Client.TLS
import Network.X509.SelfSigned hiding (name)

import Servant.API
import Servant.Client

import System.Logger hiding (StdOut)
import System.Random
import System.Random.MWC (createSystemRandom, uniformR)
import System.Random.MWC.Distributions (normal)

import Text.Pretty.Simple (pPrintNoColor)

-- PACT
import Pact.ApiReq
import Pact.Parse (ParsedDecimal(..), ParsedInteger(..))
import Pact.Types.API
import qualified Pact.Types.ChainMeta as CM
import Pact.Types.Command (Command(..), RequestKey(..))
import Pact.Types.Crypto
import qualified Pact.Types.Hash as H

-- CHAINWEB
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Pact.RestAPI
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

import TXG.Simulate.Contracts.CoinContract
import qualified TXG.Simulate.Contracts.Common as Sim
import TXG.Simulate.Contracts.HelloWorld
import TXG.Simulate.Contracts.SimplePayments
import TXG.Simulate.Utils
import TXG.Types

import Utils.Logging
import qualified Utils.Logging.Config as U

---

generateDelay :: MonadIO m => TXG m Int
generateDelay = do
  distribution <- asks _confTimingDist
  gen <- gets _gsGen
  case distribution of
    Just (Gaussian gmean gvar) -> liftIO (truncate <$> normal gmean gvar gen)
    Just (Uniform ulow uhigh) -> liftIO (truncate <$> uniformR (ulow, uhigh) gen)
    Nothing -> error "generateDelay: impossible"

generateSimpleTransaction
  :: (MonadIO m, MonadLog SomeLogMessage m)
  => TXG m (ChainId, Command Text)
generateSimpleTransaction = do
  delay <- generateDelay
  stdgen <- liftIO newStdGen
  let (operandA, operandB, op) =
        flip evalState stdgen $ do
            a <- state $ randomR (1, 100 :: Integer)
            b <- state $ randomR (1, 100 :: Integer)
            ind <- state $ randomR (0, 2 :: Int)
            let operation = "+-*" !! ind
            pure (a, b, operation)
      theCode = "(" ++ [op] ++ " " ++ show operandA ++ " " ++ show operandB ++ ")"

  -- Choose a Chain to send this transaction to, and cycle the state.
  cid <- uses gsChains NES.head
  gsChains %= rotate

  -- Delay, so as not to hammer the network.
  liftIO $ threadDelay delay
  -- lift . logg Info . toLogMessage . T.pack $ "The delay is " ++ show delay ++ " seconds."
  lift . logg Info . toLogMessage . T.pack $ printf "Sending expression %s to %s" theCode (show cid)
  kps <- liftIO testSomeKeyPairs


  let publicmeta = CM.PublicMeta
                   (CM.ChainId $ chainIdToText cid)
                   ("sender" <> toText cid)
                   (ParsedInteger 100)
                   (ParsedDecimal 0.0001)
      theData = object ["test-admin-keyset" .= fmap formatB16PubKey kps]
  cmd <- liftIO $ mkExec theCode theData publicmeta kps Nothing
  pure (cid, cmd)

-- | O(1). The head value is moved to the end.
rotate :: NESeq a -> NESeq a
rotate (h :<|| rest) = rest :||> h

generateTransaction
  :: (MonadIO m, MonadLog SomeLogMessage m)
  => TXG m (ChainId, Command Text)
generateTransaction = do
  contractIndex <- liftIO $ randomRIO @Int (0, 2)

  -- Choose a Chain to send this transaction to, and cycle the state.
  cid <- uses gsChains NES.head
  gsChains %= rotate

  sample <- case contractIndex of
    -- COIN CONTRACT
    0 -> do
      cks <- view confKeysets
      case M.lookup cid cks >>= traverse (M.lookup (Sim.ContractName "coin")) of
        Nothing -> error "A ChainId that should have `Account` entries does not."
        Just coinaccts -> liftIO $ do
          coinContractRequest <- mkRandomCoinContractRequest coinaccts >>= generate
          createCoinContractRequest (Sim.makeMeta cid) coinContractRequest
    1 -> liftIO $ generate fake >>= helloRequest
    2 -> do
      cks <- view confKeysets
      case M.lookup cid cks >>= traverse (M.lookup (Sim.ContractName "payment")) of
        Nothing -> error "A ChainId that should have `Account` entries does not."
        Just paymentAccts -> liftIO $ do
          paymentsRequest <- mkRandomSimplePaymentRequest paymentAccts >>= generate
          case paymentsRequest of
            SPRequestPay fromAccount _ _ -> case M.lookup fromAccount paymentAccts of
              Nothing ->
                error "This account does not have an associated keyset!"
              Just keyset ->
                createSimplePaymentRequest (Sim.makeMeta cid) paymentsRequest $ Just keyset
            SPRequestGetBalance _account ->
              createSimplePaymentRequest (Sim.makeMeta cid) paymentsRequest Nothing
            _ -> error "SimplePayments.CreateAccount code generation not supported"
    _ -> error "No contract here"
  delay <- generateDelay
  liftIO $ threadDelay delay
  lift $ logg Info (toLogMessage $ T.pack $ "The delay was " ++ show delay ++ " seconds.")
  pure (cid, sample)

sendTransaction
  :: MonadIO m
  => ChainId
  -> Command Text
  -> TXG m (Either ClientError RequestKeys)
sendTransaction cid cmd = do
  TXGConfig _ _ cenv v <- ask
  liftIO $ runClientM (send v cid $ SubmitBatch [cmd]) cenv

loop
  :: (MonadIO m, MonadLog SomeLogMessage m)
  => TXG m (ChainId, Command Text)
  -> TQueue Text
  -> MeasureTime
  -> TXG m ()
loop f tq measure@(MeasureTime _) = do
  (cid, transaction) <- f
  (_, requestKeys) <- measureDiffTime $ sendTransaction cid transaction
  countTV <- gets _gsCounter
  liftIO . atomically $ modifyTVar' countTV (+ 1)
  count <- liftIO $ readTVarIO countTV

  liftIO . atomically $ do
    -- writeTQueue tq $ "Sent transaction with request keys: " <> sshow requestKeys
    -- when mtime $ writeTQueue tq $ "Sending it took: " <> sshow timeTaken
    writeTQueue tq $ "Transaction count: " <> sshow count

  case requestKeys of
    Left servantError -> lift . logg Error $ toLogMessage (sshow servantError :: Text)
    Right rks -> do
      bq <- gets _gsRespTimes
      forkedListens tq bq cid rks

  logs <- liftIO . atomically $ flushTQueue tq
  lift $ traverse_ (logg Info . toLogMessage) logs
  loop f tq measure

forkedListens
  :: MonadIO m
  => TQueue Text
  -> TVar (BQ.BQueue Int)
  -> ChainId
  -> RequestKeys
  -> TXG m ()
forkedListens tq bq cid (RequestKeys rks) = do
  TXGConfig _ _ ce v <- ask
  liftIO $ traverse_ (forkedListen ce v) rks
  where
    forkedListen :: ClientEnv -> ChainwebVersion -> RequestKey -> IO ()
    forkedListen ce v rk = void . async $ do
      (!time, _) <- measureDiffTime $ runClientM (listen v cid $ ListenerRequest rk) ce
      atomically $ do
        q <- readTVar bq
        let q' = BQ.cons (floor time) q
        writeTVar bq q'
        writeTQueue tq $ "Average complete result time: " <> sshow (BQ.average q')
        -- writeTQueue tq $ sshow res
        -- writeTQueue tq $ "The associated request is " <> sshow rk <> "\n" <> sshow res

mkTXGConfig :: Maybe TimingDistribution -> ScriptConfig -> HostAddress -> IO TXGConfig
mkTXGConfig mdistribution config host =
  TXGConfig mdistribution mempty
  <$> cenv
  <*> pure (_nodeVersion config)
  where
    cenv :: IO ClientEnv
    cenv = do
       mgrSettings <- certificateCacheManagerSettings TlsInsecure Nothing
       let timeout = responseTimeoutMicro (1000000 * 60 * 4)
       mgr <- newTlsManagerWith (mgrSettings { managerResponseTimeout = timeout })
       let url = BaseUrl Https
                 (T.unpack . hostnameToText $ _hostAddressHost host)
                 (fromIntegral $ _hostAddressPort host)
                 ""
       pure $! mkClientEnv mgr url

mainInfo :: ProgramInfo ScriptConfig
mainInfo =
  programInfo
    "Chainweb-TransactionGenerator"
    scriptConfigParser
    defaultScriptConfig

type ContractLoader = CM.PublicMeta -> [SomeKeyPair] -> IO (Command Text)

loadContracts :: MeasureTime -> ScriptConfig -> HostAddress -> [ContractLoader] -> IO ()
loadContracts (MeasureTime mtime) config host contractLoaders = do
  (timeTaken, !_action) <- measureDiffTime go
  when mtime
    . withConsoleLogger Info
    . logg Info
    $ "Loading supplied contracts took: " <> sshow timeTaken
  where
    go :: IO ()
    go = do
      TXGConfig _ _ ce v <- mkTXGConfig Nothing config host
      forM_ (_nodeChainIds config) $ \cid -> do
        let !meta = Sim.makeMeta cid
        ts <- testSomeKeyPairs
        contracts <- traverse (\f -> f meta ts) contractLoaders
        pollresponse <- runExceptT $ do
          rkeys <- ExceptT $ runClientM (send v cid $ SubmitBatch contracts) ce
          ExceptT $ runClientM (poll v cid . Poll $ _rkRequestKeys rkeys) ce
        withConsoleLogger Info . logg Info $ sshow pollresponse

sendTransactions
  :: MeasureTime
  -> ScriptConfig
  -> HostAddress
  -> TVar Int64
  -> TimingDistribution
  -> LoggerT SomeLogMessage IO ()
sendTransactions measure config host tv distribution = do
  cfg@(TXGConfig _ _ ce v) <- liftIO $ mkTXGConfig (Just distribution) config host

  accountMap <- fmap (M.fromList . toList) . forM (_nodeChainIds config) $ \cid -> do
    let !meta = Sim.makeMeta cid
    (paymentKS, paymentAcc) <- liftIO $ unzip <$> Sim.createPaymentsAccounts meta
    (coinKS, coinAcc) <- liftIO $ unzip <$> Sim.createCoinAccounts meta
    pollresponse <- liftIO . runExceptT $ do
      rkeys <- ExceptT $ runClientM (send v cid . SubmitBatch $ paymentAcc ++ coinAcc) ce
      ExceptT $ runClientM (poll v cid . Poll $ _rkRequestKeys rkeys) ce
    logg Info $ toLogMessage (sshow pollresponse :: Text)
    logg Info $ toLogMessage ("Transactions are being generated" :: Text)
    let accounts = buildGenAccountsKeysets Sim.accountNames paymentKS coinKS
    pure (cid, accounts)

  -- Set up values for running the effect stack.
  gen <- liftIO createSystemRandom
  tq  <- liftIO newTQueueIO
  bq  <- liftIO . newTVarIO $ BQ.empty 32
  let act = loop generateTransaction tq measure
      env = set confKeysets accountMap cfg
      chs = maybe (versionChains $ _nodeVersion config) NES.fromList
            . NEL.nonEmpty
            $ _nodeChainIds config
      stt = TXGState gen tv chs bq

  evalStateT (runReaderT (runTXG act) env) stt
  where
    buildGenAccountsKeysets
      :: [Sim.Account]
      -> [[SomeKeyPair]]
      -> [[SomeKeyPair]]
      -> Map Sim.Account (Map Sim.ContractName [SomeKeyPair])
    buildGenAccountsKeysets accs pks cks = M.fromList $ zipWith3 go accs pks cks

    go :: Sim.Account
       -> [SomeKeyPair]
       -> [SomeKeyPair]
       -> (Sim.Account, Map Sim.ContractName [SomeKeyPair])
    go name pks cks = (name, M.fromList [ps, cs])
      where
        ps = (Sim.ContractName "payment", pks)
        cs = (Sim.ContractName "coin", cks)

versionChains :: ChainwebVersion -> NESeq ChainId
versionChains = NES.fromList . NEL.fromList . HS.toList . graphChainIds . _chainGraph

sendSimpleExpressions
  :: MeasureTime
  -> ScriptConfig
  -> HostAddress
  -> TVar Int64
  -> TimingDistribution
  -> LoggerT SomeLogMessage IO ()
sendSimpleExpressions measure config host tv distribution = do
  logg Info $ toLogMessage ("Transactions are being generated" :: Text)
  gencfg <- lift $ mkTXGConfig (Just distribution) config host

  -- Set up values for running the effect stack.
  gen <- liftIO createSystemRandom
  tq  <- liftIO newTQueueIO
  bq  <- liftIO . newTVarIO $ BQ.empty 32
  let chs = maybe (versionChains $ _nodeVersion config) NES.fromList
             . NEL.nonEmpty
             $ _nodeChainIds config
      stt = TXGState gen tv chs bq

  evalStateT (runReaderT (runTXG (loop generateSimpleTransaction tq measure)) gencfg) stt

pollRequestKeys :: MeasureTime -> ScriptConfig -> HostAddress -> RequestKey -> IO ()
pollRequestKeys (MeasureTime mtime) config host rkey = do
  (timeTaken, !_action) <- measureDiffTime go
  when mtime (putStrLn $ "" <> show timeTaken)
  where
    -- | It is assumed that the user has passed in a single, specific Chain that
    -- they wish to query.
    cid :: ChainId
    cid = fromMaybe (unsafeChainId 0) . listToMaybe $ _nodeChainIds config

    go :: IO a
    go = do
      putStrLn "Polling your requestKey"
      TXGConfig _ _ ce v <- mkTXGConfig Nothing config host
      response <- runClientM (poll v cid $ Poll [rkey]) ce
      case response of
        Left _ -> putStrLn "Failure" >> exitWith (ExitFailure 1)
        Right (PollResponses a)
          | null a -> putStrLn "Failure no result returned" >> exitWith (ExitFailure 1)
          | otherwise -> print a >> exitSuccess

listenerRequestKey :: MeasureTime -> ScriptConfig -> HostAddress -> ListenerRequest -> IO ()
listenerRequestKey (MeasureTime mtime) config host listenerRequest = do
  (timeTaken, response) <- measureDiffTime go
  when mtime (putStrLn $ "" <> show timeTaken)
  case response of
    Left err -> print err >> exitWith (ExitFailure 1)
    Right r -> print (_arResult r) >> exitSuccess
  where
    -- | It is assumed that the user has passed in a single, specific Chain that
    -- they wish to query.
    cid :: ChainId
    cid = fromMaybe (unsafeChainId 0) . listToMaybe $ _nodeChainIds config

    go :: IO (Either ClientError ApiResult)
    go = do
      putStrLn "Listening..."
      TXGConfig _ _ ce v <- mkTXGConfig Nothing config host
      runClientM (listen v cid listenerRequest) ce

work :: ScriptConfig -> IO ()
work cfg = do
  mgr <- newManager defaultManagerSettings
  tv  <- newTVarIO 0
  withBaseHandleBackend "transaction-generator" mgr (defconfig ^. U.logConfigBackend)
    $ \baseBackend -> do
      let loggerBackend = logHandles [] baseBackend
      withLogger (U._logConfigLogger defconfig) loggerBackend $ \l ->
        mapConcurrently_ (\host -> runLoggerT (act tv host) l) $ _hostAddresses cfg
  where
    transH :: U.HandleConfig
    transH = _logHandleConfig cfg

    defconfig :: U.LogConfig
    defconfig =
      U.defaultLogConfig
      & U.logConfigLogger . loggerConfigThreshold .~ Info
      & U.logConfigBackend . U.backendConfigHandle .~ transH
      & U.logConfigTelemetryBackend . enableConfigConfig . U.backendConfigHandle .~ transH

    act :: TVar Int64 -> HostAddress -> LoggerT SomeLogMessage IO ()
    act tv host@(HostAddress h p) = localScope (\_ -> [(toText h, toText p)]) $ do
      case _scriptCommand cfg of
        DeployContracts [] mtime -> liftIO $
          loadContracts mtime cfg host $ initAdminKeysetContract : defaultContractLoaders
        DeployContracts cs mtime -> liftIO $
          loadContracts mtime cfg host $ initAdminKeysetContract : map createLoader cs
        RunStandardContracts distribution mtime ->
          sendTransactions mtime cfg host tv distribution
        RunSimpleExpressions distribution mtime ->
          sendSimpleExpressions mtime cfg host tv distribution
        PollRequestKeys rk mtime -> liftIO $
          pollRequestKeys mtime cfg host . RequestKey $ H.Hash rk
        ListenerRequestKey rk mtime -> liftIO $
          listenerRequestKey mtime cfg host . ListenerRequest . RequestKey $ H.Hash rk

main :: IO ()
main = runWithConfiguration mainInfo $ \config -> do
  let chains = graphChainIds . _chainGraph $ _nodeVersion config
      isMem  = all (`HS.member` chains) $ _nodeChainIds config
  unless isMem $ error $
    printf "Invalid chain %s for given version\n" (show $ _nodeChainIds config)
  pPrintNoColor config
  work config

-- TODO: This is here for when a user wishes to deploy their own
-- contract to chainweb. We will have to carefully consider which
-- chain we'd like to send the contract to.

-- TODO: This function should also incorporate a user's keyset as well
-- if it is given.
createLoader :: Sim.ContractName -> ContractLoader
createLoader (Sim.ContractName contractName) meta kp = do
  theCode <- readFile (contractName <> ".pact")
  adminKeyset <- testSomeKeyPairs
  -- TODO: theData may change later
  let theData = object
                ["admin-keyset" .= fmap formatB16PubKey adminKeyset
                , T.append (T.pack contractName) "-keyset" .= fmap formatB16PubKey kp]
  mkExec theCode theData meta adminKeyset Nothing

defaultContractLoaders :: [ContractLoader]
defaultContractLoaders = [helloWorldContractLoader , simplePaymentsContractLoader]
  -- Remember coin contract is already loaded.

api version chainid =
  case someChainwebVersionVal version of
    SomeChainwebVersionT (_ :: Proxy cv) ->
      case someChainIdVal chainid of
        SomeChainIdT (_ :: Proxy cid) ->
          client
            (Proxy :: Proxy (PactApi cv cid))

send :: ChainwebVersion -> ChainId -> SubmitBatch -> ClientM RequestKeys
send version chainid = go
  where
    go :<|> _ :<|> _ :<|> _ = api version chainid

poll :: ChainwebVersion -> ChainId -> Poll -> ClientM PollResponses
poll version chainid = go
  where
    _ :<|> go :<|> _ :<|> _ = api version chainid

listen :: ChainwebVersion -> ChainId -> ListenerRequest -> ClientM ApiResult
listen version chainid = go
  where
    _ :<|> _ :<|> go :<|> _ = api version chainid

---------------------------
-- FOR DEBUGGING IN GHCI --
---------------------------
_genapi2 :: ChainwebVersion -> ChainId -> Text
_genapi2 version chainid =
  case someChainwebVersionVal version of
    SomeChainwebVersionT (_ :: Proxy cv) ->
      case someChainIdVal chainid of
        SomeChainIdT (_ :: Proxy cid) ->
          let p = (Proxy :: Proxy ('ChainwebEndpoint cv :> ChainEndpoint cid :> "pact" :> Reassoc SendApi))
          in toUrlPiece $ safeLink (Proxy :: (Proxy (PactApi cv cid))) p
