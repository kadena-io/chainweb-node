{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Lens hiding (op, (.=), (|>))
import Control.Monad.Except
import Control.Monad.Reader hiding (local)
import Control.Monad.State.Strict

import Data.Generics.Product.Fields (field)
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.LogMessage
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence.NonEmpty (NESeq(..))
import qualified Data.Sequence.NonEmpty as NES
import Data.Text (Text)
import qualified Data.Text as T

import Fake (fake, generate)

import Network.HTTP.Client hiding (Proxy, host)

import Servant.API
import Servant.Client

import System.Logger hiding (StdOut)
import System.Random
import System.Random.MWC (createSystemRandom, uniformR)
import System.Random.MWC.Distributions (normal)

import Text.Pretty.Simple (pPrintNoColor)

-- PACT
import Pact.ApiReq
import Pact.Types.API
import qualified Pact.Types.ChainMeta as CM
import Pact.Types.Command
import Pact.Types.Crypto
import qualified Pact.Types.Hash as H

-- CHAINWEB
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Pact.RestAPI
#if !MIN_VERSION_servant(0,15,0)
import Chainweb.RestAPI.Utils
#endif
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
  distribution <- asks confTimingDist
  gen <- gets gsGen
  case distribution of
    Just (GaussianTD (Gaussian gmean gvar)) -> liftIO (truncate <$> normal gmean gvar gen)
    Just (UniformTD (Uniform ulow uhigh)) -> liftIO (truncate <$> uniformR (ulow, uhigh) gen)
    Nothing -> error "generateDelay: impossible"

generateSimpleTransactions
  :: (MonadIO m, MonadLog SomeLogMessage m)
  => TXG m (ChainId, NonEmpty (Command Text))
generateSimpleTransactions = do
  -- Choose a Chain to send these transactions to, and cycle the state.
  cid <- NES.head <$> gets gsChains
  field @"gsChains" %= rotate
  -- Generate a batch of transactions
  stdgen <- liftIO newStdGen
  BatchSize batch <- asks confBatchSize
  cmds <- liftIO . sequenceA . nelReplicate batch $ f cid stdgen
  -- Delay, so as not to hammer the network.
  delay <- generateDelay
  liftIO $ threadDelay delay
  pure (cid, cmds)
  where
    f :: ChainId -> StdGen -> IO (Command Text)
    f cid stdgen = do
      let (operandA, operandB, op) = flip evalState stdgen $ do
            a <- state $ randomR (1, 100 :: Integer)
            b <- state $ randomR (1, 100 :: Integer)
            ind <- state $ randomR (0, 2 :: Int)
            let operation = "+-*" !! ind
            pure (a, b, operation)
          theCode = "(" ++ [op] ++ " " ++ show operandA ++ " " ++ show operandB ++ ")"

      -- this contains the key of sender00
      kps <- testSomeKeyPairs

      -- TODO Use `mkMeta` function
      let publicmeta = CM.PublicMeta
                       (CM.ChainId $ chainIdToText cid)
                       "sender00" 10 0.0001
          theData = object ["test-admin-keyset" .= fmap formatB16PubKey kps]
      mkExec theCode theData publicmeta (NEL.toList kps) Nothing

-- | O(1). The head value is moved to the end.
rotate :: NESeq a -> NESeq a
rotate (h :<|| rest) = rest :||> h

data CmdChoice = CoinContract | HelloWorld | Payments
  deriving (Show, Eq, Ord, Bounded, Enum)

randomEnum :: forall a. (Enum a, Bounded a) => IO a
randomEnum = toEnum <$> randomRIO @Int (0, fromEnum $ maxBound @a)

generateTransactions
  :: forall m. (MonadIO m, MonadLog SomeLogMessage m)
  => TXG m (ChainId, NonEmpty (Command Text))
generateTransactions = do
  contractIndex <- liftIO randomEnum

  -- Choose a Chain to send this transaction to, and cycle the state.
  cid <- NES.head <$> gets gsChains
  field @"gsChains" %= rotate

  cks <- asks confKeysets
  case M.lookup cid cks of
    Nothing -> error $ printf "%s is missing Accounts!" (show cid)
    Just accs -> do
      BatchSize batch <- asks confBatchSize
      cmds <- liftIO . sequenceA . nelReplicate batch $
        case contractIndex of
          CoinContract -> coinContract cid $ accounts "coin" accs
          HelloWorld -> generate fake >>= helloRequest
          Payments -> payments cid $ accounts "payment" accs
      generateDelay >>= liftIO . threadDelay
      pure (cid, cmds)
  where
    accounts :: String -> Map Sim.Account (Map Sim.ContractName a) -> Map Sim.Account a
    accounts s = fromJuste . traverse (M.lookup (Sim.ContractName s))

    coinContract :: ChainId -> Map Sim.Account (NonEmpty SomeKeyPair) -> IO (Command Text)
    coinContract cid coinaccts = do
      coinContractRequest <- mkRandomCoinContractRequest coinaccts >>= generate
      createCoinContractRequest (Sim.makeMeta cid) coinContractRequest

    payments :: ChainId -> Map Sim.Account (NonEmpty SomeKeyPair) -> IO (Command Text)
    payments cid paymentAccts = do
      paymentsRequest <- mkRandomSimplePaymentRequest paymentAccts >>= generate
      case paymentsRequest of
        SPRequestPay fromAccount _ _ -> case M.lookup fromAccount paymentAccts of
          Nothing ->
            error "This account does not have an associated keyset!"
          Just keyset ->
            simplePayReq (Sim.makeMeta cid) paymentsRequest $ Just keyset
        SPRequestGetBalance _account ->
          simplePayReq (Sim.makeMeta cid) paymentsRequest Nothing
        _ -> error "SimplePayments.CreateAccount code generation not supported"

sendTransactions
  :: TXGConfig
  -> ChainId
  -> NonEmpty (Command Text)
  -> IO (Either ClientError RequestKeys)
sendTransactions (TXGConfig _ _ cenv v _) cid cmds =
  runClientM (send v cid $ SubmitBatch cmds) cenv

loop
  :: (MonadIO m, MonadLog SomeLogMessage m)
  => TXG m (ChainId, NonEmpty (Command Text))
  -> TXG m ()
loop f = do
  (cid, transactions) <- f
  config <- ask
  requestKeys <- liftIO $ sendTransactions config cid transactions

  case requestKeys of
    Left servantError ->
      lift . logg Error $ toLogMessage (sshow servantError :: Text)
    Right _ -> do
      countTV <- gets gsCounter
      batch <- asks confBatchSize
      liftIO . atomically $ modifyTVar' countTV (+ fromIntegral batch)
      count <- liftIO $ readTVarIO countTV
      lift . logg Info $ toLogMessage ("Transaction count: " <> sshow count :: Text)

  loop f

type ContractLoader = CM.PublicMeta -> NonEmpty SomeKeyPair -> IO (Command Text)

loadContracts :: Args -> HostAddress -> NonEmpty ContractLoader -> IO ()
loadContracts config host contractLoaders = do
  TXGConfig _ _ ce v _ <- mkTXGConfig Nothing config host
  forM_ (nodeChainIds config) $ \cid -> do
    let !meta = Sim.makeMeta cid
    ts <- testSomeKeyPairs
    contracts <- traverse (\f -> f meta ts) contractLoaders
    pollresponse <- runExceptT $ do
      rkeys <- ExceptT $ runClientM (send v cid $ SubmitBatch contracts) ce
      ExceptT $ runClientM (poll v cid . Poll $ _rkRequestKeys rkeys) ce
    withConsoleLogger Info . logg Info $ sshow pollresponse

realTransactions
  :: Args
  -> HostAddress
  -> TVar TXCount
  -> TimingDistribution
  -> LoggerT SomeLogMessage IO ()
realTransactions config host tv distribution = do
  cfg@(TXGConfig _ _ ce v _) <- liftIO $ mkTXGConfig (Just distribution) config host

  let chains = maybe (versionChains $ nodeVersion config) NES.fromList
               . NEL.nonEmpty
               $ nodeChainIds config

  accountMap <- fmap (M.fromList . toList) . forM chains $ \cid -> do
    let !meta = Sim.makeMeta cid
    (paymentKS, paymentAcc) <- liftIO $ NEL.unzip <$> Sim.createPaymentsAccounts meta
    (coinKS, coinAcc) <- liftIO $ NEL.unzip <$> Sim.createCoinAccounts meta
    pollresponse <- liftIO . runExceptT $ do
      rkeys <- ExceptT $ runClientM (send v cid . SubmitBatch $ paymentAcc <> coinAcc) ce
      ExceptT $ runClientM (poll v cid . Poll $ _rkRequestKeys rkeys) ce
    case pollresponse of
      Left e -> logg Error $ toLogMessage (sshow e :: Text)
      Right _ -> pure ()
    let accounts = buildGenAccountsKeysets Sim.accountNames paymentKS coinKS
    pure (cid, accounts)

  logg Info $ toLogMessage ("Real Transactions: Transactions are being generated" :: Text)

  -- Set up values for running the effect stack.
  gen <- liftIO createSystemRandom
  let act = loop generateTransactions
      env = set (field @"confKeysets") accountMap cfg
      stt = TXGState gen tv chains

  evalStateT (runReaderT (runTXG act) env) stt
  where
    buildGenAccountsKeysets
      :: NonEmpty Sim.Account
      -> NonEmpty (NonEmpty SomeKeyPair)
      -> NonEmpty (NonEmpty SomeKeyPair)
      -> Map Sim.Account (Map Sim.ContractName (NonEmpty SomeKeyPair))
    buildGenAccountsKeysets accs pks cks =
      M.fromList . NEL.toList $ nelZipWith3 go accs pks cks

    go :: Sim.Account
       -> NonEmpty SomeKeyPair
       -> NonEmpty SomeKeyPair
       -> (Sim.Account, Map Sim.ContractName (NonEmpty SomeKeyPair))
    go name pks cks = (name, M.fromList [ps, cs])
      where
        ps = (Sim.ContractName "payment", pks)
        cs = (Sim.ContractName "coin", cks)

versionChains :: ChainwebVersion -> NESeq ChainId
versionChains = NES.fromList . NEL.fromList . HS.toList . graphChainIds . _chainGraph

simpleExpressions
  :: Args
  -> HostAddress
  -> TVar TXCount
  -> TimingDistribution
  -> LoggerT SomeLogMessage IO ()
simpleExpressions config host tv distribution = do
  logg Info $ toLogMessage ("Simple Expressions: Transactions are being generated" :: Text)
  gencfg <- lift $ mkTXGConfig (Just distribution) config host

  -- Set up values for running the effect stack.
  gen <- liftIO createSystemRandom
  let chs = maybe (versionChains $ nodeVersion config) NES.fromList
             . NEL.nonEmpty
             $ nodeChainIds config
      stt = TXGState gen tv chs

  evalStateT (runReaderT (runTXG (loop generateSimpleTransactions)) gencfg) stt

pollRequestKeys :: Args -> HostAddress -> RequestKey -> IO ()
pollRequestKeys config host rkey = do
  TXGConfig _ _ ce v _ <- mkTXGConfig Nothing config host
  response <- runClientM (poll v cid . Poll $ pure rkey) ce
  case response of
    Left _ -> putStrLn "Failure" >> exitWith (ExitFailure 1)
    Right (PollResponses a)
      | null a -> putStrLn "Failure no result returned" >> exitWith (ExitFailure 1)
      | otherwise -> print a >> exitSuccess
 where
    -- | It is assumed that the user has passed in a single, specific Chain that
    -- they wish to query.
    cid :: ChainId
    cid = fromMaybe (unsafeChainId 0) . listToMaybe $ nodeChainIds config

listenerRequestKey :: Args -> HostAddress -> ListenerRequest -> IO ()
listenerRequestKey config host listenerRequest = do
  TXGConfig _ _ ce v _ <- mkTXGConfig Nothing config host
  runClientM (listen v cid listenerRequest) ce >>= \case
    Left err -> print err >> exitWith (ExitFailure 1)
    Right r -> print r >> exitSuccess
  where
    -- | It is assumed that the user has passed in a single, specific Chain that
    -- they wish to query.
    cid :: ChainId
    cid = fromMaybe (unsafeChainId 0) . listToMaybe $ nodeChainIds config

-- | Send a single transaction to the network, and immediately listen for its result.
singleTransaction :: Args -> HostAddress -> SingleTX -> IO ()
singleTransaction args host (SingleTX c cid)
  | not . HS.member cid . chainIds $ nodeVersion args =
    putStrLn "Invalid target ChainId" >> exitWith (ExitFailure 1)
  | otherwise = do
      cfg <- mkTXGConfig Nothing args host
      kps <- testSomeKeyPairs
      cmd <- mkExec (T.unpack c) (datum kps) (Sim.makeMeta cid) (NEL.toList kps) Nothing
      runExceptT (f cfg cmd) >>= \case
        Left e -> print e >> exitWith (ExitFailure 1)
        Right res -> pPrintNoColor res
  where
    datum :: NonEmpty SomeKeyPair -> Value
    datum kps = object ["test-admin-keyset" .= fmap formatB16PubKey kps]

    f :: TXGConfig -> Command Text -> ExceptT ServantError IO ListenResponse
    f cfg@(TXGConfig _ _ ce v _) cmd = do
      RequestKeys (rk :| _) <- ExceptT . sendTransactions cfg cid $ pure cmd
      ExceptT $ runClientM (listen v cid $ ListenerRequest rk) ce

work :: Args -> IO ()
work cfg = do
  mgr <- newManager defaultManagerSettings
  tv  <- newTVarIO 0
  withBaseHandleBackend "transaction-generator" mgr (defconfig ^. U.logConfigBackend)
    $ \baseBackend -> do
      let loggerBackend = logHandles [] baseBackend
      withLogger (U._logConfigLogger defconfig) loggerBackend $ \l ->
        mapConcurrently_ (\host -> runLoggerT (act tv host) l) $ hostAddresses cfg
  where
    transH :: U.HandleConfig
    transH = logHandleConfig cfg

    defconfig :: U.LogConfig
    defconfig =
      U.defaultLogConfig
      & U.logConfigLogger . loggerConfigThreshold .~ Info
      & U.logConfigBackend . U.backendConfigHandle .~ transH
      & U.logConfigTelemetryBackend . enableConfigConfig . U.backendConfigHandle .~ transH

    act :: TVar TXCount -> HostAddress -> LoggerT SomeLogMessage IO ()
    act tv host@(HostAddress h p) = localScope (\_ -> [(toText h, toText p)]) $ do
      case scriptCommand cfg of
        DeployContracts [] -> liftIO $
          loadContracts cfg host $ NEL.cons initAdminKeysetContract defaultContractLoaders
        DeployContracts cs -> liftIO $
          loadContracts cfg host $ initAdminKeysetContract :| map createLoader cs
        RunStandardContracts distribution ->
          realTransactions cfg host tv distribution
        RunSimpleExpressions distribution ->
          simpleExpressions cfg host tv distribution
        PollRequestKeys rk -> liftIO $
          pollRequestKeys cfg host . RequestKey $ H.Hash rk
        ListenerRequestKey rk -> liftIO $
          listenerRequestKey cfg host . ListenerRequest . RequestKey $ H.Hash rk
        SingleTransaction stx -> liftIO $
          singleTransaction cfg host stx

main :: IO ()
main = runWithConfiguration mainInfo $ \config -> do
  let chains = graphChainIds . _chainGraph $ nodeVersion config
      isMem  = all (`HS.member` chains) $ nodeChainIds config
  unless isMem $ error $
    printf "Invalid chain %s for given version\n" (show $ nodeChainIds config)
  pPrintNoColor config
  work config

mainInfo :: ProgramInfo Args
mainInfo =
  programInfo
    "Chainweb-TransactionGenerator"
    scriptConfigParser
    defaultArgs

-- TODO: This is here for when a user wishes to deploy their own
-- contract to chainweb. We will have to carefully consider which
-- chain we'd like to send the contract to.

-- TODO: This function should also incorporate a user's keyset as well
-- if it is given.
createLoader :: Sim.ContractName -> ContractLoader
createLoader (Sim.ContractName contractName) meta kp = do
  theCode <- readFile (contractName <> ".pact")
  adminKS <- testSomeKeyPairs
  -- TODO: theData may change later
  let theData = object
                ["admin-keyset" .= fmap formatB16PubKey adminKS
                , T.append (T.pack contractName) "-keyset" .= fmap formatB16PubKey kp]
  mkExec theCode theData meta (NEL.toList adminKS) Nothing

-- Remember that coin contract is already loaded.
defaultContractLoaders :: NonEmpty ContractLoader
defaultContractLoaders =
  NEL.fromList [ helloWorldContractLoader, simplePaymentsContractLoader ]

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

listen :: ChainwebVersion -> ChainId -> ListenerRequest -> ClientM ListenResponse
listen version chainid = go
  where
    _ :<|> _ :<|> go :<|> _ = api version chainid

---------------------------
-- FOR DEBUGGING IN GHCI --
---------------------------
-- genapi2 :: ChainwebVersion -> ChainId -> Text
-- genapi2 version chainid =
--   case someChainwebVersionVal version of
--     SomeChainwebVersionT (_ :: Proxy cv) ->
--       case someChainIdVal chainid of
--         SomeChainIdT (_ :: Proxy cid) ->
--           let p = (Proxy :: Proxy ('ChainwebEndpoint cv :> ChainEndpoint cid :> "pact" :> Reassoc SendApi))
--           in toUrlPiece $ safeLink (Proxy :: (Proxy (PactApi cv cid))) p
