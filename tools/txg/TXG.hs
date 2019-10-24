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
{-# LANGUAGE TupleSections #-}
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
import qualified Data.List.NonEmpty as NEL
import Data.LogMessage
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence.NonEmpty (NESeq(..))
import qualified Data.Sequence.NonEmpty as NES
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
import qualified Pact.Types.ChainId as CI
import qualified Pact.Types.ChainMeta as CM
import Pact.Types.Command
import qualified Pact.Types.Hash as H
import Pact.Types.Capability
import Pact.Types.Info (mkInfo)
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.Exp (Literal(..))

-- CHAINWEB
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Pact.RestAPI
#if !MIN_VERSION_servant(0,16,0)
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

import qualified Chainweb.Logging.Config as U
import Utils.Logging
import qualified Utils.Logging.Config as U

import Chainweb.Mempool.RestAPI
import Chainweb.Mempool.Mempool
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
  => TXG m (ChainId, NonEmpty (Maybe Text), NonEmpty (Command Text))
generateSimpleTransactions = do
  -- Choose a Chain to send these transactions to, and cycle the state.
  cid <- NES.head <$> gets gsChains
  field @"gsChains" %= rotate
  -- Generate a batch of transactions
  stdgen <- liftIO newStdGen
  BatchSize batch <- asks confBatchSize
  v <- asks confVersion
  (msgs, cmds) <- liftIO . fmap NEL.unzip . sequenceA . nelReplicate batch $ f cid v stdgen
  -- Delay, so as not to hammer the network.
  delay <- generateDelay
  liftIO $ threadDelay delay
  pure (cid, msgs, cmds)
  where
    f :: ChainId -> ChainwebVersion -> StdGen -> IO (Maybe Text, Command Text)
    f cid v stdgen = do
      let (operandA, operandB, op) = flip evalState stdgen $ do
            a <- state $ randomR (1, 100 :: Integer)
            b <- state $ randomR (1, 100 :: Integer)
            ind <- state $ randomR (0, 2 :: Int)
            let operation = "+-*" !! ind
            pure (a, b, operation)
          theCode = "(" ++ [op] ++ " " ++ show operandA ++ " " ++ show operandB ++ ")"

      -- this contains the key of sender00
      kps <- testSomeKeyPairs

      let theData = object ["test-admin-keyset" .= fmap (formatB16PubKey . fst) kps]
      meta <- Sim.makeMeta cid
      (Nothing,) <$> mkExec theCode theData meta (NEL.toList kps) (Just $ CI.NetworkId $ toText v) Nothing

-- | O(1). The head value is moved to the end.
rotate :: NESeq a -> NESeq a
rotate (h :<|| rest) = rest :||> h

data CmdChoice = CoinContract | HelloWorld | Payments
  deriving (Show, Eq, Ord, Bounded, Enum)

randomEnum :: forall a. (Enum a, Bounded a) => IO a
randomEnum = toEnum <$> randomRIO @Int (0, fromEnum $ maxBound @a)

generateTransactions
    :: forall m. (MonadIO m, MonadLog SomeLogMessage m)
    => Bool
    -> Verbose
    -> CmdChoice
    -> TXG m (ChainId, NonEmpty (Maybe Text) , NonEmpty (Command Text))
generateTransactions ifCoinOnlyTransfers isVerbose contractIndex = do
  -- Choose a Chain to send this transaction to, and cycle the state.
  cid <- NES.head <$> gets gsChains
  field @"gsChains" %= rotate

  cks <- asks confKeysets
  version <- asks confVersion
  case M.lookup cid cks of
    Nothing -> error $ printf "%s is missing Accounts!" (show cid)
    Just accs -> do
      BatchSize batch <- asks confBatchSize
      (mmsgs, cmds) <- liftIO . fmap NEL.unzip . sequenceA . nelReplicate batch $
        case contractIndex of
          CoinContract -> coinContract version ifCoinOnlyTransfers isVerbose cid $ accounts "coin" accs
          HelloWorld -> (Nothing,) <$> (generate fake >>= helloRequest version)
          Payments -> (Nothing,) <$> (payments version cid $ accounts "payment" accs)
      generateDelay >>= liftIO . threadDelay
      pure (cid, mmsgs, cmds)
  where
    accounts :: String -> Map Sim.Account (Map Sim.ContractName a) -> Map Sim.Account a
    accounts s = fromJuste . traverse (M.lookup (Sim.ContractName s))

    coinContract
        :: ChainwebVersion
        -> Bool
        -> Verbose
        -> ChainId
        -> Map Sim.Account (NonEmpty SomeKeyPairCaps)
        -> IO (Maybe Text, Command Text)
    coinContract version transfers (Verbose vb) cid coinaccts = do
      coinContractRequest <- mkRandomCoinContractRequest transfers coinaccts >>= generate
      let msg = if vb then Just $ sshow coinContractRequest else Nothing
      let acclookup sn@(Sim.Account accsn) =
            case M.lookup sn coinaccts of
              Just ks -> (sn, ks)
              Nothing -> error $ "Couldn't find account: <" ++ accsn ++ ">"
      let (Sim.Account sender, ks) =
            case coinContractRequest of
              CoinCreateAccount account (Guard guardd) -> (account, guardd)
              CoinAccountBalance account -> acclookup account
              CoinTransfer (SenderName sn) rcvr amt -> (mkTransferCaps rcvr amt) $ acclookup sn
              CoinTransferAndCreate (SenderName acc) rcvr (Guard guardd) amt -> (mkTransferCaps rcvr amt) (acc, guardd)
      meta <- Sim.makeMetaWithSender sender cid
      (msg,) <$> createCoinContractRequest version meta ks coinContractRequest

    mkTransferCaps :: ReceiverName -> Sim.Amount -> (Sim.Account, NonEmpty SomeKeyPairCaps) -> (Sim.Account, NonEmpty SomeKeyPairCaps)
    mkTransferCaps (ReceiverName (Sim.Account r)) (Sim.Amount m) (s@(Sim.Account ss),ks) = (s, (caps <$) <$> ks)
      where caps = [gas,tfr]
            gas = SigCapability (QualifiedName "coin" "GAS" (mkInfo "coin.GAS")) []
            tfr = SigCapability (QualifiedName "coin" "TRANSFER" (mkInfo "coin.TRANSFER"))
                  [ PLiteral $ LString $ T.pack ss
                  , PLiteral $ LString $ T.pack r
                  , PLiteral $ LDecimal m]

    payments :: ChainwebVersion -> ChainId -> Map Sim.Account (NonEmpty SomeKeyPairCaps) -> IO (Command Text)
    payments v cid paymentAccts = do
      paymentsRequest <- mkRandomSimplePaymentRequest paymentAccts >>= generate
      case paymentsRequest of
        SPRequestPay fromAccount _ _ -> case M.lookup fromAccount paymentAccts of
          Nothing ->
            error "This account does not have an associated keyset!"
          Just keyset -> do
            meta <- Sim.makeMeta cid
            simplePayReq v meta paymentsRequest $ Just keyset
        SPRequestGetBalance _account -> do
          meta <- Sim.makeMeta cid
          simplePayReq v meta paymentsRequest Nothing
        _ -> error "SimplePayments.CreateAccount code generation not supported"

sendTransactions
  :: TXGConfig
  -> ChainId
  -> NonEmpty (Command Text)
  -> IO (Either ClientError RequestKeys)
sendTransactions (TXGConfig _ _ cenv v _ _) cid cmds =
  runClientM (send v cid $ SubmitBatch cmds) cenv

loop
  :: (MonadIO m, MonadLog SomeLogMessage m)
  => TXG m (ChainId, NonEmpty (Maybe Text), NonEmpty (Command Text))
  -> TXG m ()
loop f = do
  (cid, msgs, transactions) <- f
  config <- ask
  requestKeys <- liftIO $ sendTransactions config cid transactions

  case requestKeys of
    Left servantError ->
      lift . logg Error $ toLogMessage (sshow servantError :: Text)
    Right rk -> do
      countTV <- gets gsCounter
      batch <- asks confBatchSize
      liftIO . atomically $ modifyTVar' countTV (+ fromIntegral batch)
      count <- liftIO $ readTVarIO countTV
      lift . logg Info $ toLogMessage ("Transaction count: " <> sshow count :: Text)
      lift . logg Info $ toLogMessage ("Transaction requestKey: " <> sshow rk :: Text)
      forM_ (Compose msgs) $ \m ->
        lift . logg Info $ toLogMessage $ ("Actual transaction: " <> m :: Text)

  loop f

type ContractLoader
    = CM.PublicMeta -> NonEmpty SomeKeyPairCaps -> IO (Command Text)

loadContracts :: Args -> HostAddress -> NonEmpty ContractLoader -> IO ()
loadContracts config host contractLoaders = do
  TXGConfig _ _ ce v _ (Verbose vb) <- mkTXGConfig Nothing config host
  forM_ (nodeChainIds config) $ \cid -> do
    !meta <- Sim.makeMeta cid
    ts <- testSomeKeyPairs
    contracts <- traverse (\f -> f meta ts) contractLoaders
    pollresponse <- runExceptT $ do
      rkeys <- ExceptT $ runClientM (send v cid $ SubmitBatch contracts) ce
      when vb $ do
            withConsoleLogger Info $ do
                logg Info $ "sent contracts with request key: " <> sshow rkeys
      ExceptT $ runClientM (poll v cid . Poll $ _rkRequestKeys rkeys) ce
    withConsoleLogger Info . logg Info $ sshow pollresponse

realTransactions
  :: Args
  -> HostAddress
  -> TVar TXCount
  -> TimingDistribution
  -> LoggerT SomeLogMessage IO ()
realTransactions config host tv distribution = do
  cfg@(TXGConfig _ _ ce v _ _) <- liftIO $ mkTXGConfig (Just distribution) config host

  let chains = maybe (versionChains $ nodeVersion config) NES.fromList
               . NEL.nonEmpty
               $ nodeChainIds config

  accountMap <- fmap (M.fromList . toList) . forM chains $ \cid -> do
    !meta <- liftIO $ Sim.makeMeta cid
    (paymentKS, paymentAcc) <- liftIO $ NEL.unzip <$> Sim.createPaymentsAccounts v meta
    (coinKS, coinAcc) <- liftIO $ NEL.unzip <$> Sim.createCoinAccounts v meta
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
  let act = loop (liftIO randomEnum >>= generateTransactions False (verbose config))
      env = set (field @"confKeysets") accountMap cfg
      stt = TXGState gen tv chains

  evalStateT (runReaderT (runTXG act) env) stt
  where
    buildGenAccountsKeysets
      :: NonEmpty Sim.Account
      -> NonEmpty (NonEmpty SomeKeyPairCaps)
      -> NonEmpty (NonEmpty SomeKeyPairCaps)
      -> Map Sim.Account (Map Sim.ContractName (NonEmpty SomeKeyPairCaps))
    buildGenAccountsKeysets accs pks cks =
      M.fromList . NEL.toList $ nelZipWith3 go accs pks cks

    go :: Sim.Account
       -> NonEmpty SomeKeyPairCaps
       -> NonEmpty SomeKeyPairCaps
       -> (Sim.Account, Map Sim.ContractName (NonEmpty SomeKeyPairCaps))
    go name pks cks = (name, M.fromList [ps, cs])
      where
        ps = (Sim.ContractName "payment", pks)
        cs = (Sim.ContractName "coin", cks)

realCoinTransactions
  :: Args
  -> HostAddress
  -> TVar TXCount
  -> TimingDistribution
  -> LoggerT SomeLogMessage IO ()
realCoinTransactions config host tv distribution = do
  cfg <- liftIO $ mkTXGConfig (Just distribution) config host

  let chains = maybe (versionChains $ nodeVersion config) NES.fromList
               . NEL.nonEmpty
               $ nodeChainIds config

  accountMap <- fmap (M.fromList . toList) . forM chains $ \cid -> do
    let f (Sim.Account sender) = do
          meta <- liftIO $ Sim.makeMetaWithSender sender cid
          Sim.createCoinAccount (confVersion cfg) meta sender
    (coinKS, _coinAcc) <-
        liftIO $ unzip <$> traverse f Sim.coinAccountNames
    let accounts = buildGenAccountsKeysets (NEL.fromList Sim.coinAccountNames) (NEL.fromList coinKS)
    pure (cid, accounts)

  logg Info $ toLogMessage ("Real Transactions: Transactions are being generated" :: Text)

  -- Set up values for running the effect stack.
  gen <- liftIO createSystemRandom
  let act = loop (generateTransactions True (verbose config) CoinContract)
      env = set (field @"confKeysets") accountMap cfg
      stt = TXGState gen tv chains

  evalStateT (runReaderT (runTXG act) env) stt
  where
    buildGenAccountsKeysets
      :: NonEmpty Sim.Account
      -> NonEmpty (NonEmpty SomeKeyPairCaps)
      -> Map Sim.Account (Map Sim.ContractName (NonEmpty SomeKeyPairCaps))
    buildGenAccountsKeysets accs cks =
      M.fromList . NEL.toList $ NEL.zipWith go accs cks

    go :: Sim.Account
       -> NonEmpty SomeKeyPairCaps
       -> (Sim.Account, Map Sim.ContractName (NonEmpty SomeKeyPairCaps))
    go name cks = (name, M.singleton (Sim.ContractName "coin") cks)

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
  TXGConfig _ _ ce v _ _ <- mkTXGConfig Nothing config host
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
  TXGConfig _ _ ce v _ _ <- mkTXGConfig Nothing config host
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
      meta <- Sim.makeMeta cid
      let v = confVersion cfg
      cmd <- mkExec (T.unpack c) (datum kps) meta (NEL.toList kps) (Just $ CI.NetworkId $ toText v) Nothing
      runExceptT (f cfg cmd) >>= \case
        Left e -> print e >> exitWith (ExitFailure 1)
        Right res -> pPrintNoColor res
  where
    datum :: NonEmpty SomeKeyPairCaps -> Value
    datum kps = object ["test-admin-keyset" .= fmap (formatB16PubKey . fst) kps]

    f :: TXGConfig -> Command Text -> ExceptT ClientError IO ListenResponse
    f cfg@(TXGConfig _ _ ce v _ _) cmd = do
      RequestKeys (rk :| _) <- ExceptT . sendTransactions cfg cid $ pure cmd
      ExceptT $ runClientM (listen v cid $ ListenerRequest rk) ce


inMempool :: Args -> HostAddress -> (ChainId, [TransactionHash]) -> IO ()
inMempool args host (cid, txhashes)
    | not . HS.member cid . chainIds $ nodeVersion args =
      putStrLn "Invalid target ChainId" >> exitWith (ExitFailure 1)
    | otherwise = do
        (TXGConfig _ _ ce v _ _) <- mkTXGConfig Nothing args host
        runClientM (isMempoolMember v cid txhashes) ce >>= \case
          Left e -> print e >> exitWith (ExitFailure 1)
          Right res -> pPrintNoColor res

-- If we want package information in txg logs the following list should be
-- populated with the respective information from the PkgInfo module.
--
pkgInfoScopes:: [(T.Text, T.Text)]
pkgInfoScopes = []

work :: Args -> IO ()
work cfg = do
  mgr <- newManager defaultManagerSettings
  tv  <- newTVarIO 0
  withBaseHandleBackend "transaction-generator" mgr pkgInfoScopes (defconfig ^. U.logConfigBackend)
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
        DeployContracts [] -> liftIO $ do
          let v = nodeVersion cfg
          loadContracts cfg host $ NEL.cons (initAdminKeysetContract v) (defaultContractLoaders v)
        DeployContracts cs -> liftIO $ do
          let v = nodeVersion cfg
          loadContracts cfg host $ (initAdminKeysetContract v) :| map (createLoader v) cs
        RunStandardContracts distribution ->
          realTransactions cfg host tv distribution
        RunCoinContract distribution ->
          realCoinTransactions cfg host tv distribution
        RunSimpleExpressions distribution ->
          simpleExpressions cfg host tv distribution
        PollRequestKeys rk -> liftIO $ pollRequestKeys cfg host
          . RequestKey
          . H.Hash
          . T.encodeUtf8
          $ rk
        ListenerRequestKey rk -> liftIO $ listenerRequestKey cfg host
          . ListenerRequest
          . RequestKey
          . H.Hash
          . T.encodeUtf8
          $ rk
        SingleTransaction stx -> liftIO $
          singleTransaction cfg host stx
        MempoolMember req -> liftIO $ inMempool cfg host req


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
createLoader :: ChainwebVersion -> Sim.ContractName -> ContractLoader
createLoader v (Sim.ContractName contractName) meta kp = do
  theCode <- readFile (contractName <> ".pact")
  adminKS <- testSomeKeyPairs
  -- TODO: theData may change later
  let theData = object
          ["admin-keyset" .= fmap (formatB16PubKey . fst) adminKS
          , T.append (T.pack contractName) "-keyset" .= fmap (formatB16PubKey . fst) kp
          ]
  mkExec theCode theData meta (NEL.toList adminKS) (Just $ CI.NetworkId $ toText v) Nothing

-- Remember that coin contract is already loaded.
defaultContractLoaders :: ChainwebVersion -> NonEmpty ContractLoader
defaultContractLoaders v =
  NEL.fromList [ helloWorldContractLoader v, simplePaymentsContractLoader v]

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

isMempoolMember :: ChainwebVersion -> ChainId -> [TransactionHash] -> ClientM [Bool]
isMempoolMember version chainid = go
  where
    _ :<|> go :<|> _ :<|> _ = genapiMempool version chainid


genapiMempool version chainid =
  case someChainwebVersionVal version of
    SomeChainwebVersionT (_ :: Proxy cv) ->
      case someChainIdVal chainid of
        SomeChainIdT (_ :: Proxy cid) -> client (Proxy :: Proxy (MempoolApi cv cid))

---------------------------
-- FOR DEBUGGING IN GHCI --
---------------------------
{-
_genapi3 :: ChainwebVersion -> ChainId -> Text
_genapi3 version chainid =
  case someChainwebVersionVal version of
    SomeChainwebVersionT (_ :: Proxy cv) ->
      case someChainIdVal chainid of
        SomeChainIdT (_ :: Proxy cid) ->
          -- client (Proxy :: Proxy (MempoolApi cv cid))
          let p = (Proxy :: Proxy (MempoolMemberApi cv cid))
          in toUrlPiece $ safeLink (Proxy :: (Proxy (MempoolApi cv cid))) p
-}
