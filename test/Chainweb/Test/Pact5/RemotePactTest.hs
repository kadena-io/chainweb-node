{-# language
    DataKinds
    , DeriveAnyClass
    , DerivingStrategies
    , FlexibleContexts
    , ImpredicativeTypes
    , ImportQualifiedPost
    , LambdaCase
    , NumericUnderscores
    , OverloadedStrings
    , PackageImports
    , ScopedTypeVariables
    , TypeApplications
    , TemplateHaskell
    , RecordWildCards
    , TupleSections
#-}

{-# options_ghc -fno-warn-gadt-mono-local-binds #-}

-- temporary
{-# options_ghc -Wwarn #-}

module Chainweb.Test.Pact5.RemotePactTest
    ( tests
    ) where

import Network.Connection qualified as HTTP
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.Socket qualified as Network
import Network.Wai qualified as W
import Network.Wai.Handler.Warp qualified as W
import Network.Wai.Handler.WarpTLS qualified as W
import Chainweb.RestAPI.Utils (someServerApplication)
import "pact" Pact.Types.API qualified as Pact4
import "pact" Pact.Types.Command qualified as Pact4
import "pact" Pact.Types.Hash qualified as Pact4
import Chainweb.ChainId
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Mempool.Mempool (TransactionHash(..))
import Chainweb.Pact.RestAPI.Client
import Chainweb.Pact.RestAPI.Server
import Chainweb.Pact.Types
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.Pact5.CutFixture qualified as CutFixture
import Chainweb.Test.Pact5.Utils
import Chainweb.Test.RestAPI.Utils (getCurrentBlockHeight)
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils (ChainwebNetwork(..), NodeDbDirs(..), withNodesAtLatestBehavior, withNodeDbDirs, deadbeef)
import Chainweb.Test.Utils (testRetryPolicy)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService
import Control.Concurrent
import Control.Exception (Exception, AsyncException(..))
import Control.Lens
import Control.Monad.Catch (Handler(..), throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, allocate)
import Control.Retry
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Network.X509.SelfSigned
import Pact.Core.Command.Types
import Pact.Core.Errors
import Pact.Core.Gas.Types
import Pact.Core.Hash qualified as Pact5
import Pact.Core.Info
import Pact.Core.StableEncoding
import Pact.JSON.Encode qualified as J
import PredicateTransformers as PT
import Servant.Client
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

data Fixture = Fixture
    { _cutFixture :: CutFixture.Fixture
    , _serviceClientEnv :: ClientEnv
    }
makeLenses ''Fixture

mkFixture :: RocksDb -> ResourceT IO Fixture
mkFixture baseRdb = do
    cutFixture <- CutFixture.mkFixture v testPactServiceConfig baseRdb
    logger <- liftIO getTestLogger

    let mkSomePactServerData chainId = PactServerData
            { _pactServerDataCutDb = cutFixture ^. CutFixture.fixtureCutDb
            , _pactServerDataMempool = cutFixture ^. CutFixture.fixtureMempools ^?! atChain chainId
            , _pactServerDataLogger = logger
            , _pactServerDataPact = mkPactExecutionService (cutFixture ^. CutFixture.fixturePactQueues ^?! atChain chainId)
            }
    let pactServer = somePactServers v $ List.map (\chainId -> (chainId, mkSomePactServerData chainId)) (HashSet.toList (chainIds v))
    let app = someServerApplication pactServer

    (_fingerprint, cert, key) <- liftIO $ generateLocalhostCertificate @RsaCert 1

    -- Run pact server API
    (port, socket) <- snd <$> allocate W.openFreePort (Network.close . snd)
    _ <- allocate
        (forkIO $ do
            W.runTLSSocket (tlsServerSettings cert key) W.defaultSettings socket app
        )
        (\tid -> throwTo tid ThreadKilled)

    serviceClientEnv <- liftIO $ do
        httpManager <- HTTP.newTlsManagerWith (HTTP.mkManagerSettings (HTTP.TLSSettingsSimple True False False) Nothing)
        return $ mkClientEnv httpManager $ BaseUrl
            { baseUrlScheme = Https
            , baseUrlHost = "127.0.0.1"
            , baseUrlPort = port
            , baseUrlPath = ""
            }

    return $ Fixture
        { _cutFixture = cutFixture
        , _serviceClientEnv = serviceClientEnv
        }

tests :: RocksDb -> TestTree
tests rdb = testGroup "Pact5 RemotePactTest"
    [ -- testCase "pollingBadlistTest" (pollingBadlistTest rdb)
        --testCase "pollingConfirmationDepthTest" (pollingConfirmationDepthTest rdb)
    ]

{-
pollingBadlistTest :: RocksDb -> IO ()
pollingBadlistTest baseRdb = runResourceT $ do
    fixture <- mkFixture baseRdb
    let clientEnv = fixture ^. fixtureClientEnv

    liftIO $ do
        pollResult <- polling clientEnv (NE.singleton pactDeadBeef)
        case pollResult ^?! ix pactDeadBeef . crResult of
            PactResultOk _ -> do
                assertFailure "expected PactResultErr badlist"
            PactResultErr (PEPact5Error _pactErrorCode) -> do
                assertFailure "pollingBadlistTest doesn't support pact5 error codes yet"
                -- idk how this works
                --assertEqual "Transaction was badlisted" (_peCode pactErrorCode)
            PactResultErr (PELegacyError legacyError) -> do
                assertBool "Transaction was badlisted" ("badlisted" `Text.isInfixOf` _leMessage legacyError)
            -- _ -> assertFailure "expected PactResultError"

pollingConfirmationDepthTest :: RocksDb -> IO ()
pollingConfirmationDepthTest baseRdb = runResourceT $ do
    fixture <- mkFixture baseRdb
    let clientEnv = fixture ^. fixtureClientEnv

    liftIO $ do
        cmd1 <- buildTextCmd v (trivialTx 42)
        cmd2 <- buildTextCmd v (trivialTx 43)
        rks <- sending clientEnv (cmd1 NE.:| [cmd2])
        putStrLn $ "pollingConfirmationDepth requestKeys: " ++ show rks

        beforePolling <- getCurrentBlockHeight v clientEnv cid
        putStrLn $ "beforePolling: " ++ show beforePolling
        pollResponse <- pollingWithDepth clientEnv rks Nothing --(Just (ConfirmationDepth 10))
        afterPolling <- getCurrentBlockHeight v clientEnv cid
        putStrLn $ "afterPolling: " ++ show afterPolling

        assertEqual "there are two command results" 2 (length (HashMap.keys pollResponse))
-}

newtype PollingException = PollingException String
    deriving stock (Show)
    deriving anyclass (Exception)

polling :: ()
    => ClientEnv
    -> NonEmpty RequestKey
    -> IO (HashMap RequestKey TestPact5CommandResult)
polling clientEnv rks = do
    pollingWithDepth clientEnv rks Nothing

pollingWithDepth :: ()
    => ClientEnv
    -> NonEmpty RequestKey
    -> Maybe ConfirmationDepth
    -> IO (HashMap RequestKey TestPact5CommandResult)
pollingWithDepth clientEnv rks mConfirmationDepth = do
    recovering testRetryPolicy [retryHandler] $ \_iterNumber -> do
        let rksPact4 = NE.map toPact4RequestKey rks
        poll <- runClientM (pactPollWithQueryApiClient v cid mConfirmationDepth (Pact4.Poll rksPact4)) clientEnv
        case poll of
            Left e -> do
                throwM (PollingException (show e))
            Right (Pact4.PollResponses response) -> do
                return (convertPollResponse response)
    where
        retryHandler :: RetryStatus -> Handler IO Bool
        retryHandler _ = Handler $ \case
            PollingException _ -> return True

newtype SendingException = SendingException String
    deriving stock (Show)
    deriving anyclass (Exception)

sending :: ()
    => ClientEnv
    -> NonEmpty (Command Text)
    -> IO (NonEmpty RequestKey)
sending clientEnv cmds = do
    putStrLn $ "sending: pact5 batch: " ++ show cmds
    recovering testRetryPolicy [retryHandler] $ \_iterNumber -> do
        let batch = Pact4.SubmitBatch (NE.map toPact4Command cmds)
        putStrLn $ "sending: pact4 batch: " ++ show batch
        send <- runClientM (pactSendApiClient v cid batch) clientEnv
        case send of
            Left e -> do
                throwM (SendingException (show e))
            Right (Pact4.RequestKeys response) -> do
                return (NE.map toPact5RequestKey response)
    where
        retryHandler :: RetryStatus -> Handler IO Bool
        retryHandler _ = Handler $ \case
            SendingException _ -> return True

toPact4RequestKey :: RequestKey -> Pact4.RequestKey
toPact4RequestKey = \case
    RequestKey (Pact5.Hash bytes) -> Pact4.RequestKey (Pact4.Hash bytes)

toPact5RequestKey :: Pact4.RequestKey -> RequestKey
toPact5RequestKey = \case
    Pact4.RequestKey (Pact4.Hash bytes) -> RequestKey (Pact5.Hash bytes)

toPact4Command :: Command Text -> Pact4.Command Text
toPact4Command cmd4 = case Aeson.eitherDecodeStrictText (J.encodeText cmd4) of
    Left err -> error $ "toPact4Command: decode failed: " ++ err
    Right cmd5 -> cmd5

toPact5CommandResult :: ()
    => Pact4.CommandResult Pact4.Hash
    -> TestPact5CommandResult
toPact5CommandResult cr4 = case Aeson.eitherDecodeStrictText (J.encodeText cr4) of
    Left err -> error $ "toPact5CommandResult: decode failed: " ++ err
    Right cr5 -> cr5

convertPollResponse :: ()
    => HashMap Pact4.RequestKey (Pact4.CommandResult Pact4.Hash)
    -> HashMap RequestKey TestPact5CommandResult
convertPollResponse pact4Response = HashMap.fromList
    $ List.map (\(rk, cr) -> (toPact5RequestKey rk, toPact5CommandResult cr))
    $ HashMap.toList pact4Response

trivialTx :: Word -> CmdBuilder
trivialTx n = defaultCmd
    { _cbRPC = mkExec' (sshow n)
    , _cbSigners =
        [ mkEd25519Signer' sender00 []
        ]
    , _cbSender = "sender00"
    , _cbChainId = cid
    , _cbGasPrice = GasPrice 0.1
    , _cbGasLimit = GasLimit (Gas 1_000)
    }

_successfulTx :: Predicatory p => Pred p (CommandResult log err)
_successfulTx = pt _crResult ? match _PactResultOk something

pactDeadBeef :: RequestKey
pactDeadBeef = case deadbeef of
    TransactionHash bytes -> RequestKey (Pact5.Hash bytes)

cid :: ChainId
cid = unsafeChainId 0

v :: ChainwebVersion
v = pact5InstantCpmTestVersion singletonChainGraph

type TestPact5CommandResult = CommandResult Aeson.Value (PactErrorCompat (StableEncoding SpanInfo))