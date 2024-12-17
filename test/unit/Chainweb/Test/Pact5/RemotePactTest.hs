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
{-# options_ghc -Wwarn -fno-warn-name-shadowing -fno-warn-unused-top-binds #-}

module Chainweb.Test.Pact5.RemotePactTest
    ( tests
    ) where

import Pact.Core.DefPacts.Types
import Pact.Core.SPV
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Lazy qualified as BL
import Data.Aeson qualified as A
import Pact.Core.Command.RPC (ContMsg(..))
import Control.Monad (replicateM_)
import Chainweb.SPV.CreateProof (createTransactionOutputProof_)
import Chainweb.BlockHeader (blockHeight)
import Data.Maybe (fromMaybe)
import Pact.Core.Names
import Pact.Core.Capabilities
import Pact.Core.PactValue
import Pact.Core.Command.Server qualified as Pact5
import Chainweb.CutDB.RestAPI.Server (someCutGetServer)
import Network.Connection qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.Socket qualified as Network
import Network.TLS qualified as TLS
import Network.Wai.Handler.Warp qualified as W
import Network.Wai.Handler.WarpTLS qualified as W
import Chainweb.RestAPI.Utils (someServerApplication)
import "pact" Pact.Types.API qualified as Pact4
import "pact" Pact.Types.Hash qualified as Pact4
import Chainweb.ChainId
import Chainweb.Graph (singletonChainGraph, petersonChainGraph)
import Chainweb.Mempool.Mempool (TransactionHash(..))
import Chainweb.Pact.RestAPI.Client
import Chainweb.Pact.RestAPI.Server
import Chainweb.Pact.Types
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.Pact5.CutFixture qualified as CutFixture
import Chainweb.Test.Pact5.Utils
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils (deadbeef, TestPact5CommandResult)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService
import Control.Concurrent
import Control.Exception (Exception, AsyncException(..), try)
import Control.Lens
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, allocate)
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Network.X509.SelfSigned
import Pact.Core.Command.Types
import Pact.Core.Gas.Types
import Pact.Core.Hash qualified as Pact5
import Pact.JSON.Encode qualified as J
import PropertyMatchers ((?))
import PropertyMatchers qualified as P
import Servant.Client
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, testCase)
import qualified Pact.Types.Command as Pact4
import qualified Pact.Core.Command.Types as Pact5
import qualified Data.HashMap.Strict as HM
import GHC.Stack (HasCallStack)
import Data.String (fromString)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

data Fixture = Fixture
    { _cutFixture :: CutFixture.Fixture
    , _serviceClientEnv :: ClientEnv
    }
makeLenses ''Fixture

mkFixture :: ChainwebVersion -> RocksDb -> ResourceT IO Fixture
mkFixture v baseRdb = do
    fixture <- CutFixture.mkFixture v testPactServiceConfig baseRdb
    logger <- liftIO getTestLogger

    let mkSomePactServerData chainId = PactServerData
            { _pactServerDataCutDb = fixture ^. CutFixture.fixtureCutDb
            , _pactServerDataMempool = fixture ^. CutFixture.fixtureMempools ^?! atChain chainId
            , _pactServerDataLogger = logger
            , _pactServerDataPact = mkPactExecutionService (fixture ^. CutFixture.fixturePactQueues ^?! atChain chainId)
            }
    let pactServer = somePactServers v $ List.map (\chainId -> (chainId, mkSomePactServerData chainId)) (HashSet.toList (chainIds v))
    let cutGetServer = someCutGetServer v (fixture ^. CutFixture.fixtureCutDb)
    let app = someServerApplication (pactServer <> cutGetServer)

    (_fingerprint, cert, key) <- liftIO $ generateLocalhostCertificate @RsaCert 1

    -- Run pact server API
    (port, socket) <- snd <$> allocate W.openFreePort (Network.close . snd)
    _ <- allocate
        (forkIO $ do
            W.runTLSSocket (tlsServerSettings cert key) W.defaultSettings socket app
        )
        (\tid -> throwTo tid ThreadKilled)

    serviceClientEnv <- liftIO $ do
        let defaultTLSSettings = (HTTP.TLSSettingsSimple True False False TLS.defaultSupported)
        httpManager <- HTTP.newTlsManagerWith (HTTP.mkManagerSettings defaultTLSSettings Nothing)
        return $ mkClientEnv httpManager $ BaseUrl
            { baseUrlScheme = Https
            , baseUrlHost = "127.0.0.1"
            , baseUrlPort = port
            , baseUrlPath = ""
            }

    return $ Fixture
        { _cutFixture = fixture
        , _serviceClientEnv = serviceClientEnv
        }

tests :: RocksDb -> TestTree
tests rdb = testGroup "Pact5 RemotePactTest"
    [ testCase "pollingInvalidRequestKeyTest" (pollingInvalidRequestKeyTest rdb)
    , testCase "pollingConfirmationDepthTest" (pollingConfirmationDepthTest rdb)
    , testCase "spvTest" (spvTest rdb)
    , testCase "invalidTxsTest" (invalidTxsTest rdb)
    ]

pollingInvalidRequestKeyTest :: RocksDb -> IO ()
pollingInvalidRequestKeyTest baseRdb = runResourceT $ do
    let v = pact5InstantCpmTestVersion singletonChainGraph
    let cid = unsafeChainId 0
    fixture <- mkFixture v baseRdb
    let clientEnv = fixture ^. serviceClientEnv

    liftIO $ do
        pollResult <- polling v cid clientEnv (NE.singleton pactDeadBeef)
        assertEqual "invalid poll should return no results" pollResult HashMap.empty

pollingConfirmationDepthTest :: RocksDb -> IO ()
pollingConfirmationDepthTest baseRdb = runResourceT $ do
    let v = pact5InstantCpmTestVersion singletonChainGraph
    let cid = unsafeChainId 0
    fixture <- mkFixture v baseRdb
    let clientEnv = fixture ^. serviceClientEnv

    liftIO $ do
        cmd1 <- buildTextCmd v (trivialTx cid 42)
        cmd2 <- buildTextCmd v (trivialTx cid 43)
        rks <- sending v cid clientEnv (cmd1 NE.:| [cmd2])

        pollingWithDepth v cid clientEnv rks Nothing >>= \response -> do
            assertEqual "there are no command results at depth 0" response HashMap.empty
        pollingWithDepth v cid clientEnv rks (Just (ConfirmationDepth 0)) >>= \response -> do
            assertEqual "there are no command results at depth 0" response HashMap.empty

        CutFixture.advanceAllChains_ (fixture ^. cutFixture)

        pollingWithDepth v cid clientEnv rks Nothing >>= \response -> do
            assertEqual "results are visible at depth 0" 2 (HashMap.size response)
        pollingWithDepth v cid clientEnv rks (Just (ConfirmationDepth 0)) >>= \response -> do
            assertEqual "results are visible at depth 0" 2 (HashMap.size response)
        pollingWithDepth v cid clientEnv rks (Just (ConfirmationDepth 1)) >>= \response -> do
            assertEqual "results are not visible at depth 1" 0 (HashMap.size response)

        CutFixture.advanceAllChains_ (fixture ^. cutFixture)

        pollingWithDepth v cid clientEnv rks Nothing >>= \response -> do
            assertEqual "results are visible at depth 0" 2 (HashMap.size response)
        pollingWithDepth v cid clientEnv rks (Just (ConfirmationDepth 0)) >>= \response -> do
            assertEqual "results are visible at depth 0" 2 (HashMap.size response)
        pollingWithDepth v cid clientEnv rks (Just (ConfirmationDepth 1)) >>= \response -> do
            assertEqual "results are visible at depth 1" 2 (HashMap.size response)
        pollingWithDepth v cid clientEnv rks (Just (ConfirmationDepth 2)) >>= \response -> do
            assertEqual "results are not visible at depth 2" 0 (HashMap.size response)

        CutFixture.advanceAllChains_ (fixture ^. cutFixture)

        pollingWithDepth v cid clientEnv rks Nothing >>= \response -> do
            assertEqual "results are visible at depth 0" 2 (HashMap.size response)
        pollingWithDepth v cid clientEnv rks (Just (ConfirmationDepth 0)) >>= \response -> do
            assertEqual "results are visible at depth 0" 2 (HashMap.size response)
        pollingWithDepth v cid clientEnv rks (Just (ConfirmationDepth 1)) >>= \response -> do
            assertEqual "results are visible at depth 1" 2 (HashMap.size response)
        pollingWithDepth v cid clientEnv rks (Just (ConfirmationDepth 2)) >>= \response -> do
            assertEqual "results are visible at depth 2" 2 (HashMap.size response)
        pollingWithDepth v cid clientEnv rks (Just (ConfirmationDepth 3)) >>= \response -> do
            assertEqual "results are not visible at depth 3" 0 (HashMap.size response)

        return ()

spvTest :: RocksDb -> IO ()
spvTest baseRdb = runResourceT $ do
    let v = pact5InstantCpmTestVersion petersonChainGraph
    fixture <- mkFixture v baseRdb
    let clientEnv = fixture ^. serviceClientEnv

    let srcChain = unsafeChainId 0
    let targetChain = unsafeChainId 9

    liftIO $ do
        send <- buildTextCmd v
            $ set cbSigners
                [ mkEd25519Signer' sender00
                    [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) []
                    , CapToken (QualifiedName "TRANSFER_XCHAIN" (ModuleName "coin" Nothing))
                        [ PString "sender00"
                        , PString "sender01"
                        , PDecimal 1.0
                        , PString (chainIdToText targetChain)
                        ]
                    ]
                ]
            $ set cbRPC (mkExec ("(coin.transfer-crosschain \"sender00\" \"sender01\" (read-keyset 'k) \"" <> chainIdToText targetChain <> "\" 1.0)") (mkKeySetData "k" [sender01]))
            $ set cbSender "sender00"
            $ set cbChainId srcChain
            $ set cbGasPrice (GasPrice 0.01)
            $ set cbGasLimit (GasLimit (Gas 1_000))
            $ defaultCmd

        sendReqKey <- fmap NE.head $ sending v srcChain clientEnv (NE.singleton send)
        (sendCut, _) <- CutFixture.advanceAllChains (fixture ^. cutFixture)
        sendCr <- fmap (HashMap.! sendReqKey) $ pollingWithDepth v srcChain clientEnv (NE.singleton sendReqKey) (Just (ConfirmationDepth 0))
        let cont = fromMaybe (error "missing continuation") (_crContinuation sendCr)

        _ <- replicateM_ 10 $ do
            CutFixture.advanceAllChains (fixture ^. cutFixture)
        let sendHeight = sendCut ^?! ixg srcChain . blockHeight
        spvProof <- createTransactionOutputProof_ (fixture ^. cutFixture . CutFixture.fixtureWebBlockHeaderDb) (fixture ^. cutFixture . CutFixture.fixturePayloadDb) targetChain srcChain sendHeight 0
        let contMsg = ContMsg
                { _cmPactId = _peDefPactId cont
                , _cmStep = succ $ _peStep cont
                , _cmRollback = _peStepHasRollback cont
                , _cmData = PUnit
                , _cmProof = Just (ContProof (B64U.encode (BL.toStrict (A.encode spvProof))))
                }

        recv <- buildTextCmd v
            $ set cbSigners
                [ mkEd25519Signer' sender00
                    [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) []
                    ]
                ]
            $ set cbRPC (mkCont contMsg)
            $ set cbChainId targetChain
            $ set cbGasPrice (GasPrice 0.01)
            $ set cbGasLimit (GasLimit (Gas 1_000))
            $ defaultCmd
        recvReqKey <- fmap NE.head $ sending v targetChain clientEnv (NE.singleton recv)
        _ <- CutFixture.advanceAllChains (fixture ^. cutFixture)
        recvCr <- fmap (HashMap.! recvReqKey) $ polling v targetChain clientEnv (NE.singleton recvReqKey)
        recvCr
            & P.allTrue
                [ P.fun _crResult ? P.match _PactResultOk P.succeed
                , P.fun _crEvents ? P.propful
                    [ P.succeed
                    , P.allTrue
                        [ P.fun _peName ? P.equals "TRANSFER_XCHAIN_RECD"
                        , P.fun _peArgs ? P.equals
                            [PString "", PString "sender01", PDecimal 1.0, PString (chainIdToText srcChain)]
                        ]
                    , P.fun _peName ? P.equals "X_RESUME"
                    , P.succeed
                    ]
                ]

        pure ()

    pure ()

fails :: Exception e => P.Prop e -> P.Prop (IO a)
fails p actual = try actual >>= \case
    Left e -> p e
    _ -> P.fail "a failed computation" actual

invalidTxsTest :: RocksDb -> IO ()
invalidTxsTest baseRdb = runResourceT $ do
    let v = pact5InstantCpmTestVersion petersonChainGraph
    fixture <- mkFixture v baseRdb
    let clientEnv = fixture ^. serviceClientEnv

    let cid = unsafeChainId 0

    let assertExnContains expectedErrStr (SendingException actualErrStr)
            | expectedErrStr `List.isInfixOf` actualErrStr = P.succeed actualErrStr
            | otherwise =
                P.fail ("Error containing: " <> fromString expectedErrStr) actualErrStr

    let validationFailedPrefix cmd = "Validation failed for hash " <> sshow (_cmdHash cmd) <> ": "

    liftIO $ do
        cmdParseFailure <- buildTextCmd v
            $ set cbChainId cid
            $ set cbRPC (mkExec "(+ 1" PUnit)
            $ defaultCmd
        sending v cid clientEnv (NE.singleton cmdParseFailure)
            & fails ? assertExnContains "Pact parse error"

        cmdInvalidPayloadHash <- do
            bareCmd <- buildTextCmd v
                $ set cbChainId cid
                $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                $ defaultCmd
            pure $ bareCmd
                { _cmdHash = Pact5.hash "fakehash"
                }
        sending v cid clientEnv (NE.singleton cmdInvalidPayloadHash)
            & fails ? assertExnContains (validationFailedPrefix cmdInvalidPayloadHash <> "Invalid transaction hash")

        cmdSignersSigsLengthMismatch1 <- do
            bareCmd <- buildTextCmd v
                $ set cbSigners [mkEd25519Signer' sender00 []]
                $ set cbChainId cid
                $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                $ defaultCmd
            pure $ bareCmd
                { _cmdSigs = []
                }
        sending v cid clientEnv (NE.singleton cmdSignersSigsLengthMismatch1)
            & fails ? assertExnContains (validationFailedPrefix cmdSignersSigsLengthMismatch1 <> "Invalid transaction sigs")

        cmdSignersSigsLengthMismatch2 <- liftIO $ do
            bareCmd <- buildTextCmd v
                $ set cbSigners []
                $ set cbChainId cid
                $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                $ defaultCmd
            pure $ bareCmd
                { -- This is an invalid ED25519 signature, but length signers == length signatures is checked first
                _cmdSigs = [ED25519Sig "fakeSig"]
                }
        sending v cid clientEnv (NE.singleton cmdSignersSigsLengthMismatch2)
            & fails ? assertExnContains (validationFailedPrefix cmdSignersSigsLengthMismatch2 <> "Invalid transaction sigs")

        cmdInvalidUserSig <- liftIO $ do
            bareCmd <- buildTextCmd v
                $ set cbSigners [mkEd25519Signer' sender00 []]
                $ set cbChainId cid
                $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                $ defaultCmd
            pure $ bareCmd
                { _cmdSigs = [ED25519Sig "fakeSig"]
                }

        sending v cid clientEnv (NE.singleton cmdInvalidUserSig)
            & fails ? assertExnContains (validationFailedPrefix cmdInvalidUserSig <> "Invalid transaction sigs")

        cmdGood <- buildTextCmd v
            $ set cbSigners [mkEd25519Signer' sender00 []]
            $ set cbChainId cid
            $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
            $ defaultCmd
        -- Test that [badCmd, goodCmd] fails on badCmd, and the batch is rejected.
        -- We just re-use a previously built bad cmd.
        sending v cid clientEnv (NE.fromList [cmdInvalidUserSig, cmdGood])
            & fails ? assertExnContains (validationFailedPrefix cmdInvalidUserSig <> "Invalid transaction sigs")
        -- Test that [goodCmd, badCmd] fails on badCmd, and the batch is rejected.
        -- Order matters, and the error message also indicates the position of the
        -- failing tx.
        -- We just re-use a previously built bad cmd.
        sending v cid clientEnv (NE.fromList [cmdGood, cmdInvalidUserSig])
            & fails ? assertExnContains (validationFailedPrefix cmdInvalidUserSig <> "Invalid transaction sigs")


{-
          recvPwos <- runCutWithTx v pacts targetMempoolRef blockDb $ \_n _bHeight _bHash bHeader -> do
            buildCwCmd "transfer-crosschain" v
              $ set cbSigners [mkEd25519Signer' sender00 [mkGasCap]]
              $ set cbRPC (mkCont contMsg)
              $ setFromHeader bHeader
              $ set cbChainId targetChain
              $ set cbGasPrice 0.01
              $ set cbTTL 100
              $ defaultCmd
-}
{-
spvTest :: Pact.TxCreationTime -> ClientEnv -> (String -> IO ()) -> IO ()
spvTest t cenv step = do
    batch <- mkTxBatch
    sid <- mkChainId v maxBound 1
    r <- flip runClientM cenv $ do

      void $ liftIO $ step "sendApiClient: submit batch"
      rks <- liftIO $ sending v sid cenv batch

      void $ liftIO $ step "pollApiClient: poll until key is found"
      void $ liftIO $ polling v sid cenv rks ExpectPactResult

      void $ liftIO $ step "spvApiClient: submit request key"
      liftIO $ spv v sid cenv (SpvRequest (NEL.head $ _rkRequestKeys rks) tid)

    case r of
      Left e -> assertFailure $ "output proof failed: " <> sshow e
      Right _ -> return ()
  where
    tid = Pact.ChainId "2"

    mkTxBatch = do
      ks <- liftIO $ testKeyPairs sender00
        (Just [mkGasCap, mkXChainTransferCap "sender00" "sender01" 1.0 "2"])
      let pm = Pact.PublicMeta (Pact.ChainId "1") "sender00" 100_000 0.01 defaultMaxTTL t
      cmd1 <- liftIO $ Pact.mkExec txcode txdata pm ks [] (Just vNetworkId) (Just "1")
      cmd2 <- liftIO $ Pact.mkExec txcode txdata pm ks [] (Just vNetworkId) (Just "2")
      return $ SubmitBatch (pure cmd1 <> pure cmd2)

    txcode = T.unlines
      [ "(coin.transfer-crosschain"
      , "  'sender00"
      , "  'sender01"
      , "  (read-keyset 'sender01-keyset)"
      , "  (read-msg 'target-chain-id)"
      , "  1.0)"
      ]

    txdata = A.object
        [ "sender01-keyset" A..= [fst sender01]
        , "target-chain-id" A..= J.toJsonViaEncode tid
        ]
-}

newtype PollingException = PollingException String
    deriving stock (Show)
    deriving anyclass (Exception)

polling :: ()
    => ChainwebVersion
    -> ChainId
    -> ClientEnv
    -> NonEmpty RequestKey
    -> IO (HashMap RequestKey TestPact5CommandResult)
polling v cid clientEnv rks = do
    pollingWithDepth v cid clientEnv rks Nothing

pollingWithDepth :: ()
    => ChainwebVersion
    -> ChainId
    -> ClientEnv
    -> NonEmpty RequestKey
    -> Maybe ConfirmationDepth
    -> IO (HashMap RequestKey TestPact5CommandResult)
pollingWithDepth v cid clientEnv rks mConfirmationDepth = do
    poll <- runClientM (pactPollWithQueryApiClient v cid mConfirmationDepth (Pact5.PollRequest rks)) clientEnv
    case poll of
        Left e -> do
            throwM (PollingException (show e))
        Right (Pact5.PollResponse response) -> do
            return response

newtype SendingException = SendingException String
    deriving stock (Show)
    deriving anyclass (Exception)

sending :: ()
    => ChainwebVersion
    -> ChainId
    -> ClientEnv
    -> NonEmpty (Command Text)
    -> IO (NonEmpty RequestKey)
sending v cid clientEnv cmds = do
    let batch = Pact4.SubmitBatch (NE.map toPact4Command cmds)
    send <- runClientM (pactSendApiClient v cid batch) clientEnv
    case send of
        Left (FailureResponse _req resp) -> do
            throwM (SendingException (T.unpack $ T.decodeUtf8 $ BL.toStrict (responseBody resp)))
        Left e ->
            throwM (SendingException (show e))
        Right (Pact4.RequestKeys response) -> do
            return (NE.map toPact5RequestKey response)

toPact5RequestKey :: Pact4.RequestKey -> RequestKey
toPact5RequestKey = \case
    Pact4.RequestKey (Pact4.Hash bytes) -> RequestKey (Pact5.Hash bytes)

toPact4Command :: Command Text -> Pact4.Command Text
toPact4Command cmd4 = case Aeson.eitherDecodeStrictText (J.encodeText cmd4) of
    Left err -> error $ "toPact4Command: decode failed: " ++ err
    Right cmd5 -> cmd5

trivialTx :: ChainId -> Word -> CmdBuilder
trivialTx cid n = defaultCmd
    { _cbRPC = mkExec' (sshow n)
    , _cbSigners =
        [ mkEd25519Signer' sender00 []
        ]
    , _cbSender = "sender00"
    , _cbChainId = cid
    , _cbGasPrice = GasPrice 0.1
    , _cbGasLimit = GasLimit (Gas 1_000)
    }

_successfulTx :: P.Prop (CommandResult log err)
_successfulTx = P.fun _crResult ? P.match _PactResultOk P.succeed

pactDeadBeef :: RequestKey
pactDeadBeef = case deadbeef of
    TransactionHash bytes -> RequestKey (Pact5.Hash bytes)
