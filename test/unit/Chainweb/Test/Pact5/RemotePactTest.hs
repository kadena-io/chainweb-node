{-# language
    ConstraintKinds
    , DataKinds
    , DeriveAnyClass
    , DerivingStrategies
    , FlexibleContexts
    , ImplicitParams
    , ImpredicativeTypes
    , ImportQualifiedPost
    , LambdaCase
    , NumericUnderscores
    , OverloadedStrings
    , PatternSynonyms
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
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Chainweb.Test.Pact5.RemotePactTest
    ( tests
    ) where

import Control.Concurrent.Async hiding (poll)
import Control.Exception (evaluate)
import Control.Exception.Safe
import Control.Lens
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate, runResourceT)
import Data.Aeson qualified as A
import Data.Aeson.Lens qualified as A
import Data.Aeson.KeyMap qualified as A.KeyMap
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (forM_, traverse_)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Stack
import Network.Connection qualified as HTTP
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS qualified as HTTP
import Network.HTTP.Types.Status (notFound404, badRequest400)
import Network.Socket qualified as Network
import Network.TLS qualified as TLS
import Network.Wai.Handler.Warp qualified as W
import Network.Wai.Handler.WarpTLS qualified as W
import Network.X509.SelfSigned
import Prettyprinter qualified as PP
import PropertyMatchers ((?))
import PropertyMatchers qualified as P
import Servant.Client
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit (testCaseSteps, testCase)

import Pact.Core.Capabilities
import Pact.Core.ChainData (TxCreationTime(..))
import Pact.Core.Command.Crypto (signEd25519, exportEd25519Signature, importEd25519KeyPair, PrivateKeyBS (..))
import Pact.Core.Command.RPC (ContMsg (..))
import Pact.Core.Command.Server qualified as Pact5
import Pact.Core.Command.Types
import Pact.Core.DefPacts.Types
import Pact.Core.Errors
import Pact.Core.Gas.Types
import Pact.Core.Guards (Guard(GKeySetRef), KeySetName (..))
import Pact.Core.Hash qualified as Pact5
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.SPV
import Pact.JSON.Encode qualified as J
import Pact.Types.Command qualified as Pact4
import Pact.Types.API qualified as Pact4
import Pact.Types.Hash qualified as Pact4

import Chainweb.BlockHeader (blockHeight)
import Chainweb.ChainId
import Chainweb.CutDB.RestAPI.Server (someCutGetServer)
import Chainweb.Graph (petersonChainGraph, singletonChainGraph, twentyChainGraph)
import Chainweb.Mempool.Mempool (TransactionHash (..))
import Chainweb.Pact.RestAPI.Client
import Chainweb.Pact.RestAPI.Server
import Chainweb.Pact.Types
import Chainweb.RestAPI.Utils (someServerApplication)
import Chainweb.SPV.CreateProof (createTransactionOutputProof_)
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.Pact5.CutFixture (advanceAllChains, advanceAllChains_)
import Chainweb.Test.Pact5.CutFixture qualified as CutFixture
import Chainweb.Test.Pact5.Utils
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils (TestPact5CommandResult, deadbeef, withResource', withResourceT, unsafeHeadOf)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService
import Chainweb.Version.Mainnet (mainnet)
import qualified Data.Vector as V

data Fixture = Fixture
    { _cutFixture :: CutFixture.Fixture
    , _serviceClientEnv :: ClientEnv
    }
makeLenses ''Fixture

instance CutFixture.HasFixture Fixture where
    cutFixture = return . _cutFixture

class HasFixture a where
    remotePactTestFixture :: a -> IO Fixture

instance HasFixture Fixture where
    remotePactTestFixture = return
instance HasFixture a => HasFixture (IO a) where
    remotePactTestFixture = (>>= remotePactTestFixture)

type Step = String -> IO ()

mkFixture :: ChainwebVersion -> RocksDb -> ResourceT IO Fixture
mkFixture v baseRdb = do
    fx <- CutFixture.mkFixture v testPactServiceConfig baseRdb
    logger <- liftIO getTestLogger

    let mkSomePactServerData chainId = PactServerData
            { _pactServerDataCutDb = fx ^. CutFixture.fixtureCutDb
            , _pactServerDataMempool = fx ^. CutFixture.fixtureMempools ^?! atChain chainId
            , _pactServerDataLogger = logger
            , _pactServerDataPact = mkPactExecutionService (fx ^. CutFixture.fixturePactQueues ^?! atChain chainId)
            }
    let pactServer = somePactServers v $ List.map (\chainId -> (chainId, mkSomePactServerData chainId)) (HashSet.toList (chainIds v))
    let cutGetServer = someCutGetServer v (fx ^. CutFixture.fixtureCutDb)
    let app = someServerApplication (pactServer <> cutGetServer)

    -- Run pact server API
    (port, socket) <- snd <$> allocate W.openFreePort (Network.close . snd)
    _ <- allocate
        (async $
            W.runTLSSocket (tlsServerSettings cert key) W.defaultSettings socket app
        )
        cancel

    let serviceClientEnv = mkClientEnv httpManager $ BaseUrl
            { baseUrlScheme = Https
            , baseUrlHost = "127.0.0.1"
            , baseUrlPort = port
            , baseUrlPath = ""
            }

    return $ Fixture
        { _cutFixture = fx
        , _serviceClientEnv = serviceClientEnv
        }

-- generating this cert and making an HTTP manager take quite a while relative
-- to the rest of the tests, so they're shared globally.
-- there's no apparent reason to ever switch them out, either.
cert :: X509CertPem
key :: X509KeyPem
(_, cert, key) = unsafePerformIO $ generateLocalhostCertificate @RsaCert 1
defaultTLSSettings :: HTTP.TLSSettings
defaultTLSSettings = (HTTP.TLSSettingsSimple True False False TLS.defaultSupported)
httpManager :: Manager
httpManager = unsafePerformIO $ HTTP.newTlsManagerWith (HTTP.mkManagerSettings defaultTLSSettings Nothing)

tests :: RocksDb -> TestTree
tests rdb = withResource' (evaluate httpManager >> evaluate cert) $ \_ ->
    testGroup "Pact5 RemotePactTest"
        [ testCaseSteps "spvTest" (spvTest rdb)
        , sendInvalidTxsTest rdb
        , testCaseSteps "caplistTest" (caplistTest rdb)
        , testCaseSteps "pollingInvalidRequestKeyTest" (pollingInvalidRequestKeyTest rdb)
        , testCaseSteps "pollingConfirmationDepthTest" (pollingConfirmationDepthTest rdb)
        , testCaseSteps "pollingMetadataTest" (pollingMetadataTest rdb)
        , testCaseSteps "allocationTest" (allocationTest rdb)
        , testCaseSteps "webAuthnSignatureTest" (webAuthnSignatureTest rdb)
        , localTests rdb
        ]

pollingInvalidRequestKeyTest :: RocksDb -> Step -> IO ()
pollingInvalidRequestKeyTest baseRdb _step = runResourceT $ do
    let v = pact5InstantCpmTestVersion singletonChainGraph
    let cid = unsafeChainId 0
    fx <- mkFixture v baseRdb

    liftIO $ do
        poll fx v cid [pactDeadBeef] >>=
            P.equals [Nothing]

pollingConfirmationDepthTest :: RocksDb -> Step -> IO ()
pollingConfirmationDepthTest baseRdb _step = runResourceT $ do
    let v = pact5InstantCpmTestVersion singletonChainGraph
    let cid = unsafeChainId 0
    fx <- mkFixture v baseRdb

    let trivialTx :: ChainId -> Word -> CmdBuilder
        trivialTx cid n = (defaultCmd cid)
            { _cbRPC = mkExec' (sshow n)
            }

    liftIO $ do
        cmd1 <- buildTextCmd v (trivialTx cid 42)
        cmd2 <- buildTextCmd v (trivialTx cid 43)
        let rks = [cmdToRequestKey cmd1, cmdToRequestKey cmd2]

        let expectSuccessful :: (HasCallStack, _) => P.Prop [Maybe TestPact5CommandResult]
            expectSuccessful = P.alignExact
                [ P.match _Just ? P.fun _crResult ? P.equals (PactResultOk (PInteger 42))
                , P.match _Just ? P.fun _crResult ? P.equals (PactResultOk (PInteger 43))
                ]

        let expectEmpty :: (HasCallStack, _) => _
            expectEmpty = traverse_ (P.equals Nothing)

        send fx v cid [cmd1, cmd2]

        pollWithDepth fx v cid rks Nothing
            >>= expectEmpty
        pollWithDepth fx v cid rks (Just (ConfirmationDepth 0))
            >>= expectEmpty

        advanceAllChains_ fx

        pollWithDepth fx v cid rks Nothing
            >>= expectSuccessful
        pollWithDepth fx v cid rks (Just (ConfirmationDepth 0))
            >>= expectSuccessful
        pollWithDepth fx v cid rks (Just (ConfirmationDepth 1))
            >>= expectEmpty

        advanceAllChains_ fx

        pollWithDepth fx v cid rks Nothing
            >>= expectSuccessful
        pollWithDepth fx v cid rks (Just (ConfirmationDepth 0))
            >>= expectSuccessful
        pollWithDepth fx v cid rks (Just (ConfirmationDepth 1))
            >>= expectSuccessful
        pollWithDepth fx v cid rks (Just (ConfirmationDepth 2))
            >>= expectEmpty

        advanceAllChains_ fx

        pollWithDepth fx v cid rks Nothing
            >>= expectSuccessful
        pollWithDepth fx v cid rks (Just (ConfirmationDepth 0))
            >>= expectSuccessful
        pollWithDepth fx v cid rks (Just (ConfirmationDepth 1))
            >>= expectSuccessful
        pollWithDepth fx v cid rks (Just (ConfirmationDepth 2))
            >>= expectSuccessful
        pollWithDepth fx v cid rks (Just (ConfirmationDepth 3))
            >>= expectEmpty

        return ()

spvTest :: RocksDb -> Step -> IO ()
spvTest baseRdb step = runResourceT $ do
    let v = pact5InstantCpmTestVersion petersonChainGraph
    fx <- mkFixture v baseRdb

    let srcChain = unsafeChainId 0
    let targetChain = unsafeChainId 9

    liftIO $ do
        initiator <- buildTextCmd v
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
            $ defaultCmd srcChain

        step "xchain initiate"
        send fx v srcChain [initiator]
        let initiatorReqKey = cmdToRequestKey initiator
        (sendCut, _) <- advanceAllChains fx
        [Just sendCr] <- pollWithDepth fx v srcChain [initiatorReqKey] (Just (ConfirmationDepth 0))
        let cont = fromMaybe (error "missing continuation") (_crContinuation sendCr)

        step "waiting"
        replicateM_ (int $ diameter petersonChainGraph + 1) $ advanceAllChains_ fx
        let sendHeight = sendCut ^?! ixg srcChain . blockHeight
        spvProof <- createTransactionOutputProof_ (fx ^. cutFixture . CutFixture.fixtureWebBlockHeaderDb) (fx ^. cutFixture . CutFixture.fixturePayloadDb) targetChain srcChain sendHeight 0
        let contMsg = ContMsg
                { _cmPactId = _peDefPactId cont
                , _cmStep = succ $ _peStep cont
                , _cmRollback = _peStepHasRollback cont
                , _cmData = PUnit
                , _cmProof = Just (ContProof (B64U.encode (BL.toStrict (A.encode spvProof))))
                }
        step "xchain recv"

        recv <- buildTextCmd v
            $ set cbRPC (mkCont contMsg)
            $ defaultCmd targetChain
        send fx v targetChain [recv]
        let recvReqKey = cmdToRequestKey recv
        advanceAllChains_ fx
        [Just recvCr] <- poll fx v targetChain [recvReqKey]
        recvCr
            & P.checkAll
                [ P.fun _crResult ? P.match _PactResultOk P.succeed
                , P.fun _crEvents ? P.alignExact
                    [ P.succeed
                    , P.checkAll
                        [ P.fun _peName ? P.equals "TRANSFER_XCHAIN_RECD"
                        , P.fun _peArgs ? P.equals
                            [PString "", PString "sender01", PDecimal 1.0, PString (chainIdToText srcChain)]
                        ]
                    , P.fun _peName ? P.equals "X_RESUME"
                    , P.succeed
                    ]
                ]


-- this test suite really wants you not to put any transactions into the final block.
sendInvalidTxsTest :: RocksDb -> TestTree
sendInvalidTxsTest rdb = withResourceT (mkFixture v rdb) $ \fx ->
    sequentialTestGroup "invalid txs in /send" AllFinish
        [ testGroup "send txs"
            [ testCase "syntax error" $ do
                cmdParseFailure <- buildTextCmd v
                    $ set cbRPC (mkExec' "(+ 1")
                    $ defaultCmd cid
                send fx v cid [cmdParseFailure]
                    & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains "Pact parse error"

            , testCase "invalid hash" $ do
                cmdInvalidPayloadHash <- do
                    bareCmd <- buildTextCmd v
                        $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                        $ defaultCmd cid
                    pure $ bareCmd
                        { _cmdHash = Pact5.hash "fakehash"
                        }
                send fx v cid [cmdInvalidPayloadHash]
                    & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed cmdInvalidPayloadHash "Invalid transaction hash")

            , testCase "signature length mismatch" $ do
                cmdSignersSigsLengthMismatch1 <- do
                    bareCmd <- buildTextCmd v
                        $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                        $ defaultCmd cid
                    pure $ bareCmd
                        { _cmdSigs = []
                        }
                send fx v cid [cmdSignersSigsLengthMismatch1]
                    & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed cmdSignersSigsLengthMismatch1 "Invalid transaction sigs")

                cmdSignersSigsLengthMismatch2 <- do
                    bareCmd <- buildTextCmd v
                        $ set cbSigners []
                        $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                        $ defaultCmd cid
                    pure $ bareCmd
                        {
                        -- This is an invalid ED25519 signature,
                        -- but length signers == length signatures is checked first
                        _cmdSigs = [ED25519Sig "fakeSig"]
                        }
                send fx v cid [cmdSignersSigsLengthMismatch2]
                    & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed cmdSignersSigsLengthMismatch2 "Invalid transaction sigs")

            , testCase "invalid signatures" $ do
                cmdInvalidUserSig <- mkCmdInvalidUserSig
                send fx v cid [cmdInvalidUserSig]
                    & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed cmdInvalidUserSig "Invalid transaction sigs")

            , testCase "batches are rejected with any invalid txs" $ do
                cmdGood <- mkCmdGood
                cmdInvalidUserSig <- mkCmdInvalidUserSig
                -- Test that [badCmd, goodCmd] fails on badCmd, and the batch is rejected.
                -- We just re-use a previously built bad cmd.
                send fx v cid [cmdInvalidUserSig, cmdGood]
                    & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed cmdInvalidUserSig "Invalid transaction sigs")
                -- Test that [goodCmd, badCmd] fails on badCmd, and the batch is rejected.
                -- Order matters, and the error message also indicates the position of the
                -- failing tx.
                -- We just re-use a previously built bad cmd.
                send fx v cid [cmdGood, cmdInvalidUserSig]
                    & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed cmdInvalidUserSig "Invalid transaction sigs")

            , testCase "invalid metadata" $ do
                cmdGood <- mkCmdGood
                send fx v wrongChain [cmdGood]
                    & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed cmdGood "Transaction metadata (chain id, chainweb version) conflicts with this endpoint")

                send fx wrongV cid [cmdGood]
                    & fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals notFound404
                        , P.fun responseBody ? P.equals ""
                        ]

                let invalidCid = "invalid chain ID"
                cmdInvalidChain <- buildTextCmd v (defaultCmd cid & set cbChainId invalidCid)
                send fx v wrongChain [cmdInvalidChain]
                    & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed cmdInvalidChain "insert error: Unparsable ChainId")

                cmdWrongV <- buildTextCmd wrongV
                    $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                    $ defaultCmd cid
                send fx v cid [cmdWrongV]
                    & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed cmdWrongV "Transaction metadata (chain id, chainweb version) conflicts with this endpoint")

                cmdExpiredTTL <- buildTextCmd v (defaultCmd cid & cbCreationTime .~ Just (TxCreationTime 0))
                send fx v cid [cmdExpiredTTL]
                    & fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? textContains
                            (validationFailed cmdExpiredTTL "Transaction time-to-live is expired")
                        ]

            , testCase "cannot buy gas" $ do
                cmdExcessiveGasLimit <- buildTextCmd v
                    $ set cbGasLimit (GasLimit $ Gas 100000000000000)
                    $ defaultCmd cid
                send fx v cid [cmdExcessiveGasLimit]
                    & fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? textContains
                            (validationFailed cmdExcessiveGasLimit "Transaction gas limit exceeds block gas limit")
                        ]

                cmdGasPriceTooPrecise <- buildTextCmd v
                    $ set cbGasPrice (GasPrice 0.00000000000000001)
                    $ defaultCmd cid
                send fx v cid [cmdGasPriceTooPrecise]
                    & fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? textContains
                            (validationFailed cmdGasPriceTooPrecise "insert error: This transaction's gas price: 0.00000000000000001 is not correctly rounded. It should be rounded to at most 12 decimal places.")
                        ]

                cmdNotEnoughGasFunds <- buildTextCmd v
                    $ set cbGasPrice (GasPrice 10_000_000_000)
                    $ set cbGasLimit (GasLimit (Gas 10_000))
                    $ defaultCmd cid
                send fx v cid [cmdNotEnoughGasFunds]
                    & fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? textContains
                            (validationFailed cmdNotEnoughGasFunds "Attempt to buy gas failed with: BuyGasPactError (PEUserRecoverableError (UserEnforceError \"Insufficient funds\")")
                        ]

                cmdInvalidSender <- buildTextCmd v
                    $ set cbSender "invalid-sender"
                    $ defaultCmd cid
                send fx v cid [cmdInvalidSender]
                    & fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? textContains
                            -- TODO: the full error is far more verbose than this,
                            -- perhaps that's something we should fix.
                            (validationFailed cmdInvalidSender "Attempt to buy gas failed")
                        ]

            ]

        -- the final test! none of the previous tests should have
        -- submitted even one single valid transaction.
        , testCase "none make it into a block" $ do
            (_, cmdResults) <- advanceAllChains fx
            forM_ cmdResults (P.alignExact mempty)

        ]
    where
    v = pact5InstantCpmTestVersion petersonChainGraph
    wrongV = pact5InstantCpmTestVersion twentyChainGraph

    cid = unsafeChainId 0
    wrongChain = unsafeChainId 1

    validationFailed cmd msg = "Validation failed for hash " <> sshow (_cmdHash cmd) <> ": " <> msg

    mkCmdInvalidUserSig = mkCmdGood <&> set cmdSigs [ED25519Sig "fakeSig"]

    mkCmdGood = buildTextCmd v
        $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
        $ defaultCmd cid


caplistTest :: RocksDb -> Step -> IO ()
caplistTest baseRdb step = runResourceT $ do
    let v = pact5InstantCpmTestVersion petersonChainGraph
    fx <- mkFixture v baseRdb

    let cid = unsafeChainId 0

    liftIO $ do
        tx0 <- buildTextCmd v
            $ set cbSigners
                [ mkEd25519Signer' sender00
                    [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) []
                    , CapToken (QualifiedName "TRANSFER" (ModuleName "coin" Nothing))
                        [ PString "sender00"
                        , PString "sender01"
                        , PDecimal 100.0
                        ]
                    ]
                ]
            $ set cbRPC (mkExec' "(coin.transfer \"sender00\" \"sender01\" 100.0)")
            $ defaultCmd cid

        step "sending"
        send fx v cid [tx0]

        let recvReqKey = cmdToRequestKey tx0

        step "advancing chains"

        advanceAllChains_ fx

        step "polling"

        poll fx v cid [recvReqKey]
            >>= P.alignExact ? List.singleton ? P.match _Just ?
                P.checkAll
                    [ P.fun _crResult ? P.match (_PactResultOk . _PString) ? P.equals "Write succeeded"
                    , P.fun _crMetaData ? P.match (_Just . A._Object . at "blockHash") ? P.match _Just P.succeed
                    ]


allocation01KeyPair :: (Text, Text)
allocation01KeyPair =
    ( "b4c8a3ea91d3146b0560994740f0e3eed91c59d2eeca1dc99f0c2872845c294d"
    , "5dbbbd8b765b7d0cf8426d6992924b057c70a2138ecd4cf60cfcde643f304ea9"
    )

allocation02KeyPair :: (Text, Text)
allocation02KeyPair =
    ( "e9e4e71bd063dcf7e06bd5b1a16688897d15ca8bd2e509c453c616219c186cc5"
    , "45f026b7a6bb278ed4099136c13e842cdd80138ab7c5acd4a1f0e6c97d1d1e3c"
    )

allocation02KeyPair' :: (Text, Text)
allocation02KeyPair' =
    ( "0c8212a903f6442c84acd0069acc263c69434b5af37b2997b16d6348b53fcd0a"
    , "2f75b5d875dd7bf07cc1a6973232a9e53dc1d4ffde2bab0bbace65cd87e87f53"
    )

allocationTest :: RocksDb -> (String -> IO ()) -> IO ()
allocationTest rdb step = runResourceT $ do
    let v = pact5InstantCpmTestVersion petersonChainGraph
    let cid = unsafeChainId 0
    fx <- mkFixture v rdb

    liftIO $ do
        do
            step "allocation00"
            release00Cmd <- buildTextCmd v
                $ set cbSigners [mkEd25519Signer' allocation00KeyPair [], mkEd25519Signer' sender00 []]
                $ set cbRPC (mkExec' "(coin.release-allocation \"allocation00\")")
                $ defaultCmd cid
            send fx v cid [release00Cmd]
            advanceAllChains_ fx
            poll fx v cid [cmdToRequestKey release00Cmd] >>=
                P.alignExact
                    [ P.match _Just ? P.checkAll
                        [ P.fun _crResult ? P.match _PactResultOk ? P.succeed
                        , P.fun _crEvents ? P.startingWith
                            (event
                                (P.equals "RELEASE_ALLOCATION")
                                (P.equals [PString "allocation00", PDecimal 1000000])
                                (P.equals coinModuleName))
                        ]
                    ]

            buildTextCmd v
                (set cbRPC (mkExec' "(coin.details \"allocation00\")") $ defaultCmd cid)
                >>= local fx v cid Nothing Nothing Nothing
                >>= P.match _Pact5LocalResultLegacy
                    ? P.fun _crResult
                    ? P.match _PactResultOk
                    ? P.match _PObject
                    ? P.alignExact ? Map.fromList
                        [ ("account", P.equals (PString "allocation00"))
                        , ("balance", P.equals (PDecimal 1100000))
                        , ("guard", P.succeed)
                        ]

        step "allocation01"
        do
            buildTextCmd v
                (set cbRPC (mkExec' "(coin.release-allocation \"allocation01\")") $ defaultCmd cid)
                >>= local fx v cid Nothing Nothing Nothing
                >>= P.match _Pact5LocalResultLegacy ?
                    P.fun _crResult ? P.match (_PactResultErr . _PEPact5Error . to _peMsg) ?
                        P.fun _boundedText ? textContains "funds locked until \"2100-10-31T18:00:00Z\"."

        step "allocation02"
        do
            redefineKeysetCmd <- buildTextCmd v
                $ set cbSigners [mkEd25519Signer' allocation02KeyPair []]
                $ set cbSender "allocation02"
                $ set cbRPC (mkExec
                    "(define-keyset \"allocation02\" (read-keyset \"allocation02-keyset\"))"
                    (mkKeySetData "allocation02-keyset" [allocation02KeyPair'])
                    )
                $ defaultCmd cid
            send fx v cid [redefineKeysetCmd]
            advanceAllChains_ fx
            poll fx v cid [cmdToRequestKey redefineKeysetCmd]
                >>= P.alignExact [P.match _Just successfulTx]

            releaseAllocationCmd <- buildTextCmd v
                $ set cbSender "allocation02"
                $ set cbSigners [mkEd25519Signer' allocation02KeyPair' []]
                $ set cbRPC (mkExec' "(coin.release-allocation \"allocation02\")")
                $ defaultCmd cid
            send fx v cid [releaseAllocationCmd]
            advanceAllChains_ fx
            poll fx v cid [cmdToRequestKey releaseAllocationCmd]
                >>= P.alignExact [P.match _Just successfulTx]

            buildTextCmd v (set cbRPC (mkExec' "(coin.details \"allocation02\")") $ defaultCmd cid)
                >>= local fx v cid Nothing Nothing Nothing
                >>= P.match _Pact5LocalResultLegacy
                    ? P.fun _crResult
                    ? P.match _PactResultOk
                    ? P.match _PObject
                    ? P.equals ? Map.fromList
                        [ ("account", (PString "allocation02"))
                        , ("balance", (PDecimal 1_099_999.9748)) -- 1k + 1mm - gas
                        , ("guard", (PGuard $ GKeySetRef (KeySetName "allocation02" Nothing)))
                        ]

successfulTx :: P.Prop (CommandResult log err)
successfulTx = P.fun _crResult ? P.match _PactResultOk P.succeed

-- Test that transactions signed with (mock) WebAuthn keypairs are accepted
-- by the pact service.
webAuthnSignatureTest :: RocksDb -> Step -> IO ()
webAuthnSignatureTest rdb _step = runResourceT $ do
    let v = pact5InstantCpmTestVersion petersonChainGraph
    let cid = unsafeChainId 0
    fx <- mkFixture v rdb
    liftIO $ do
        cmd <- buildTextCmd v
            $ set cbSigners [mkWebAuthnSigner' sender02WebAuthn [], mkEd25519Signer' sender00 []]
            $ set cbRPC (mkExec' "(concat [\"chainweb-\" \"node\"])")
            $ defaultCmd cid
        send fx v cid [cmd]
        advanceAllChains_ fx
        poll fx v cid [cmdToRequestKey cmd] >>=
            P.alignExact [P.match _Just successfulTx]

localTests :: RocksDb -> TestTree
localTests baseRdb = let
    v = pact5InstantCpmTestVersion petersonChainGraph
    cid = unsafeChainId 0
    in testGroup "tests for local"
        [ testCase "ordinary txs" $ runResourceT $ do
            fx <- mkFixture v baseRdb
            liftIO $ do
                let expectation = P.checkAll
                        [ P.fun _crResult ? P.match _PactResultOk ? P.equals (PInteger 1)
                        , P.fun _crMetaData ? P.match _Just ? P.match A._Object ? P.alignExact ? A.KeyMap.fromList
                            [ ("blockHeight", P.equals ? A.Number 2)
                            , ("blockTime", P.match A._Number P.succeed)
                            , ("prevBlockHash", P.match A._String P.succeed)
                            , ("publicMeta", P.match A._Object ? P.alignExact ? A.KeyMap.fromList
                                [ ("chainId", P.equals ? A.String "0")
                                , ("creationTime", P.match A._Number P.succeed)
                                , ("gasLimit", P.match A._Number P.succeed)
                                , ("gasPrice", P.match A._Number P.succeed)
                                , ("sender", P.equals ? A.String "sender00")
                                , ("ttl", P.equals ? A.Number 300)
                                ])
                            ]
                        ]
                buildTextCmd v (defaultCmd cid)
                    >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                    >>= P.match _Pact5LocalResultWithWarns ? P.fun fst ? expectation

                buildTextCmd v (defaultCmd cid)
                    >>= local fx v cid (Just PreflightSimulation) (Just NoVerify) Nothing
                    >>= P.match _Pact5LocalResultWithWarns ? P.fun fst ? expectation

        , testCase "signature with the wrong key" $ runResourceT $ do
            fx <- mkFixture v baseRdb
            liftIO $ do
                let buildSender00Cmd = defaultCmd cid
                        & cbSigners .~ [mkEd25519Signer' sender00 []]
                goodCmdHash <- _cmdHash <$> buildTextCmd v buildSender00Cmd
                sender01KeyPair <- either error return $ importEd25519KeyPair Nothing
                    (PrivBS $ either error id $ B16.decode $ T.encodeUtf8 $ snd sender01)
                let sender01Sig = ED25519Sig $ T.decodeUtf8 $ B16.encode $ exportEd25519Signature $
                        signEd25519 (fst sender01KeyPair) (snd sender01KeyPair) goodCmdHash

                buildTextCmd v buildSender00Cmd
                    <&> set cmdSigs [sender01Sig]
                    -- preflight mode, verify signatures
                    >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                    & fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? P.equals "Metadata validation failed: [\"The signature at position 0 is invalid: invalid ed25519 signature.\"]"
                        ]

                buildTextCmd v buildSender00Cmd
                    <&> set cmdSigs [sender01Sig]
                    -- preflight mode, do not verify signatures
                    >>= local fx v cid (Just PreflightSimulation) (Just NoVerify) Nothing
                    >>= P.succeed

                buildTextCmd v buildSender00Cmd
                    <&> set cmdSigs [sender01Sig]
                    -- non-preflight mode, verify signatures
                    >>= local fx v cid Nothing Nothing Nothing
                    & P.fails ? P.match _FailureResponse
                    ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? P.equals "Metadata validation failed: [\"The signature at position 0 is invalid: invalid ed25519 signature.\"]"
                        ]

                buildTextCmd v buildSender00Cmd
                    <&> set cmdSigs [sender01Sig]
                    -- non-preflight mode, do not verify signatures
                    >>= local fx v cid Nothing (Just NoVerify) Nothing
                    >>= P.match _LocalResultLegacy
                    ? P.succeed

        , testCase "invalid tx metadata" $ runResourceT $ do
            fx <- mkFixture v baseRdb
            liftIO $ do
                let wrongChain = unsafeChainId maxBound
                buildTextCmd v (defaultCmd wrongChain)
                    >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                    & fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? P.equals "Metadata validation failed: [\"Chain id mismatch\"]"
                        ]

                -- /local without preflight does not care about incorrect chains
                buildTextCmd v (defaultCmd wrongChain)
                    >>= local fx v cid Nothing Nothing Nothing
                    >>= P.succeed

                let invalidChain = "not a real chain"
                buildTextCmd v (defaultCmd cid & set cbChainId invalidChain)
                    >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                    & fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? P.equals "Metadata validation failed: [\"Chain id mismatch\"]"
                        ]

                buildTextCmd v (defaultCmd cid & set cbChainId invalidChain)
                    >>= local fx v cid Nothing Nothing Nothing
                    >>= P.succeed

                buildTextCmd v (set cbGasLimit (GasLimit $ Gas 100000000000000) $ defaultCmd cid)
                    >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                    & fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? P.equals "Metadata validation failed: [\"Transaction Gas limit exceeds block gas limit\"]"
                        ]

                buildTextCmd v (set cbGasPrice (GasPrice 0.00000000000000001) $ defaultCmd cid)
                    >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                    & fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? P.equals "Metadata validation failed: [\"Gas price decimal precision too high\"]"
                        ]

                buildTextCmd mainnet (defaultCmd cid)
                    >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                    & fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? P.equals "Metadata validation failed: [\"Network id mismatch\"]"
                        ]

                let sigs' = replicate 101 $ mkEd25519Signer' sender00 []
                buildTextCmd v (defaultCmd cid & set cbSigners sigs')
                    >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                    & fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? P.equals "Metadata validation failed: [\"Signature list size too big\"]"
                        ]

                buildTextCmd v
                    (defaultCmd cid
                        & set cbGasPrice (GasPrice 10_000_000_000)
                        & set cbGasLimit (GasLimit (Gas 10_000)))
                    >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                    >>= P.match (_Pact5LocalResultWithWarns . _1)
                    ? P.fun _crResult
                    -- TODO: a more detailed check here than "is an error" might be nice
                    ? P.match _PactResultErr P.succeed

        , withResourceT (mkFixture v baseRdb) $ \fx -> testCase "local with depth" $ do
            startBalance <- buildTextCmd v (defaultCmd cid & set cbRPC (mkExec' "(coin.details 'sender00)"))
                >>= local fx v cid Nothing Nothing (Just (RewindDepth 0))
                <&> unsafeHeadOf
                    ? _Pact5LocalResultLegacy
                    . to _crResult
                    . _PactResultOk . _PObject
                    . at "balance" . _Just
                    . _PDecimal
            let
                hasBalance :: (HasCallStack, _) => _
                hasBalance p = P.fun _crResult
                    ? P.match _PactResultOk
                    ? P.match (_PObject . at "balance" . _Just)
                    ? P.match _PDecimal p
                hasBlockHeight :: (HasCallStack, _) => _
                hasBlockHeight p = P.fun _crMetaData
                    ? P.match (_Just . A._Object . at "blockHeight" . _Just . A._Number) p

            transfer <- buildTextCmd v $ set cbSigners
                [ mkEd25519Signer' sender00
                    [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) []
                    , CapToken (QualifiedName "TRANSFER" (ModuleName "coin" Nothing))
                        [ PString "sender00", PString "sender01", PDecimal 100.0 ]
                    ]
                ]
                $ set cbRPC (mkExec' "(coin.transfer \"sender00\" \"sender01\" 100.0)")
                $ defaultCmd cid
            send fx v cid [transfer]
            advanceAllChains_ fx

            buildTextCmd v (defaultCmd cid & set cbRPC (mkExec' "(coin.details 'sender00)"))
                >>= local fx v cid Nothing Nothing Nothing
                >>= P.match _Pact5LocalResultLegacy
                ? P.checkAll
                [ hasBalance ? P.lt startBalance
                , hasBlockHeight (P.equals 3)
                ]

            buildTextCmd v (defaultCmd cid & set cbRPC (mkExec' "(coin.details 'sender00)"))
                >>= local fx v cid Nothing Nothing (Just (RewindDepth 0))
                >>= P.match _Pact5LocalResultLegacy
                ? P.checkAll
                [ hasBalance ? P.lt startBalance
                , hasBlockHeight (P.equals 3)
                ]

            buildTextCmd v (defaultCmd cid & set cbRPC (mkExec' "(coin.details 'sender00)"))
                >>= local fx v cid Nothing Nothing (Just (RewindDepth 1))
                >>= P.match _Pact5LocalResultLegacy
                ? P.checkAll
                [ hasBalance ? P.equals startBalance
                , hasBlockHeight (P.equals 2)
                ]

            buildTextCmd v (defaultCmd cid & set cbRPC (mkExec' "(coin.details 'sender00)"))
                >>= local fx v cid Nothing Nothing (Just (RewindDepth 2))
                >>= P.match _Pact5LocalResultLegacy
                ? P.checkAll
                [ hasBalance ? P.equals startBalance
                , hasBlockHeight (P.equals 1)
                ]

            buildTextCmd v (defaultCmd cid & set cbRPC (mkExec' "(coin.details 'sender00)"))
                >>= local fx v cid Nothing Nothing (Just (RewindDepth 3))
                >>= P.match _Pact5LocalResultLegacy
                ? P.checkAll
                [ hasBalance ? P.equals startBalance
                , hasBlockHeight (P.equals 1)
                ]

        , withResourceT (mkFixture v baseRdb) $ \fx -> testCase "local continuation" $ do
            let code = "(namespace 'free)(module m G (defcap G () true) (defpact p () (step (yield { \"a\" : (+ 1 1) })) (step (resume { \"a\" := a } a))))(free.m.p)"
            initiator <- buildTextCmd v
                $ set cbGasLimit (GasLimit (Gas 70_000))
                $ set cbRPC (mkExec' code)
                $ defaultCmd cid
            send fx v cid [initiator]
            advanceAllChains_ fx
            Just defPactId <- poll fx v cid [cmdToRequestKey initiator]
                <&> preview (ix 0 . _Just . crContinuation . _Just . peDefPactId)
            continuer <- buildTextCmd v
                $ set cbRPC (mkCont (mkContMsg defPactId 1))
                $ defaultCmd cid
            local fx v cid Nothing Nothing Nothing continuer
                >>= P.match _Pact5LocalResultLegacy ? P.fun _crResult
                ? P.match _PactResultOk ? P.equals (PInteger 2)

        ]


pollingMetadataTest :: RocksDb -> Step -> IO ()
pollingMetadataTest baseRdb _step = runResourceT $ do
    let v = pact5InstantCpmTestVersion singletonChainGraph
    let cid = unsafeChainId 0
    fx <- mkFixture v baseRdb

    liftIO $ do
        cmd <- buildTextCmd v (defaultCmd cid)
        send fx v cid [cmd]
        (_, commandResults) <- advanceAllChains fx
        -- there is no metadata in the actual block outputs
        commandResults
            & P.alignLax P.succeed ? onChain cid ? P.match _Just
            ? P.alignExact ? V.singleton
            ? P.fun _crMetaData ? P.equals Nothing

        -- the metadata reported by poll has a different shape from that
        -- reported by /local
        poll fx v cid [cmdToRequestKey cmd] >>=
            P.alignExact
            [ P.match _Just ? P.fun _crMetaData ? P.match _Just ? P.match A._Object ? P.alignExact ? A.KeyMap.fromList
                [ ("blockHash", P.match A._String P.succeed)
                , ("blockHeight", P.equals (A.Number 2))
                , ("blockTime", P.match A._Number P.succeed)
                , ("prevBlockHash", P.match A._String P.succeed)
                ]
            ]

newtype PollException = PollException String
    deriving stock (Show)
    deriving anyclass (Exception)

poll
    :: HasFixture fx
    => fx
    -> ChainwebVersion
    -> ChainId
    -> [RequestKey]
    -> IO [Maybe TestPact5CommandResult]
poll fx v cid rks = pollWithDepth fx v cid rks Nothing

pollWithDepth
    :: HasFixture fx
    => fx
    -> ChainwebVersion
    -> ChainId
    -> [RequestKey]
    -> Maybe ConfirmationDepth
    -> IO [Maybe TestPact5CommandResult]
pollWithDepth fx v cid rks mConfirmationDepth = do
    clientEnv <- _serviceClientEnv <$> remotePactTestFixture fx
    let rksNel = NE.fromList rks
    poll <- runClientM (pactPollWithQueryApiClient v cid mConfirmationDepth (Pact5.PollRequest rksNel)) clientEnv
    case poll of
        Left e -> do
            throwM (PollException (show e))
        Right (Pact5.PollResponse response) -> do
            -- the poll should only return results for commands
            -- that were polled for
            response & P.fun HashMap.keys ? traverse_ ? P.fun (\rk -> elem rk rks) ? P.bool
            return
                (rks <&> (\rk -> HashMap.lookup rk response))

data ClientException = ClientException CallStack ClientError
    deriving stock (Show)
instance Exception ClientException where
    displayException (ClientException callStack err) =
        "Client error: " <> show err
        <> "\n" <> GHC.Stack.prettyCallStack callStack
_FailureResponse :: Fold ClientException (ResponseF Text)
_FailureResponse = folding $ \case
    ClientException _ (FailureResponse _req resp) -> Just (TL.toStrict . TL.decodeUtf8 <$> resp)
    _ -> Nothing

send :: (HasCallStack, HasFixture fx)
    => fx
    -> ChainwebVersion
    -> ChainId
    -> [Command Text]
    -> IO ()
send fx v cid cmds = do
    let commands = NE.fromList $ toListOf each cmds
    let batch = Pact4.SubmitBatch (fmap toPact4Command commands)
    clientEnv <- _serviceClientEnv <$> remotePactTestFixture fx
    send <- runClientM (pactSendApiClient v cid batch) clientEnv
    case send of
        Left e -> do
            throwM (ClientException callStack e)
        Right (Pact4.RequestKeys (fmap toPact5RequestKey -> response)) -> do
            -- the returned request keys should always be exactly the hashes
            -- of the commands
            response & P.equals (cmdToRequestKey <$> commands)

local :: (HasCallStack, HasFixture fx)
    => fx
    -> ChainwebVersion
    -> ChainId
    -> Maybe LocalPreflightSimulation
    -> Maybe LocalSignatureVerification
    -> Maybe RewindDepth
    -> Command Text
    -> IO LocalResult
local fx v cid preflight sigVerify depth cmd = do
    -- send a single local request and return the result
    --
    clientEnv <- _serviceClientEnv <$> remotePactTestFixture fx
    r <- runClientM (pactLocalWithQueryApiClient v cid preflight sigVerify depth (toPact4Command cmd)) clientEnv
    case r of
        Right r -> return r
        Left e -> throwM $ ClientException callStack e

toPact5RequestKey :: Pact4.RequestKey -> RequestKey
toPact5RequestKey = \case
    Pact4.RequestKey (Pact4.Hash bytes) -> RequestKey (Pact5.Hash bytes)

toPact4Command :: Command Text -> Pact4.Command Text
toPact4Command cmd5 = case A.eitherDecodeStrictText (J.encodeText cmd5) of
    Left err -> error $ "toPact4Command: decode failed: " ++ err
    Right cmd4 -> cmd4

toPact5CommandResult :: Pact4.CommandResult Pact4.Hash -> TestPact5CommandResult
toPact5CommandResult cr4 = case A.eitherDecodeStrictText (J.encodeText cr4) of
    Left err -> error $ "toPact5CommandResult: decode failed: " ++ err
    Right cmd5 -> cmd5

pactDeadBeef :: RequestKey
pactDeadBeef = case deadbeef of
    TransactionHash bytes -> RequestKey (Pact5.Hash bytes)

fails :: Exception e => P.Prop e -> P.Prop (IO a)
fails p actual = try actual >>= \case
    Left e -> p e
    _ -> P.fail "a failed computation" actual

textContains :: HasCallStack => _
textContains expectedStr actualStr
    | expectedStr `T.isInfixOf` actualStr = P.succeed actualStr
    | otherwise =
        P.fail ("String containing: " <> PP.pretty expectedStr) actualStr
