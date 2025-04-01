{-# language
    ConstraintKinds
    , DataKinds
    , DeriveAnyClass
    , DerivingStrategies
    , FlexibleContexts
    , FlexibleInstances
    , ImplicitParams
    , ImportQualifiedPost
    , ImpredicativeTypes
    , LambdaCase
    , MultiParamTypeClasses
    , NamedFieldPuns
    , NumericUnderscores
    , OverloadedStrings
    , PackageImports
    , PartialTypeSignatures
    , PatternSynonyms
    , RecordWildCards
    , ScopedTypeVariables
    , TemplateHaskell
    , TupleSections
    , TypeApplications
    , UndecidableInstances
    , ViewPatterns
#-}

-- temporary
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Chainweb.Test.Pact5.RemotePactTest
    ( tests
    , mkFixture
    , Fixture(..)
    , HasFixture(..)
    , poll
    , pollWithDepth
    , PollException(..)
    , ClientException(..)
    , _FailureResponse
    , send
    , local
    , textContains
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
import Data.Foldable (forM_, traverse_)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Pact.JSON.Encode (getJsonText)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Vector qualified as V
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
import Test.Tasty.HUnit (testCaseSteps, testCase, assertFailure)

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
import Pact.Core.Hash
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.SPV
import Pact.Types.API qualified as Pact4
import Pact.Types.ChainId qualified as Pact4

import Chainweb.ChainId
import Chainweb.CutDB.RestAPI.Server (someCutGetServer)
import Chainweb.Graph (petersenChainGraph, singletonChainGraph, twentyChainGraph)
import Chainweb.Mempool.Mempool (TransactionHash (..))
import Chainweb.Pact.RestAPI.Client
import Chainweb.Pact.RestAPI.Server
import Chainweb.Pact.Types
import Chainweb.RestAPI.Utils (someServerApplication)
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.Pact5.CutFixture (advanceAllChains, advanceAllChains_)
import Chainweb.Test.Pact5.CutFixture qualified as CutFixture
import Chainweb.Test.Pact5.Utils
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService
import Chainweb.Version.Mainnet (mainnet)


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
        [ testCaseSteps "crosschainTest" (crosschainTest rdb)
        , sendInvalidTxsTest rdb
        , testCaseSteps "caplistTest" (caplistTest rdb)
        , testCaseSteps "pollingInvalidRequestKeyTest" (pollingInvalidRequestKeyTest rdb)
        , testCaseSteps "pollingConfirmationDepthTest" (pollingConfirmationDepthTest rdb)
        , testCaseSteps "pollingMetadataTest" (pollingMetadataTest rdb)
        , testCaseSteps "allocationTest" (allocationTest rdb)
        , testCaseSteps "webAuthnSignatureTest" (webAuthnSignatureTest rdb)
        , testCaseSteps "gasPurchaseFailureMessages" (gasPurchaseFailureMessages rdb)
        , testCaseSteps "transition occurs" (transitionOccurs rdb)
        , testCaseSteps "transition crosschain" (transitionCrosschain rdb)
        , testCaseSteps "upgradeNamespaceTests" (upgradeNamespaceTests rdb)
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

    let trivialTx :: Word -> CmdBuilder
        trivialTx n = (defaultCmd cid)
            { _cbRPC = mkExec' (sshow n)
            }

    liftIO $ do
        cmd1 <- buildTextCmd v (trivialTx 42)
        cmd2 <- buildTextCmd v (trivialTx 43)
        let rks = [cmdToRequestKey cmd1, cmdToRequestKey cmd2]

        let expectSuccessful :: (HasCallStack) => P.Prop [Maybe TestPact5CommandResult]
            expectSuccessful = P.alignExact
                [ P.match _Just ? P.fun _crResult ? P.equals (PactResultOk (PInteger 42))
                , P.match _Just ? P.fun _crResult ? P.equals (PactResultOk (PInteger 43))
                ]

        let expectEmpty :: (HasCallStack, Foldable t, Eq a) => t (Maybe a) -> IO ()
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

crosschainTest :: RocksDb -> Step -> IO ()
crosschainTest baseRdb step = runResourceT $ do
    let v = pact5InstantCpmTestVersion petersenChainGraph
    fx <- mkFixture v baseRdb

    let srcChain = unsafeChainId 0
    let targetChain = unsafeChainId 9

    liftIO $ do
        step "xchain initiate"
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

        send fx v srcChain [initiator]
        let initiatorReqKey = cmdToRequestKey initiator

        -- what if the source chain hasn't got the xchain transfer in a block yet?
        spvTxOutProof fx v targetChain srcChain initiatorReqKey
            & P.fails ? P.match _FailureResponse ? P.fun responseBody
            ? P.equals ("Transaction hash not found: " <> sshow initiatorReqKey)

        advanceAllChains_ fx
        [Just sendCr] <- pollWithDepth fx v srcChain [initiatorReqKey] (Just (ConfirmationDepth 0))
        let cont = fromMaybe (error "missing continuation") (_crContinuation sendCr)

        -- what if the target chain isn't aware of the source xchain transfer yet?
        spvTxOutProof fx v targetChain srcChain initiatorReqKey
            & P.fails ? P.match _FailureResponse ? P.fun responseBody
            ? P.equals "SPV target not reachable: target chain not reachable. Chainweb instance is too young"

        step "waiting"
        replicateM_ (int $ diameter petersenChainGraph) $ advanceAllChains_ fx
        TransactionOutputProofB64 spvProof <- spvTxOutProof fx v targetChain srcChain initiatorReqKey
        let contMsg = ContMsg
                { _cmPactId = _peDefPactId cont
                , _cmStep = succ $ _peStep cont
                , _cmRollback = _peStepHasRollback cont
                , _cmData = PUnit
                , _cmProof = Just (ContProof (T.encodeUtf8 spvProof))
                }
        step "xchain recv"

        -- what if we try to finish the xchain on the wrong chain?
        recvWrongChain <- buildTextCmd v
            $ set cbRPC (mkCont contMsg)
            $ defaultCmd srcChain
        send fx v srcChain [recvWrongChain]
        let recvWrongChainReqKey = cmdToRequestKey recvWrongChain
        advanceAllChains_ fx
        poll fx v srcChain [recvWrongChainReqKey]
            >>= P.match (_head . _Just)
            ? P.fun _crResult ? P.match _PactResultErr ? P.fun _peMsg
            -- sic
            ? P.equals "Continuation error: verifyCont: cannot redeem continuation proof on wrong targget chain"

        recv <- buildTextCmd v
            $ set cbRPC (mkCont contMsg)
            $ defaultCmd targetChain
        send fx v targetChain [recv]
        let recvReqKey = cmdToRequestKey recv
        advanceAllChains_ fx
        poll fx v targetChain [recvReqKey]
            >>= P.match (_head . _Just)
            ? P.checkAll
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

        -- what if we try to complete an already-completed xchain?
        recvRepeated <- buildTextCmd v
            $ set cbRPC (mkCont contMsg)
            $ defaultCmd targetChain
        send fx v targetChain [recvRepeated]
        let recvRepeatedReqKey = cmdToRequestKey recvRepeated
        advanceAllChains_ fx
        poll fx v targetChain [recvRepeatedReqKey]
            >>= P.match (_head . _Just)
            ? P.fun _crResult ? P.match _PactResultErr ? P.fun _peMsg ? P.fun _boundedText
            ? P.equals ("Requested defpact execution already completed for defpact id: " <> T.take 20 (renderDefPactId $ _peDefPactId cont) <> "...")

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
                    & P.fails ? P.match _FailureResponse ? P.fun responseBody ? textContains "Pact parse error: Expected: [')']"

            , testCase "invalid hash" $ do
                cmdInvalidPayloadHash <- do
                    bareCmd <- buildTextCmd v
                        $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                        $ defaultCmd cid
                    pure $ bareCmd
                        { _cmdHash = hash "fakehash"
                        }
                send fx v cid [cmdInvalidPayloadHash]
                    & P.fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed 0 cmdInvalidPayloadHash "Invalid transaction hash")

            , testCase "signature length mismatch" $ do
                cmdSignersSigsLengthMismatch1 <- do
                    bareCmd <- buildTextCmd v
                        $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                        $ defaultCmd cid
                    pure $ bareCmd
                        { _cmdSigs = []
                        }
                send fx v cid [cmdSignersSigsLengthMismatch1]
                    & P.fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed 0 cmdSignersSigsLengthMismatch1 "Invalid transaction sigs: The number of signers and signatures do not match. Number of signers: 1. Number of signatures: 0.")

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
                    & P.fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed 0 cmdSignersSigsLengthMismatch2 "Invalid transaction sigs: The number of signers and signatures do not match. Number of signers: 0. Number of signatures: 1.")

            , testCase "invalid signatures" $ do
                cmdInvalidUserSig <- mkCmdInvalidUserSig
                send fx v cid [cmdInvalidUserSig]
                    & P.fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed 0 cmdInvalidUserSig "Invalid transaction sigs: The signature at position 0 is invalid: failed to parse ed25519 signature: invalid bytestring size.")

            , testCase "batches are rejected with any invalid txs" $ do
                cmdGood <- mkCmdGood
                cmdInvalidUserSig <- mkCmdInvalidUserSig
                -- Test that [badCmd, goodCmd] fails on badCmd, and the batch is rejected.
                -- We just re-use a previously built bad cmd.
                send fx v cid [cmdInvalidUserSig, cmdGood]
                    & P.fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed 0 cmdInvalidUserSig "Invalid transaction sigs: The signature at position 0 is invalid: failed to parse ed25519 signature: invalid bytestring size.")
                -- Test that [goodCmd, badCmd] fails on badCmd, and the batch is rejected.
                -- Order matters, and the error message also indicates the position of the
                -- failing tx.
                -- We just re-use a previously built bad cmd.
                send fx v cid [cmdGood, cmdInvalidUserSig]
                    & P.fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed 1 cmdInvalidUserSig "Invalid transaction sigs: The signature at position 0 is invalid: failed to parse ed25519 signature: invalid bytestring size.")

            , testCase "multiple bad txs in batch" $ do
                cmdGood <- mkCmdGood
                cmdInvalidUserSig <- mkCmdInvalidUserSig
                cmdParseFailure <- buildTextCmd v
                    $ set cbRPC (mkExec' "(+ 1")
                    $ defaultCmd cid
                send fx v cid [cmdInvalidUserSig, cmdGood, cmdParseFailure]
                    & P.fails ? P.match _FailureResponse ? P.fun responseBody ? P.checkAll
                        [ textContains (validationFailed 0 cmdInvalidUserSig "Invalid transaction sigs: The signature at position 0 is invalid: failed to parse ed25519 signature: invalid bytestring size.")
                        , textContains (validationFailed 2 cmdParseFailure "Pact parse error: Expected: [')']")
                        ]

            , testCase "invalid metadata" $ do
                cmdGood <- mkCmdGood
                send fx v wrongChain [cmdGood]
                    & P.fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed 0 cmdGood "Transaction metadata (chain id, chainweb version) conflicts with this endpoint")

                send fx wrongV cid [cmdGood]
                    & P.fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals notFound404
                        , P.fun responseBody ? P.equals ""
                        ]

                let invalidCid = "invalid chain ID"
                cmdInvalidChain <- buildTextCmd v (defaultCmd cid & set cbChainId invalidCid)
                send fx v wrongChain [cmdInvalidChain]
                    & P.fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed 0 cmdInvalidChain "insert error: Unparsable ChainId")

                cmdWrongV <- buildTextCmd wrongV
                    $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                    $ defaultCmd cid
                send fx v cid [cmdWrongV]
                    & P.fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                        (validationFailed 0 cmdWrongV "Transaction metadata (chain id, chainweb version) conflicts with this endpoint")

                cmdExpiredTTL <- buildTextCmd v (defaultCmd cid & cbCreationTime .~ Just (TxCreationTime 0))
                send fx v cid [cmdExpiredTTL]
                    & P.fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? textContains
                            (validationFailed 0 cmdExpiredTTL "Transaction time-to-live is expired")
                        ]

            , testCase "cannot buy gas" $ do
                cmdExcessiveGasLimit <- buildTextCmd v
                    $ set cbGasLimit (GasLimit $ Gas 100000000000000)
                    $ defaultCmd cid
                send fx v cid [cmdExcessiveGasLimit]
                    & P.fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? textContains
                            (validationFailed 0 cmdExcessiveGasLimit "Transaction gas limit exceeds block gas limit")
                        ]

                cmdGasPriceTooPrecise <- buildTextCmd v
                    $ set cbGasPrice (GasPrice 0.00000000000000001)
                    $ defaultCmd cid
                send fx v cid [cmdGasPriceTooPrecise]
                    & P.fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? textContains
                            (validationFailed 0 cmdGasPriceTooPrecise "insert error: This transaction's gas price: 0.00000000000000001 is not correctly rounded. It should be rounded to at most 12 decimal places.")
                        ]

                cmdNotEnoughGasFunds <- buildTextCmd v
                    $ set cbGasPrice (GasPrice 10_000_000_000)
                    $ set cbGasLimit (GasLimit (Gas 10_000))
                    $ defaultCmd cid
                send fx v cid [cmdNotEnoughGasFunds]
                    & P.fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? textContains
                            (validationFailed 0 cmdNotEnoughGasFunds "Attempt to buy gas failed with: " <> sshow (_cmdHash cmdNotEnoughGasFunds) <> " Failed to buy gas: Insufficient funds")
                        ]

                cmdInvalidSender <- buildTextCmd v
                    $ set cbSender "invalid-sender"
                    $ defaultCmd cid
                send fx v cid [cmdInvalidSender]
                    & P.fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? textContains
                            -- TODO: the full error is far more verbose than this,
                            -- perhaps that's something we should fix.
                            (validationFailed 0 cmdInvalidSender "Attempt to buy gas failed")
                        ]

            ]

        -- the final test! none of the previous tests should have
        -- submitted even one single valid transaction.
        , testCase "none make it into a block" $ do
            (_, cmdResults) <- advanceAllChains fx
            forM_ cmdResults (P.alignExact mempty)

        ]
    where
    v = pact5InstantCpmTestVersion petersenChainGraph
    wrongV = pact5InstantCpmTestVersion twentyChainGraph

    cid = unsafeChainId 0
    wrongChain = unsafeChainId 1

    validationFailed i cmd msg = "Transaction " <> sshow (_cmdHash cmd) <> " at index " <> sshow @Int i <> " failed with: " <> msg

    mkCmdInvalidUserSig = mkCmdGood <&> set cmdSigs [ED25519Sig "fakeSig"]

    mkCmdGood = buildTextCmd v
        $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
        $ defaultCmd cid


caplistTest :: RocksDb -> Step -> IO ()
caplistTest baseRdb step = runResourceT $ do
    let v = pact5InstantCpmTestVersion petersenChainGraph
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
    let v = pact5InstantCpmTestVersion petersenChainGraph
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
                (set cbRPC
                    (mkExec' "(coin.release-allocation \"allocation01\")")
                    $ set cbSigners [mkEd25519Signer' allocation01KeyPair [], mkEd25519Signer' sender00 []]
                    $ defaultCmd cid)
                >>= local fx v cid Nothing Nothing Nothing
                >>= P.match _Pact5LocalResultLegacy
                    ? P.fun _crResult
                    ? P.match _PactResultErr
                    ? P.checkAll
                        [ P.fun _peType ? P.equals ? ErrorType "TxFailure"
                        , P.fun _peMsg ? P.fun _boundedText ? textContains "funds locked until \"2100-10-31T18:00:00Z\"."
                        ]

            buildTextCmd v
                (set cbRPC
                    (mkExec' "(coin.release-allocation \"allocation01\")")
                    $ set cbSigners [mkEd25519Signer' allocation01KeyPair [], mkEd25519Signer' sender00 []]
                    $ defaultCmd cid)
                >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                >>= P.match (_Pact5LocalResultWithWarns . _1) ?
                    P.fun _crResult ? P.match _PactResultErr ?
                    P.checkAll
                        [ P.fun _peType ? P.equals ? ErrorType "TxFailure"
                        , P.fun _peMsg ? P.fun _boundedText ? textContains "funds locked until \"2100-10-31T18:00:00Z\"."
                        ]

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

gasPurchaseFailureMessages :: RocksDb -> Step -> IO ()
gasPurchaseFailureMessages rdb _step = runResourceT $ do
    let v = pact5InstantCpmTestVersion petersenChainGraph
    let cid = unsafeChainId 0
    fx <- mkFixture v rdb

    -- Check the ways buyGas can fail and its error messages.
    -- Each case is checked with both `/local` (with preflight) and `/send`.
    liftIO $ do
        -- buyGas with insufficient balance to pay for the full supply
        -- (gas price * gas limit) should return an error
        -- this relies on sender00's starting balance.
        do
            cmd <- buildTextCmd v
                $ set cbSender "sender00"
                $ set cbSigners [mkEd25519Signer' sender00 []]
                $ set cbGasPrice (GasPrice 70_000)
                $ set cbGasLimit (GasLimit (Gas 100_000))
                $ defaultCmd cid

            local fx v cid (Just PreflightSimulation) Nothing Nothing cmd
                >>= P.match _Pact5LocalResultWithWarns
                ? P.fun fst
                ? P.fun _crResult
                ? P.match _PactResultErr
                ? P.checkAll
                    [ P.fun _peType ? P.equals ? ErrorType "EvalError"
                    , P.fun _peMsg ? P.fun _boundedText ? textContains "Failed to buy gas: Insufficient funds"
                    ]

            send fx v cid [cmd]
                & P.fails
                ? P.match _FailureResponse
                ? P.fun responseBody
                ? textContains "Failed to buy gas: Insufficient funds"

        -- multiple gas payer caps should lead to an error, because it's unclear
        -- which module will pay for gas
        do
            cmd <- buildTextCmd v
                $ set cbSender "sender00"
                $ set cbSigners
                    [ mkEd25519Signer' sender00
                        [ CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) []
                        , CapToken (QualifiedName "GAS_PAYER" (ModuleName "coin" Nothing)) []
                        , CapToken (QualifiedName "GAS_PAYER" (ModuleName "coin2" Nothing)) []
                        ]
                    ]
                $ defaultCmd cid

            local fx v cid (Just PreflightSimulation) Nothing Nothing cmd
                >>= P.match _Pact5LocalResultWithWarns
                ? P.fun fst
                ? P.fun _crResult
                ? P.match _PactResultErr
                ? P.checkAll
                    [ P.fun _peType ? P.equals ? ErrorType "EvalError"
                    , P.fun _peMsg ? P.fun _boundedText ? textContains "Failed to buy gas: Multiple gas payer capabilities"
                    ]

            send fx v cid [cmd]
                & P.fails
                ? P.match _FailureResponse
                ? P.fun responseBody
                ? textContains "Failed to buy gas: Multiple gas payer capabilities"

transitionOccurs :: RocksDb -> Step -> IO ()
transitionOccurs rdb _step = runResourceT $ do
    let v = instantCpmTransitionTestVersion petersenChainGraph
    let cid = unsafeChainId 0
    fx <- mkFixture v rdb

    liftIO $ do
        checkPactVersion fx v cid >>= P.equals Pact4
        forM_ @_ @_ @Word [1..17] $ \i -> do
            advanceAllChains_ fx
            -- index trick to show which iteration fails, if any
            (i,) <$> checkPactVersion fx v cid >>= P.equals (i, Pact4)
        advanceAllChains_ fx
        checkPactVersion fx v cid >>= P.equals Pact5

-- | Test that xchains work across the Pact4->Pact4 transition boundary.
--   This is mostly the same as 'spvTest', except it waits for the transition.
transitionCrosschain :: RocksDb -> Step -> IO ()
transitionCrosschain rdb step = runResourceT $ do
    let v = instantCpmTransitionTestVersion petersenChainGraph
    let srcChain = unsafeChainId 0
    let targetChain = unsafeChainId 9
    fx <- mkFixture v rdb

    let checkIsVersion pv = do
            checkPactVersion fx v srcChain >>= P.equals pv
            checkPactVersion fx v targetChain >>= P.equals pv

    liftIO $ do
        checkIsVersion Pact4

        step "xchain initiate"
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

        send fx v srcChain [initiator]
        let initiatorReqKey = cmdToRequestKey initiator
        advanceAllChains_ fx
        [Just sendCr] <- pollWithDepth fx v srcChain [initiatorReqKey] (Just (ConfirmationDepth 0))
        let cont = fromMaybe (error "missing continuation") (_crContinuation sendCr)

        step "waiting until pact5 transition"

        step "... performing transition"
        replicateM_ 16 $ advanceAllChains_ fx
        checkIsVersion Pact4
        advanceAllChains_ fx
        checkIsVersion Pact5

        TransactionOutputProofB64 spvProof <- spvTxOutProof fx v targetChain srcChain initiatorReqKey
        let contMsg = ContMsg
                { _cmPactId = _peDefPactId cont
                , _cmStep = succ $ _peStep cont
                , _cmRollback = _peStepHasRollback cont
                , _cmData = PUnit
                , _cmProof = Just (ContProof (T.encodeUtf8 spvProof))
                }
        step "xchain recv"

        recv <- buildTextCmd v
            $ set cbRPC (mkCont contMsg)
            $ defaultCmd targetChain
        send fx v targetChain [recv]
        let recvReqKey = cmdToRequestKey recv
        advanceAllChains_ fx
        poll fx v targetChain [recvReqKey]
            >>= P.match (_head . _Just)
                ? P.checkAll
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

    return ()

-- Test that transactions signed with (mock) WebAuthn keypairs are accepted
-- by the pact service.
webAuthnSignatureTest :: RocksDb -> Step -> IO ()
webAuthnSignatureTest rdb _step = runResourceT $ do
    let v = pact5InstantCpmTestVersion petersenChainGraph
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
    v = pact5InstantCpmTestVersion petersenChainGraph
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
                    & P.fails ? P.match _FailureResponse ? P.checkAll
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
                    & P.fails ? P.match _FailureResponse ? P.checkAll
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
                    & P.fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? P.equals "Metadata validation failed: [\"Chain id mismatch\"]"
                        ]

                buildTextCmd v (defaultCmd cid & set cbChainId invalidChain)
                    >>= local fx v cid Nothing Nothing Nothing
                    >>= P.succeed

                buildTextCmd v (set cbGasLimit (GasLimit $ Gas 100000000000000) $ defaultCmd cid)
                    >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                    & P.fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? P.equals "Metadata validation failed: [\"Transaction Gas limit exceeds block gas limit\"]"
                        ]

                buildTextCmd v (set cbGasPrice (GasPrice 0.00000000000000001) $ defaultCmd cid)
                    >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                    & P.fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? P.equals "Metadata validation failed: [\"Gas price decimal precision too high\"]"
                        ]

                buildTextCmd mainnet (defaultCmd cid)
                    >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                    & P.fails ? P.match _FailureResponse ? P.checkAll
                        [ P.fun responseStatusCode ? P.equals badRequest400
                        , P.fun responseBody ? P.equals "Metadata validation failed: [\"Network id mismatch\"]"
                        ]

                let sigs' = replicate 101 $ mkEd25519Signer' sender00 []
                buildTextCmd v (defaultCmd cid & set cbSigners sigs')
                    >>= local fx v cid (Just PreflightSimulation) Nothing Nothing
                    & P.fails ? P.match _FailureResponse ? P.checkAll
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
                hasBalance :: (HasCallStack) => _
                hasBalance p = P.fun _crResult
                    ? P.match _PactResultOk
                    ? P.match (_PObject . at "balance" . _Just)
                    ? P.match _PDecimal p
                hasBlockHeight :: (HasCallStack) => _
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
            & P.match (atChain cid)
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

upgradeNamespaceTests :: RocksDb -> Step -> IO ()
upgradeNamespaceTests baseRdb _step = runResourceT $ do
    let v = pact5InstantCpmTestVersion singletonChainGraph
    let cid = unsafeChainId 0
    fx <- mkFixture v baseRdb

    liftIO $ do
        upgradeNsContract <- T.readFile "pact/namespaces/ns.pact"
        do
            unprivilegedUpgradeCmd <- buildTextCmd v $
                set cbRPC (mkExec' upgradeNsContract) $
                set cbSigners
                    [mkEd25519Signer' sender00
                        -- sender00 controls ns, but module upgrades require unscoped signatures
                        [CapToken (QualifiedName "GAS" (ModuleName "coin" Nothing)) []]
                    ] $
                defaultCmd cid
            send fx v cid [unprivilegedUpgradeCmd]
            advanceAllChains_ fx
            poll fx v cid [cmdToRequestKey unprivilegedUpgradeCmd]
                >>= P.match (_head . _Just)
                ? P.fun _crResult
                ? P.match _PactResultErr
                ? P.fun _peMsg
                ? P.fun _boundedText
                ? textContains "Keyset failure"
        do
            privilegedUpgradeCmd <- buildTextCmd v $
                set cbRPC (mkExec' upgradeNsContract) $
                set cbSigners
                    -- sender00 controls ns, and module upgrades require unscoped signatures
                    [mkEd25519Signer' sender00 []
                    ] $
                defaultCmd cid
            send fx v cid [privilegedUpgradeCmd]
            advanceAllChains_ fx
            poll fx v cid [cmdToRequestKey privilegedUpgradeCmd]
                >>= P.match (_head . _Just)
                ? P.fun _crResult
                ? P.match _PactResultOk
                ? P.match _PString
                ? textContains "Loaded module ns"


----------------------------------------------------

data Fixture = Fixture
    { _cutFixture :: CutFixture.Fixture
    , _serviceClientEnv :: ClientEnv
    }

instance CutFixture.HasFixture Fixture where
    cutFixture = return . _cutFixture

class HasFixture a where
    remotePactTestFixture :: a -> IO Fixture

instance HasFixture Fixture where
    remotePactTestFixture = return
instance HasFixture a => HasFixture (IO a) where
    remotePactTestFixture = (>>= remotePactTestFixture)

mkFixture :: ChainwebVersion -> RocksDb -> ResourceT IO Fixture
mkFixture v baseRdb = do
    fx <- CutFixture.mkFixture v testPactServiceConfig baseRdb
    logger <- liftIO getTestLogger

    let mkSomePactServerData cid = PactServerData
            { _pactServerDataCutDb = fx ^. CutFixture.fixtureCutDb
            , _pactServerDataMempool = fx ^. CutFixture.fixtureMempools ^?! atChain cid
            , _pactServerDataLogger = logger
            , _pactServerDataPact = mkPactExecutionService (fx ^. CutFixture.fixturePactQueues ^?! atChain cid)
            }
    let pactServer = somePactServers v $ List.map (\cid -> (cid, mkSomePactServerData cid)) (HashSet.toList (chainIds v))
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
    pollResult <- runClientM (pactPollWithQueryApiClient v cid mConfirmationDepth (Pact5.PollRequest rksNel)) clientEnv
    case pollResult of
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
    displayException (ClientException errCallStack err) =
        "Client error: " <> show err
        <> "\n" <> GHC.Stack.prettyCallStack errCallStack
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
    sendResult <- runClientM (pactSendApiClient v cid batch) clientEnv
    case sendResult of
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
    either (throwM . ClientException callStack) return r

spvTxOutProof :: (HasCallStack, HasFixture fx)
    => fx
    -> ChainwebVersion
    -> ChainId
    -> ChainId
    -> RequestKey
    -> IO TransactionOutputProofB64
spvTxOutProof fx v trgChain srcChain reqKey = do
    clientEnv <- _serviceClientEnv <$> remotePactTestFixture fx
    let pact4TrgChain = Pact4.ChainId $ toText trgChain
    let pact4ReqKey = toPact4RequestKey reqKey
    r <- runClientM (pactSpvApiClient v srcChain (SpvRequest pact4ReqKey pact4TrgChain)) clientEnv
    either (throwM . ClientException callStack) return r

pactDeadBeef :: RequestKey
pactDeadBeef = case deadbeef of
    TransactionHash bytes -> RequestKey (Hash bytes)

textContains :: HasCallStack => Text -> P.Prop Text
textContains expectedStr actualStr
    | expectedStr `T.isInfixOf` actualStr = P.succeed actualStr
    | otherwise =
        P.fail ("String containing: " <> PP.pretty expectedStr) actualStr

checkPactVersion :: Fixture -> ChainwebVersion -> ChainId -> IO PactVersion
checkPactVersion fx v cid = do
    cmd <- buildTextCmd v
        $ set cbRPC (mkExec' "(do 1)")
        $ defaultCmd cid
    r <- local fx v cid (Just PreflightSimulation) Nothing Nothing cmd
    case r of
        LocalResultLegacy (getJsonText -> txt) -> do
            if extractError txt == Just "Cannot resolve do"
            then return Pact4
            else return Pact5
        LocalResultWithWarns (getJsonText -> txt) _warns -> do
            if extractError txt == Just "Cannot resolve do"
            then return Pact4
            else return Pact5
        anythingElse -> do
            assertFailure $ "checkPactVersion: Unexpected result: " ++ show anythingElse
    where
        extractError :: Text -> Maybe Text
        extractError json = json ^? A.key "result" . A.key "error" . A.key "message" . A._String
