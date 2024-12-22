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
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens qualified as A
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (forM_, traverse_)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Stack
import Network.Connection qualified as HTTP
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS qualified as HTTP
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
import Pact.Core.Command.RPC (ContMsg (..))
import Pact.Core.Command.Server qualified as Pact5
import Pact.Core.Command.Types
import Pact.Core.DefPacts.Types
import Pact.Core.Gas.Types
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
import Chainweb.Test.Pact5.CutFixture qualified as CutFixture
import Chainweb.Test.Pact5.Utils
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils (TestPact5CommandResult, deadbeef, withResource', withResourceT)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService
import Network.HTTP.Types.Status (notFound404)
import GHC.Exts (WithDict(..))
import Pact.Core.Errors
import qualified Data.Map.Strict as Map
import Pact.Core.Guards (Guard(GKeySetRef), KeySetName (..))

data Fixture = Fixture
    { _cutFixture :: CutFixture.Fixture
    , _serviceClientEnv :: ClientEnv
    }
makeLenses ''Fixture

type Step = String -> IO ()

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
        { _cutFixture = fixture
        , _serviceClientEnv = serviceClientEnv
        }

class HasFixture where
    remotePactTestFixture :: IO Fixture

withFixture' :: IO Fixture -> ((CutFixture.HasFixture, HasFixture) => a) -> a
withFixture' fixture tests =
    withDict @HasFixture fixture $
    CutFixture.withFixture' (_cutFixture <$> remotePactTestFixture) tests

withFixture :: Fixture -> ((CutFixture.HasFixture, HasFixture) => a) -> a
withFixture fixture tests = withFixture' (return fixture) tests

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
        [ testCaseSteps "pollingInvalidRequestKeyTest" (pollingInvalidRequestKeyTest rdb)
        , testCaseSteps "pollingConfirmationDepthTest" (pollingConfirmationDepthTest rdb)
        , testCaseSteps "spvTest" (spvTest rdb)
        , invalidTxsTest rdb
        , testCaseSteps "caplistTest" (caplistTest rdb)
        , testCaseSteps "allocationTest" (allocationTest rdb)
        ]

pollingInvalidRequestKeyTest :: RocksDb -> Step -> IO ()
pollingInvalidRequestKeyTest baseRdb _step = runResourceT $ do
    let v = pact5InstantCpmTestVersion singletonChainGraph
    let cid = unsafeChainId 0
    fixture <- mkFixture v baseRdb

    withFixture fixture $ liftIO $ do
        poll v cid [pactDeadBeef] >>=
            P.equals [Nothing]

pollingConfirmationDepthTest :: RocksDb -> Step -> IO ()
pollingConfirmationDepthTest baseRdb _step = runResourceT $ do
    let v = pact5InstantCpmTestVersion singletonChainGraph
    let cid = unsafeChainId 0
    fixture <- mkFixture v baseRdb

    let trivialTx :: ChainId -> Word -> CmdBuilder
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

    withFixture fixture $ liftIO $ do
        cmd1 <- buildTextCmd v (trivialTx cid 42)
        cmd2 <- buildTextCmd v (trivialTx cid 43)
        let rks = [cmdToRequestKey cmd1, cmdToRequestKey cmd2]

        let expectSuccessful :: (HasCallStack, _) => P.Prop [Maybe TestPact5CommandResult]
            expectSuccessful = P.propful
                [ P.match _Just ? P.fun _crResult ? P.equals (PactResultOk (PInteger 42))
                , P.match _Just ? P.fun _crResult ? P.equals (PactResultOk (PInteger 43))
                ]

        let expectEmpty :: (HasCallStack, _) => _
            expectEmpty = traverse_ (P.equals Nothing)

        send v cid [cmd1, cmd2]

        pollWithDepth v cid rks Nothing
            >>= expectEmpty
        pollWithDepth v cid rks (Just (ConfirmationDepth 0))
            >>= expectEmpty

        CutFixture.advanceAllChains_

        pollWithDepth v cid rks Nothing
            >>= expectSuccessful
        pollWithDepth v cid rks (Just (ConfirmationDepth 0))
            >>= expectSuccessful
        pollWithDepth v cid rks (Just (ConfirmationDepth 1))
            >>= expectEmpty

        CutFixture.advanceAllChains_

        pollWithDepth v cid rks Nothing
            >>= expectSuccessful
        pollWithDepth v cid rks (Just (ConfirmationDepth 0))
            >>= expectSuccessful
        pollWithDepth v cid rks (Just (ConfirmationDepth 1))
            >>= expectSuccessful
        pollWithDepth v cid rks (Just (ConfirmationDepth 2))
            >>= expectEmpty

        CutFixture.advanceAllChains_

        pollWithDepth v cid rks Nothing
            >>= expectSuccessful
        pollWithDepth v cid rks (Just (ConfirmationDepth 0))
            >>= expectSuccessful
        pollWithDepth v cid rks (Just (ConfirmationDepth 1))
            >>= expectSuccessful
        pollWithDepth v cid rks (Just (ConfirmationDepth 2))
            >>= expectSuccessful
        pollWithDepth v cid rks (Just (ConfirmationDepth 3))
            >>= expectEmpty

        return ()

spvTest :: RocksDb -> Step -> IO ()
spvTest baseRdb step = runResourceT $ do
    let v = pact5InstantCpmTestVersion petersonChainGraph
    fixture <- mkFixture v baseRdb

    let srcChain = unsafeChainId 0
    let targetChain = unsafeChainId 9

    withFixture fixture $ liftIO $ do
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
            $ set cbSender "sender00"
            $ set cbChainId srcChain
            $ set cbGasPrice (GasPrice 0.01)
            $ set cbGasLimit (GasLimit (Gas 1_000))
            $ defaultCmd

        step "xchain initiate"
        send v srcChain [initiator]
        let initiatorReqKey = cmdToRequestKey initiator
        (sendCut, _) <- CutFixture.advanceAllChains
        [Just sendCr] <- pollWithDepth v srcChain [initiatorReqKey] (Just (ConfirmationDepth 0))
        let cont = fromMaybe (error "missing continuation") (_crContinuation sendCr)

        step "waiting"
        replicateM_ (int $ diameter petersonChainGraph + 1) $ CutFixture.advanceAllChains_
        let sendHeight = sendCut ^?! ixg srcChain . blockHeight
        spvProof <- createTransactionOutputProof_ (fixture ^. cutFixture . CutFixture.fixtureWebBlockHeaderDb) (fixture ^. cutFixture . CutFixture.fixturePayloadDb) targetChain srcChain sendHeight 0
        let contMsg = ContMsg
                { _cmPactId = _peDefPactId cont
                , _cmStep = succ $ _peStep cont
                , _cmRollback = _peStepHasRollback cont
                , _cmData = PUnit
                , _cmProof = Just (ContProof (B64U.encode (BL.toStrict (A.encode spvProof))))
                }
        step "xchain recv"

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
        send v targetChain [recv]
        let recvReqKey = cmdToRequestKey recv
        CutFixture.advanceAllChains_
        [Just recvCr] <- poll v targetChain [recvReqKey]
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


invalidTxsTest :: RocksDb -> TestTree
invalidTxsTest rdb = withResourceT (mkFixture v rdb) $ \fixtureIO -> withFixture' fixtureIO $
    sequentialTestGroup "invalid txs tests" AllSucceed
        [ testCase "syntax error" $ do
            cmdParseFailure <- buildTextCmd v
                $ set cbChainId cid
                $ set cbRPC (mkExec' "(+ 1")
                $ defaultCmd
            send v cid [cmdParseFailure]
                & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains "Pact parse error"

        , testCase "invalid hash" $ do
            cmdInvalidPayloadHash <- do
                bareCmd <- buildTextCmd v
                    $ set cbChainId cid
                    $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                    $ defaultCmd
                pure $ bareCmd
                    { _cmdHash = Pact5.hash "fakehash"
                    }
            send v cid [cmdInvalidPayloadHash]
                & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                    (validationFailedPrefix cmdInvalidPayloadHash <> "Invalid transaction hash")

        , testCase "signature length mismatch" $ do
            cmdSignersSigsLengthMismatch1 <- do
                bareCmd <- buildTextCmd v
                    $ set cbSigners [mkEd25519Signer' sender00 []]
                    $ set cbChainId cid
                    $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                    $ defaultCmd
                pure $ bareCmd
                    { _cmdSigs = []
                    }
            send v cid [cmdSignersSigsLengthMismatch1]
                & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                    (validationFailedPrefix cmdSignersSigsLengthMismatch1 <> "Invalid transaction sigs")

            cmdSignersSigsLengthMismatch2 <- do
                bareCmd <- buildTextCmd v
                    $ set cbSigners []
                    $ set cbChainId cid
                    $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                    $ defaultCmd
                pure $ bareCmd
                    {
                    -- This is an invalid ED25519 signature,
                    -- but length signers == length signatures is checked first
                    _cmdSigs = [ED25519Sig "fakeSig"]
                    }
            send v cid [cmdSignersSigsLengthMismatch2]
                & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                    (validationFailedPrefix cmdSignersSigsLengthMismatch2 <> "Invalid transaction sigs")

        , testCase "invalid signatures" $ do

            cmdInvalidUserSig <- mkCmdInvalidUserSig

            send v cid [cmdInvalidUserSig]
                & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                    (validationFailedPrefix cmdInvalidUserSig <> "Invalid transaction sigs")

        , testCase "batches are rejected with any invalid txs" $ do
            cmdGood <- mkCmdGood
            cmdInvalidUserSig <- mkCmdInvalidUserSig
            -- Test that [badCmd, goodCmd] fails on badCmd, and the batch is rejected.
            -- We just re-use a previously built bad cmd.
            send v cid [cmdInvalidUserSig, cmdGood]
                & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                    (validationFailedPrefix cmdInvalidUserSig <> "Invalid transaction sigs")
            -- Test that [goodCmd, badCmd] fails on badCmd, and the batch is rejected.
            -- Order matters, and the error message also indicates the position of the
            -- failing tx.
            -- We just re-use a previously built bad cmd.
            send v cid [cmdGood, cmdInvalidUserSig]
                & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                    (validationFailedPrefix cmdInvalidUserSig <> "Invalid transaction sigs")

        , testCase "invalid chain or version" $ do
            cmdGood <- mkCmdGood
            send v wrongChain [cmdGood]
                & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                    (validationFailedPrefix cmdGood <> "Transaction metadata (chain id, chainweb version) conflicts with this endpoint")

            send wrongV cid [cmdGood]
                & fails ? P.match _FailureResponse ? P.allTrue
                    [ P.fun responseStatusCode ? P.equals notFound404
                    , P.fun responseBody ? P.equals ""
                    ]

            cmdWrongV <- buildTextCmd wrongV
                $ set cbSigners [mkEd25519Signer' sender00 []]
                $ set cbChainId cid
                $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
                $ defaultCmd

            send v cid [cmdWrongV]
                & fails ? P.match _FailureResponse ? P.fun responseBody ? textContains
                    (validationFailedPrefix cmdWrongV <> "Transaction metadata (chain id, chainweb version) conflicts with this endpoint")
        -- must be the final test!
        , testCase "none make it into a block" $ do
            (_, cmdResults) <- CutFixture.advanceAllChains
            forM_ cmdResults (P.propful mempty)
        ]
    where
    v = pact5InstantCpmTestVersion petersonChainGraph
    wrongV = pact5InstantCpmTestVersion twentyChainGraph

    cid = unsafeChainId 0
    wrongChain = unsafeChainId 4

    validationFailedPrefix cmd = "Validation failed for hash " <> sshow (_cmdHash cmd) <> ": "

    mkCmdInvalidUserSig = mkCmdGood <&> set cmdSigs [ED25519Sig "fakeSig"]

    mkCmdGood = buildTextCmd v
        $ set cbSigners [mkEd25519Signer' sender00 []]
        $ set cbChainId cid
        $ set cbRPC (mkExec "(+ 1 2)" (mkKeySetData "sender00" [sender00]))
        $ defaultCmd


caplistTest :: RocksDb -> Step -> IO ()
caplistTest baseRdb step = runResourceT $ do
    let v = pact5InstantCpmTestVersion petersonChainGraph
    fixture <- mkFixture v baseRdb

    let cid = unsafeChainId 0

    withFixture fixture $ liftIO $ do
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
            $ set cbChainId cid
            $ set cbRPC (mkExec' "(coin.transfer \"sender00\" \"sender01\" 100.0)")
            $ defaultCmd

        step "sending"
        send v cid [tx0]

        let recvReqKey = cmdToRequestKey tx0

        step "advancing chains"

        CutFixture.advanceAllChains_

        step "polling"

        poll v cid [recvReqKey]
            >>= P.propful ? List.singleton ? P.match _Just ?
                P.allTrue
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
    fixture <- mkFixture v rdb

    withFixture fixture $ liftIO $ do
        do
            step "allocation00"
            release00Cmd <- buildTextCmd v
                $ set cbChainId cid
                $ set cbSigners [mkEd25519Signer' allocation00KeyPair [], mkEd25519Signer' sender00 []]
                $ set cbRPC (mkExec' "(coin.release-allocation \"allocation00\")")
                $ set cbSender "sender00"
                $ defaultCmd
            send v cid [release00Cmd]
            CutFixture.advanceAllChains_
            poll v cid [cmdToRequestKey release00Cmd] >>=
                P.propful
                    [ P.match _Just ? P.allTrue
                        [ P.fun _crResult ? P.match _PactResultOk ? P.succeed
                        , P.fun _crEvents ? P.startingWith
                            (event
                                (P.equals "RELEASE_ALLOCATION")
                                (P.equals [PString "allocation00", PDecimal 1000000])
                                (P.equals coinModuleName))
                        ]
                    ]

            buildTextCmd v
                (set cbRPC (mkExec' "(coin.details \"allocation00\")") defaultCmd)
                >>= local v cid Nothing Nothing Nothing
                >>= P.match _Pact5LocalResultLegacy
                    ? P.fun _crResult
                    ? P.match _PactResultOk
                    ? P.match _PObject
                    ? P.propful ? Map.fromList
                        [ ("account", P.equals (PString "allocation00"))
                        , ("balance", P.equals (PDecimal 1100000))
                        , ("guard", P.succeed)
                        ]

        step "allocation01"
        do
            buildTextCmd v
                (set cbRPC (mkExec' "(coin.release-allocation \"allocation01\")") defaultCmd)
                >>= local v cid Nothing Nothing Nothing
                >>= P.match _Pact5LocalResultLegacy ?
                    P.fun _crResult ? P.match (_PactResultErr . _PEPact5Error . _2) ?
                        P.fun _boundedText ? textContains "funds locked until \"2100-10-31T18:00:00Z\"."

        step "allocation02"
        do
            let c = "(define-keyset \"allocation02\" (read-keyset \"allocation02-keyset\"))"
            let d = mkKeySetData "allocation02-keyset" [allocation02KeyPair']
            redefineKeysetCmd <- buildTextCmd v
                $ set cbSigners [mkEd25519Signer' allocation02KeyPair []]
                $ set cbSender "allocation02"
                $ set cbRPC (mkExec c d)
                $ defaultCmd
            send v cid [redefineKeysetCmd]
            CutFixture.advanceAllChains_
            poll v cid [cmdToRequestKey redefineKeysetCmd]
                >>= P.propful [P.match _Just successfulTx]

            releaseAllocationCmd <- buildTextCmd v
                $ set cbSender "allocation02"
                $ set cbSigners [mkEd25519Signer' allocation02KeyPair' []]
                $ set cbRPC (mkExec' "(coin.release-allocation \"allocation02\")")
                $ defaultCmd
            send v cid [releaseAllocationCmd]
            CutFixture.advanceAllChains_
            poll v cid [cmdToRequestKey releaseAllocationCmd]
                >>= P.propful [P.match _Just successfulTx]

            buildTextCmd v (set cbRPC (mkExec' "(coin.details \"allocation02\")") defaultCmd)
                >>= local v cid Nothing Nothing Nothing
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

-- TODO: backport into Pact 5
_PEPact5Error :: Prism' (PactErrorCompat c) (ErrorCode, BoundedText _, c)
_PEPact5Error = prism' (PEPact5Error . uncurry3 PactErrorCode) $ \case
    PEPact5Error (PactErrorCode {_peCode, _peMsg, _peInfo}) ->
        Just (_peCode, _peMsg, _peInfo)
    _ -> Nothing

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
      rks <- liftIO $ send v sid cenv batch

      void $ liftIO $ step "pollApiClient: poll until key is found"
      void $ liftIO $ poll v sid cenv rks ExpectPactResult

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

newtype PollException = PollException String
    deriving stock (Show)
    deriving anyclass (Exception)

poll :: HasFixture
    => ChainwebVersion
    -> ChainId
    -> [RequestKey]
    -> IO [Maybe TestPact5CommandResult]
poll v cid rks = pollWithDepth v cid rks Nothing

pollWithDepth :: HasFixture
    => ChainwebVersion
    -> ChainId
    -> [RequestKey]
    -> Maybe ConfirmationDepth
    -> IO [Maybe TestPact5CommandResult]
pollWithDepth v cid rks mConfirmationDepth = do
    clientEnv <- _serviceClientEnv <$> remotePactTestFixture
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

send :: (HasCallStack, HasFixture)
    => ChainwebVersion
    -> ChainId
    -> [Command Text]
    -> IO ()
send v cid cmds = do
    let commands = NE.fromList $ toListOf each cmds
    let batch = Pact4.SubmitBatch (fmap toPact4Command commands)
    clientEnv <- _serviceClientEnv <$> remotePactTestFixture
    send <- runClientM (pactSendApiClient v cid batch) clientEnv
    case send of
        Left e -> do
            throwM (ClientException callStack e)
        Right (Pact4.RequestKeys (fmap toPact5RequestKey -> response)) -> do
            -- the returned request keys should always be exactly the hashes
            -- of the commands
            response & P.equals (cmdToRequestKey <$> commands)

local :: (HasCallStack, HasFixture)
    => ChainwebVersion
    -> ChainId
    -> Maybe LocalPreflightSimulation
    -> Maybe LocalSignatureVerification
    -> Maybe RewindDepth
    -> Command Text
    -> IO LocalResult
local v cid preflight sigVerify depth cmd = do
    -- send a single local request and return the result
    --
    clientEnv <- _serviceClientEnv <$> remotePactTestFixture
    r <- runClientM (pactLocalWithQueryApiClient v cid preflight sigVerify depth (toPact4Command cmd)) clientEnv
    case r of
        Right r -> return r
        Left e -> throwM $ ClientException callStack e

toPact5RequestKey :: Pact4.RequestKey -> RequestKey
toPact5RequestKey = \case
    Pact4.RequestKey (Pact4.Hash bytes) -> RequestKey (Pact5.Hash bytes)

toPact4Command :: Command Text -> Pact4.Command Text
toPact4Command cmd5 = case Aeson.eitherDecodeStrictText (J.encodeText cmd5) of
    Left err -> error $ "toPact4Command: decode failed: " ++ err
    Right cmd4 -> cmd4

toPact5CommandResult :: Pact4.CommandResult Pact4.Hash -> TestPact5CommandResult
toPact5CommandResult cr4 = case Aeson.eitherDecodeStrictText (J.encodeText cr4) of
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
