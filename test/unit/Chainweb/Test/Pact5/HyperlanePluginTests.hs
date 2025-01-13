{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Test.Pact5.HyperlanePluginTests (tests) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Vector qualified as V

import PropertyMatchers ((?))
import PropertyMatchers qualified as P
import Test.Tasty
import Test.Tasty.HUnit (testCaseSteps)

import Chainweb.Graph
import Chainweb.Storage.Table.RocksDB (RocksDb)
import Chainweb.Test.Pact5.CmdBuilder
import Chainweb.Test.Pact5.CutFixture (advanceAllChains_)
import Chainweb.Test.Pact5.RemotePactTest hiding (tests)
import Chainweb.Test.Pact5.Utils
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Version
import Pact.Core.Command.Types
import Pact.Core.Gas
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Capabilities
import Pact.Core.Verifiers
import Pact.Core.Signer
import Pact.Core.Errors

tests :: RocksDb -> TestTree
tests baseRdb = testGroup "Pact5 HyperlanePluginTests"
    [ testCaseSteps "hyperlaneValidatorAnnouncementTest"
        $ hyperlaneValidatorAnnouncementTest baseRdb
    ]

hyperlaneValidatorAnnouncementTest :: RocksDb -> Step -> IO ()
hyperlaneValidatorAnnouncementTest baseRdb step = runResourceT $ do
    let v = pact5InstantCpmTestVersion petersonChainGraph
    fx <- mkFixture v baseRdb
    cid <- mkChainId v maxBound 0
    liftIO $ do
        let pluginName = "hyperlane_v3_announcement"

        step "deploy"

        deploy <- buildTextCmd v
            $ set cbGasLimit (GasLimit (Gas 100000))
            $ set cbRPC (mkExec' $ mconcat
                [ "(namespace 'free)"
                , "(module m G"
                , "(defcap G () true)"
                , "(defcap K (location:string signer:string mailbox:string) (enforce-verifier '" <> pluginName <> "))"
                , "(defun x () (with-capability (K \"storagelocation\" \"0x6c414e7a15088023e28af44ad0e1d593671e4b15\" \"kb-mailbox\") 1)))"
                ])
            $ defaultCmd cid
        send fx v cid [deploy]
        advanceAllChains_ fx
        poll fx v cid [cmdToRequestKey deploy]
            >>= P.list [P.match _Just successfulTx]

        step "use successfully"
        let cap =
                CapToken (QualifiedName "K" (ModuleName "m" (Just (NamespaceName "free"))) )
                    [PString "storagelocation", PString "0x6c414e7a15088023e28af44ad0e1d593671e4b15", PString "kb-mailbox"]
        usePlugin <- buildTextCmd v
            $ set cbRPC (mkExec' "(free.m.x)")
            $ set cbVerifiers
                [Verifier
                    (VerifierName pluginName)
                    (ParsedVerifierProof $
                    PList $ V.fromList
                        [ PString "storagelocation"
                        -- TODO: generate instead of using the precomputed value
                        , PString "U7oftiGhn7rpWJydP6t0FKStdcRd223a8uSTqKjs8K8nJW7U84tzBOgPZTtGKnncwiu8l1185vB38c7-Ov7avBw"
                        , PString "kb-mailbox"
                        ]
                    )
                    [SigCapability cap]]
            $ set cbGasLimit (GasLimit (Gas 100000))
            $ defaultCmd cid
        send fx v cid [usePlugin]
        advanceAllChains_ fx
        poll fx v cid [cmdToRequestKey usePlugin]
            >>= P.list
                [P.match _Just ? P.checkAll
                    [ successfulTx
                    , P.fun _crGas ? P.equals (Gas 16342)
                    ]
                ]

        step "use with bad signature"
        useBadSignature <- buildTextCmd v
            $ set cbRPC (mkExec' "(free.m.x)")
            $ set cbVerifiers
                [Verifier
                    (VerifierName pluginName)
                    (ParsedVerifierProof $
                    PList $ V.fromList
                        [ PString "storagelocation"
                        -- bad signature (same as from the previous test but the different first symbol)
                        , PString "Q7oftiGhn7rpWJydP6t0FKStdcRd223a8uSTqKjs8K8nJW7U84tzBOgPZTtGKnncwiu8l1185vB38c7-Ov7avBw"
                        , PString "kb-mailbox"
                        ]
                    )
                    [SigCapability cap]]
            $ set cbGasLimit (GasLimit (Gas 100000))
            $ defaultCmd cid
        send fx v cid [useBadSignature]
        advanceAllChains_ fx
        poll fx v cid [cmdToRequestKey useBadSignature]
            >>= P.list
                [ P.match _Just ? P.checkAll
                    [ P.fun _crResult ? P.match (_PactResultErr  . _PEPact5Error) ? P.checkAll
                        [ P.fun _peCode ? P.equals (ErrorCode 1407374883553280)
                        , P.fun _peMsg ? P.equals "Failed to recover the address from the signature"
                        ]
                    , P.fun _crGas ? P.equals (Gas 100000)
                    ]
                ]

        step "deploy with different signer"
        deployDifferentSigner <- buildTextCmd v
            $ set cbGasLimit (GasLimit (Gas 100000))
            $ set cbRPC (mkExec' $ mconcat
                [ "(namespace 'free)"
                , "(module m G"
                , "(defcap G () true)"
                , "(defcap K (location:string signer:string mailbox:string) (enforce-verifier '" <> pluginName <> "))"
                , "(defun x () (with-capability (K \"storagelocation\" \"0x5c414e7a15088023e28af44ad0e1d593671e4b15\" \"kb-mailbox\") 1)))"
                ])
            $ defaultCmd cid
        send fx v cid [deployDifferentSigner]
        advanceAllChains_ fx
        poll fx v cid [cmdToRequestKey deployDifferentSigner]
            >>= P.list [P.match _Just successfulTx]

        let capWrongSigner =
                CapToken (QualifiedName "K" (ModuleName "m" (Just (NamespaceName "free"))) )
                    [PString "storagelocation", PString "0x5c414e7a15088023e28af44ad0e1d593671e4b15", PString "kb-mailbox"]
            -- bad signer (same as from the previous test but the different first symbol)

        step "use with wrong signer"
        useWrongSigner <- buildTextCmd v
            $ set cbRPC (mkExec' "(free.m.x)")
            $ set cbVerifiers
                [Verifier
                    (VerifierName pluginName)
                    (ParsedVerifierProof $
                    PList $ V.fromList
                        [ PString "storagelocation"
                        -- TODO: generate instead of using the precomputed value
                        , PString "U7oftiGhn7rpWJydP6t0FKStdcRd223a8uSTqKjs8K8nJW7U84tzBOgPZTtGKnncwiu8l1185vB38c7-Ov7avBw"
                        , PString "kb-mailbox"
                        ]
                    )
                    [SigCapability capWrongSigner]]
            $ set cbGasLimit (GasLimit (Gas 100000))
            $ defaultCmd cid
        send fx v cid [useWrongSigner]
        advanceAllChains_ fx
        poll fx v cid [cmdToRequestKey useWrongSigner]
            >>= P.list
                [ P.match _Just ? P.checkAll
                    [ P.fun _crResult ? P.match (_PactResultErr  . _PEPact5Error) ? P.checkAll
                        [ P.fun _peCode ? P.equals (ErrorCode 1407374883553280)
                        , P.fun _peMsg ? P.equals "Incorrect signer. Expected: PLiteral (LString {_lString = \"0x6c414e7a15088023e28af44ad0e1d593671e4b15\"}) but got PLiteral (LString {_lString = \"0x5c414e7a15088023e28af44ad0e1d593671e4b15\"})"
                        ]
                    , P.fun _crGas ? P.equals (Gas 100000)
                    ]
                ]
