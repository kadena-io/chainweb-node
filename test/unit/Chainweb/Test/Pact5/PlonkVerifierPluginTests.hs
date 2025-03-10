{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Test.Pact5.PlonkVerifierPluginTests (tests) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString qualified as B
import Data.Map.Strict qualified as M
import Data.Text qualified as T
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
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.VerifierPlugin.Hyperlane.Binary
import Chainweb.VerifierPlugin.Hyperlane.Utils
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
tests baseRdb = testGroup "Pact5 PlonkVerifierPluginTests"
    [ testCaseSteps "Verification Test"
        $ basicVerificationTest baseRdb

    ]

v :: ChainwebVersion
v = pact5InstantCpmTestVersion petersonChainGraph

chain0 :: ChainId
chain0 = unsafeChainId 0

basicVerificationTest :: RocksDb -> Step -> IO ()
basicVerificationTest baseRdb step = runResourceT $ do
    fx <- mkFixture v baseRdb
    liftIO $ do
        let pluginName = "plonk"
            vk = "v4.0.0-rc.3"
            progId = "0x002234f862365e788a1bf34da74e3979aa66a4f9971e501b858d175557995e28"
            pub = "0x0060b00000000000a8204fe8710deb377187fdd13b229340a7a9b2dc1477c8e75c546d93b0808468a8204fe8710deb377187fdd13b229340a7a9b2dc1477c8e75c546d93b080846869a93228b3a9a44a2d02ec6b771d3693bc762d25851887db488f06b59323696a"
            proof = "GzT-EQYcZFASIAH2oMC7Eothr5y1NGUps4OkRADiYSFuEjTLFbOExOh2cgZ7Jx476fzN4uzFGYIjiiAa0nLVVgPnoF4fbw6WGlzll6SJ6tBdbD2cJJRekjP-i6HGEPw2dw7WNywgcbsFGH5r1iwD_ipJxDjfA4k2Okdy286p8ie9YP6_L8lE7wzKHNo7xp7cBEJDhadTH-nwngTCBpNhI6vOEGsv0H9jHio1vibQMldGBcDBYQLgCV7LLx093jU1OTvKWQSqsCRvyN-ff_DFFBL8mQJrwq6Vp_XexAzY2f-KhX4uCL8BhoAIkovxj8_8J-U4-LCT1A67n55jSrs5IYxBwfonm5eoOI_L54jqlLAIJEGc89ZQ0jVoxkCp20JLQgcyFgN39jagJpWOl1ezMeKhGmeUcFaor6K1N2OkP4H1gE-xL7N-EXPt70vAOCg3jqDD1b8eFUKAUYSNIVisZ9eotBAXDT4zSq832jg24o81hbPQqhjvAEeXOhdP5y_qxfsOAxDuKM92hcYB8r8E4XPh43WF7yFz4FWqX360DTTiRo35FiL65csgA7Qx9C7jJkdRFztRZMJINY6pGsUOeLzPr_ktw2UGR6xnl56XzUxh3Oa-vkuy7e02tV4FXn-zwx5_oRqOzCQkQBrd9sRCnxL7fwJFxta7ottIVK8b0PMR_jNiG3YYX3MZ5nhql2KyT37OcyO9V5dg_QEIfZUWoep0h38D5x04JKZ3GXK_eX9e5C9DCwIY5UOKGnpL9kBSuOFTsAOTQrpDAfs08U5sJH-4JZQH-Lk7trdXBIqbnsmBPfjfHQ2HdITRCD39RC6oK2fmjep5uWTP2ZYU-A8kQg9J1yUVkGr0UtCxrPoE5cfoU72P3-YE8IxZVkYnEY49-GZREhhnWXkn9JKY3OzWUaLCtExLp2JBn8cOoYvZufG8ojX1IIQ0X3KSl1LFUs-L4xBf3dHVH465RqoAMGQcPILNzOEtJjmyFWyhWfCzBtFHzpBfhRGwq_fCb6YtGxyX41y3tR_tqMHgiZCMQBd2jw6R8zMdSgjI9aIhpJ1f4pUjIi5XJ-jbKCYlEBzVq0WCsQWrzNgjA_LSWow2v7cRu3_U3UotXtUYOEgyeg88wpnQ3psS_BaM5_OQ9zc2PV8qvT_2Fw"

        step "deploy contract"
        deploy <- buildTextCmd v
            $ set cbGasLimit (GasLimit (Gas 100000))
            $ set cbRPC (mkExec' $ mconcat
                [ "(namespace 'free)"
                , "(module m G"
                , "  (defcap G () true)"
                , "  (defcap K (vk progid pub) (enforce-verifier '" <> pluginName <> "))"
                , "  (defun x () (with-capability (K \""<> vk <>"\" \""<> progId <>"\" \""<> pub <> "\") 1)))"
                ])
            $ defaultCmd chain0
        send fx v chain0 [deploy]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey deploy]
            >>= P.list [P.match _Just successfulTx]

        step "use successfully"
        let cap =
                CapToken (QualifiedName "K" (ModuleName "m" (Just (NamespaceName "free"))) )
                    [PString vk, PString progId, PString pub]
        usePlugin <- buildTextCmd v
            $ set cbRPC (mkExec' "(free.m.x)")
            $ set cbVerifiers
                [Verifier
                    (VerifierName pluginName)
                    (ParsedVerifierProof $ PString proof)
                    [SigCapability cap]]
            $ set cbGasLimit (GasLimit (Gas 100000))
            $ defaultCmd chain0
        send fx v chain0 [usePlugin]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey usePlugin]
            >>= P.list
                [P.match _Just ? P.checkAll
                    [ successfulTx
                    , P.fun _crGas ? P.equals (Gas 199)
                    ]
                ]
