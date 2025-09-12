{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Test.Pact5.SignedListPluginTests (tests) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Data.Aeson qualified as Aeson
import Data.Vector qualified as V

import Data.Text (Text)

import Data.Aeson.QQ.Simple

import PropertyMatchers ((?))
import PropertyMatchers qualified as P
import Test.Tasty
import Test.Tasty.HUnit (testCaseSteps, assertFailure)

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

-- | Test suite

v :: ChainwebVersion
v = pact5InstantCpmTestVersion petersenChainGraph

chain0 :: ChainId
chain0 = unsafeChainId 0

-- | Record type describing each test case
data SignedListTest = SignedListTest
  { sltName           :: String
  , sltProof          :: Aeson.Value
  , sltCapArgsEnv     :: [PactValue]
  , sltCapArgsContract:: Text
  , sltExpectSuccess  :: Bool
  , sltExpectedErrMsg :: Maybe String
  }

-- | Test suite definition
tests :: RocksDb -> TestTree
tests baseRdb = testGroup "Pact5 SignedList Plugin Tests" $ map (signedListTestTemplate baseRdb)
  [ SignedListTest "signedListValidatorTest"
      simpleExampleMsgProof
      [plist [], PString "02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7"]
      "[] \"02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7\""
      True
      Nothing
      
  , SignedListTest "signedListValidatorTestNested"
      simpleExampleMsgProof2
      nestedCapArgs
      "[[\"issue\",\"finp2p\",\"citi:102:d0c3eb56-0fff-4670-adfd-ad291a4314c3\",\"finId\",\"02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7\",1.0]] \"02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7\""
      True
      Nothing
  , SignedListTest "signedListValidatorTestMalformedProof"
      malformedProof
      []
      "[]"
      False
      (Just "Malformed inputs for signature verification")
  
  , SignedListTest "signedListValidatorTestMismatchedCap"
      simpleExampleMsgProof
      mismatchedCapArgs
      "[]"
      False
      (Just "Capability arguments do not match proof data")
  , SignedListTest "signedListValidatorTest - bad signature"
      simpleExampleMsgBadProof
      [plist [], PString "02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7"]
      "[] \"02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7\""
      False
      (Just "Capability arguments do not match proof data")
      
  ]

plist :: [PactValue] -> PactValue
plist = PList . V.fromList

signedListTestTemplate :: RocksDb -> SignedListTest -> TestTree
signedListTestTemplate baseRdb SignedListTest{..} = testCaseSteps sltName $ \step -> runResourceT $ do
  fx <- mkFixture v baseRdb
  liftIO $ do
    deployContract fx step
    step "use plugin"
    prf <- case Aeson.fromJSON sltProof of
      Aeson.Success p -> pure p
      _ -> assertFailure "Failed to decode proof JSON"

    let cap = CapToken (QualifiedName "K" (ModuleName "m" (Just (NamespaceName "free")))) sltCapArgsEnv
    usePlugin <- buildTextCmd v
      $ set cbRPC (mkExec' "(free.m.x)")
      $ set cbVerifiers [Verifier (VerifierName "signed_list") prf [SigCapability cap]]
      $ set cbGasLimit (GasLimit (Gas 100000))
      $ defaultCmd chain0
    send fx v chain0 [usePlugin]
    advanceAllChains_ fx
    poll fx v chain0 [cmdToRequestKey usePlugin] >>= \result -> do
      P.list [P.match _Just ? P.checkAll
              [ if sltExpectSuccess then successfulTx
                else case sltExpectedErrMsg of
                  Just _ -> P.succeed
                  Nothing -> P.fail "should have suceed..."
              ]] result
  where
    deployContract :: Fixture -> Step -> IO ()
    deployContract fx step = do
      step "deploy contract"
      deploy <- buildTextCmd v
        $ set cbGasLimit (GasLimit (Gas 100000))
        $ set cbRPC (mkExec' $ mconcat
            [ "(namespace 'free)"
            , "(module m G"
            , "(defcap G () true)"
            , "(defcap K (asset-id:list dst-fin-id:string) (enforce-verifier 'signed_list))"
            , "(defun x () (with-capability (K ", sltCapArgsContract ,") 1)))"
            ])
        $ defaultCmd chain0
      send fx v chain0 [deploy]
      advanceAllChains_ fx
      poll fx v chain0 [cmdToRequestKey deploy] >>= P.list [P.match _Just successfulTx]


-- -- | Test data

simpleExampleMsgProof, simpleExampleMsgBadProof, simpleExampleMsgProof2, malformedProof :: Aeson.Value
simpleExampleMsgProof = [aesonQQ|
[
  [{"0x":"faf3fd67e908cd840298a6f3523631716d4c8df06d69c9a32415f59eaa56825d"},
   {"0x":"e8a6bc590ed71a70e2507ad842a237cf55bbb751abb38fca61c0c83c4b07bd5f"}],
  "02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7",
  "db96110667579fd876c6b74d8fd848d29d0fb22f114c912202661010a27ef5087b3c9e33fc9af83e1f025e7d180612c7fc0c5f847a61e348dd35309855b29a44"
]
|]

simpleExampleMsgBadProof = [aesonQQ|
[
  [{"0x":"faf3fd67e908cd840298a6f3523631716d4c8df06d69c9a32415f59eaa56825d"},
   {"0x":"e8a6bc590ed71a70e2507ad842a237cf55bbb751abb38fca61c0c83c4b07bd5f"}],
  "02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7",
  "db96110667579fd876c6b74d8fd848d29d0fb22f114c912202661010a27ef5087b3c9e33fc9af83e1f025e7d180612c7fc0c5f847a61e348dd35309855b29a45"
]
|]

simpleExampleMsgProof2 = [aesonQQ|
[
  [[{"0x":"16f56a399856b1a6243f5b9afb3e523ea73294be4c95ef0a0000000066d97261"},"issue","finp2p","citi:102:d0c3eb56-0fff-4670-adfd-ad291a4314c3","finId","02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7",1],
   {"0x":"e8a6bc590ed71a70e2507ad842a237cf55bbb751abb38fca61c0c83c4b07bd5f"}],
  "02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7",
  "db96110667579fd876c6b74d8fd848d29d0fb22f114c912202661010a27ef5087b3c9e33fc9af83e1f025e7d180612c7fc0c5f847a61e348dd35309855b29a44"
]
|]

malformedProof = [aesonQQ| ["bad_data"] |]

nestedCapArgs, mismatchedCapArgs :: [PactValue]
nestedCapArgs = [plist [plist ((PString <$> ["issue","finp2p","citi:102:d0c3eb56-0fff-4670-adfd-ad291a4314c3","finId","02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7"]) ++ [ PDecimal 1 ])], PString "02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7"]
mismatchedCapArgs = [plist [PString "wrong"], PString "invalid"]
