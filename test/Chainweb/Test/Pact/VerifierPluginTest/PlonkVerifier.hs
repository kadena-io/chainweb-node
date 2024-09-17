{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 

module Chainweb.Test.Pact.VerifierPluginTest.PlonkVerifier
  (tests) where

import Chainweb.Miner.Pact
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.Types
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Pact.VerifierPluginTest.Transaction.Utils
import Chainweb.Utils
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import qualified Data.ByteString.Base16 as B16
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Term
import Pact.Types.Verifier hiding (verifierName)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup testName
  [ test generousConfig getGasModel "verifierTest" verifierTest
  ]
  where
    testName = "Chainweb.Test.Pact.VerifierPluginTest.PlonkVerifier"
    generousConfig = testPactServiceConfig { _pactBlockGasLimit = 300_000 }

    test pactConfig gasmodel tname f =
      withDelegateMempool $ \dmpio -> testCaseSteps tname $ \step ->
        withTestBlockDb testVersion $ \bdb -> do
          (iompa,mpa) <- dmpio
          let logger = hunitDummyLogger step
          withWebPactExecutionService logger testVersion pactConfig bdb mpa gasmodel $ \(pact,_) ->
            runReaderT f $
            SingleEnv bdb pact (return iompa) noMiner cid


mkCap :: T.Text -> T.Text -> T.Text -> SigCapability
mkCap vk progId pub = SigCapability (QualifiedName (ModuleName "m" (Just (NamespaceName "free"))) "P" def)
                      [pString vk, pString progId, pString pub]


verifierTest :: PactTestM ()
verifierTest = do
  runToHeight 118
  runBlockTest
    [ PactTxTest
        (buildBasic (mkExec' "(enforce-verifier 'plonk)"))
        (assertTxFailure "Should not resolve enforce-verifier" "Cannot resolve enforce-verifier")
    , PactTxTest
        (buildBasic'
          (set cbVerifiers
            [ Verifier
              (VerifierName "missing")
              (ParsedVerifierProof $ pString "")
              [mkCap "" "" ""]
            ]
          )
          (mkExec' "1"))
        (assertTxSuccess
          "Should not run verifiers before they're enabled" (pDecimal 1))
    ]

  runBlockTest
    [ PactTxTest
      (buildBasicGas 70000
      $ mkExec' $ mconcat
        [ "(namespace 'free)"
        , "(module m G"
        , "(defcap G () true)"
        , "(defcap P (vk:string progId:string pub:string) (enforce-verifier 'plonk))"
        , "(defun x (vk:string progId:string pub:string) (with-capability (P vk progId pub) 1)))"
        ]
      )
      (assertTxSuccess
        "Should allow enforce-verifier in a capability"
        (pString "Loaded module free.m, hash lSmhuQeyEkkTElGBuMAC0IH5rJZK3_4bZZ0vCPtwros")
      )
    , epocheChangeTest
    , inclusionTest
    , fibonacciTest
    ]

mkTest :: T.Text -> T.Text -> T.Text -> T.Text -> Gas -> PactTxTest
mkTest vk progId pub proofMsg g = PactTxTest
  (buildBasic'
    (set cbVerifiers
      [ Verifier
        (VerifierName "plonk")
        (ParsedVerifierProof $ pString proofMsg)
        [mkCap vk progId pub]
      ]
    )
    (mkExec' $ "(free.m.x \""<> vk <> "\" \""<> progId <>"\" \""<> pub <> "\")")
  )
  (\cr -> liftIO $ do
      assertTxSuccess "should have succeeded" (pDecimal 1) cr
      assertEqual "gas should have been charged" g (_crGas cr)
  )

-- | Epoche change test
--
-- https://github.com/argumentcomputer/lurk-hs/blob/d3931808f33a02e2a043028b33b29328855d9572/assets/epoch_change.json
epocheChangeTest :: PactTxTest
epocheChangeTest = mkTest vk progId pub proofMsg 362
  where
    vk = "v1.0.8-testnet"
    progId = hexToBase64 "0x0028418ec600456b3768cd78d1af143a057fc71a3cf522c557c7b473762946ee"
    pub = "0xe0e58f00000000005d32119aae2ee9f88867d5787af5c4df68884a4bf8fff525ff8c408e8f98805085382a0c8b1b38485a3d816f31ab5b23a0eae94d86c90086cd4e7b6e8c5c46825ebd1cf9ea54ce88af740aad4d7e95e742157209f36867ff5d7d490afa91c6bf"
    proofMsg = hexToBase64 "0x017c7eb73cc842da1f0de7e633cafe08d3443d7d2ab8b2840505436a1917ed8c24e2579f01e06d4b564a4e67cb7eb5e6223c176f5198aa6037a305a57dc24dbf0c5e784927edad806a06f27025febc0d0de8e2a79ed364ae9d1fbe7f16afe1811ccf7e9d2b556a474a902893460b55575b95e7e1ef5056965e7a008d9abc01cb1ba237f078fb45cec3204013d9973930c953c753bc3b5ff6015e1f22b87556f3272df6e02b68324f35e7082d9435428daa1fe036114bf2a0aca55388415eba6106716a421310b52e8490d2a416fd2458a9880106015729bb26ff592c9cf09ca01d623d186a3156de18defc205bc014624f8e4671abd419e9331d1702218deada2bdf332bb58aaf4e0a3e9812295ee36952a68419bba7271bc5813881bffc21771511696756276892e56c0d8c690bdd9c5d7dd79fd4801e20fd634ad956a770990758e274e64e041597728e73246ee4473e7a889772b217d414fe0605c0f8c0fd04d818dec517ae77f199d45b71a6ed2d1d253a497c5960fbff9b46a7641358b603d3f9ea0990150c10370190bd713982ebaf6ce021e2c6a1c77cf2ce3807b8cf1e12092cd52e0766402215fe41bce9da2dc2aa160e5e2aa62cac1a0f594785102a15f9501c3c4486566a21307c56aa4fa35dc39f05e49e4488731ba47f6cad3a230a188cbe9243fb5067bd8ec31d4fa8f1bb5802d93adcc213a5bfc530bc29de00000007093eba33099b7c086aaa18a4ed5d8358987eb303754202f712b4a19a51cc4643278b264dea8506db28133297289a13af02badf6d98900fd552a363a74dd2131d148efa4fb4abd437d9499f10bac59b124b128ee012c719264129c816821ec2910011875e35f8d2d28bd6ec36ce36a1680a546f28541f29681a22045b7dda321f2ec85e7ef610c112479e66d8776bcffb7202e2c6f9ed8333035326b5e6ac9e8b0f51671f56074d8ef83da4ab571a019f7f50cf85b3ddf3bf1f9b43c08f93274908e28c8e7138f858f1ff2227c265be7aabd17fc4091f96a12df3c236f8a8c74d2d35d17dfc4c2893c1e31d21c079fe0866657d34dac442ac76cb960d93cc19092234c977bdfeeee0ee0c32c61fe0fb04920f53f0decdd6b3cccb9d612b6ef9a20579d16dfa9e86f6be075fea1bf3fa1f9de0a18fe59ea8564c7cc1b30950cca40000000121ae71eb42b0d35cb26c2c981d9d314d9017a195401ba77e0ac8774b93f78f6102f3c6622b785034e3393a6009886b42e8d64293adff9028e35b06fc08a67204"

-- | Inclusion Test
--
-- https://github.com/argumentcomputer/lurk-hs/blob/d3931808f33a02e2a043028b33b29328855d9572/assets/inclusion_fixture.json
inclusionTest :: PactTxTest
inclusionTest = mkTest vk progId pub proofMsg 364
  where
    vk ="v1.0.8-testnet"
    progId = hexToBase64 "0x005835dfcad599fa418a0df2b5ecde903b801f7e0706e9530959119ec75aa9e3"
    pub = "0xe0fc9100000000000969ed235cf75d25800ea6845c2584af013c1f9617ad2de87202d7e9b93739c95c69bee701ef814a2b6a3edd4b1652cb9cc5aa6f22002fe30a172d0a479f6add89c63b29dce29b6071b3c7e486b0fb4bc431f88501000000000000002000000000000000290decd9548b62a8ef0d3e6ac11e2d7b95a49e22ecf57fc6044b6f007ca2b2ba010000000000000080"
    proofMsg = hexToBase64 "0x19b3db1870a51a91be455ba82d2e39cbe5629abdb0b684133a8f69e43302baed21f64f8c3a017c0c322ee0910ea20fb0deff14ebc9d28b22ae1b65bd049f5fe5068f5e2f1842492c1ec64675c53c7b541923d5ad88f4efafd5535061c06b1ee7107a492104941224643fae9f051fbcef1beb7ac19cb527748ff79ba4f7de2b48193bc9921fb16667289153bb6031a4fe01af87f693235319e6d3fea954ee91ac2440212dec1b6c62ab2cc1d8b61a4b9416e1b0d9237be712bdb8c603013c6b7200fe827e9235aaa8e2bbd3db172e46fff9161193e3a568ad8f6e7af50572a858150e60e282674711b44a801fcfe5bf8692a1feac0be30b6184f0c90987ba38fe02194c1c9c8429d24788e46f75568d03a3191c7e83a6dee6def281c6d36183522cdaa119cf660847b8c29cef6e4435499ef4086238825e9c441c3a28960cfc1b0a647a89d8494b5de2caa394fb59a4381a14a523f364ea302d6a84b32054308900662eaf0058fbc6d436602e672e2eba696e8110cd8d0228129496c1a708f29a291691d639d11a9ae2300a24b9fe7b50ccbc292ec9092ef367b98922afaa33300b02166502f811e1dbc324e0858c6f63b9db42152410aab9a96e5fd1ae5d624e01ea3694a8278162c9d962ca1ef2f6a70590a20a534e973acccee2f01af40fa204ddc41227632b53c764b99b837d32d785987a43f5edf397cd34beb9402b854900000007169c4da2bfc39950f8fe20225faf4da7b46ec38da8d4c22b2e40841ec30ef83e0d6aef7b402e7f0de5cb7f6798328268d2fc5e8759f6f12663ac3bff27c76cb1074224fa19ec12fb01bd95d10f4f53436b3e8441c3e93247d2f21fbe94579d43075306065f6ce1f0c966bd219fb1bc2f7347bcb8f149994d5cf023e0b408f34c0416a6c00e69acd5a402c776c53ee60ad3e3a01dea16621d5f110245e14501cf0e464ef0f83a41ad025dbf85cccffd2af8f2bbcfd0eddc5519e86d1ec362249b0d50b96c17005da5b900fad884a0a85402a07b92e4a746f3273b42f31b0e24e92c3e0ef03c15cfec9d03001e9dd208b257e7932f30780c90b5031206ea38e12d2916661b232cd46be404137b62a5268815cc9915685a5d9dcbcfed7d3e6872fe117973aaa3fcfadfbe2c42cbcf7537a5dc1df78c38e931066b3bc3125078dee7000000012ca60493e5d5edafe7948cbe0e3736b1c59aa29998f216a8d7e4b89c06c536a32129520c5ed2ecce4e9d8263b11b757956d2999f74dac0efc16992d923b94bc3"

-- | Fibonacci Test
--
-- https://github.com/argumentcomputer/lurk-hs/blob/d3931808f33a02e2a043028b33b29328855d9572/assets/fibonacci_fixture.json
fibonacciTest :: PactTxTest
fibonacciTest = mkTest vk progId pub proofMsg 1
  where
    vk = "v1.0.8-testnet"
    progId = hexToBase64 "0x2acfe95638a7aa71feaa167c1d53339708571ba2082f000d39bedebac6c872"
    pub = "0x190e5a3ed690bd3b533c087ef7339d00e25113d217f7dac4575a01b79dacd553"
    proofMsg = hexToBase64 "0x070ebe597360e09c92562b810cfd411a4cadac13904a6f5ef80597d31992b55611cc3d8557ad399cb5153cf87460901db1fd8dc92f16c5ea2ba1e8f357a1c5dd192120e7e19011e6e8028677870b4d59c1688fc0e729a547691ff7b9760aefbf242cfd52dbf3b98ddc003d31c8b86f2acf5f02ac891f990accba9a9c8477eb701d9f43bbd2c0566e7f48469c84a1769c17e74b8ed53602ce12f7f9a0181e9ab70548028c15e5a49611db64135b80545bf38afa4195e95d1289e20b945d7139ef1a94705f2ddbd481817316ef262053e9ee6a2419c63de7e3f339cac54bfa8c222e740ee8470ead68edb38aae2e9519fe01ce2f20707d6f81bc8643f0306c61ed1d37a65d87c876e13b3353bb42f02ab86423b4da43341e7bc5461b185a3946691d1015cb04fd6ffc38f9e7f52c92cffc3c44de7c58c9728d71e9f4c8266a831c2484b36f3ea92dd827d668dcf59ab7a0dee33f46fe289b90df06b15e4d288a5f1bee1bc0214e6a77647db6a2f4f51df8b100b1ddaef494618b3a3b0369ca18631f2d81effd2f9185afed9a0712d302b1d439c31d44a2ca3f20fd5f70a95eb30c132f39448df14f1e5311b906d22068ca160ac7d574df55605555df41208c9ab41634986f88e8af3b475ca428eb66e17f35c5d5808f358ec9884317db04d11a2d07200c82c257f1fdf6de2265f8cfd280c9728303f36dd0af7fc797b5ac232ae60000000720e4c44119a39f2303913d419ccff44f27639a1efe8634c93d82e581a4af1dee26b61ebb2d1eac5fe7e918786ced3a2a6b441dee0e8574341bb2e5bcbb27dc4f294ecca8cf684af42cecc403954747d3fb9b8ffb2a8081f2a809d3315f96bfca10d8e46e711998c3cd612849af775cb156caebcd3e2ce2bc7f6c7825dc536e310d69f3e8a506635f24f681b32fc30435c93d82ee0f93c83d7daf695d33acd87e1dbd2d55668d9a07c45d1f261173e9155bf35ce61b73becdc86ea76ca228f45a264f228a3b4f5b02022cd03bd8604a5ad62fd3a3706713bbc4f9846b31856b9608e7cdef78fc19f96cf6d5ada170bd3332f797f3a67756b83fa53e628f1db9a121042af248f08e8df4c9b046dc8807f7b514eb1dd7e6e2b6b80d060097a296cf22b56debdb623976250d281713f6e34b96ef5f71e92e748106d84675e30f9dc6000000010a0b17cbd37bd5c99d4ee00bf0a469c39494d8df9bbdbf57342e5d43e167dba20b659c2534a428edb9063ad60ddfb4867608b0520457348b535d1e5c1d040704"


hexToBase64 :: T.Text -> T.Text
hexToBase64 t = let
  sstr = fromMaybe t $ T.stripPrefix "0x" t
  t16 = either (error "decoding failed") id $ B16.decode $ T.encodeUtf8 sstr
  in encodeB64UrlNoPaddingText t16
