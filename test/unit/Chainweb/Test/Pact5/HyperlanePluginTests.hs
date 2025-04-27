{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Test.Pact5.HyperlanePluginTests (tests) where

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
tests baseRdb = testGroup "Pact5 HyperlanePluginTests"
    [ testCaseSteps "hyperlaneValidatorAnnouncementTest"
        $ hyperlaneValidatorAnnouncementTest baseRdb
    , testCaseSteps "hyperlaneVerifySuccess"
        $ hyperlaneVerifySuccess baseRdb
    , testCaseSteps "hyperlaneVerifyMoreValidatorsSuccess"
        $ hyperlaneVerifyMoreValidatorsSuccess baseRdb
    , testCaseSteps "hyperlaneVerifyThresholdZeroError"
        $ hyperlaneVerifyThresholdZeroError baseRdb
    , testCaseSteps "hyperlaneVerifyWrongSignersFailure"
        $ hyperlaneVerifyWrongSignersFailure baseRdb
    , testCaseSteps "hyperlaneVerifyNotEnoughRecoveredSignaturesFailure"
        $ hyperlaneVerifyNotEnoughRecoveredSignaturesFailure baseRdb
    , testCaseSteps "hyperlaneVerifyNotEnoughCapabilitySignaturesFailure"
        $ hyperlaneVerifyNotEnoughCapabilitySignaturesFailure baseRdb
    , testCaseSteps "hyperlaneVerifyMerkleIncorrectProofFailure"
        $ hyperlaneVerifyMerkleIncorrectProofFailure baseRdb
    , testCaseSteps "hyperlaneVerifyFailureNotEnoughSignaturesToPassThreshold"
        $ hyperlaneVerifyFailureNotEnoughSignaturesToPassThreshold baseRdb

    ]

v :: ChainwebVersion
v = pact5InstantCpmTestVersion petersenChainGraph

chain0 :: ChainId
chain0 = unsafeChainId 0

hyperlaneValidatorAnnouncementTest :: RocksDb -> Step -> IO ()
hyperlaneValidatorAnnouncementTest baseRdb step = runResourceT $ do
    fx <- mkFixture v baseRdb
    liftIO $ do
        let pluginName = "hyperlane_v3_announcement"

        step "deploy contract"
        deploy <- buildTextCmd v
            $ set cbGasLimit (GasLimit (Gas 100000))
            $ set cbRPC (mkExec' $ mconcat
                [ "(namespace 'free)"
                , "(module m G"
                , "(defcap G () true)"
                , "(defcap K (location:string signer:string mailbox:string) (enforce-verifier '" <> pluginName <> "))"
                , "(defun x () (with-capability (K \"storagelocation\" \"0x6c414e7a15088023e28af44ad0e1d593671e4b15\" \"kb-mailbox\") 1)))"
                ])
            $ defaultCmd chain0
        send fx v chain0 [deploy]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey deploy]
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
            $ defaultCmd chain0
        send fx v chain0 [usePlugin]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey usePlugin]
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
            $ defaultCmd chain0
        send fx v chain0 [useBadSignature]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey useBadSignature]
            >>= P.list
                [ P.match _Just ? P.checkAll
                    [ P.fun _crResult ? P.match _PactResultErr ? P.checkAll
                        [ P.fun _peType ? P.equals (ErrorType "EvalError")
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
            $ defaultCmd chain0
        send fx v chain0 [deployDifferentSigner]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey deployDifferentSigner]
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
            $ defaultCmd chain0
        send fx v chain0 [useWrongSigner]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey useWrongSigner]
            >>= P.list
                [ P.match _Just ? P.checkAll
                    [ P.fun _crResult ? P.match _PactResultErr ? P.checkAll
                        [ P.fun _peType ? P.equals (ErrorType "EvalError")
                        , P.fun _peMsg ? P.equals "Incorrect signer. Expected: PLiteral (LString {_lString = \"0x6c414e7a15088023e28af44ad0e1d593671e4b15\"}) but got PLiteral (LString {_lString = \"0x5c414e7a15088023e28af44ad0e1d593671e4b15\"})"
                        ]
                    , P.fun _crGas ? P.equals (Gas 100000)
                    ]
                ]

-- | Hyperlane test message TokenMessageERC20 encoded in base64:
--
--  { "amount": 0.000000000000000123
--  , "chainId": "4"
--  , "recipient": { "test-keys" : {"pred": "keys-all", "keys": ["da1a339bd82d2c2e9180626a00dc043275deb3ababb27b5738abf6b9dcee8db6"]} }
--  }
--
hyperlaneTokenMessageBase64 :: T.Text
hyperlaneTokenMessageBase64 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHsABHsicHJlZCI6ICJrZXlzLWFsbCIsICJrZXlzIjpbImRhMWEzMzliZDgyZDJjMmU5MTgwNjI2YTAwZGMwNDMyNzVkZWIzYWJhYmIyN2I1NzM4YWJmNmI5ZGNlZThkYjYiXX0"

-- | Hyperlane test message encoded in base64
hyperlaneMessageBase64 :: T.Text
hyperlaneMessageBase64 = encodeB64UrlNoPaddingText $ runPutS $ putHyperlaneMessage HyperlaneMessage
    { hmVersion = 3
    , hmNonce = 0
    , hmOriginDomain = 31337
    , hmSender = either (error . show) id $ decodeB64UrlNoPaddingText "AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY"
    , hmDestinationDomain = 626
    , hmRecipient = either (error . show) id $ decodeB64UrlNoPaddingText "AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU"
    , hmMessageBody = either (error . show) id $ decodeB64UrlNoPaddingText hyperlaneTokenMessageBase64
    }

hyperlaneMessageId :: T.Text
hyperlaneMessageId = encodeB64UrlNoPaddingText $ keccak256ByteString $ either (error . show) id $ decodeB64UrlNoPaddingText $ hyperlaneMessageBase64

-- | Hyperlane test MerkleTree Metadata encoded in base64
-- Test data was generated using the following test in the hyperlane codebase
-- https://github.com/hyperlane-xyz/hyperlane-monorepo/blob/b14f997810ebd7dbdff2ac6622a149ae77010ae3/solidity/test/isms/MultisigIsm.t.sol#L35
mkHyperlaneMerkleTreeMetadataBase64 :: B.ByteString -> [B.ByteString] -> T.Text
mkHyperlaneMerkleTreeMetadataBase64 proof signatures =
    encodeB64UrlNoPaddingText $ runPutS $ putMerkleRootMultisigIsmMetadata
        MerkleRootMultisigIsmMetadata
            { mrmimOriginMerkleTreeAddress = decodeHexUnsafe "0x2e234dae75c793f67a35089c9d99245e1c58470b"
            , mrmimMessageIdIndex = 0
            , mrmimSignedCheckpointMessageId = either (error . show) id $ decodeB64UrlNoPaddingText hyperlaneMessageId
            , mrmimMerkleProof = proof
            , mrmimSignedCheckpointIndex = 0
            , mrmimSignatures = signatures
            }

hyperlaneMerkleTreeCorrectProof :: B.ByteString
hyperlaneMerkleTreeCorrectProof = decodeHexUnsafe "0x0000000000000000000000000000000000000000000000000000000000000000ad3228b676f7d3cd4284a5443f17f1962b36e491b30a40b2405849e597ba5fb5b4c11951957c6f8f642c4af61cd6b24640fec6dc7fc607ee8206a99e92410d3021ddb9a356815c3fac1026b6dec5df3124afbadb485c9ba5a3e3398a04b7ba85e58769b32a1beaf1ea27375a44095a0d1fb664ce2dd358e7fcbfb78c26a193440eb01ebfc9ed27500cd4dfc979272d1f0913cc9f66540d7e8005811109e1cf2d887c22bd8750d34016ac3c66b5ff102dacdd73f6b014e710b51e8022af9a1968ffd70157e48063fc33c97a050f7f640233bf646cc98d9524c6b92bcf3ab56f839867cc5f7f196b93bae1e27e6320742445d290f2263827498b54fec539f756afcefad4e508c098b9a7e1d8feb19955fb02ba9675585078710969d3440f5054e0f9dc3e7fe016e050eff260334f18a5d4fe391d82092319f5964f2e2eb7c1c3a5f8b13a49e282f609c317a833fb8d976d11517c571d1221a265d25af778ecf8923490c6ceeb450aecdc82e28293031d10c7d73bf85e57bf041a97360aa2c5d99cc1df82d9c4b87413eae2ef048f94b4d3554cea73d92b0f7af96e0271c691e2bb5c67add7c6caf302256adedf7ab114da0acfe870d449a3a489f781d659e8beccda7bce9f4e8618b6bd2f4132ce798cdc7a60e7e1460a7299e3c6342a579626d22733e50f526ec2fa19a22b31e8ed50f23cd1fdf94c9154ed3a7609a2f1ff981fe1d3b5c807b281e4683cc6d6315cf95b9ade8641defcb32372f1c126e398ef7a5a2dce0a8a7f68bb74560f8f71837c2c2ebbcbf7fffb42ae1896f13f7c7479a0b46a28b6f55540f89444f63de0378e3d121be09e06cc9ded1c20e65876d36aa0c65e9645644786b620e2dd2ad648ddfcbf4a7e5b1a3a4ecfe7f64667a3f0b7e2f4418588ed35a2458cffeb39b93d26f18d2ab13bdce6aee58e7b99359ec2dfd95a9c16dc00d6ef18b7933a6f8dc65ccb55667138776f7dea101070dc8796e3774df84f40ae0c8229d0d6069e5c8f39a7c299677a09d367fc7b05e3bc380ee652cdc72595f74c7b1043d0e1ffbab734648c838dfb0527d971b602bc216c9619ef0abf5ac974a1ed57f4050aa510dd9c74f508277b39d7973bb2dfccc5eeb0618db8cd74046ff337f0a7bf2c8e03e10f642c1886798d71806ab1e888d9e5ee87d0838c5655cb21c6cb83313b5a631175dff4963772cce9108188b34ac87c81c41e662ee4dd2dd7b2bc707961b1e646c4047669dcb6584f0d8d770daf5d7e7deb2e388ab20e2573d171a88108e79d820e98f26c0b84aa8b2f4aa4968dbb818ea32293237c50ba75ee485f4c22adf2f741400bdf8d6a9cc7df7ecae576221665d7358448818bb4ae4562849e949e17ac16e0be16688e156b5cf15e098c627c0056a9"

hyperlaneMerkleTreeIncorrectProof :: B.ByteString
hyperlaneMerkleTreeIncorrectProof
    = B.take 31 hyperlaneMerkleTreeCorrectProof
    <> "\x19"
    <> B.drop 32 hyperlaneMerkleTreeCorrectProof

validSigner :: T.Text
validSigner = "0x2bd2e3ba4861fae19d87cd77f8557bbad8e92d23"

validSignature :: T.Text
validSignature = "0xfabe80dd5bf4440e5e7fbc3cdf12325df9c00beb1281c5ddf12e77177046790c49f531ccebb29ba9c9664a581ed1870873850e0cf0c231b779e21f48a1d0dcea1b"

-- | Deploys a contract with a valid signer
deployContractWith :: Fixture -> [T.Text] -> Integer -> IO ()
deployContractWith fx signers threshold = do
    let deployCode =
            [ "(namespace 'free)"
            , "(module m G"
            , "(defschema hyperlane_message"
            , "    version:integer"
            , "    nonce:integer"
            , "    originDomain:integer"
            , "    destinationDomain:integer"
            , "    sender:string"
            , "    recipient:string"
            , "    messageBody:string"
            , ")"
            , "(defcap G () true)"
            , "(defcap K (messageId:string message:object{hyperlane_message} signers:[string] threshold:integer)"
            , "  (enforce-verifier 'hyperlane_v3_message)"
            , "  (enforce (= messageId \"" <> hyperlaneMessageId <> "\") \"invalid messageId\")"
            , "  (enforce (= messageId (hyperlane-message-id message)) \"invalid calculated messageId\")"
            , "  (enforce (= signers [" <> (T.intercalate "," $ map (\s -> "\"" <> s <> "\"") signers) <> "]) \"invalid signers\")"
            , "  (enforce (= (at \"sender\" message) \"AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY\") \"invalid sender\")"
            , "  (enforce (= (at \"recipient\" message) \"AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU\") \"invalid recipient\")"
            , "  (bind (hyperlane-decode-token-message (at \"messageBody\" message)) "
            , "    { \"amount\" := amount, "
            , "      \"chainId\" := chain-id, "
            , "      \"recipient\" := recipient-guard }"
            , "    (enforce (= amount 0.000000000000000123) \"invalid amount\")"
            , "    (enforce (= (create-principal recipient-guard) \"k:da1a339bd82d2c2e9180626a00dc043275deb3ababb27b5738abf6b9dcee8db6\") \"invalid recipient guard\")"
            , "    (enforce (= (hyperlane-encode-token-message {\"amount\": 123.0, \"chainId\": chain-id, \"recipient\": \"eyJwcmVkIjogImtleXMtYWxsIiwgImtleXMiOlsiZGExYTMzOWJkODJkMmMyZTkxODA2MjZhMDBkYzA0MzI3NWRlYjNhYmFiYjI3YjU3MzhhYmY2YjlkY2VlOGRiNiJdfQ\""
            , "}) (at \"messageBody\" message)) \"invalid encoded message\")"
            , "  )"
            , ")"
            , "(defun x () (with-capability (K "
            , "\"" <> hyperlaneMessageId <> "\""
            , " {"
            , "  \"version\": 3,"
            , "  \"nonce\": 0,"
            , "  \"originDomain\": 31337,"
            , "  \"sender\": \"AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY\","
            , "  \"destinationDomain\": 626,"
            , "  \"recipient\": \"AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU\","
            , "  \"messageBody\": \"" <> hyperlaneTokenMessageBase64 <> "\""
            , "}"
            , " [" <> (T.intercalate "," $ map (\s -> "\"" <> s <> "\"") signers) <> "]"
            , " " <> sshow threshold
            , ")"
            , " \"succeeded\")))"
            ]
    deployCmd <- buildTextCmd v
        $ set cbRPC (mkExec' $ mconcat deployCode)
        $ set cbGasLimit (GasLimit $ Gas 70000)
        $ defaultCmd chain0
    send fx v chain0 [deployCmd]
    advanceAllChains_ fx
    poll fx v chain0 [cmdToRequestKey deployCmd] >>=
        P.list [P.match _Just ? successfulTx]

-- | Calls '(free.m.x)' from 'deployContractWithValidSigner'
mkMerkleMetadataCallWithGas :: GasLimit -> B.ByteString -> [T.Text] -> [T.Text] -> Integer -> IO (Command T.Text)
mkMerkleMetadataCallWithGas gas merkleProof signatures signersText threshold = buildTextCmd v
    $ set cbGasLimit gas
    $ set cbVerifiers
        [Verifier
            (VerifierName "hyperlane_v3_message")
            (ParsedVerifierProof $ PList $ V.fromList
                [ PString hyperlaneMessageBase64
                , PString $ mkHyperlaneMerkleTreeMetadataBase64 merkleProof $ map decodeHexUnsafe signatures
                ])
            [cap]
        ]
    $ set cbRPC
        (mkExec' "(free.m.x)")
    $ defaultCmd chain0
    where
    messageId = PString hyperlaneMessageId
    message = PObject . M.fromList $
        [ ("version", PInteger 3)
        , ("nonce", PInteger 0)
        , ("originDomain", PInteger 31337)
        , ("destinationDomain", PInteger 626)
        , ("sender", PString "AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY")
        , ("recipient", PString "AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU")
        , ("messageBody", PString hyperlaneTokenMessageBase64)
        ]

    signers = PList $ V.fromList $ map PString signersText
    cap = SigCapability
        $ CapToken (QualifiedName "K" (ModuleName "m" (Just (NamespaceName "free"))) )
            [messageId, message, signers, PInteger threshold]

mkMerkleMetadataCall :: B.ByteString -> [T.Text] -> [T.Text] -> Integer -> IO (Command T.Text)
mkMerkleMetadataCall = mkMerkleMetadataCallWithGas (GasLimit $ Gas 20000)

checkVerifierNotInTx :: Fixture -> T.Text -> IO ()
checkVerifierNotInTx fx pluginName = do
    cmd <- buildTextCmd v $ set cbRPC (mkExec' "(free.m.x)") $ defaultCmd chain0
    send fx v chain0 [cmd]
    advanceAllChains_ fx
    poll fx v chain0 [cmdToRequestKey cmd] >>= P.list
        [ P.match _Just
            ? P.fun _crResult
            ? P.match _PactResultErr
            ? P.checkAll
                [ P.fun _peType ? P.equals (ErrorType "TxFailure")
                , P.fun _peMsg ? P.fun _boundedText ? P.equals
                    ("Verifier " <> pluginName <> " failed with the message: not in transaction")
                ]
        ]

hyperlaneVerifySuccess :: RocksDb -> Step -> IO ()
hyperlaneVerifySuccess baseRdb step = runResourceT $ do
    fx <- mkFixture v baseRdb
    liftIO $ do
        let threshold = 1
        step "deploy contract"
        deployContractWith fx [validSigner] threshold
        checkVerifierNotInTx fx "hyperlane_v3_message"
        step "use verifier"
        cmd <- mkMerkleMetadataCall hyperlaneMerkleTreeCorrectProof [validSignature] [validSigner] threshold
        send fx v chain0 [cmd]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey cmd] >>= P.list
            [P.match _Just
                ? P.checkAll
                    [ successfulTx
                    , P.fun _crGas ? P.equals (Gas 16398)
                    ]
            ]


hyperlaneVerifyMoreValidatorsSuccess :: RocksDb -> Step -> IO ()
hyperlaneVerifyMoreValidatorsSuccess baseRdb step = runResourceT $ do
    fx <- mkFixture v baseRdb
    liftIO $ do
        step "deploy contract"
        let threshold = 1
        let signers = ["wrongSigner", validSigner]
        deployContractWith fx signers threshold
        checkVerifierNotInTx fx "hyperlane_v3_message"
        step "use verifier"
        cmd <- mkMerkleMetadataCall hyperlaneMerkleTreeCorrectProof [validSignature] signers threshold
        send fx v chain0 [cmd]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey cmd] >>= P.list
            [ P.match _Just ? P.checkAll
                [ successfulTx
                , P.fun _crGas ? P.equals (Gas 16398)
                ]
            ]

hyperlaneVerifyThresholdZeroError :: RocksDb -> Step -> IO ()
hyperlaneVerifyThresholdZeroError baseRdb step = runResourceT $ do
    fx <- mkFixture v baseRdb
    liftIO $ do
        step "deploy contract"
        let code = mconcat
                [ "(namespace 'free)"
                , "(module m G"
                , "(defcap G () true)"
                , "(defcap K (messageId:string message signers:[string] threshold:integer)"
                , "  (enforce-verifier 'hyperlane_v3_message)"
                , "  (enforce (= signers []) \"invalid signers\")"
                , ")"
                , "(defun x () (with-capability (K "
                , "\"" <> hyperlaneMessageId <> "\""
                , " {"
                , "  \"version\": 3,"
                , "  \"nonce\": 0,"
                , "  \"originDomain\": 31337,"
                , "  \"sender\": \"AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY\","
                , "  \"destinationDomain\": 626,"
                , "  \"recipient\": \"AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU\","
                , "  \"messageBody\": \"" <> hyperlaneTokenMessageBase64 <> "\""
                , "}"
                , " [] 0"
                , ")"
                , " \"succeeded\")))"
                ]
        deployCmd <- buildTextCmd v
            $ set cbRPC (mkExec' code)
            $ set cbGasLimit (GasLimit $ Gas 70000)
            $ defaultCmd chain0
        send fx v chain0 [deployCmd]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey deployCmd] >>= P.list
            [ P.match _Just ? successfulTx ]
        checkVerifierNotInTx fx "hyperlane_v3_message"
        step "use verifier"
        cmd <- mkMerkleMetadataCall hyperlaneMerkleTreeCorrectProof [] [] 0
        send fx v chain0 [cmd]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey cmd] >>= P.list
            [ P.match _Just
            ? P.checkAll
                [ P.fun _crResult
                    ? P.match _PactResultErr
                    ? P.checkAll
                        [ P.fun _peType ? P.equals (ErrorType "EvalError")
                        , P.fun _peMsg ? P.equals "Threshold should be greater than 0"
                        ]
                , P.fun _crGas ? P.equals (Gas 20000)
                ]
            ]

hyperlaneVerifyWrongSignersFailure :: RocksDb -> Step -> IO ()
hyperlaneVerifyWrongSignersFailure baseRdb step = runResourceT $ do
    fx <- mkFixture v baseRdb
    liftIO $ do
        step "deploy contract"
        let threshold = 1
        deployContractWith fx ["wrongSigner"] threshold
        checkVerifierNotInTx fx "hyperlane_v3_message"
        step "use verifier"
        cmd <- mkMerkleMetadataCall hyperlaneMerkleTreeCorrectProof [validSignature] ["wrongSigner"] threshold
        send fx v chain0 [cmd]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey cmd] >>= P.list
            [ P.match _Just
            ? P.checkAll
                [ P.fun _crResult
                    ? P.match _PactResultErr
                    ? P.checkAll
                        [ P.fun _peType ? P.equals (ErrorType "EvalError")
                        , P.fun _peMsg ? P.equals "Verification failed"
                        ]
                , P.fun _crGas ? P.equals (Gas 20000)
                ]
            ]

hyperlaneVerifyNotEnoughRecoveredSignaturesFailure :: RocksDb -> Step -> IO ()
hyperlaneVerifyNotEnoughRecoveredSignaturesFailure baseRdb step = runResourceT $ do
    fx <- mkFixture v baseRdb
    liftIO $ do
        step "deploy contract"
        let threshold = 1
        deployContractWith fx ["wrongSigner"] threshold
        checkVerifierNotInTx fx "hyperlane_v3_message"

        step "use verifier"
        cmd <- mkMerkleMetadataCall hyperlaneMerkleTreeCorrectProof [] ["wrongSigner"] threshold

        send fx v chain0 [cmd]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey cmd] >>= P.list
            [ P.match _Just
            ? P.checkAll
                [ P.fun _crResult
                    ? P.match _PactResultErr
                    ? P.checkAll
                        [ P.fun _peType ? P.equals (ErrorType "EvalError")
                        , P.fun _peMsg ? P.equals "The number of signatures can't be less than threshold"
                        ]
                , P.fun _crGas ? P.equals (Gas 20000)
                ]
            ]

hyperlaneVerifyNotEnoughCapabilitySignaturesFailure :: RocksDb -> Step -> IO ()
hyperlaneVerifyNotEnoughCapabilitySignaturesFailure baseRdb step = runResourceT $ do
    fx <- mkFixture v baseRdb
    liftIO $ do
        step "deploy contract"
        let threshold = 2
        deployContractWith fx [validSigner] threshold
        checkVerifierNotInTx fx "hyperlane_v3_message"
        step "use verifier"
        cmd <- mkMerkleMetadataCallWithGas
            (GasLimit $ Gas 40000)
            hyperlaneMerkleTreeCorrectProof
            [validSignature, validSignature]
            [validSigner]
            threshold
        send fx v chain0 [cmd]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey cmd] >>= P.list
            [ P.match _Just
            ? P.checkAll
                [ P.fun _crResult
                    ? P.match _PactResultErr
                    ? P.checkAll
                        [ P.fun _peType ? P.equals (ErrorType "EvalError")
                        , P.fun _peMsg ? P.equals "Verification failed"
                        ]
                , P.fun _crGas ? P.equals (Gas 40000)
                ]
            ]

hyperlaneVerifyMerkleIncorrectProofFailure :: RocksDb -> Step -> IO ()
hyperlaneVerifyMerkleIncorrectProofFailure baseRdb step = runResourceT $ do
    fx <- mkFixture v baseRdb
    liftIO $ do
        step "deploy contract"
        let threshold = 1
        deployContractWith fx [validSigner] threshold
        checkVerifierNotInTx fx "hyperlane_v3_message"
        step "use verifier"
        cmd <- mkMerkleMetadataCall
            hyperlaneMerkleTreeIncorrectProof
            [validSignature]
            [validSigner]
            threshold
        send fx v chain0 [cmd]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey cmd] >>= P.list
            [ P.match _Just
            ? P.checkAll
                [ P.fun _crResult
                    ? P.match _PactResultErr
                    ? P.checkAll
                        [ P.fun _peType ? P.equals (ErrorType "EvalError")
                        , P.fun _peMsg ? P.equals "Verification failed"
                        ]
                , P.fun _crGas ? P.equals (Gas 20000)
                ]
            ]

-- | We pass 2 signatures, 1st one matches to the correct validator,
-- but there is no second valid validator for the 2nd signature, and the verification fails.
hyperlaneVerifyFailureNotEnoughSignaturesToPassThreshold :: RocksDb -> Step -> IO ()
hyperlaneVerifyFailureNotEnoughSignaturesToPassThreshold baseRdb step = runResourceT $ do
    fx <- mkFixture v baseRdb
    liftIO $ do
        step "deploy contract"
        let threshold = 2
        let signers = ["wrongSigner", validSigner, "wrongSigner"]
        deployContractWith fx [validSigner] threshold
        checkVerifierNotInTx fx "hyperlane_v3_message"
        step "use verifier"
        cmd <-
            mkMerkleMetadataCallWithGas
            (GasLimit $ Gas 40000)
            hyperlaneMerkleTreeCorrectProof
            [validSignature, validSignature] signers threshold
        send fx v chain0 [cmd]
        advanceAllChains_ fx
        poll fx v chain0 [cmdToRequestKey cmd] >>= P.list
            [ P.match _Just
            ? P.checkAll
                [ P.fun _crResult
                    ? P.match _PactResultErr
                    ? P.checkAll
                        [ P.fun _peType ? P.equals (ErrorType "EvalError")
                        , P.fun _peMsg ? P.equals "Verification failed"
                        ]
                , P.fun _crGas ? P.equals (Gas 40000)
                ]
            ]
