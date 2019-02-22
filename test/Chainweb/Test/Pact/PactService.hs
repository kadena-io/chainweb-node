{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Chainweb.Test.PactService
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact Http execution in Chainweb

module Chainweb.Test.Pact.PactService where

import Control.Monad.Zip

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Default
import Data.Maybe
import Data.String.Conv (toS)
import qualified Data.Text as T
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Word

import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Network.Wai.Handler.Warp as Warp

import Servant
import Servant.Client
import Servant.Client.Internal.HttpClient (ClientM(..))

import System.FilePath
import System.IO.Extra
import System.Random
import System.Time.Extra

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Pact.Service.PactApi
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Test.Utils
import Chainweb.Version

import qualified Pact.ApiReq as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Crypto as P
import qualified Pact.Types.RPC as P

tests :: IO TestTree
tests = testGroup "Pact service tests" <$> pactTestApp

pactTestApp :: IO [TestTree]
pactTestApp = do
    (port, socket) <- Warp.openFreePort
    withPactServiceApp (Left socket) "127.0.0.1" testMemPoolAccess $ do
        let headers = V.fromList $ getBlockHeaders 4
        base <- parseBaseUrl ("http://localhost:" ++ show port)
        mgr <- newManager defaultManagerSettings
        let clientEnv = mkClientEnv mgr base
        -- testing:  /new
        idResp0 <- runClientM (testGetNewBlock (headers ! 0)) clientEnv
        tt0 <- case idResp0 of
                  (Left servantError) -> assertFailure $
                      "No requestId returned from testGetBlock" ++ show servantError
                  (Right rqid) -> do
                      rspM <- pollForTestResp clientEnv rqid
                      case rspM of
                          Nothing -> assertFailure "Polling timeout for testGetNewBlock"
                          Just rsp -> checkRespTrans "block-results-expected-0.txt" rsp
        -- testing:  /validate
        idResp0b <- runClientM (testValidate (headers ! 0)) clientEnv
        tt0b <- case idResp0b of
                  (Left servantError) -> assertFailure $
                      "No requestId returned from testValidate" ++ show servantError
                  (Right rqid) -> do
                      rspM <- pollForTestResp clientEnv rqid
                      case rspM of
                          Nothing -> assertFailure "Polling timeout for testValidate"
                          Just rsp -> checkRespTrans "block-results-expected-0.txt" rsp
        -- testing:  /validate
        idResp1 <- runClientM (testValidate (headers ! 1)) clientEnv
        tt1 <- case idResp1 of
                  (Left servantError) -> assertFailure $
                      "No requestId returned from testValidate" ++ show servantError
                  (Right rqid) -> do
                      rspM <- pollForTestResp clientEnv rqid
                      case rspM of
                          Nothing -> assertFailure "Polling timeout for testValidate"
                          Just rsp -> checkRespTrans "block-results-expected-1.txt" rsp
        return $ tt0 : tt0b : [tt1]

pollForTestResp
    :: ClientEnv
    -> RequestId
    -> IO (Maybe (Either ServantError (Either String Transactions)))
pollForTestResp clientEnv reqId =
    timeout (fromIntegral timeoutSeconds) $
        runClientM (testPoll reqId) clientEnv

timeoutSeconds :: Int
timeoutSeconds = 30 -- seconds

checkRespTrans :: FilePath -> Either ServantError (Either String Transactions) -> IO TestTree
checkRespTrans _ (Left servantError) = assertFailure $ "Servant error: " ++ show servantError
checkRespTrans fp (Right x) =
    case x of
        Left err -> assertFailure $ "Error in pact response: "  ++ show err
        Right ts ->
            return $ goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) ioBs
            where
                ioBs = return $ toS $ show $ toJSON ts

generatePort :: IO Int
generatePort = getStdRandom (randomR (1024,65535))

testGetNewBlock :: BlockHeader -> ClientM RequestId
testValidate :: BlockHeader -> ClientM RequestId
testPoll :: RequestId -> ClientM (Either String Transactions)

testGetNewBlock
    :<|> testValidate
    :<|> testPoll
       = client (Proxy :: Proxy PactAPI)

getGenesisBlockHeader :: BlockHeader
getGenesisBlockHeader = do
    let testId = testChainId (1 :: Word32)
    genesisBlockHeader (Test peterson) testId

getBlockHeaders :: Int -> [BlockHeader]
getBlockHeaders n = do
    let testId = testChainId (1 :: Word32)
    let gbh0 = genesisBlockHeader (Test peterson) testId
    let after0s = take (n - 1) $ testBlockHeaders gbh0
    gbh0 : after0s

testMemPoolAccess :: BlockHeight -> IO [Transaction]
testMemPoolAccess (BlockHeight 0) = do
    moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
    let cmdStrs =
          [ moduleStr
          , "(create-table test1.accounts)"
          , "(test1.create-global-accounts)"
          , "(test1.transfer \"Acct1\" \"Acct2\" 1.00)" ]
    mkPactTestTransactions cmdStrs
testMemPoolAccess (BlockHeight n) = do
    let cmdStrs = cmdBlocks ! fromIntegral n
    mkPactTestTransactions cmdStrs

cmdBlocks :: Vector [String]
cmdBlocks =  V.fromList [ [ "(test1.transfer \"Acct1\" \"Acct2\" 5.00)"
                          , "(test1.transfer \"Acct1\" \"Acct2\" 6.00)" ]
                        , [ "(test1.transfer \"Acct1\" \"Acct2\" 10.00)"
                          , "(test1.transfer \"Acct1\" \"Acct2\" 11.00)" ]
                        ]

mkPactTestTransactions :: [String] -> IO [Transaction]
mkPactTestTransactions cmdStrs = do
    let theData = object ["test-admin-keyset" .= fmap P._kpPublic testKeyPairs]
    let intSeq = [0, 1 ..] :: [Word64]
    -- using 1 as the nonce here so the hashes match for the same commands (for testing only)
    return $ zipWith (mkPactTransaction testKeyPairs theData "1" )
             intSeq cmdStrs

mkPactTransaction
  :: [P.KeyPair]
  -> Value
  -> T.Text
  -> Word64
  -> String
  -> Transaction
mkPactTransaction keyPair theData nonce txId theCode =
    let pubMeta = def :: P.PublicMeta
        cmd = P.mkCommand
              (map (\P.KeyPair {..} -> (P.ED25519, _kpSecret, _kpPublic)) keyPair)
              pubMeta
              nonce
              (P.Exec (P.ExecMsg (T.pack theCode) theData))
    in Transaction {_tTxId = txId, _tCmd = cmd}

testKeyPairs :: [P.KeyPair]
testKeyPairs =
    let mPair = mzip (P.importPrivate testPrivateBs) (P.importPublic testPublicBs)
        mKeyPair = fmap
                   (\(sec, pub) -> P.KeyPair {_kpSecret = sec, _kpPublic = pub})
                   mPair
    in maybeToList mKeyPair

testPactFilesDir :: String
testPactFilesDir = "test/config/"

testPrivateBs :: ByteString
testPrivateBs = "53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d2"

testPublicBs :: ByteString
testPublicBs = "201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c0dbc"
