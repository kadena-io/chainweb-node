{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Chainweb.Test.PactInProcApi
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution via (inprocess) API  in Chainweb

module Chainweb.Test.Pact.PactInProcApi where

import Control.Concurrent.MVar.Strict
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

import System.FilePath
import System.IO.Extra

import Test.Tasty
import Test.Tasty.Golden

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Pact.Types
import Chainweb.Test.Utils
import Chainweb.Version

import qualified Pact.ApiReq as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Crypto as P
import qualified Pact.Types.RPC as P

tests :: IO TestTree
tests = testGroup "Pact Http tests" <$> pactApiTest

pactApiTest :: IO [TestTree]
pactApiTest = do
    ------------------------------------------------------------------------------------------------
    -- Init for tests
    ------------------------------------------------------------------------------------------------
    reqQ <- initPactExec' testMemPoolAccess
    let headers = V.fromList $ getBlockHeaders 4

    ------------------------------------------------------------------------------------------------
    -- newBlock test
    ------------------------------------------------------------------------------------------------
    respVar0 <- newEmptyMVar :: IO (MVar Transactions)
    newBlock (headers ! 0) reqQ respVar0

    -- wait for response
    rsp0 <- takeMVar respVar0
    tt0 <- checkRespTrans "block-results-expected-0.txt" rsp0

    ------------------------------------------------------------------------------------------------
    -- validate the same transactions sent to newBlock above
    ------------------------------------------------------------------------------------------------
    respVar0b <- newEmptyMVar :: IO (MVar Transactions)
    validateBlock (headers ! 0) reqQ respVar0b

    -- wait for response
    rsp0b <- takeMVar respVar0b
    tt0b <- checkRespTrans "block-results-expected-0.txt" rsp0b

    ------------------------------------------------------------------------------------------------
    -- validate a different set of transactions (not sent to newBlock)
    ------------------------------------------------------------------------------------------------
    respVar1 <- newEmptyMVar :: IO (MVar Transactions)
    validateBlock (headers ! 1) reqQ respVar1

    -- wait for response
    rsp1 <- takeMVar respVar1
    tt1 <- checkRespTrans "block-results-expected-1.txt" rsp1

    ------------------------------------------------------------------------------------------------
    -- clean-up for tests
    ------------------------------------------------------------------------------------------------
    closeQueue reqQ
    return $ tt0 : tt0b : [tt1]


checkRespTrans :: FilePath -> Transactions -> IO TestTree
checkRespTrans fp txs =
    return $ goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) ioBs
    where
        ioBs = return $ toS $ show $ toJSON txs

getGenesisBlockHeader :: BlockHeader
getGenesisBlockHeader = do
    let testId = testChainId (1 :: Word32)
    genesisBlockHeader Test peterson testId

getBlockHeaders :: Int -> [BlockHeader]
getBlockHeaders n = do
    let testId = testChainId (1 :: Word32)
    let gbh0 = genesisBlockHeader Test peterson testId
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
