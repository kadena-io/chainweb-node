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

import qualified Data.Aeson as A (encode)
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
import Chainweb.Payload
import Chainweb.Test.Utils

import qualified Pact.ApiReq as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Crypto as P
import qualified Pact.Types.RPC as P

tests :: IO TestTree
tests = testGroup "Pact in-proc API tests" <$> pactApiTest

pactApiTest :: IO [TestTree]
pactApiTest = do
    ------------------------------------------------------------------------------------------------
    -- Init for tests
    ------------------------------------------------------------------------------------------------
    withPactService' testMemPoolAccess (\reqQ -> do
        let headers = V.fromList $ getBlockHeaders 4

        ------------------------------------------------------------------------------------------------
        -- newBlock test
        ------------------------------------------------------------------------------------------------
        respVar0 <- newBlock (headers ! 0) reqQ
        rsp0 <- takeMVar respVar0 -- wait for response
        tt0 <- checkNewResponse "new-block-expected-0" rsp0

        ------------------------------------------------------------------------------------------------
        -- validate the same transactions sent to newBlock above
        ------------------------------------------------------------------------------------------------
        respVar0b <- validateBlock (headers ! 0) reqQ
        rsp0b <- takeMVar respVar0b -- wait for response
        tt0b <- checkValidateResponse "validateBlock-expected-0" rsp0b

        ------------------------------------------------------------------------------------------------
        -- validate a different set of transactions (not sent to newBlock)
        ------------------------------------------------------------------------------------------------
        respVar1 <- validateBlock (headers ! 1) reqQ
        rsp1 <- takeMVar respVar1 -- wait for response
        tt1 <- checkValidateResponse "validateBlock-expected-1" rsp1

        ------------------------------------------------------------------------------------------------
        -- end of tests
        ------------------------------------------------------------------------------------------------
        return $ tt0 : tt0b : [tt1] )

checkNewResponse :: FilePath -> (BlockTransactions, BlockPayloadHash) -> IO TestTree
checkNewResponse filePrefix (bTrans, bplHash) = do
    ttBlockTxs <- checkBlockTransactions filePrefix bTrans
    ttBlockPayHash <- checkBlockPayloadHash filePrefix bplHash
    return $ testGroup "newResponse" (ttBlockTxs : [ttBlockPayHash])

checkValidateResponse :: FilePath -> (BlockTransactions, BlockOutputs) -> IO TestTree
checkValidateResponse filePrefix (bTrans, bOuts) = do
    ttBlockTxs <- checkBlockTransactions filePrefix bTrans
    ttBlockPayHash <- checkBlockOutputs filePrefix bOuts
    return $ testGroup "validate" (ttBlockTxs : [ttBlockPayHash])

checkBlockTransactions :: FilePath -> BlockTransactions -> IO TestTree
checkBlockTransactions filePrefix bTrans = do
    let fp = filePrefix ++ "-blockTrans.txt"
    let ioBsTrans = return $ foldMap (toS . _transactionBytes) (_blockTransactions bTrans)
    let ttTrans = goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) ioBsTrans

    let fp2 = filePrefix ++ "-blockTrans-hash.txt"
    let ioBsHash = return $ toS $ A.encode $ _blockTransactionsHash bTrans
    let ttTransHash = goldenVsString (takeBaseName fp2) (testPactFilesDir ++ fp2) ioBsHash

    return $ testGroup "BlockTransactions" $ ttTrans : [ttTransHash]

checkBlockPayloadHash :: FilePath -> BlockPayloadHash -> IO TestTree
checkBlockPayloadHash filePrefix bPayHash = do
   let fp = filePrefix ++ "-blockPayHash.txt"
   return $ goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) ioBs
   where
       ioBs = return $ A.encode bPayHash

checkBlockOutputs :: FilePath -> BlockOutputs -> IO TestTree
checkBlockOutputs filePrefix bOuts = do
   let fp = filePrefix ++ "-blockOuts.txt"
   let ioBsOuts = return $ foldMap (toS . _transactionOutputBytes) (_blockOutputs bOuts)
   let ttOuts = goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) ioBsOuts

   let fp2 = filePrefix ++ "-blockOut-hash.txt"
   let ioBsOutsHash = return $ A.encode $ _blockOutputsHash bOuts
   let ttOutsHash = goldenVsString (takeBaseName fp2) (testPactFilesDir ++ fp2) ioBsOutsHash

   return $ testGroup "BlockOutputs" $ ttOuts : [ttOutsHash]

getBlockHeaders :: Int -> [BlockHeader]
getBlockHeaders n = do
    let gbh0 = genesis
    let after0s = take (n - 1) $ testBlockHeaders gbh0
    gbh0 : after0s

testMemPoolAccess :: BlockHeight -> IO [PactTransaction]
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

mkPactTestTransactions :: [String] -> IO [PactTransaction]
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
  -> PactTransaction
mkPactTransaction keyPair theData nonce txId theCode =
    let pubMeta = def :: P.PublicMeta
        cmd = P.mkCommand
              (map (\P.KeyPair {..} -> (P.ED25519, _kpSecret, _kpPublic)) keyPair)
              pubMeta
              nonce
              (P.Exec (P.ExecMsg (T.pack theCode) theData))
    in PactTransaction {_ptTxId = txId, _ptCmd = cmd}

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

------------
-- UTILITIES -- Borrowed from Chainweb.Bench.Bench
------------
genesis :: BlockHeader
genesis = toyGenesis chainId0

chainId0 :: ChainId
chainId0 = testChainId 0
