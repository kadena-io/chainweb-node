{-# LANGUAGE OverloadedStrings #-}

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

import qualified Data.Aeson as A (encode)
import Data.Sequence (Seq)
import Data.String.Conv (toS)
import qualified Data.Text.IO as T
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import System.FilePath
import System.IO.Extra
import System.LogLevel

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.Golden

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Test.Pact.Utils
import Chainweb.Version (ChainwebVersion(..))
import Chainweb.BlockHeader.Genesis


tests :: IO TestTree
tests = do
  tt0 <- pactApiTest
  tt1 <- pactEmptyBlockTest
  return $ testGroup "PactExecutionTest" (tt0 ++ [tt1])

pactApiTest :: IO [TestTree]
pactApiTest = do
    let logger = genericLogger Warn T.putStrLn
        cid = testChainId 0

    mv <- newEmptyMVar
    -- Init for tests
    withPactService' Testnet00 cid logger testMemPoolAccess mv $ \reqQ -> do
        let headers = V.fromList $ getBlockHeaders 2

        -- newBlock test
        let genesisHeader = genesisBlockHeader Testnet00 cid
        respVar0 <- newBlock noMiner genesisHeader reqQ
        mvr <- takeMVar respVar0 -- wait for response
        plwo <- case mvr of
          Left e -> assertFailure (show e)
          Right r -> return r

        tt0 <- checkNewResponse "new-block-expected-0" plwo

        -- validate the same transactions sent to newBlock above
        let matchingPlHash = _payloadWithOutputsPayloadHash plwo
        let plData = PayloadData
              { _payloadDataTransactions = fst <$> _payloadWithOutputsTransactions plwo
              , _payloadDataMiner = _payloadWithOutputsMiner plwo
              , _payloadDataPayloadHash = matchingPlHash
              , _payloadDataTransactionsHash = _payloadWithOutputsTransactionsHash plwo
              , _payloadDataOutputsHash = _payloadWithOutputsOutputsHash plwo
              }
        let toValidateHeader =
              (headers ! 1) { _blockPayloadHash = matchingPlHash, _blockParent = _blockHash genesisHeader }
        respVar0b <- validateBlock toValidateHeader plData reqQ
        rsp0b <- takeMVar respVar0b -- wait for response

        tt0b <- checkValidateResponse "validateBlock-expected-0" rsp0b

        return $ tt0 : [tt0b]

pactEmptyBlockTest :: IO TestTree
pactEmptyBlockTest = do
    let logger = genericLogger Warn T.putStrLn
        cid = testChainId 0

    mv <- newEmptyMVar

    withPactService' Testnet00 cid logger testEmptyMemPool mv $ \reqQ -> do
        let genesisHeader = genesisBlockHeader Testnet00 cid
        respVar0 <- newBlock noMiner genesisHeader reqQ
        mvr <- takeMVar respVar0 -- wait for response
        plwo <- case mvr of
          Left e -> assertFailure (show e)
          Right r -> return r
        tt0 <- checkNewResponse "new-empty-expected-0" plwo
        return tt0

checkNewResponse :: FilePath -> PayloadWithOutputs -> IO TestTree
checkNewResponse filePrefix plwo = checkPayloadWithOutputs filePrefix "newBlock" plwo

checkValidateResponse :: FilePath -> Either PactException PayloadWithOutputs -> IO TestTree
checkValidateResponse filePrefix (Left s) = assertFailure $ filePrefix ++ ": " ++ show s
checkValidateResponse filePrefix (Right plwo) =
    checkPayloadWithOutputs filePrefix "validateBlock" plwo

checkPayloadWithOutputs :: FilePath -> String -> PayloadWithOutputs-> IO TestTree
checkPayloadWithOutputs filePrefix groupName plwo = do
    ttTrans <- checkTransactions filePrefix
               (fst <$> _payloadWithOutputsTransactions plwo)
    ttTransOut <- checkTransOut filePrefix
                  (snd <$> _payloadWithOutputsTransactions plwo)
    ttBlockPlHash <- checkBlockPayloadHash filePrefix (_payloadWithOutputsPayloadHash plwo)
    ttBlockTransHash <- checkBlockTransHash filePrefix (_payloadWithOutputsTransactionsHash plwo)
    ttBlockOutsHash <- checkBlockOutsHash filePrefix (_payloadWithOutputsOutputsHash plwo)
    return $ testGroup groupName
        (ttTrans : [ttTransOut, ttBlockPlHash, ttBlockTransHash, ttBlockOutsHash])

checkTransactions :: FilePath -> Seq Transaction -> IO TestTree
checkTransactions filePrefix trans = do
    let fp = filePrefix ++ "-trans.txt"
    let ioBsTrans = return $ foldMap (toS . _transactionBytes) trans
    return $ goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) ioBsTrans

checkTransOut :: FilePath -> Seq TransactionOutput -> IO TestTree
checkTransOut filePrefix transOuts = do
    let fp = filePrefix ++ "-transOuts.txt"
    let ioTransOuts = return $ foldMap (toS . _transactionOutputBytes) transOuts
    return $ goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) ioTransOuts

checkBlockPayloadHash :: FilePath -> BlockPayloadHash -> IO TestTree
checkBlockPayloadHash filePrefix bPayHash = do
   let fp = filePrefix ++ "-blockPayHash.txt"
   return $ goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) ioBs
   where
       ioBs = return $ A.encode bPayHash

checkBlockTransHash :: FilePath -> BlockTransactionsHash -> IO TestTree
checkBlockTransHash filePrefix bTransHash = do
   let fp = filePrefix ++ "-blockTransHash.txt"
   return $ goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) ioBs
   where
       ioBs = return $ A.encode bTransHash

checkBlockOutsHash :: FilePath -> BlockOutputsHash -> IO TestTree
checkBlockOutsHash filePrefix bOutsHash = do
   let fp2 = filePrefix ++ "-blockOuts-hash.txt"
   let ioBsOutsHash = return $ A.encode bOutsHash
   return $ goldenVsString (takeBaseName fp2) (testPactFilesDir ++ fp2) ioBsOutsHash

checkBlockTransactions :: FilePath -> BlockTransactions -> IO TestTree
checkBlockTransactions filePrefix bTrans = do
    let fp = filePrefix ++ "-blockTrans.txt"
    let ioBsTrans = return $ foldMap (toS . _transactionBytes) (_blockTransactions bTrans)
    let ttTrans = goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) ioBsTrans

    let fp2 = filePrefix ++ "-blockTrans-hash.txt"
    let ioBsHash = return $ toS $ A.encode $ _blockTransactionsHash bTrans
    let ttTransHash = goldenVsString (takeBaseName fp2) (testPactFilesDir ++ fp2) ioBsHash

    return $ testGroup "BlockTransactions" $ ttTrans : [ttTransHash]

getBlockHeaders :: Int -> [BlockHeader]
getBlockHeaders n = do
    let gbh0 = genesis
    let after0s = take (n - 1) $ testBlockHeaders gbh0
    gbh0 : after0s

testMemPoolAccess :: MemPoolAccess
testMemPoolAccess _bHeight _bHash = do
    moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
    let cmdStrs = V.fromList
          [ moduleStr
          , "(create-table test1.accounts)"
          , "(test1.create-global-accounts)"
          , "(test1.transfer \"Acct1\" \"Acct2\" 1.00)" ]
    mkPactTestTransactions cmdStrs

testEmptyMemPool :: MemPoolAccess
testEmptyMemPool _bHeight _bHash = mkPactTestTransactions V.empty

cmdBlocks :: Vector (Vector String)
cmdBlocks =  V.fromList [ V.fromList
                              [ "(test1.transfer \"Acct1\" \"Acct2\" 5.00)"
                              , "(test1.transfer \"Acct1\" \"Acct2\" 6.00)" ]
                        , V.fromList
                              [ "(test1.transfer \"Acct1\" \"Acct2\" 10.00)"
                              , "(test1.transfer \"Acct1\" \"Acct2\" 11.00)" ]
                        ]
