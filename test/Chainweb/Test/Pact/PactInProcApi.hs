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

import qualified Data.Aeson as A (encode)
import Data.String.Conv (toS)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import System.FilePath
import System.IO.Extra

import Test.Tasty
import Test.Tasty.Golden

import Chainweb.BlockHeader
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Test.Pact.Utils


tests :: IO TestTree
tests = testGroup "Pact in-proc API tests" <$> pactApiTest

pactApiTest :: IO [TestTree]
pactApiTest = do

    -- Init for tests
    withPactService' testMemPoolAccess $ \reqQ -> do
        let headers = V.fromList $ getBlockHeaders 4

        -- newBlock test
        respVar0 <- newBlock (headers ! 0) reqQ
        rsp0 <- takeMVar respVar0 -- wait for response
        tt0 <- checkNewResponse "new-block-expected-0" rsp0

        -- validate the same transactions sent to newBlock above
        respVar0b <- validateBlock (headers ! 0) reqQ
        rsp0b <- takeMVar respVar0b -- wait for response
        tt0b <- checkValidateResponse "validateBlock-expected-0" rsp0b

        -- validate a different set of transactions (not sent to newBlock)
        respVar1 <- validateBlock (headers ! 1) reqQ
        rsp1 <- takeMVar respVar1 -- wait for response
        tt1 <- checkValidateResponse "validateBlock-expected-1" rsp1

        return $ tt0 : tt0b : [tt1]


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

testMemPoolAccess :: MemPoolAccess
testMemPoolAccess (BlockHeight 0) _bHash = do
    moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
    let cmdStrs = V.fromList
          [ moduleStr
          , "(create-table test1.accounts)"
          , "(test1.create-global-accounts)"
          , "(test1.transfer \"Acct1\" \"Acct2\" 1.00)" ]
    mkPactTestTransactions cmdStrs
testMemPoolAccess (BlockHeight n) _bHash = do
    let cmdStrs = cmdBlocks ! fromIntegral n
    mkPactTestTransactions cmdStrs

cmdBlocks :: Vector (Vector String)
cmdBlocks =  V.fromList [ V.fromList
                              [ "(test1.transfer \"Acct1\" \"Acct2\" 5.00)"
                              , "(test1.transfer \"Acct1\" \"Acct2\" 6.00)" ]
                        , V.fromList
                              [ "(test1.transfer \"Acct1\" \"Acct2\" 10.00)"
                              , "(test1.transfer \"Acct1\" \"Acct2\" 11.00)" ]
                        ]
