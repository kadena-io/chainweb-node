{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Module: Chainweb.Test.CutDB.Test
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Pact.SPV
( test
) where



import Control.Concurrent.MVar (newMVar)
import Control.Lens hiding ((.=))

import Data.Aeson ((.=), object)
import Data.Default (def)
import Data.LogMessage
import qualified Data.Text.IO as T
import Data.Vector as Vector

import NeatInterpolation (text)
import System.LogLevel

-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Pact.Types
import Chainweb.Test.CutDB
import Chainweb.Test.Pact.Utils
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService

import Data.CAS.RocksDB

-- internal pact modules

import Pact.Types.Term (KeySet(..), PublicKey(..), Name(..))


test :: IO ()
test = do
    -- Pact service that is used to initialize the cut data base
    pact0 <- testWebPactExecutionService v Nothing (return mempty)
    withTempRocksDb "chainweb-sbv-tests"  $ \rdb ->
        withTestCutDb rdb v 20 pact0 logg $ \cutDb -> do
            cdb <- newMVar cutDb
            -- pact service, that is used to extend the cut data base
            pact <- testWebPactExecutionService v (Just cdb) txGenerator
            syncPact cutDb pact

            c <- _cut cutDb
            let h = c ^?! ixg (someChainId c)
            p <- _webPactNewBlock pact defaultMiner h

            print p

            -- 1. We have the cut db so we can decide what we put in there.
            --

            -- 2. We will need another pact service to handle single proof consumption

  where
    v = TimedCPM petersonChainGraph
    logg l
        | l <= Warn = T.putStrLn . logText
        | otherwise = const $ return ()

-- | Generate burn/create Pact Service commands
txGenerator
    :: ChainId
    -> BlockHeight
    -> BlockHash
    -> IO (Vector ChainwebTransaction)
txGenerator cid _bhe _bha =
    mkPactTestTransactions' (Vector.fromList txs)
  where
    chain = chainIdToText cid
    txs = [ PactTransaction (tx1Code chain) tx1Data
       --   , PactTransaction tx2Code tx2Data
          ]
    -- standard admin keys
    keys =
      let k = KeySet
            [ PublicKey "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" ]
            ( Name "keys-all" def )
      in Just $ object [ "sender01-keys" .=  k]

    -- Burn coin on chain '$cid' and create on chain 2, creating SPV proof
    tx1Code c =
      [text|
        (coin.delete-coin 'sender00 $c 'sender01 (read-keyset 'sender01-keys) 1.0)
        |]
    tx1Data = keys

    -- tx2Code = [text| (verify-spv "TXOUT" { "foo" : 1 }) |]
    -- tx2Data = keys

{-
  (defun delete-coin (delete-account create-chain-id create-account create-account-guard quantity)
    (with-capability (TRANSFER)
      (debit delete-account quantity)
      { "create-chain-id": create-chain-id
      , "create-account": create-account
      , "create-account-guard": create-account-guard
      , "quantity": quantity
      , "delete-chain-id": (at "chain-id" (chain-data))
      , "delete-account": delete-account
      , "delete-tx-hash": (tx-hash)
      }))

  (defun create-coin (proof)
    (let ((outputs (at "outputs" (verify-spv "TXOUT" proof))))
      (enforce (= 1 (length outputs)) "only one tx in outputs")
      (bind (at 0 outputs)
        { "create-chain-id":= create-chain-id
        , "create-account" := create-account
        , "create-account-guard" := create-account-guard
        , "quantity" := quantity
        , "delete-tx-hash" := delete-tx-hash
        , "delete-chain-id" := delete-chain-id
        }
        (enforce (= (at "chain-id" (chain-data)) create-chain-id "enforce correct create chain ID"))
        (let ((create-id (format "%:%" [delete-tx-hash delete-chain-id])))
          (with-default-read create-id creates-table
            { "exists": false }
            { "exists":= exists }
            (enforce (not exists) (format "enforce unique usage of %" [create-id]))
            (insert creates-table create-id { "exists": true })
            (with-capability (TRANSFER)
              (credit create-account create-account-guard quantity)))
          )))
    )
-}
