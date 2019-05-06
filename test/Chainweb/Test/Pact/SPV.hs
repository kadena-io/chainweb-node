{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Chainweb.Graph
import Chainweb.Test.CutDB
import Chainweb.Test.Pact.Utils
import Chainweb.Transaction
import Chainweb.Version

import Data.Aeson ((.=), object)
import Data.CAS.RocksDB
import Data.LogMessage
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector as Vector

import System.LogLevel

test :: IO ()
test = do
    -- Pact service that is used to initialize the cut data base
    pact0 <- testWebPactExecutionService v txGenerator
    withTempRocksDb "chainweb-sbv-tests"  $ \rdb ->
        withTestCutDb rdb v 20 pact0 logg $ \cutDb -> do

            -- pact service, that is used to extend the cut data base
            pact <- testWebPactExecutionService v txGenerator
            syncPact cutDb pact
            extendTestCutDb cutDb pact 20
  where
    v = TimedCPM petersonChainGraph
    txGenerator _cid _bockHeight _blockHash = return mempty {- Vector of ChainwebTransaction -}
    logg l
        | l <= Warn = T.putStrLn . logText
        | otherwise = const $ return ()

spvTransactions :: IO (Vector ChainwebTransaction)
spvTransactions =
    mkPactTestTransactions' $ Vector.fromList txs
  where
    txs =
      [ PactTransaction tx1Code tx1Data]

    tx1Code =
      "(coin.delete-coin \"Acct1\" 2 \"Acct2\" (read-keyset 'acc2-keys) 1.0)"
    tx1Data = Just $ object
      [ ("acc2-keys"::Text) .=  ("368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"::Text)
      ]

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
