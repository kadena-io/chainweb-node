{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Chainweb.Pact.Transactions.UpgradeTransactions
( upgradeTransactions
, twentyChainUpgradeTransactions
) where

import Chainweb.Version
import Chainweb.Transaction
import Chainweb.Pact.Service.Types
import Chainweb.Utils

import qualified Chainweb.Pact.Transactions.Mainnet0Transactions as MN0
import qualified Chainweb.Pact.Transactions.Mainnet1Transactions as MN1
import qualified Chainweb.Pact.Transactions.Mainnet2Transactions as MN2
import qualified Chainweb.Pact.Transactions.Mainnet3Transactions as MN3
import qualified Chainweb.Pact.Transactions.Mainnet4Transactions as MN4
import qualified Chainweb.Pact.Transactions.Mainnet5Transactions as MN5
import qualified Chainweb.Pact.Transactions.Mainnet6Transactions as MN6
import qualified Chainweb.Pact.Transactions.Mainnet7Transactions as MN7
import qualified Chainweb.Pact.Transactions.Mainnet8Transactions as MN8
import qualified Chainweb.Pact.Transactions.Mainnet9Transactions as MN9
import qualified Chainweb.Pact.Transactions.MainnetKADTransactions as MNKAD
import qualified Chainweb.Pact.Transactions.DevelopmentTransactions as Devnet
import qualified Chainweb.Pact.Transactions.OtherTransactions as Other

upgradeTransactions :: ChainwebVersion -> ChainId -> IO [ChainwebTransaction]
upgradeTransactions Mainnet01 cid = case cidInt of
  0 -> MN0.transactions
  1 -> MN1.transactions
  2 -> MN2.transactions
  3 -> MN3.transactions
  4 -> MN4.transactions
  5 -> MN5.transactions
  6 -> MN6.transactions
  7 -> MN7.transactions
  8 -> MN8.transactions
  9 -> MN9.transactions
  c | c >= 10, c <= 19 -> return []
  c -> internalError $ "Invalid mainnet chain id: " <> sshow c
  where cidInt :: Int
        cidInt = chainIdInt cid
upgradeTransactions Development cid = case chainIdInt @Int cid of
  c | c >= 0, c <= 9 -> Devnet.transactions
  c | c >= 10, c <= 19 -> return []
  c -> internalError $ "Invalid devnet chain id: "  <> sshow c
upgradeTransactions _ _ = Other.transactions

twentyChainUpgradeTransactions :: ChainwebVersion -> ChainId -> IO [ChainwebTransaction]
twentyChainUpgradeTransactions Mainnet01 cid = case chainIdInt @Int cid of
  0 -> MNKAD.transactions
  c | c >= 1, c <= 19 -> return []
  c -> internalError $ "Invalid mainnet chain id: " <> sshow c
twentyChainUpgradeTransactions Development cid = case chainIdInt @Int cid of
  0 -> MNKAD.transactions -- just remeds
  c | c >= 1, c <= 19 -> return []
  c -> internalError $ "Invalid devnet chain id: " <> sshow c
twentyChainUpgradeTransactions _ _ = return []
