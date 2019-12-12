module Chainweb.Pact.Transactions.UpgradeTransactions
  ( upgradeTransactions
  ) where

import Chainweb.Version
import Chainweb.Transaction

import qualified Chainweb.Pact.Transactions.Mainnet0Transactions as MN0
import qualified Chainweb.Pact.Transactions.DevelopmentTransactions as Devnet
import qualified Chainweb.Pact.Transactions.OtherTransactions as Other

upgradeTransactions :: ChainwebVersion -> ChainId -> IO [ChainwebTransaction]
upgradeTransactions Mainnet01 cid = case cidInt of
  0 -> MN0.transactions
  _ -> Other.transactions
  where cidInt :: Int
        cidInt = chainIdInt cid
upgradeTransactions Development _ = Devnet.transactions
upgradeTransactions _ _ = Other.transactions
