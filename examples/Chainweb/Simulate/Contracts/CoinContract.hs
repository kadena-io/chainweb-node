{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

-- |
module Chainweb.Simulate.Contracts.CoinContract where

import Control.Monad hiding (guard)

import Data.Aeson
-- import Data.Char
-- import Data.Decimal
-- import Data.Maybe
import Data.Text (Text)
-- import Data.Map (Map (..))
-- import qualified Data.Map as M
-- import qualified Data.Text as T

import Fake

-- import GHC.Generics hiding (from, to)

import System.Random

import Text.Printf

-- PACT

import Pact.ApiReq (mkExec)
import Pact.Types.Command (Command(..))
import Pact.Types.Crypto (SomeKeyPair)
import Pact.Types.ChainMeta (PublicMeta(..))

-- CHAINWEB

-- import Chainweb.ChainId
import Chainweb.Simulate.Contracts.Common
import Chainweb.Simulate.Utils

data CoinContractRequest
  = CoinCreateAccount Account Guard
  | CoinAccountBalance Account
  | CoinTransfer SenderName ReceiverName Guard Amount

instance Show CoinContractRequest where
  show (CoinCreateAccount account _guard) = "CoinContract.CoinCreateAccount: " ++ parens (show account)
  show (CoinAccountBalance account) = "CoinContract.CoinAccountBalance: " ++ parens (show account)
  show (CoinTransfer sender receiver _guard amount) =
    "CoinContract.CoinTransfer: "
    ++ parens (show sender)
    ++ parens (show receiver)
    ++ parens (show amount)

type Guard = [SomeKeyPair]

-- for simplicity
type SenderName = Account
type ReceiverName = Account
type MinerName = Account
type Name = Account
newtype Address = Address String

errPrefix :: String
errPrefix = "CoinContract."

mkRandomCoinContractRequest :: [(Account,Maybe [SomeKeyPair])] -> IO (FGen CoinContractRequest)
mkRandomCoinContractRequest kacts = do
  request <- randomRIO (0, 1 :: Int)
  return $
    case request of
      0 -> CoinAccountBalance <$> fake
      1 -> do
        (from, to) <- distinctPair
        case join (lookup to kacts) of
          Nothing -> error (errmsg ++ getAccount to)
          Just keyset -> CoinTransfer from to keyset <$> fake
      _ -> error "mkRandomCoinContractRequest: impossible case"
  where
    errmsg =
      "mkRandomCoinContractRequest: something went wrong." ++
      " Cannot find account name"

createCoinContractRequest :: PublicMeta -> CoinContractRequest -> IO (Command Text)
createCoinContractRequest meta request =
  case request of
    CoinCreateAccount (Account account) guard -> do
      adminKeyset <- testSomeKeyPairs
      let theCode =
            printf
            "(coin.create-account \"%s\" (read-keyset \"%s\"))"
            account
            ("create-account-guard" :: String)
          theData =
            object
              [ "admin-keyset" .= fmap formatB16PubKey adminKeyset
              , "create-account-guard" .= fmap formatB16PubKey guard
              ]
      mkExec theCode theData meta adminKeyset Nothing
    CoinAccountBalance (Account account) -> do
      adminKeyset <- testSomeKeyPairs
      let theData = Null
          theCode =
            printf
            "(coin.account-balance \"%s\")"
            account
      mkExec theCode theData meta adminKeyset Nothing
    CoinTransfer (Account sendername) (Account receivername) guard (Amount amount) -> do
      adminKeyset <- testSomeKeyPairs
      let theCode =
            printf
            "(coin.transfer \"%s\" \"%s\" (read-keyset \"%s\") %s)"
            sendername
            receivername
            ("receiver-guard" :: String)
            (show amount)
          theData =
            object
              [ "admin-keyset" .= fmap formatB16PubKey adminKeyset
              , "receiver-guard" .= fmap formatB16PubKey guard
              ]
      mkExec theCode theData meta adminKeyset Nothing
  where
    _functionErrPrefix :: String
    _functionErrPrefix = "createCoinContractRequest:"

-----------------------------------
-- Code that may be resurrected! --
-----------------------------------
{-
    BuyGas (Account sender) (Amount amount) -> do
      adminKeyset <- testSomeKeyPairs
      let theCode =
            printf "(coin.buy-gas \"%s\" %s)" sender (show amount)
          theData = object ["admin-keyset" .= fmap formatB16PubKey adminKeyset]
      mkExec theCode theData meta adminKeyset Nothing
    -- guard should just be a keyset here.
    RedeemGas (Account minername) guard (Account name) amount -> do
      adminKeyset <- testSomeKeyPairs
      let theCode =
            printf
              "(coin.reedem-gas \"%s\" (read-keyset \"%s\") \"%s\" %s)"
              minername
              ("miner-guard" :: String)
              name
              (show amount)
          theData =
            object
              [ "admin-keyset" .= fmap formatB16PubKey adminKeyset
              , "miner-guard"  .= fmap formatB16PubKey guard
              ]
      mkExec theCode theData meta adminKeyset Nothing
-}

-----------------------------------
-- Code that may be resurrected! --
-----------------------------------
{-
    Coinbase (Address address) guard (Amount amount) -> do
      adminKeyset <- testSomeKeyPairs
      let theCode =
            printf
            "(coin.coinbase \"%s\" (read-keyset \"%s\") %s)"
            address
            ("adress-guard" :: String)
            (show amount)
          theData =
            object
              [ "admin-keyset" .= fmap formatB16PubKey adminKeyset
              , "address-guard" .= fmap formatB16PubKey guard
              ]
      mkExec theCode theData meta adminKeyset Nothing
    FundTx _sender _miner _guard _amount -> do
      undefined                 -- TODO: need to figure out what to do
                                -- with a defpact
    Debit _account _amount -> do
      undefined
    Credit _account _guard _amount -> do
      undefined
    DeleteCoin _accountA _chainid _accountB _guard _amount -> do
      undefined
    CreateCoin _proof -> do
      undefined


  -- BuyGas SenderName Amount
  -- --| RedeemGas MinerName Guard Name Amount
  -- --| Coinbase Address Guard Amount
  -- --| FundTx SenderName MinerName Guard Amount
  -- --| Debit Account Amount
  -- --| Credit Account Guard Amount
  -- --| DeleteCoin Account ChainId Account Guard Amount
  -- --| CreateCoin Proof

-- data Proof = Proof              -- dunno yet
-- data Guard = Guard              -- dunno yet

-}
