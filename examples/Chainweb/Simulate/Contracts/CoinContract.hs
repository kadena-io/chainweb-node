{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

-- |
module Chainweb.Simulate.Contracts.CoinContract where

import Control.Monad hiding (guard)

import Data.Aeson
import Data.Text (Text)

import Fake

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

type Guard = [SomeKeyPair]

-- for simplicity
type SenderName = Account
type ReceiverName = Account

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
