{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
module TXG.Simulate.Contracts.CoinContract where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import Data.Text (Text)

import Fake

import System.Random

import Text.Printf

-- PACT
import Pact.ApiReq (mkExec)
import Pact.Types.ChainMeta (PublicMeta(..))
import Pact.Types.Command (Command(..))
import Pact.Types.Crypto (SomeKeyPair)

-- CHAINWEB
import TXG.Simulate.Contracts.Common
import TXG.Simulate.Utils

---

data CoinContractRequest
  = CoinCreateAccount Account Guard
  | CoinAccountBalance Account
  | CoinTransfer SenderName ReceiverName Guard Amount

newtype Guard = Guard (NonEmpty SomeKeyPair)
newtype SenderName = SenderName Account
newtype ReceiverName = ReceiverName Account

mkRandomCoinContractRequest
  :: M.Map Account (NonEmpty SomeKeyPair)
  -> IO (FGen CoinContractRequest)
mkRandomCoinContractRequest kacts = do
  request <- randomRIO @Int (0, 1)
  pure $ case request of
    0 -> CoinAccountBalance <$> fake
    1 -> do
      (from, to) <- distinctPair
      case M.lookup to kacts of
        Nothing ->
          error $ errmsg ++ getAccount to
        Just keyset ->
          CoinTransfer (SenderName from) (ReceiverName to) (Guard keyset) <$> fake
    _ -> error "mkRandomCoinContractRequest: impossible case"
  where
    errmsg =
      "mkRandomCoinContractRequest: something went wrong." ++
      " Cannot find account name"

createCoinContractRequest :: PublicMeta -> CoinContractRequest -> IO (Command Text)
createCoinContractRequest meta request =
  case request of
    CoinCreateAccount (Account account) (Guard guard) -> do
      adminKS <- testSomeKeyPairs
      let theCode =
            printf
            "(coin.create-account \"%s\" (read-keyset \"%s\"))"
            account
            ("create-account-guard" :: String)
          theData =
            object
              [ "admin-keyset" .= fmap formatB16PubKey adminKS
              , "create-account-guard" .= fmap formatB16PubKey guard
              ]
      mkExec theCode theData meta (NEL.toList adminKS) Nothing
    CoinAccountBalance (Account account) -> do
      adminKS <- testSomeKeyPairs
      let theData = Null
          theCode =
            printf
            "(coin.account-balance \"%s\")"
            account
      mkExec theCode theData meta (NEL.toList adminKS) Nothing
    CoinTransfer (SenderName (Account sn)) (ReceiverName (Account rn)) (Guard guard) (Amount amount) -> do
      adminKS <- testSomeKeyPairs
      let theCode =
            printf
            "(coin.transfer \"%s\" \"%s\" (read-keyset \"%s\") %s)"
            sn
            rn
            ("receiver-guard" :: String)
            (show amount)
          theData =
            object
              [ "admin-keyset" .= fmap formatB16PubKey adminKS
              , "receiver-guard" .= fmap formatB16PubKey guard
              ]
      mkExec theCode theData meta (NEL.toList adminKS) Nothing
