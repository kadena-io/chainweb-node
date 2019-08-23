{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
module TXG.Simulate.Contracts.CoinContract where

import Data.Aeson
import Data.Bool
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
  | CoinTransferAndCreate SenderName ReceiverName Guard Amount
  | CoinTransfer SenderName ReceiverName Amount

newtype Guard = Guard (NonEmpty SomeKeyPair)
newtype SenderName = SenderName Account
newtype ReceiverName = ReceiverName Account

mkRandomCoinContractRequest
    :: Bool
    -> M.Map Account (NonEmpty SomeKeyPair)
    -> IO (FGen CoinContractRequest)
mkRandomCoinContractRequest transfersPred kacts = do
    request <- bool (randomRIO @Int (0, 1)) (return 0) transfersPred
    pure $ case request of
      0 -> CoinAccountBalance <$> fake
      1 -> do
          (from, to) <- distinctPair
          case M.lookup to kacts of
              Nothing -> error $ errmsg ++ getAccount to
              Just keyset -> CoinTransferAndCreate
                  (SenderName from)
                  (ReceiverName to)
                  (Guard keyset)
                  <$> fake
      _ -> error "mkRandomCoinContractRequest: impossible case"
    where
      errmsg =
        "mkRandomCoinContractRequest: something went wrong." ++
        " Cannot find account name"

type Keyset = NEL.NonEmpty SomeKeyPair

createCoinContractRequest :: PublicMeta -> Keyset -> CoinContractRequest -> IO (Command Text)
createCoinContractRequest meta ks request =
    case request of
      CoinCreateAccount (Account account) (Guard guard) -> do
        let theCode =
              printf
              "(coin.create-account \"%s\" (read-keyset \"%s\"))"
              account
              ("create-account-guard" :: String)
            theData =
              object
                [ "create-account-guard" .= fmap formatB16PubKey guard
                ]
        mkExec theCode theData meta (NEL.toList ks) Nothing
      CoinAccountBalance (Account account) -> do
        let theData = Null
            theCode =
              printf
              "(coin.account-balance \"%s\")"
              account
        mkExec theCode theData meta (NEL.toList ks) Nothing
      CoinTransferAndCreate (SenderName (Account sn)) (ReceiverName (Account rn)) (Guard guard) (Amount amount) -> do
        let theCode =
              printf
              "(coin.transfer-and-create \"%s\" \"%s\" (read-keyset \"%s\") %f)"
              sn
              rn
              ("receiver-guard" :: String)
              amount
            theData =
              object
                [ "receiver-guard" .= fmap formatB16PubKey guard
                ]
        mkExec theCode theData meta (NEL.toList ks) Nothing

      CoinTransfer (SenderName (Account sn)) (ReceiverName (Account rn)) (Amount amount) -> do
        let theCode =
              printf
              "(coin.transfer \"%s\" \"%s\" %f)"
              sn
              rn
              -- Super janky, but gets the job done for now
              amount
            theData = object []
        mkExec theCode theData meta (NEL.toList ks) Nothing
