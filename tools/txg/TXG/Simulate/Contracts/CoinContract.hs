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
import Pact.Types.ChainId
import Pact.Types.Command (Command(..), SomeKeyPairCaps)

-- CHAINWEB

import Chainweb.Utils
import Chainweb.Version

import TXG.Simulate.Contracts.Common
import TXG.Simulate.Utils

---

data CoinContractRequest
  = CoinCreateAccount Account Guard
  | CoinAccountBalance Account
  | CoinTransfer SenderName ReceiverName Amount
  | CoinTransferAndCreate SenderName ReceiverName Guard Amount
  deriving Show

newtype Guard = Guard (NonEmpty SomeKeyPairCaps)
newtype SenderName = SenderName Account
newtype ReceiverName = ReceiverName Account

instance Show Guard where
    show _ = "<guard>"

instance Show SenderName where
    show (SenderName account) = "sender: " ++ show account

instance Show ReceiverName where
    show (ReceiverName account) = "sender: " ++ show account


mkRandomCoinContractRequest
    :: Bool
    -> M.Map Account (NonEmpty SomeKeyPairCaps)
    -> IO (FGen CoinContractRequest)
mkRandomCoinContractRequest transfersPred kacts = do
    request <- bool (randomRIO @Int (0, 1)) (return 1) transfersPred
    pure $ case request of
      0 -> CoinAccountBalance <$> fake
      1 -> do
          (from, to) <- distinctPairSenders
          case M.lookup to kacts of
              Nothing -> error $ errmsg ++ getAccount to
              Just _keyset -> CoinTransfer
                  (SenderName from)
                  (ReceiverName to)
                  <$> fake
      _ -> error "mkRandomCoinContractRequest: impossible case"
    where
      errmsg =
        "mkRandomCoinContractRequest: something went wrong." ++
        " Cannot find account name: "


createCoinContractRequest
    :: ChainwebVersion
    -> PublicMeta
    -> NEL.NonEmpty SomeKeyPairCaps
    -> CoinContractRequest
    -> IO (Command Text)
createCoinContractRequest v meta ks request =
    case request of
      CoinCreateAccount (Account account) (Guard guard) -> do
        let theCode =
              printf
              "(coin.create-account \"%s\" (read-keyset \"%s\"))"
              account
              ("create-account-guard" :: String)
            theData =
              object
                [ "create-account-guard" .= fmap (formatB16PubKey . fst) guard
                ]
        mkExec theCode theData meta (NEL.toList ks) (Just $ NetworkId $ toText v) Nothing
      CoinAccountBalance (Account account) -> do
        let theData = Null
            theCode =
              printf
              "(coin.get-balance \"%s\")"
              account
        mkExec theCode theData meta (NEL.toList ks) (Just $ NetworkId $ toText v) Nothing
      CoinTransferAndCreate (SenderName (Account sn)) (ReceiverName (Account rn)) (Guard guard) (Amount amount) -> do
        let theCode =
              printf
              "(coin.transfer-create \"%s\" \"%s\" (read-keyset \"%s\") %f)"
              sn
              rn
              ("receiver-guard" :: String)
              (fromRational @Double $ toRational amount)
            theData =
              object
                [ "receiver-guard" .= fmap (formatB16PubKey . fst) guard
                ]
        mkExec theCode theData meta (NEL.toList ks) (Just $ NetworkId $ toText v) Nothing

      CoinTransfer (SenderName (Account sn)) (ReceiverName (Account rn)) (Amount amount) -> do
        let theCode =
              printf
              "(coin.transfer \"%s\" \"%s\" %f)"
              sn
              rn
              -- Super janky, but gets the job done for now
              (fromRational @Double $ toRational amount)
            theData = object []
        mkExec theCode theData meta (NEL.toList ks) (Just $ NetworkId $ toText v) Nothing
