{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Chainweb.Simulate.Contracts.SimplePayments where

import Control.Monad

import Data.Aeson
-- import Data.ByteString (ByteString)
import Data.Decimal
import qualified Data.Text as T
import Data.Text (Text)
-- import Data.Text.Encoding (decodeUtf8)
import Data.Default

import Fake
import Fake.Provider.Person.EN_US (firstName)
import Fake.Provider.Lang (SingleWord(..))



import GHC.Generics hiding (from, to)

import NeatInterpolation

import System.Random

-- PACT
import Pact.ApiReq (KeyPair(..), mkExec)
import Pact.Types.Command (Command (..))

import Chainweb.Simulate.Utils

simplePaymentsContractLoader :: [KeyPair] -> IO (Command Text)
simplePaymentsContractLoader adminKeyset = mkExec (T.unpack theCode) theData def adminKeyset Nothing
  where
    theData = object ["admin-keyset" .= adminKeyset]
    theCode = [text| ;; Simple accounts model.
;;
;;---------------------------------
;;
;;  Create keysets named 'adminKeyset', 'sarahKeyset' and 'jamesKeyset' and
;;  add some keys to them for loading this contract.
;;
;;  Make sure the message is signed with those added keys as well.
;;
;;---------------------------------


;define keyset to guard module
(define-keyset 'admin-keyset (read-keyset 'admin-keyset))

;define smart-contract code
(module payments 'admin-keyset

  (defschema payments
    balance:decimal
    keyset:keyset)

  (deftable payments-table:{payments})

  (defun create-account (id initial-balance keyset)
    "Create a new account for ID with INITIAL-BALANCE funds, must be administrator."
    (enforce-keyset 'admin-keyset)
    (enforce (>= initial-balance 0.0) "Initial balances must be >= 0.")
    (insert payments-table id
            { "balance": initial-balance,
              "keyset": keyset }))

  (defun get-balance (id)
    "Only users or admin can read balance."
    (with-read payments-table id
      { "balance":= balance, "keyset":= keyset }
      (enforce-one "Access denied"
        [(enforce-keyset keyset)
         (enforce-keyset 'admin-keyset)])
      balance))

  (defun pay (from to amount)
    (with-read payments-table from { "balance":= from-bal, "keyset":= keyset }
      (enforce-keyset keyset)
      (with-read payments-table to { "balance":= to-bal }
        (enforce (> amount 0.0) "Negative Transaction Amount")
        (enforce (>= from-bal amount) "Insufficient Funds")
        (update payments-table from
                { "balance": (- from-bal amount) })
        (update payments-table to
                { "balance": (+ to-bal amount) })
        (format "{} paid {} {}" [from to amount])))))
|]

newtype Identifier = Identifier
  { getIdentifier :: Text
  } deriving (Eq, Show, Generic)

instance Fake Identifier where
  fake = do
    cap <- fakeCapitalLetter
    rest <- replicateM 19 fakeLetter
    return $ Identifier $ T.pack $ cap : rest

newtype Account = Account
  { getAccount :: Text
  } deriving (Eq, Show, Generic)

accountNames :: [Account]
accountNames =
  map
    (Account . T.pack)
    (words
       "Mary Elizabeth Patricia Jennifer Linda Barbara Margaret Susan Dorothy Jessica") ++
  map
    (Account . T.pack)
    (words
       "James John Robert Michael William David Richard Joseph Charles Thomas")


instance Fake Account where
  fake = do
    name <- unSingleWord <$> firstName
    return $ Account $ T.append name "Account"

newtype Amount = Amount
  { getAmount :: Decimal
  } deriving (Eq, Show, Generic)

instance Fake Amount where
  fake =
    Amount <$>
    (realFracToDecimal <$> fromRange (0, 20) <*>
     (fromRange (lowerLimit, upperLimit) :: FGen Double))
    where
      lowerLimit = 0
      upperLimit = 1000

newtype Balance = Balance
  { getBalance :: Integer
  } deriving (Eq, Show, Generic)

instance Fake Balance where
  fake = Balance <$> fromRange (0, 100000)

mkRandomSimplePaymentRequest ::  StdGen -> FGen SimplePaymentRequest
mkRandomSimplePaymentRequest gen = go
  where
    go =
      let (i, gen') = randomR (0, 2 :: Int) gen
       in case i of
            0 -> RequestGetBalance <$> fake
            1 -> RequestPay <$> fake <*> fake <*> fake
            2 ->
              CreateAccount <$> fake <*> fake <*>
              (let k =
                     fst $
                     randomR (0, max 0 (subtract 2 $ length {- testKeyPairs -} (undefined :: [ApiKeyPair]))) gen'
                in return $ take 1 $ drop k {- testKeyPairs -} (undefined :: [ApiKeyPair]))
            _ -> error "mkRandomSimplePaymentRequest: error in case statement."

data SimplePaymentRequest
  = RequestGetBalance Identifier
  | RequestPay Account
               Account
               Amount
  | CreateAccount Identifier
                  Balance
                  [ApiKeyPair]

getInitialBalance :: Balance -> Text
getInitialBalance = T.pack . show . getBalance

showAmount :: Amount -> Text
showAmount = T.pack . show . getAmount

-- createSimplePaymentRequest :: Nonce -> SimplePaymentRequest -> Command ByteString
createSimplePaymentRequest :: SimplePaymentRequest -> IO (Command Text)
createSimplePaymentRequest (CreateAccount (Identifier identifier) (getInitialBalance -> initialBalance) keyset) =
  mkExec (T.unpack theCode) theData def keyset Nothing
  where
    theCode = [text|(payments.create-account $identifier $initialBalance (read-keyset 'create-account-keyset))|]
    theData = object ["create-account-keyset" .= keyset]
createSimplePaymentRequest (RequestGetBalance (Identifier identifier)) =
  mkExec (T.unpack theCode) theData def [] Nothing
  where
    theCode = [text|(payments.get-balance $identifier)|]
    theData = Null
createSimplePaymentRequest (RequestPay (Account from) (Account to) (showAmount -> amount)) =
  mkExec (T.unpack theCode) theData def [] Nothing
  where

    theCode = [text|(payments.pay $from $to $amount)|]
    theData = Null

{- some example usage of the above contract

;define table
(create-table payments-table)

;create accounts
(create-account "Sarah" 100.25 (read-keyset "sarah-keyset"))
(create-account "James" 250.0 (read-keyset "james-keyset"))


;; do payment, simluating SARAH keyset.
(pay "Sarah" "James" 25.0)
(format "Sarah's balance is {}" [(get-balance "Sarah")])

;; read James' balance as JAMES
(format "James's balance is {}" [(get-balance "James")])

-}
