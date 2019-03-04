{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
module Chainweb.Simulate.Contracts.SimplePayments where

import Control.Monad
import Control.Monad.Zip

import Data.ByteString (ByteString)
import Data.Decimal
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)

import Fake

import GHC.Generics hiding (from, to)

import NeatInterpolation

import System.Random

import Text.Printf

-- pact
import Pact.ApiReq

-- import Pact.Types.Command
import Pact.Types.Crypto

-- import Pact.Types.RPC
simplePaymentsContract :: Text -> Text
simplePaymentsContract adminKeyset =
  [text| ;; Simple accounts model.
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
(define-keyset '$adminKeyset (read-keyset "$adminKeyset"))

;define smart-contract code
(module payments '$adminKeyset

  (defschema payments
    balance:decimal
    keyset:keyset)

  (deftable payments-table:{payments})

  (defun create-account (id initial-balance keyset)
    "Create a new account for ID with INITIAL-BALANCE funds, must be administrator."
    (enforce-keyset '$adminKeyset)
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
         (enforce-keyset '$adminKeyset)])
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

instance Fake Account where
  fake = do
    cap <- fakeCapitalLetter
    rest <- replicateM 19 fakeLetter
    return $ Account $ T.pack $ cap : rest

newtype Amount = Amount
  { getAmount :: Decimal
  } deriving (Eq, Show, Generic)

instance Fake Amount where
  fake =
    Amount <$>
    (realFracToDecimal <$> fromRange (0, 50) <*>
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

createSimplePaymentRequest :: SimplePaymentRequest -> Text
createSimplePaymentRequest (CreateAccount actualIdentifier actualInitialBalance actualKeyset) =
  [text|(create-account $identifier $initialBalance $keyset) |]
  where
    identifier = getIdentifier actualIdentifier
    initialBalance = T.pack $ show $ getBalance actualInitialBalance
    -- TODO: -- integrating Command datatype from Pact should save
    -- this horror show.
    keyset =
      T.pack $
      unlines $
      map
        (\kp -> printf "") --s:%S p:%s" (show undefined) (show undefined))
        actualKeyset
createSimplePaymentRequest (RequestGetBalance actualIdentifier) =
  [text|(get-balance $identifier)|]
  where
    identifier = getIdentifier actualIdentifier
createSimplePaymentRequest (RequestPay actualFrom actualTo actualAmount) =
  [text|(pay $from $to $amount)|]
  where
    from = getAccount actualFrom
    to = getAccount actualTo
    amount = T.pack $ show $ getAmount actualAmount

-- testAdminPrivates :: ByteString
-- testAdminPrivates =
--   "53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d2"

-- testAdminPublics :: ByteString
-- testAdminPublics =
--   "201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c0dbc"

-- testPrivates :: [ByteString]
-- testPrivates =
--   [ "53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d3"
--   , "53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
--   ]

-- testPublics :: [ByteString]
-- testPublics =
--   [ "201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c1dbc"
--   , "201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
--   ]

-- testAdminKeyPairs :: [ApiKeyPair]
-- testAdminKeyPairs =
--   let mPair =
--         mzip (importPrivate testAdminPrivates) (importPublic testAdminPublics)
--       mKeyPair =
--         fmap (\(sec, pub) -> KeyPair {_kpSecret = sec, _kpPublic = pub}) mPair
--    in maybeToList mKeyPair

-- testKeyPairs :: [ApiKeyPair]
-- testKeyPairs =
--   concat $
--   zipWith
--     (\private public ->
--        maybeToList $
--        liftM2 KeyPair (importPrivate private) (importPublic public))
--     testPrivates
--     testPublics
