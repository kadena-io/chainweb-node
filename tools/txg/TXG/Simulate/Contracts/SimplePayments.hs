{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- |
module TXG.Simulate.Contracts.SimplePayments where

import Data.Aeson
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Fake

import NeatInterpolation

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

simplePaymentsContractLoader :: PublicMeta -> [SomeKeyPair] -> IO (Command Text)
simplePaymentsContractLoader meta adminKeyset = do
    let theData = object ["admin-keyset" .= fmap formatB16PubKey adminKeyset]
    mkExec (T.unpack theCode) theData meta adminKeyset Nothing
  where
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

(module payments 'admin-keyset

  (defschema payments
    balance:decimal
    keyset:keyset)

  (deftable payments-table:{payments})

  (defun create-account (id:string initial-balance:decimal keyset:keyset)
    "Create a new account for ID with INITIAL-BALANCE funds, must be administrator."
    (enforce-keyset 'admin-keyset)
    (enforce (>= initial-balance 0.0) "Initial balances must be >= 0.")
    (insert payments-table id
            { "balance": initial-balance,
              "keyset": keyset }))

  (defun get-balance (id:string)
    "Only users or admin can read balance."
    (with-read payments-table id
      { "balance":= balance, "keyset":= keyset }
      (enforce-one "Access denied"
        [(enforce-keyset keyset)
         (enforce-keyset 'admin-keyset)])
      balance))

  (defun pay (from:string to:string amount:decimal)
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

(create-table payments-table)
  |]

mkRandomSimplePaymentRequest
  :: M.Map Account [SomeKeyPair]
  -> IO (FGen SimplePaymentRequest)
mkRandomSimplePaymentRequest _ = do
  request <- randomRIO @Int (0, 1)
  pure $ case request of
    0 -> SPRequestGetBalance <$> fake
    1 -> do
        (from, to) <- distinctPair
        SPRequestPay from to <$> fake
    -- -- Lol, this might be used later. For now, this constructor will
    -- -- not be exercised.
    -- 2 -> return $ do
    --        acct <- fake
    --        bal <- fake
    --        case join (lookup acct kacts) of
    --         Nothing -> error (errmsg ++ (getAccount acct) ++ " " ++ show (fst <$> kacts))
    --         Just keyset -> return $ SPCreateAccount acct bal keyset
    _ -> error "mkRandomSimplePaymentRequest: error in case statement."
  -- where
    -- errmsg =
    --        "mkRandomSimplePaymentRequest: something went wrong."
    --        ++ " Cannot find account name"

data SimplePaymentRequest
  = SPRequestGetBalance Account
  | SPRequestPay Account Account Amount
  | SPCreateAccount Account Balance [SomeKeyPair]

createSimplePaymentRequest :: PublicMeta -> SimplePaymentRequest -> Maybe [SomeKeyPair] -> IO (Command Text)
createSimplePaymentRequest meta (SPCreateAccount (Account account) (Balance initialBalance) somekeyset) _ = do
  adminKeyset <- testSomeKeyPairs
  let theData = object ["keyset" .= fmap formatB16PubKey somekeyset ,"admin-keyset" .= fmap formatB16PubKey adminKeyset]
      theCode = printf "(payments.create-account \"%s\" %s)" account (show initialBalance)
  mkExec theCode theData meta somekeyset Nothing

createSimplePaymentRequest meta (SPRequestGetBalance (Account account)) _ = do
  adminKeyset <- testSomeKeyPairs
  let theCode = printf "(payments.get-balance \"%s\")" account
  mkExec theCode Null meta adminKeyset Nothing

createSimplePaymentRequest meta (SPRequestPay (Account from) (Account to) (Amount amount)) (Just keyset) = do
  let theCode = printf "(payments.pay \"%s\" \"%s\" %s)" from to (show amount)
  mkExec theCode Null meta keyset Nothing
createSimplePaymentRequest _ _ _ = error "createSimplePaymentRequest: impossible"
