{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
module Chainweb.Simulate.Contracts.SimplePayments where

import Data.Aeson
import Data.Decimal
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T

import Formatting hiding (text)
import Fake

import GHC.Generics hiding (from, to)

import NeatInterpolation

import System.Random

-- PACT

import Pact.ApiReq (mkExec)
import Pact.Types.Command (Command(..))
import Pact.Types.Crypto (SomeKeyPair, defaultScheme, genKeyPair)

-- CHAINWEB

import Chainweb.Simulate.Utils

simplePaymentsContractLoader :: [SomeKeyPair] -> IO (Command Text)
simplePaymentsContractLoader adminKeyset = do
    let theData = object ["admin-keyset" .= fmap formatB16PubKey adminKeyset]
    mkExec (T.unpack theCode) theData def adminKeyset Nothing
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

createAccount :: Text -> IO ([SomeKeyPair], Command Text)
createAccount name = do
    adminKeyset <- testSomeKeyPairs
    nameKeyset <- return <$> genKeyPair defaultScheme
    let theData = object [T.append name "-keyset" .= fmap formatB16PubKey nameKeyset]
    res <- mkExec (T.unpack theCode) theData def adminKeyset Nothing
    return (nameKeyset, res)
  where
    theCode = sformat ("(payments.create-account \"" % stext % "\" " % float % " (read-keyset \"" % stext % "-keyset\"))") (T.toTitle name) (1000000.0 :: Decimal) name

createAccounts :: IO [([SomeKeyPair], Command Text)]
createAccounts = traverse createAccount (T.words names)

names :: Text
names = "mary elizabeth patricia jennifer linda barbara margaret susan dorothy jessica james john robert michael william david richard joseph charles thomas"

newtype Account = Account
  { getAccount :: Text
  } deriving (Eq, Show, Generic)

accountNames :: [Account]
accountNames = map (Account . T.toTitle) (T.words names)

instance Fake Account where
  fake = elements accountNames

newtype Amount = Amount
  { getAmount :: Decimal
  } deriving (Eq, Show, Generic)

instance Fake Amount where
  fake =
    Amount <$>
    (realFracToDecimal <$> fromRange (0, 10) <*>
     (fromRange (lowerLimit, upperLimit) :: FGen Double))
    where
      lowerLimit = 0
      upperLimit = 1000

newtype Balance = Balance
  { getBalance :: Decimal
  } deriving (Eq, Show, Generic)

instance Fake Balance where
  fake = Balance . fromIntegral <$> fromRange (0, 100000 :: Integer)

distinctPair :: (Fake a, Eq a) => FGen (a,a)
distinctPair = fake >>= \a -> (,) a <$> suchThat fake (/= a)

mkRandomSimplePaymentRequest :: [(Account, [SomeKeyPair])] -> IO (FGen SimplePaymentRequest)
mkRandomSimplePaymentRequest kacts = do
  request <- randomRIO (0, 1 :: Int)
  case request of
    0 -> return $ RequestGetBalance <$> fake
    1 -> return $ do
        (from, to) <- distinctPair
        amount <- fake
        return $ RequestPay from to amount
    -- Lol, this might be used later. For now, this constructor will
    -- not be exercised.
    2 -> return $ do
           acct <- fake
           bal <- fake
           case lookup acct kacts of
            Nothing -> fail (errmsg ++ T.unpack (getAccount acct) ++ " " ++ show (fst <$> kacts))
            Just keyset -> return $ CreateAccount acct bal keyset
    _ -> error "mkRandomSimplePaymentRequest: error in case statement."
  where
    errmsg =
           "mkRandomSimplePaymentRequest: something went wrong."
           ++ " Cannot find account name"


data SimplePaymentRequest
  = RequestGetBalance Account
  | RequestPay Account Account Amount
  | CreateAccount Account Balance [SomeKeyPair]

parens :: String -> String
parens s = "(" ++ s ++ ")"

instance Show SimplePaymentRequest where
  show (RequestGetBalance account) = "RequestGetBalance: " ++ parens (show account)
  show (RequestPay accountA accountB amount) =
    "RequestPay: " ++ parens (show accountA) ++ " " ++ parens (show accountB) ++ " " ++ parens (show amount)
  show (CreateAccount account balance _) = "CreateAccount: " ++ parens (show account) ++ " " ++ parens (show balance)

createSimplePaymentRequest :: SimplePaymentRequest -> Maybe [SomeKeyPair] -> IO (Command Text)
createSimplePaymentRequest (CreateAccount (Account account) (Balance initialBalance) somekeyset) Nothing = do
  adminKeyset <- testSomeKeyPairs
  let theData = object ["keyset" .= fmap formatB16PubKey somekeyset ,"admin-keyset" .= fmap formatB16PubKey adminKeyset]
      theCode = sformat ("(payments.create-account " % stext % " " % float % ")") account initialBalance
  mkExec (T.unpack theCode) theData def somekeyset Nothing

createSimplePaymentRequest (RequestGetBalance (Account account)) Nothing = do
  adminKeyset <- testSomeKeyPairs
  let theCode = sformat ("(payments.get-balance " % stext % ")") account
  mkExec (T.unpack theCode) Null def adminKeyset Nothing

createSimplePaymentRequest (RequestPay (Account from) (Account to) (Amount amount)) (Just keyset) = do
  let theCode = sformat ("(payments.pay " % stext % " " % stext % " " % float % ")") from to amount
  mkExec (T.unpack theCode) Null def keyset Nothing
createSimplePaymentRequest _ _ = fail "This case should not be reached."
