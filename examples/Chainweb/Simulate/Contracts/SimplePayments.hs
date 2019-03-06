{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Chainweb.Simulate.Contracts.SimplePayments where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Decimal
import qualified Data.Text as T
import Data.Text (Text)
-- import Data.Text.Encoding (decodeUtf8)
import Data.Default
import Data.Maybe

import Fake
-- import Fake.Provider.Person.EN_US (firstName)
-- import Fake.Provider.Lang (SingleWord(..))

import GHC.Generics hiding (from, to)

import NeatInterpolation

import System.Random

-- PACT
import Pact.ApiReq (KeyPair(..), mkExec)
import Pact.Types.Command (Command (..))
import Pact.Types.Crypto (PPKScheme(..), importPublic, importPrivate)

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


createAccounts :: IO [Command Text]
createAccounts =
  sequence
    [ createMaryAccount
    , createElizabethAccount
    , createPatriciaAccount
    , createJenniferAccount
    , createLindaAccount
    , createBarbaraAccount
    , createMargaretAccount
    , createSusanAccount
    , createDorothyAccount
    , createJessicaAccount
    , createJamesAccount
    , createJohnAccount
    , createRobertAccount
    , createMichaelAccount
    , createWilliamAccount
    , createDavidAccount
    , createRichardAccount
    , createJosephAccount
    , createCharlesAccount
    , createThomasAccount
    ]

newtype Account = Account
  { getAccount :: Text
  } deriving (Eq, Show, Generic)

accountNames :: [Account]
accountNames =
  map
    (\name -> Account $ T.pack name)
    (words
       "Mary Elizabeth Patricia Jennifer Linda Barbara Margaret Susan Dorothy Jessica James John Robert Michael William David Richard Joseph Charles Thomas")

instance Fake Account where
  fake = elements (accountNames)

-- instance Fake Account where
--   fake = do
--     name <- unSingleWord <$> firstName
--     return $ Account $ T.append name "Account"

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
  = RequestGetBalance Account
  | RequestPay Account
               Account
               Amount
  | CreateAccount Account
                  Balance
                  [ApiKeyPair]

getInitialBalance :: Balance -> Text
getInitialBalance = T.pack . show . getBalance

showAmount :: Amount -> Text
showAmount = T.pack . show . getAmount

-- createSimplePaymentRequest :: Nonce -> SimplePaymentRequest -> Command ByteString
createSimplePaymentRequest :: SimplePaymentRequest -> IO (Command Text)
createSimplePaymentRequest (CreateAccount (Account account) (getInitialBalance -> initialBalance) keyset) =
  mkExec (T.unpack theCode) theData def keyset Nothing
  where
    theCode = [text|(payments.create-account "$account" $initialBalance (read-keyset 'create-account-keyset))|]
    theData = object ["create-account-keyset" .= keyset]
createSimplePaymentRequest (RequestGetBalance (Account account)) =
  mkExec (T.unpack theCode) theData def [] Nothing
  where
    theCode = [text|(payments.get-balance "$account")|]
    theData = Null
createSimplePaymentRequest (RequestPay (Account from) (Account to) (showAmount -> amount)) =
  mkExec (T.unpack theCode) theData def [] Nothing
  where
    theCode = [text|(payments.pay "$from" "$to" $amount)|]
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

accountKeyPairs :: [KeyPair]
accountKeyPairs =
  catMaybes $ zipWith (\sec pub -> KeyPair <$> importPrivate sec <*> importPublic pub) accountPrivates accountPublics

-- preAccountKeyPairs :: [Maybe KeyPair]
preAccountKeyPairs = zipWith (\sec pub -> (,) <$> importPrivate sec <*> importPublic pub) accountPrivates accountPublics

accountPrivates :: [ByteString]
accountPrivates =
  [ "53109fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53110fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53111fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53112fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53113fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53114fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53115fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53116fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53117fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53118fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53119fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53120fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53121fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53122fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53123fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53124fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53125fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53126fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53127fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53128fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53129fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  , "53130fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  ]

accountPublics :: [ByteString]
accountPublics =
  [ "202a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "203a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "204a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "205a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "206a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "207a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "208a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "209a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "210a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "211a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "212a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "213a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "214a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "215a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "216a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "217a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "218a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "219a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "220a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "221a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "222a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  , "223a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  ]

maryKeyset :: [KeyPair]
maryKeyset = [accountKeyPairs !! 0]
createMaryAccount :: IO (Command Text)
createMaryAccount = mkExec (T.unpack theCode) theData def maryKeyset Nothing
  where
    theCode = [text|(define-keyset 'mary-keyset (read-keyset 'mary-keyset))
(payments.create-account "Mary" 1000000 (read-keyset 'mary-keyset))|]
    theData = object ["mary-keyset" .= maryKeyset]
elizabethKeyset :: [KeyPair]
elizabethKeyset = [accountKeyPairs !! 1]
createElizabethAccount :: IO (Command Text)
createElizabethAccount = mkExec (T.unpack theCode) theData def elizabethKeyset Nothing
  where
    theCode = [text|(define-keyset 'elizabeth-keyset (read-keyset 'elizabeth-keyset))
(payments.create-account "Elizabeth" 1000000 (read-keyset 'elizabeth-keyset))|]
    theData = object ["elizabeth-keyset" .= elizabethKeyset]
patriciaKeyset :: [KeyPair]
patriciaKeyset = [accountKeyPairs !! 2]
createPatriciaAccount :: IO (Command Text)
createPatriciaAccount = mkExec (T.unpack theCode) theData def patriciaKeyset Nothing
  where
    theCode = [text|(define-keyset 'patricia-keyset (read-keyset 'patricia-keyset))
(payments.create-account "Patricia" 1000000 (read-keyset 'patricia-keyset))|]
    theData = object ["patricia-keyset" .= patriciaKeyset]
jenniferKeyset :: [KeyPair]
jenniferKeyset = [accountKeyPairs !! 3]
createJenniferAccount :: IO (Command Text)
createJenniferAccount = mkExec (T.unpack theCode) theData def jenniferKeyset Nothing
  where
    theCode = [text|(define-keyset 'jennifer-keyset (read-keyset 'jennifer-keyset))
(payments.create-account "Jennifer" 1000000 (read-keyset 'jennifer-keyset))|]
    theData = object ["jennifer-keyset" .= jenniferKeyset]
lindaKeyset :: [KeyPair]
lindaKeyset = [accountKeyPairs !! 4]
createLindaAccount :: IO (Command Text)
createLindaAccount = mkExec (T.unpack theCode) theData def lindaKeyset Nothing
  where
    theCode = [text|(define-keyset 'linda-keyset (read-keyset 'linda-keyset))
(payments.create-account "Linda" 1000000 (read-keyset 'linda-keyset))|]
    theData = object ["linda-keyset" .= lindaKeyset]
barbaraKeyset :: [KeyPair]
barbaraKeyset = [accountKeyPairs !! 5]
createBarbaraAccount :: IO (Command Text)
createBarbaraAccount = mkExec (T.unpack theCode) theData def barbaraKeyset Nothing
  where
    theCode = [text|(define-keyset 'barbara-keyset (read-keyset 'barbara-keyset))
(payments.create-account "Barbara" 1000000 (read-keyset 'barbara-keyset))|]
    theData = object ["barbara-keyset" .= barbaraKeyset]
margaretKeyset :: [KeyPair]
margaretKeyset = [accountKeyPairs !! 6]
createMargaretAccount :: IO (Command Text)
createMargaretAccount = mkExec (T.unpack theCode) theData def margaretKeyset Nothing
  where
    theCode = [text|(define-keyset 'margaret-keyset (read-keyset 'margaret-keyset))
(payments.create-account "Margaret" 1000000 (read-keyset 'margaret-keyset))|]
    theData = object ["margaret-keyset" .= margaretKeyset]
susanKeyset :: [KeyPair]
susanKeyset = [accountKeyPairs !! 7]
createSusanAccount :: IO (Command Text)
createSusanAccount = mkExec (T.unpack theCode) theData def susanKeyset Nothing
  where
    theCode = [text|(define-keyset 'susan-keyset (read-keyset 'susan-keyset))
(payments.create-account "Susan" 1000000 (read-keyset 'susan-keyset))|]
    theData = object ["susan-keyset" .= susanKeyset]
dorothyKeyset :: [KeyPair]
dorothyKeyset = [accountKeyPairs !! 8]
createDorothyAccount :: IO (Command Text)
createDorothyAccount = mkExec (T.unpack theCode) theData def dorothyKeyset Nothing
  where
    theCode = [text|(define-keyset 'dorothy-keyset (read-keyset 'dorothy-keyset))
(payments.create-account "Dorothy" 1000000 (read-keyset 'dorothy-keyset))|]
    theData = object ["dorothy-keyset" .= dorothyKeyset]
jessicaKeyset :: [KeyPair]
jessicaKeyset = [accountKeyPairs !! 9]
createJessicaAccount :: IO (Command Text)
createJessicaAccount = mkExec (T.unpack theCode) theData def jessicaKeyset Nothing
  where
    theCode = [text|(define-keyset 'jessica-keyset (read-keyset 'jessica-keyset))
(payments.create-account "Jessica" 1000000 (read-keyset 'jessica-keyset))|]
    theData = object ["jessica-keyset" .= jessicaKeyset]

jamesKeyset :: [KeyPair]
jamesKeyset = [accountKeyPairs !! 10]
createJamesAccount :: IO (Command Text)
createJamesAccount = mkExec (T.unpack theCode) theData def jamesKeyset Nothing
  where
    theCode = [text|(define-keyset 'james-keyset (read-keyset 'james-keyset))
(payments.create-account "James" 1000000 (read-keyset 'james-keyset))|]
    theData = object ["james-keyset" .= jamesKeyset]
johnKeyset :: [KeyPair]
johnKeyset = [accountKeyPairs !! 11]
createJohnAccount :: IO (Command Text)
createJohnAccount = mkExec (T.unpack theCode) theData def johnKeyset Nothing
  where
    theCode = [text|(define-keyset 'john-keyset (read-keyset 'john-keyset))
(payments.create-account "John" 1000000 (read-keyset 'john-keyset))|]
    theData = object ["john-keyset" .= johnKeyset]
robertKeyset :: [KeyPair]
robertKeyset = [accountKeyPairs !! 12]
createRobertAccount :: IO (Command Text)
createRobertAccount = mkExec (T.unpack theCode) theData def robertKeyset Nothing
  where
    theCode = [text|(define-keyset 'robert-keyset (read-keyset 'robert-keyset))
(payments.create-account "Robert" 1000000 (read-keyset 'robert-keyset))|]
    theData = object ["robert-keyset" .= robertKeyset]
michaelKeyset :: [KeyPair]
michaelKeyset = [accountKeyPairs !! 13]
createMichaelAccount :: IO (Command Text)
createMichaelAccount = mkExec (T.unpack theCode) theData def michaelKeyset Nothing
  where
    theCode = [text|(define-keyset 'michael-keyset (read-keyset 'michael-keyset))
(payments.create-account "Michael" 1000000 (read-keyset 'michael-keyset))|]
    theData = object ["michael-keyset" .= michaelKeyset]
williamKeyset :: [KeyPair]
williamKeyset = [accountKeyPairs !! 14]
createWilliamAccount :: IO (Command Text)
createWilliamAccount = mkExec (T.unpack theCode) theData def williamKeyset Nothing
  where
    theCode = [text|(define-keyset 'william-keyset (read-keyset 'william-keyset))
(payments.create-account "William" 1000000 (read-keyset 'william-keyset))|]
    theData = object ["william-keyset" .= williamKeyset]
davidKeyset :: [KeyPair]
davidKeyset = [accountKeyPairs !! 15]
createDavidAccount :: IO (Command Text)
createDavidAccount = mkExec (T.unpack theCode) theData def davidKeyset Nothing
  where
    theCode = [text|(define-keyset 'david-keyset (read-keyset 'david-keyset))
(payments.create-account "David" 1000000 (read-keyset 'david-keyset))|]
    theData = object ["david-keyset" .= davidKeyset]
richardKeyset :: [KeyPair]
richardKeyset = [accountKeyPairs !! 16]
createRichardAccount :: IO (Command Text)
createRichardAccount = mkExec (T.unpack theCode) theData def richardKeyset Nothing
  where
    theCode = [text|(define-keyset 'richard-keyset (read-keyset 'richard-keyset))
(payments.create-account "Richard" 1000000 (read-keyset 'richard-keyset))|]
    theData = object ["richard-keyset" .= richardKeyset]
josephKeyset :: [KeyPair]
josephKeyset = [accountKeyPairs !! 17]
createJosephAccount :: IO (Command Text)
createJosephAccount = mkExec (T.unpack theCode) theData def josephKeyset Nothing
  where
    theCode = [text|(define-keyset 'joseph-keyset (read-keyset 'joseph-keyset))
(payments.create-account "Joseph" 1000000 (read-keyset 'joseph-keyset))|]
    theData = object ["joseph-keyset" .= josephKeyset]
charlesKeyset :: [KeyPair]
charlesKeyset = [accountKeyPairs !! 18]
createCharlesAccount :: IO (Command Text)
createCharlesAccount = mkExec (T.unpack theCode) theData def charlesKeyset Nothing
  where
    theCode = [text|(define-keyset 'charles-keyset (read-keyset 'charles-keyset))
(payments.create-account "Charles" 1000000 (read-keyset 'charles-keyset))|]
    theData = object ["charles-keyset" .= charlesKeyset]
thomasKeyset :: [KeyPair]
thomasKeyset = [accountKeyPairs !! 19]
createThomasAccount :: IO (Command Text)
createThomasAccount = mkExec (T.unpack theCode) theData def thomasKeyset Nothing
  where
    theCode = [text|(define-keyset 'thomas-keyset (read-keyset 'thomas-keyset))
(payments.create-account "Thomas" 1000000 (read-keyset 'thomas-keyset))|]
    theData = object ["thomas-keyset" .= thomasKeyset]
