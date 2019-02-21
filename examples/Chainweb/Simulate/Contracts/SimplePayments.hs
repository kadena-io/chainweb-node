{-# LANGUAGE QuasiQuotes #-}

-- |

module Chainweb.Simulate.Contracts.SimplePayments where

import Data.Text (Text)
import NeatInterpolation

simplePaymentsContract :: Text -> Text
simplePaymentsContract adminKeyset = [text| ;; Simple accounts model.
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

data Keyset = Keyset

data Identifier = Identifier

data InitialBalance = InitialBalance

data Account = Account

data Amount = Amount

data SimplePaymentsAction
  = CreateAccount Keyset Identifier InitialBalance Keyset
  | GetBalance Keyset Identifier
  | Pay Account Account Amount

createAccount :: SimplePaymentsAction -> Text
createAccount (CreateAccount actualAdminKeyset actualIdentifier actualInitialBalance actualKeyset) = [text|
(defun create-account ($identifier $initialBalance $keyset)
    "Create a new account for ID with INITIAL-BALANCE funds, must be administrator."
    (enforce-keyset '$adminKeyset)
    (enforce (>= $initialBalance 0.0) "Initial balances must be >= 0.")
    (insert payments-table $identifier
            { "balance": $initialBalance,
              "keyset": $keyset }))
|]
  where
    adminKeyset = undefined actualAdminKeyset
    identifier = undefined actualIdentifier
    initialBalance = undefined actualInitialBalance
    keyset = undefined actualKeyset
createAccount _ = error "Chainweb.Simulate.Contracts.SimplePayments.createAccount: case analysis failure. You can only pass a CreateAccount value!"

getBalance :: SimplePaymentsAction -> Text
getBalance (GetBalance actualAdminKeyset actualIdentifier) = [text|
(defun get-balance ($identifier)
    "Only users or admin can read balance."
    (with-read payments-table $identifier
      { "balance":= balance, "keyset":= keyset }
      (enforce-one "Access denied"
        [(enforce-keyset keyset)
         (enforce-keyset '$adminKeyset)])
      balance))
|]
  where
    adminKeyset = undefined actualAdminKeyset
    identifier = undefined actualIdentifier
getBalance _ = error "Chainweb.Simulate.Contracts.SimplePayments.getBalance: case analysis failure. You can only pass a GetBalance value!"

pay :: SimplePaymentsAction -> Text
pay (Pay actualFrom actualTo actualAmount) = [text|
(defun pay ($from $to $amount)
    (with-read payments-table $from { "balance":= from-bal, "keyset":= keyset }
      (enforce-keyset keyset)
      (with-read payments-table $to { "balance":= to-bal }
        (enforce (> $amount 0.0) "Negative Transaction Amount")
        (enforce (>= from-bal $amount) "Insufficient Funds")
        (update payments-table $from
                { "balance": (- from-bal $amount) })
        (update payments-table $to
                { "balance": (+ to-bal $amount) })
        (format "{} paid {} {}" [$from $to $amount]))))
|]
  where
    from = undefined actualFrom
    to = undefined actualTo
    amount = undefined actualAmount
pay _ = error "Chainweb.Simulate.Contracts.SimplePayments.pay: case analysis failure. You can only pass a Pay value!"
