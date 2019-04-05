(module coin GOVERNANCE

  "'coin' represents the Kadena Coin Contract."


  ; (implements coin-sig)
  ; (implements spv-sig)

  ; --------------------------------------------------------------------------
  ; Schemas and Tables
  ; --------------------------------------------------------------------------

  (defschema coin-schema
    balance:decimal
    guard:guard
    exists:bool
    )

  (deftable coin-table:{coin-schema})

  (defschema creates-schema
    exists:string
    )

  (deftable creates-table:{creates-schema})

  ; --------------------------------------------------------------------------
  ; Capabilities
  ; --------------------------------------------------------------------------

  (defcap GOVERNANCE () (enforce false "upgrade disabled"))

  (defcap TRANSFER ()
    "Autonomous capability to protect debit and credit actions"
    true)

  (defcap COINBASE ()
    "Magic capability to protect miner reward"
    true)

  (defcap FUND_TX ()
    "Magic capability to execute gas purchases and redemptions"
    true)

  (defcap ACCOUNT_GUARD (account)
    "Lookup and enforce guards associated with an account"
    (with-read coin-table account { "guard" := g }
      (enforce-guard g)))

  (defcap GOVERNANCE ()
    (enforce false "Enforce non-upgradeability except in the case of a hard fork"))

  ; --------------------------------------------------------------------------
  ; Coin Contract
  ; --------------------------------------------------------------------------

  (defun buy-gas:string (sender:string total:decimal)
    @doc "This function describes the main 'gas buy' operation. At this point \
    \MINER has been chosen from the pool, and will be validated. The SENDER   \
    \of this transaction has specified a gas limit LIMIT (maximum gas) for    \
    \the transaction, and the price is the spot price of gas at that time.    \
    \The gas buy will be executed prior to executing SENDER's code."

    @model [(property (> total 0.0))]

    (require-capability (FUND_TX))
    (with-capability (TRANSFER)
       (debit sender total))
    )

  (defun redeem-gas:string (miner:string miner-guard:guard sender:string total:decimal)
    @doc "This function describes the main 'redeem gas' operation. At this    \
    \point, the SENDER's transaction has been executed, and the gas that      \
    \was charged has been calculated. MINER will be credited the gas cost,    \
    \and SENDER will receive the remainder up to the limit"

    @model [(property (> total 0.0))]

    (require-capability (FUND_TX))
    (with-capability (TRANSFER)
      (let* ((fee (read-decimal "fee"))
             (refund (- total fee)))
        (enforce (>= refund 0.0) "fee must be less than or equal to total")


        ; directly update instead of credit
        (if (> refund 0.0)
          (with-read coin-table sender
            { "balance" := balance }
            (update coin-table sender
              { "balance": (+ balance refund) })
            )
          "noop")
        (credit miner miner-guard fee)
        ))
    )

  (defun create-account:string (account:string guard:guard)
    @doc "Create an account for ACCOUNT, with ACCOUNT as a function of GUARD"
    (insert coin-table account
      { "balance" : 0.0
      , "guard"   : guard
      , "exists"  : true
      })
    )

  (defun account-balance:decimal (account:string)
    @doc "Query account balance for ACCOUNT"
    (with-capability (ACCOUNT_GUARD account)
      (with-read coin-table account
        { "balance" := balance }
        balance
        ))
    )

  (defun transfer:string (sender:string receiver:string receiver-guard:guard amount:decimal)
    @doc "Transfer between accounts SENDER and RECEIVER on the same chain.    \
    \This fails if both accounts do not exist. Create-on-transfer can be      \
    \handled by sending in a create command in the same tx."

    @model [(property (> amount 0.0))]

    (with-capability (TRANSFER)
      (debit sender amount)
      (credit receiver receiver-guard amount))
    )

  (defun coinbase:string (address:string address-guard:guard amount:decimal)
    @doc "Mint some number of tokens and allocate them to some address"
    (require-capability (COINBASE))
    (with-capability (TRANSFER)
     (credit address address-guard amount)))

  (defpact fund-tx (sender miner miner-guard total)
    @doc "'fund-tx' is a special pact to fund a transaction in two steps,     \
    \with the actual transaction transpiring in the middle:                   \
    \                                                                         \
    \  1) A buying phase, debiting the sender for total gas and fee, yielding \
    \     TX_MAX_CHARGE.                                                      \
    \  2) A settlement phase, resuming TX_MAX_CHARGE, and allocating to the   \
    \     coinbase account for used gas and fee, and sender account for bal-  \
    \     ance (unused gas, if any)."

    (step (buy-gas sender total))
    (step (redeem-gas miner miner-guard sender total))
    )

  ; --------------------------------------------------------------------------
  ; Helpers
  ; --------------------------------------------------------------------------

  (defun debit:string (account:string amount:decimal)
    @doc "Debit AMOUNT from ACCOUNT balance recording DATE and DATA"

    @model [(property (> amount 0.0))]

    (require-capability (TRANSFER))
    (with-capability (ACCOUNT_GUARD account)
      (with-read coin-table account
        { "balance" := balance }

        (enforce (<= amount balance) "Insufficient funds")
        (update coin-table account
          { "balance" : (- balance amount) }
          )))
    )


  (defun credit:string (account:string guard:guard amount:decimal)
    @doc "Credit AMOUNT to ACCOUNT balance recording DATE and DATA"

    @model [(property (> amount 0.0))]

    (require-capability (TRANSFER))
      (with-default-read coin-table account
        { "balance" : 0.0, "exists" : false }
        { "balance" := balance, "exists" := exists }

        (let
          ; bind existing guard to 'retg', or user-supplied guard if not exists
          ((retg
            (if exists
                (with-read coin-table account
                  { "guard" := guard2 }
                 guard2)
              guard)))
          ; we don't want to overwrite an existing guard with the user-supplied one
          (enforce (= retg guard) "account guards do not match")

          (write coin-table account
            { "balance" : (+ balance amount)
            , "guard"   : retg
            , "exists"  : true
            }))
        )
      )

  (defun delete-coin (delete-account create-chain-id create-account create-account-guard quantity)
    (with-capability (TRANSFER)
      (debit delete-account quantity)
      { "create-chain-id": create-chain-id
      , "create-account": create-account
      , "create-account-guard": create-account-guard
      , "quantity": quantity
      , "delete-chain-id": (at "chain-id" (chain-data))
      , "delete-account": delete-account
      , "delete-tx-hash": (tx-hash)
      }))

  (defun create-coin (proof)
    (let ((outputs (at "outputs" (verify-spv "TXOUT" proof))))
      (enforce (= 1 (length outputs)) "only one tx in outputs")
      (bind (at 0 outputs)
        { "create-chain-id":= create-chain-id
        , "create-account" := create-account
        , "create-account-guard" := create-account-guard
        , "quantity" := quantity
        , "delete-tx-hash" := delete-tx-hash
        , "delete-chain-id" := delete-chain-id
        }
        (enforce (= (at "chain-id" (chain-data)) create-chain-id "enforce correct create chain ID"))
        (let ((create-id (format "%:%" [delete-tx-hash delete-chain-id])))
          (with-default-read create-id creates-table
            { "exists": false }
            { "exists":= exists }
            (enforce (not exists) (format "enforce unique usage of %" [create-id]))
            (insert creates-table create-id { "exists": true })
            (with-capability (TRANSFER)
              (credit create-account create-account-guard quantity)))
          )))
    )


)

(create-table coin-table)
(create-table creates-table)
