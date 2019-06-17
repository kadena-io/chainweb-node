(module coin GOVERNANCE

  "'coin' represents the Kadena Coin Contract. This contract provides both the \
  \buy/redeem gas support in the form of 'fund-tx', as well as transfer,       \
  \credit, debit, coinbase, account creation and query, as well as SPV burn    \
  \create. To access the coin contract, you may use its fully-qualified name,  \
  \or issue the '(use coin)' command in the body of a module declaration."


  ;(use coin-sig)

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema coin-schema
    balance:decimal
    guard:guard
    )
  (deftable coin-table:{coin-schema})

  (defschema creates-schema
    exists:bool
    )
  (deftable creates-table:{creates-schema})

  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()
    "upgrade disabled"
    false)

  (defcap TRANSFER ()
    "Autonomous capability to protect debit and credit actions"
    true)

  (defcap COINBASE ()
    "Magic capability to protect miner reward"
    true)

  (defcap FUND_TX ()
    "Magic capability to execute gas purchases and redemptions"
    true)

  (defcap BURN_CREATE ()
    "Magic capability to protect cross-chain coin transfers"
    true)

  (defcap ACCOUNT_GUARD (account)
    "Lookup and enforce guards associated with an account"
    (with-read coin-table account { "guard" := g }
      (enforce-guard g)))

  (defcap GOVERNANCE ()
    (enforce false "Enforce non-upgradeability except in the case of a hard fork"))

  ; --------------------------------------------------------------------------
  ; Coin Contract

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
      })
    )

  (defun account-balance:decimal (account:string)
    @doc "Query account balance for ACCOUNT"
    (with-read coin-table account
      { "balance" := balance }
      balance
      )
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
      { "balance" : 0.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      ; we don't want to overwrite an existing guard with the user-supplied one
      (enforce (= retg guard)
        "account guards do not match")

      (write coin-table account
        { "balance" : (+ balance amount)
        , "guard"   : retg
        })
      ))

  (defpact cross-chain-transfer (delete-account create-chain-id create-account create-account-guard quantity)

    @doc "Step 1: Burn QUANTITY-many coins for DELETE-ACCOUNT on the current chain, and \
         \produce an SPV receipt which may be manually redeemed for an SPV      \
         \proof. Once a proof is obtained, the user may call 'create-coin' and  \
         \consume the proof on CREATE-CHAIN-ID, crediting CREATE-ACCOUNT        \
         \QUANTITY-many coins.                                                  \
         \                                                                      \
         \Step 2: Consume an SPV proof for a number of coins, and credit the    \
         \account associated with the proof the quantify of coins burned on the \
         \source chain by the burn account. Note: must be called on the correct \
         \chain id as specified in the proof."

    @model [ (property (> quantity 0.0)) ]
    (step
      (with-capability (BURN_CREATE)
        (let
          ((retv
             (delete-coin delete-account create-chain-id create-account create-account-guard quantity)
             ))

          (yield retv create-chain-id)
          retv)))

    (step
      (resume
        { "create-chain-id":= create-chain-id
        , "create-account" := create-account
        , "create-account-guard" := create-account-guard
        , "quantity" := quantity
        , "delete-tx-hash" := delete-tx-hash
        , "delete-chain-id" := delete-chain-id
        }

        (with-capability (BURN_CREATE)
          (create-coin create-chain-id create-account create-account-guard quantity delete-tx-hash delete-chain-id)
          )))
    )

  (defun delete-coin (delete-account create-chain-id create-account create-account-guard quantity)
    (require-capability (BURN_CREATE))
    (with-capability (TRANSFER)
      (debit delete-account quantity))

      { "create-chain-id": create-chain-id
      , "create-account": create-account
      , "create-account-guard": create-account-guard
      , "quantity": quantity
      , "delete-block-height": (at 'block-height (chain-data))
      , "delete-chain-id": (at 'chain-id (chain-data))
      , "delete-account": delete-account
      , "delete-tx-hash": (tx-hash)
      })

  (defun create-coin (create-chain-id create-account create-account-guard quantity delete-tx-hash delete-chain-id)

    (require-capability (BURN_CREATE))

    (enforce (= create-chain-id (at 'chain-id (chain-data)))
      "enforce correct create chain ID")

    (let ((create-id (format "{}:{}" [delete-tx-hash delete-chain-id])))
      (with-default-read creates-table create-id
        { "exists": false }
        { "exists":= exists }

        (enforce (not exists)
          (format "enforce unique usage of {}" [create-id]))

        (insert creates-table create-id { "exists": true })

        (with-capability (TRANSFER)
          (credit create-account create-account-guard quantity))

        )))
)

(create-table coin-table)
(create-table creates-table)
