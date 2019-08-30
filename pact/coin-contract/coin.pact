(module coin GOVERNANCE

  "'coin' represents the Kadena Coin Contract. This contract provides both the \
  \buy/redeem gas support in the form of 'fund-tx', as well as transfer,       \
  \credit, debit, coinbase, account creation and query, as well as SPV burn    \
  \create. To access the coin contract, you may use its fully-qualified name,  \
  \or issue the '(use coin)' command in the body of a module declaration."


  (implements coin-sig)

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema coin-schema
    balance:decimal
    guard:guard)
  (deftable coin-table:{coin-schema})

  ; the shape of a cross-chain transfer (used for typechecking)
  (defschema transfer-schema
    create-account:string
    create-account-guard:guard
    quantity:decimal
    )

  ; --------------------------------------------------------------------------
  ; Capabilities

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

  (defun buy-gas:string (sender:string total:decimal)
    @doc "This function describes the main 'gas buy' operation. At this point \
    \MINER has been chosen from the pool, and will be validated. The SENDER   \
    \of this transaction has specified a gas limit LIMIT (maximum gas) for    \
    \the transaction, and the price is the spot price of gas at that time.    \
    \The gas buy will be executed prior to executing SENDER's code."

    @model [(property (> total 0.0))]

    (enforce (> total 0.0)
      "gas supply must be a positive quantity")

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

        (enforce (>= fee 0.0)
          "fee must be a non-negative quantity")

        (enforce (>= refund 0.0)
          "refun must be a non-negative quantity")

        ; directly update instead of credit
        (if (> refund 0.0)
          (with-read coin-table sender
            { "balance" := balance }
            (update coin-table sender
              { "balance": (+ balance refund) })
            )
          "noop")

        (if (> fee 0.0)
          (credit miner miner-guard fee)
          "noop")
        ))
    )

  (defun create-account:string (account:string guard:guard)
    @doc "Create an account for ACCOUNT, with GUARD controlling access to the  \
    \account."
    (insert coin-table account
      { "balance" : 0.0
      , "guard"   : guard
      })
    )

  (defun account-balance:decimal (account:string)
    @doc "Check an account's balance."
    (with-read coin-table account
      { "balance" := balance }
      balance
      )
    )

  (defun account-info:object (account:string)
    @doc "Get all of an account's info.  This includes the balance and the    \
    \guard."
    (read coin-table account)
    )

  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer between accounts SENDER and RECEIVER on the same chain.    \
    \This fails if both accounts do not exist. Create-on-transfer can be      \
    \done using the transfer-and-create function."

    (enforce (not (= sender receiver))
      "sender cannot be the receiver of a transfer")

    (enforce (> amount 0.0)
      "transfer amount must be positive")

    (with-capability (TRANSFER)
      (debit sender amount)
      (with-read coin-table receiver
        { "guard" := g }

        (credit receiver g amount))
      )
    )

  (defun transfer-and-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
    @doc "Transfer between accounts SENDER and RECEIVER on the same chain.    \
    \This fails if the SENDER account does not exist. If the RECEIVER account \
    \does not exist, it is created and associated with GUARD."

    (enforce (not (= sender receiver))
      "sender cannot be the receiver of a transfer")

    (enforce (> amount 0.0)
      "transfer amount must be positive")

    (with-capability (TRANSFER)
      (debit sender amount)
      (credit receiver receiver-guard amount))
    )

  (defun coinbase:string (address:string address-guard:guard amount:decimal)
    @doc "Internal function for the initial creation of coins.  This function \
    \cannot be used outside of the coin contract."
    (require-capability (COINBASE))
    (with-capability (TRANSFER)
     (credit address address-guard amount))
    )

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

    @model [ (property (> amount 0.0)) ]

    (enforce (> amount 0.0)
      "debit amount must be positive")

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

    @model [ (property (> amount 0.0))
             (property (not (= account "")))
           ]

    (enforce (> amount 0.0)
      "credit amount must be positive")

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

  (defpact cross-chain-transfer
    ( delete-account:string
      create-chain-id:string
      create-account:string
      create-account-guard:guard
      quantity:decimal )

    @doc "Transfer QUANTITY coins from DELETE-ACCOUNT on current chain to           \
         \CREATE-ACCOUNT on CREATE-CHAIN-ID. Target chain id must not be the        \
         \current chain-id.                                                         \
         \                                                                          \
         \Step 1: Burn QUANTITY-many coins for DELETE-ACCOUNT on the current chain, \
         \and produce an SPV receipt which may be manually redeemed for an SPV      \
         \proof. Once a proof is obtained, the user may call 'create-coin' and      \
         \consume the proof on CREATE-CHAIN-ID, crediting CREATE-ACCOUNT QUANTITY-  \
         \many coins.                                                               \
         \                                                                          \
         \Step 2: Consume an SPV proof for a number of coins, and credit the        \
         \account associated with the proof the quantify of coins burned on the     \
         \source chain by the burn account. Note: must be called on the correct     \
         \chain id as specified in the proof."

    @model [ (property (> quantity 0.0))
           , (property (not (= create-chain-id "")))
           ]

    (step
      (with-capability (TRANSFER)
        (enforce (not (= (at 'chain-id (chain-data)) create-chain-id))
          "cannot run cross-chain transfers to the same chain")

        (enforce (> quantity 0.0)
          "transfer quantity must be positive")

        (debit delete-account quantity)
        (let
          ((retv:object{transfer-schema}
            { "create-account": create-account
            , "create-account-guard": create-account-guard
            , "quantity": quantity
            }))
          (yield retv create-chain-id)
          )))

    (step
      (resume
        { "create-account" := create-account
        , "create-account-guard" := create-account-guard
        , "quantity" := quantity
        }

        (with-capability (TRANSFER)
          (credit create-account create-account-guard quantity))
        ))
    )
)

(create-table coin-table)
