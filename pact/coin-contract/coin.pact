(module coin GOVERNANCE

  @doc "'coin' represents the Kadena Coin Contract. This contract provides both the \
  \buy/redeem gas support in the form of 'fund-tx', as well as transfer,       \
  \credit, debit, coinbase, account creation and query, as well as SPV burn    \
  \create. To access the coin contract, you may use its fully-qualified name,  \
  \or issue the '(use coin)' command in the body of a module declaration."

  @model
    [ (defproperty conserves-mass
        (= (column-delta coin-table 'balance) 0.0))

      (defproperty account (account:string)
        (and
          (>= (length account) 3)
          (<= (length account) 256)))
    ]

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema coin-schema
    @doc "The coin contract token schema"
    @model [ (invariant (>= balance 0.0)) ] ; FV problem

    balance:decimal
    guard:guard)

  (deftable coin-table:{coin-schema})

  (defschema transfer-schema
    @doc "Convenience schema for typechecking cross-chain transfers"

    create-account:string
    create-account-guard:guard
    quantity:decimal)

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
  ; Constants

  (defconst COIN_CHARSET CHARSET_LATIN1
    "The default coin contract character set")

  (defconst MINIMUM_PRECISION 12
    "Minimum allowed precision for coin transactions")

  (defconst MINIMUM_ACCOUNT_LENGTH 3
    "Minimum account length admissible for coin accounts")

  (defconst MAXIMUM_ACCOUNT_LENGTH 256
    "Maximum account name length admissible for coin accounts")

  ; --------------------------------------------------------------------------
  ; Utilities

  (defun enforce-unit (amount:decimal)
    @doc "Enforce minimum precision allowed for coin transactions"

    (enforce
      (= (floor amount MINIMUM_PRECISION)
         amount)
      (format "Amount violates minimum precision: {}" [amount]))
    )

  (defun enforce-account (account:string)
    @doc "Enforce that an account name conforms to the coin contract \
         \minimum and maximum length requirements, as well as the    \
         \latin-1 character set."

    (enforce
      (is-charset COIN_CHARSET account)
      (format
        "Account does not conform to the coin contract charset: {}"
        [account]))

    (let ((account-length (length account)))

      (enforce
        (>= account-length MINIMUM_ACCOUNT_LENGTH)
        (format
          "Account name does not conform to the min length requirement: {}"
          [account]))

      (enforce
        (<= account-length MAXIMUM_ACCOUNT_LENGTH)
        (format
          "Account name does not conform to the min length requirement: {}"
          [account]))
      )
  )

  ; --------------------------------------------------------------------------
  ; Coin Contract

  (defun buy-gas:string (sender:string total:decimal)
    @doc "This function describes the main 'gas buy' operation. At this point \
    \MINER has been chosen from the pool, and will be validated. The SENDER   \
    \of this transaction has specified a gas limit LIMIT (maximum gas) for    \
    \the transaction, and the price is the spot price of gas at that time.    \
    \The gas buy will be executed prior to executing SENDER's code."

    @model [ (property (> total 0.0))
             (property (account sender))
           ]

    (enforce-account sender)

    (enforce-unit total)
    (enforce (> total 0.0) "gas supply must be a positive quantity")

    (require-capability (FUND_TX))
    (with-capability (TRANSFER)
      (debit sender total))
    )

  (defun redeem-gas:string (miner:string miner-guard:guard sender:string total:decimal)
    @doc "This function describes the main 'redeem gas' operation. At this    \
    \point, the SENDER's transaction has been executed, and the gas that      \
    \was charged has been calculated. MINER will be credited the gas cost,    \
    \and SENDER will receive the remainder up to the limit"

    @model [ (property (> total 0.0))
             (property (account sender))
             (property (account miner))
           ]

    (enforce-account sender)
    (enforce-account miner)
    (enforce-unit total)

    (require-capability (FUND_TX))
    (with-capability (TRANSFER)
      (let* ((fee (read-decimal "fee"))
             (refund (- total fee)))

        (enforce-unit fee)

        (enforce (>= fee 0.0)
          "fee must be a non-negative quantity")

        (enforce (>= refund 0.0)
          "refund must be a non-negative quantity")

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

    @model [ (property (account account)) ]

    (enforce-account account)

    (insert coin-table account
      { "balance" : 0.0
      , "guard"   : guard
      })
    )

  (defun account-balance:decimal (account:string)
    @doc "Check an account's balance."

    @model [ (property (account account)) ]

    (enforce-account account)

    (with-read coin-table account
      { "balance" := balance }
      balance
      )
    )

  (defun account-info:object (account:string)
    @doc "Get all of an account's info.  This includes the balance and the    \
    \guard."

    @model [ (property (account account)) ]

    (enforce-account account)

    (read coin-table account)
    )

  (defun rotate-guard:string (account:string new-guard:guard)
    @doc "Rotate guard associated with ACCOUNT"

    @model [ (property (account account)) ]

    (enforce-account account)

    (with-read coin-table account
      { "guard" := old-guard }

      (enforce-guard old-guard)

      (update coin-table account
        { "guard" : new-guard }
        )))


  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer AMOUNT between accounts SENDER and RECEIVER on the same    \
    \chain. This fails if either SENDER or RECEIVER does not exist.           \
    \Create-on-transfer can be done using the 'transfer-and-create' function."

    @model [ (property conserves-mass)
             (property (> amount 0.0))
             (property (account sender))
             (property (account receiver))
             (property (!= sender receiver)) ]

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")

    (enforce-account sender)
    (enforce-account receiver)

    (enforce (> amount 0.0)
      "transfer amount must be positive")

    (enforce-unit amount)

    (with-capability (TRANSFER)
      (debit sender amount)
      (with-read coin-table receiver
        { "guard" := g }

        (credit receiver g amount))
      )
    )

  (defun transfer-create:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal )

    @doc "Transfer between accounts SENDER and RECEIVER on the same chain.    \
    \This fails if the SENDER account does not exist. If the RECEIVER account \
    \does not exist, it is created and associated with GUARD."

    @model [ ;(property conserves-mass) ;; fails on missing row, FV problem
            (property (> amount 0.0))
            (property (account sender))
            (property (account receiver))
            (property (!= sender receiver)) ]

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")

    (enforce-account sender)
    (enforce-account receiver)

    (enforce (> amount 0.0)
      "transfer amount must be positive")

    (enforce-unit amount)

    (with-capability (TRANSFER)
      (debit sender amount)
      (credit receiver receiver-guard amount))
    )

  (defun coinbase:string (account:string account-guard:guard amount:decimal)
    @doc "Internal function for the initial creation of coins.  This function \
    \cannot be used outside of the coin contract."

    @model [ (property (account account)) ]

    (enforce-account account)
    (enforce-unit amount)

    (require-capability (COINBASE))
    (with-capability (TRANSFER)
      (credit account account-guard amount))
    )

  (defpact fund-tx (sender:string miner:string miner-guard:guard total:decimal)
    @doc "'fund-tx' is a special pact to fund a transaction in two steps,     \
    \with the actual transaction transpiring in the middle:                   \
    \                                                                         \
    \  1) A buying phase, debiting the sender for total gas and fee, yielding \
    \     TX_MAX_CHARGE.                                                      \
    \  2) A settlement phase, resuming TX_MAX_CHARGE, and allocating to the   \
    \     coinbase account for used gas and fee, and sender account for bal-  \
    \     ance (unused gas, if any)."

    @model [ (property (> total 0.0))
             (property (account sender))
             (property (account miner))
             ;(property conserves-mass) not supported yet
           ]

    (step (buy-gas sender total))
    (step (redeem-gas miner miner-guard sender total))
    )

  (defun debit:string (account:string amount:decimal)
    @doc "Debit AMOUNT from ACCOUNT balance"

    @model [ (property (> amount 0.0))
             (property (account account))
           ]

    (enforce-account account)

    (enforce (> amount 0.0)
      "debit amount must be positive")

    (enforce-unit amount)

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
    @doc "Credit AMOUNT to ACCOUNT balance"

    @model [ (property (> amount 0.0))
             (property (account account))
           ]

    (enforce-account account)

    (enforce (> amount 0.0) "credit amount must be positive")
    (enforce-unit amount)

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
             (property (!= create-chain-id ""))
             (property (account delete-account))
             (property (account create-account))
           ]

    (step
      (with-capability (TRANSFER)

        (enforce-account delete-account)
        (enforce-account create-account)

        (enforce (!= "" create-chain-id) "empty create-chain-id")
        (enforce (!= (at 'chain-id (chain-data)) create-chain-id)
          "cannot run cross-chain transfers to the same chain")

        (enforce (> quantity 0.0)
          "transfer quantity must be positive")

        (enforce-unit quantity)

        ;; step 1 - debit delete-account on current chain
        (debit delete-account quantity)

        (let
          ((retv:object{transfer-schema}
            { "create-account" : create-account
            , "create-account-guard" : create-account-guard
            , "quantity" : quantity
            }))
          (yield retv create-chain-id)
          )))

    (step
      (resume
        { "create-account" := create-account
        , "create-account-guard" := create-account-guard
        , "quantity" := quantity
        }

        ;; step 2 - credit create account on target chain
        (with-capability (TRANSFER)
          (credit create-account create-account-guard quantity))
        ))
    )


  ; --------------------------------------------------------------------------
  ; Coin allocations

  (defschema allocation-schema
    @doc "The coin allocation schema for genesis lockups"
    @model [ (invariant (>= balance 0.0)) ]

    balance:decimal
    date:time
    guard:guard
    redeemed:bool)

  (deftable allocation-table:{allocation-schema})

  (defun create-allocation-account (account:string date:time guard:guard)
    @doc "Add an entry to the coin allocation table"
    @model [ (property (account-structure account)) ]

    (enforce-account account)

    (insert allocation-table account
      { "balance" : 0.0
      , "date" : date
      , "guard" : guard
      , "redeemed" : false
      }))

  (defun rotate-allocation-guard (account:string new-guard:guard)
    @doc "Rotate the guard associated with an allocation account"
    @model [ (property (account-structure account)) ]

    (enforce-account)

    (with-read allocation-table account
      { "guard" := old-guard }

      (enforce-guard old-guard)

      (update allocation-table account
        { "guard" : new-guard }))
    )

  (defun release-allocation (account:string amount:decimal)
    @doc "Release funds associated with an allocation account"
    @model
      [ (property (account-structure account))
        (property (> amount 0.0))
      ]

    (enforce-account)

    (enforce-unit amount)
    (enforce
      (> amount 0.0)
      (format
        "amount requested for unlock is not a valid quantity: {}"
        [amount]))

    (with-read allocation-table account
      { "balance" := balance
      , "date" := release-time
      , "redeemed" := redeemed
      , "guard" := guard
      }

      (let
        ((new-balance:decimal (- balance amount))
         (curr-time:time (at 'block-time (chain-data))))

        (enforce (not redeemed)
          "allocation funds have already been redeemed")

        (enforce
          (>= (diff-time release-time curr-time) 0)
          (format "funds locked until {}" [release-time]))

        (enforce-guard guard)

        (enforce
          (>= new-balance 0)
          (format
            "not enough funds to withdraw amount: {} {}"
            [balance amount]))

        (with-capability (COINBASE)
          ; if account is now empty, mark row as redeemd
          (if (= new-balance 0)
            (update allocation-table account
              { "redeemed" : true })

            "noop")

          ; release funds via coinbase to account
          (coinbase account guard amount)

          ; update balance to reflect coinbase
          (update allocation-table account
            { "balance" : new-balance })))
      )
    )

)

(create-table coin-table)
(create-table allocation-table)
