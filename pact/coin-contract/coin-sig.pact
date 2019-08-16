(interface coin-sig

  "'coin-sig' represents the Kadena Coin Contract interface. This contract     \
  \provides both the the general interface for a Kadena's token, supplying a   \
  \transfer function, coinbase, account creation and balance query."

  (defun create-account:string (account:string guard:guard)
    @doc "Create an account for ACCOUNT, with ACCOUNT as a function of GUARD"
    @model [ (property (not (= account ""))) ]
    )

  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer between accounts SENDER and RECEIVER on the same chain.    \
    \This fails if both accounts do not exist. Create-on-transfer can be      \
    \handled by sending in a create command in the same tx."

    @model [ (property (> amount 0.0))
             (property (not (= sender receiver)))
           ]
    )

  (defun transfer-and-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
    @doc "Transfer between accounts SENDER and RECEIVER on the same chain.    \
    \This fails if both accounts do not exist. Create-on-transfer can be      \
    \handled by sending in a create command in the same tx."

    @model [ (property (> amount 0.0))
             (property (not (= sender receiver)))
           ]
    )

  (defun account-balance:decimal (account:string)
    @doc "Query user account ACCOUNT balance")

  (defun coinbase:string (address:string address-guard:guard amount:decimal)
    @doc "Mint some number of tokens and allocate them to some address"

    @model [ (property (> amount 0.0))
             (property (not (= address "")))
           ]
    )

)
