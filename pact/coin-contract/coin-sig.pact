
(interface coin-sig

  "The Kadena Coin Contract"

  (defun create-account:string (account:string guard:guard)
    @doc "Create an account for ACCOUNT, with ACCOUNT as a function of GUARD")

  (defun transfer:string (sender:string reciever:string receiver-guard:guard amount:decimal)
    @doc "Transfer between accounts SENDER and RECEIVER on the same chain.    \
    \This fails if both accounts do not exist. Create-on-transfer can be      \
    \handled by sending in a create command in the same tx."
    @model [ (property (> amount 0)) ]
    )

  (defun account-balance:decimal (account:string)
    @doc "Query user account ACCOUNT balance")

  (def coinbase:string (address:string address-guard:guard amount:decimal)
    @doc "Mint some number of tokens and allocate them to some address"
    @model [ (property (> amount 0)) ]
    )
)
