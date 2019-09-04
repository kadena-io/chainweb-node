(interface coin-sig

  "'coin-sig' represents the Kadena Coin Contract interface. This contract     \
  \provides both the the general interface for a Kadena's token, supplying a   \
  \transfer function, coinbase, account creation and balance query."

  (defun create-account:string (account:string guard:guard)
    @doc "Create an account for ACCOUNT, with GUARD controlling access to the  \
    \account."
    @model [ (property (not (= account ""))) ]
    )

  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer between accounts SENDER and RECEIVER on the same chain.    \
    \This fails if both accounts do not exist. Create-on-transfer can be      \
    \done using the transfer-and-create function."

    @model [ (property (> amount 0.0))
             (property (not (= sender receiver)))
           ]
    )

  (defun transfer-and-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
    @doc "Transfer between accounts SENDER and RECEIVER on the same chain.    \
    \This fails if the SENDER account does not exist. If the RECEIVER account \
    \does not exist, it is created and associated with GUARD."

    @model [ (property (> amount 0.0))
             (property (not (= sender receiver)))
           ]
    )

  (defun account-balance:decimal (account:string)
    @doc "Check an account's balance")

  (defun account-info:object (account:string)
    @doc "Get all of an account's info.  This includes the balance and the    \
    \guard.")

  (defun coinbase:string (address:string address-guard:guard amount:decimal)
    @doc "Mint some number of tokens and allocate them to some address"

    @model [ (property (> amount 0.0))
             (property (not (= address "")))
           ]
    )

)
