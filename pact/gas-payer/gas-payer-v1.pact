(interface gas-payer-v1

  (defcap GAS_PAYER:bool
    ( user:string
      limit:integer
      price:decimal
    )
    @doc
    " Provide a capability indicating that declaring module supports \
    \ gas payment for USER for gas LIMIT and PRICE. Functionality \
    \ should require capability (coin.FUND_TX), and should validate \
    \ the spend of (limit * price), possibly updating some database \
    \ entry. \
    \ Should compose capability required for 'create-gas-payer-guard'."
    @model
    [ (property (user != ""))
      (property (limit > 0))
      (property (price > 0.0))
    ]
  )

  (defun create-gas-payer-guard:guard ()
    @doc
    " Provide a guard suitable for controlling a coin account that can \
    \ pay gas via GAS_PAYER mechanics. Generally this is accomplished \
    \ by having GAS_PAYER compose an unparameterized, unmanaged capability \
    \ that is required in this guard. Thus, if coin contract is able to \
    \ successfully acquire GAS_PAYER, the composed 'anonymous' cap required \
    \ here will be in scope, and gas buy will succeed."
  )

)
