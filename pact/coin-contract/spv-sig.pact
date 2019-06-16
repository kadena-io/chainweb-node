(interface spv-sig

  "SPV support for the Kadena Coin Contract"

  (defun delete-coin
      ( delete-acct:string
        create-chain-id:string
        create-account:string
        quantity:decimal )

    @doc "Produce an SPV receipt witnessing a cross-chain transfer from one account to another on a \
         \separate chain."
    )

  (defun create-coin:string (proof)
    @doc "Consume an SPV receipt witnessing a cross chain transfer from one account to another on a \
         \separate chain."
    )
)
