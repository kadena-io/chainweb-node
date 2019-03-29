(interface spv-sig

  "SPV support for the Kadena Coin Contract"

  (defun delete-coin:string
      ( delete-acct:string
        create-chain-id:integer
        create-account:string quantity:decimal )

    @doc "Produce an SPV receipt witnessing a cross-chain transfer from one account to another on a \
         \separate chain."
    )

  (defun create-coin:string (proof create-account-guard:guard)
    @doc "Consume an SPV receipt witnessing a cross chain transfer from one account to another on a \
         \separate chain."
    )
)
