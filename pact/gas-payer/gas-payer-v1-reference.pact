(define-keyset 'operate (read-keyset 'gas-payer-operate))

(module gas-payer-v1-reference G
  (defcap G () true)
  (implements gas-payer-v1)
  (use coin)

  (defschema gas
    balance:decimal
    guard:guard)

  (deftable ledger:{gas})

  (defcap GAS_PAYER:bool
    ( user:string
      limit:integer
      price:decimal
    )
    (with-read ledger user { 'balance:= bal, 'guard:= g}
      (let ((amount (* limit price)))
        (enforce (>= bal amount) "Insufficient gas balance")
        (enforce-guard g)
        (update ledger user
          {'balance: (- bal amount)})
        (compose-capability (ALLOW_GAS))))
  )

  (defcap ALLOW_GAS () true)

  (defun create-gas-payer-guard:guard ()
    (create-user-guard (gas-guard))
  )

  (defun gas-guard ()
    (require-capability (coin.FUND_TX))
    (require-capability (ALLOW_GAS))
  )

  (defun fund-user
    (user:string guard:guard amount:decimal)
    (enforce-keyset 'operate)
    (write ledger user
      { 'balance: amount
      , 'guard: guard
      }
      )
  )

  (defun user-balance ( user:string )
    (at 'balance (read ledger user)))

)
(create-table ledger)
