(define-keyset 'operate (read-keyset 'gas-payer-operate))

(namespace 'user)
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
        (enforce (>= bal amount)
                 (format "Insufficient gas balance: {} < {}" [bal amount]))
        (enforce-guard g)
        (update ledger user
          {'balance: (- bal amount)})
        (compose-capability (ALLOW_GAS))))
    )

  (defcap FUND_USER ()
    (enforce-keyset 'operate))

  (defcap ALLOW_GAS () true)

  (defun create-gas-payer-guard:guard ()
    (create-user-guard (gas-payer-guard))
  )

  (defun gas-payer-guard ()
    (require-capability (GAS))
    (require-capability (ALLOW_GAS))
  )

  (defun fund-user
      (user:string guard:guard amount:decimal)
    (with-capability (FUND_USER)
      (write ledger user
        { 'balance: amount
        , 'guard: guard
        }
        ))
  )

  (defun user-balance ( user:string )
    (at 'balance (read ledger user)))

)
(create-table ledger)
