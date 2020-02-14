(namespace 'user)

;; gas payer used only for continuations
(module gas-payer-for-exec G
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
    (enforce (= "exec" (at "tx-type" (read-msg))) "Inside an exec")
    (enforce (= 1 (length (at "exec-exprs" (read-msg)))) "Tx of only one expression")
    (enforce (= "list" (at "type" (at 0 (at "exec-exprs" (read-msg))))) "Expression is a list or function")
    (enforce (= "+" (at "expr" (at 0 (at "expr" (at 0 (at "exec-exprs" (read-msg))))))) "Expression if `+` function")
    (compose-capability (ALLOW_GAS))
  )

  (defcap ALLOW_GAS () true)

  (defun create-gas-payer-guard:guard ()
    (create-user-guard (gas-payer-guard))
  )

  (defun gas-payer-guard ()
    (require-capability (GAS))
    (require-capability (ALLOW_GAS))
  )
)

