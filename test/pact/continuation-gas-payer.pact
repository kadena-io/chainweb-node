(namespace 'user)

;; gas payer used only for continuations
(module gas-payer-for-cont G
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
    (enforce (= "cont" (at "tx-type" (read-msg))) "Inside a continuation")
    (enforce (= false (at "cont-has-proof" (read-msg))) "Not inside a cross-chain tx")
    (enforce (= 1 (at "cont-step" (read-msg))) "Inside 2nd step of continuation")
    (enforce (= {} (at "cont-user-data" (read-msg))) "Inside a continuation w/o user data")
    (enforce (= "9ylBanSjDGJJ6m0LgokZqb9P66P7JsQRWo9sYxqAjcQ"
                (at "cont-pact-id" (read-msg))) "Inside a particular continuation")
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

;; module with simple pacts
(module simple-cont-module G
  (defcap G () true)

  (defpact some-two-step-pact ()
    (step
      "Step One")
    (step
      "Step Two"))
)
