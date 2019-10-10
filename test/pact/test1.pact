(define-keyset 'test-admin (read-keyset "test-admin-keyset"))

(namespace 'free)

(module test1 'test-admin

  (defschema account
    balance:decimal
    amount:decimal
    data)

  (deftable accounts:{account})

  (defun create-account (id init-bal)
    (insert accounts id
         { "balance": init-bal, "amount": init-bal, "data": "Created account" }))

  (defun transfer (src dest amount)
    "transfer AMOUNT from SRC to DEST for unencrypted accounts"
    (debit src amount { "transfer-to": dest })
    (credit dest amount { "transfer-from": src }))

  (defpact payment (src-entity src dest-entity dest amount)
    "Two-phase confidential payment, sending money from SRC at SRC-ENTITY to DEST at DEST-ENTITY."

    (step-with-rollback
     src-entity
     (let ((result (debit src amount { "transfer-to": dest, "message": "Starting pact" })))
       (yield { "result": result, "amount": amount, "tx": (pact-id) }))
     (credit src amount { "rollback": (pact-id) }))

    (step
     dest-entity
     (resume { "result":= result, "amount":= debit-amount }
       (credit dest debit-amount
               { "transfer-from": src, "debit-result": result, "tx": (pact-id) }))))

  (defun debit (acct amount data)
    "Debit ACCT for AMOUNT, enforcing positive amount and sufficient funds, annotating with DATA"
    (enforce-positive amount)
    (with-read accounts acct { "balance":= balance }
      (check-balance balance amount)
      (update accounts acct
            { "balance": (- balance amount), "amount": (- amount)
            , "data": data })))

  (defun credit (acct amount data)
    "Credit ACCT for AMOUNT, enforcing positive amount"
    (enforce-positive amount)
    (with-read accounts acct { "balance":= balance }
      (update accounts acct
            { "balance": (+ balance amount), "amount": amount
            , "data": data })))


  (defun read-account (id)
    "Read data for account ID"
    (+ { "account": id } (read accounts id)))

  (defun check-balance (balance amount)
    (enforce (<= amount balance) "Insufficient funds"))

  (defun enforce-positive (amount)
    (enforce (>= amount 0.0) "amount must be positive"))

 (defun read-all ()
   (map (read-account) (keys accounts)))

 (defun read-all-global ()
   (map (read-account) ["Acct1" "Acct2"]))

 (defun create-global-accounts ()
   (create-account "Acct1" 1000000.0)
   (create-account "Acct2" 0.0)
   (read-all))

 (defpact create-private-accounts ()
   (step "Alice" (create-account "A" 1000.0))
   (step "Bob" (create-account "B" 1000.0))
   (step "Carol" (create-account "C" 1000.0))
   (step "Dinesh" (create-account "D" 1000.0)))

)
