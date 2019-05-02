(define-keyset 'test-admin (read-keyset "test-admin-keyset"))

(module reintro-test 'test-admin

  (defschema account
    balance:decimal
    amount:decimal
    data)

  (deftable accounts:{account})

  (defun create-account (id init-bal)
    (insert accounts id
         { "balance": init-bal, "amount": init-bal, "data": "Created account" }))

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

  (defun enforce-positive (amount)
    (enforce (>= amount 0.0) "amount must be positive"))

 (defun read-all ()
   (map (read-account) (keys accounts)))

 (defun read-test-acct ()
   (map (read-account) ["Acct1"]))

 (defun create-test-account ()
   (create-account "Acct1" 100.0)
   (read-all))

 (defun double-test-acct ()
    (credit "Acct1" (2 * read-test-acct) { "Double amt in Acct 1 balance" }))
)

---------
h1: create-test-account -- balance = 100
h2: double-test-acct -- balance = 200
h3: credit 10 -- balance 210
