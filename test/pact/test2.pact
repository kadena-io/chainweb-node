(namespace 'free)

(define-keyset 'test-admin-ks (read-keyset "test-admin-keyset"))

(module fork-test 'test-admin-ks

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

  (defun read-account (id)
    "Read data for account ID"
    (+ { "account": id } (read accounts id)))

  (defun create-global-accounts ()
    (create-account "Acct1" 1000000.0)
    (create-account "Acct2" 0.0)
    (read-all))

  (defun multiply-transfer (source dest mult)
    "Multiply the balance in 'dest' by 'mult', transferring the required coins from 'source'"
    (let ((x read-account ("Acct2")))
      (transfer "Acct1" "Acct2" (x * (mult-1)))

)

(create-table accounts)
(create-global-accounts)
(free.fork-test.transfer "Acct1" "Acct2" 1)
(multiply-transfer "Acct1" "Acct2" 1)