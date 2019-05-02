(define-keyset 'faucet-admin (read-keyset 'faucet-admin))

(module coin-faucet 'faucet-admin

  "'coin' represents the Kadena Coin Contract."


  ; (implements coin-sig)
  ; (implements spv-sig)

  ; --------------------------------------------------------------------------
  ; Schemas and Tables
  ; --------------------------------------------------------------------------

  (use coin)

  (defschema history
    tx-time: time
    )

  (deftable history-table: {history})

  (defconst FAUCET_ADMIN 'faucet-admin)
  (defconst TOTAL_COIN_COUNT 1000000.0)
  (defconst ONE_COIN 1.0)
  (defconst MAX_GIVEOUT_PER_DAY 10.0)

  (defun initiate-faucet (admin-guard:guard)
    (enforce (< 0.0 TOTAL_COIN_COUNT) "Seed money is negative")
    (create-account FAUCET_ADMIN admin-guard)
    (with-capability (COINBASE)
      (coinbase FAUCET_ADMIN admin-guard TOTAL_COIN_COUNT)))

  ;;
  (defun get-one-coin (address address-guard curr-time)
;    (limit-max address curr-time)
    (with-capability (ACCOUNT_GUARD address)
      (transfer FAUCET_ADMIN address address-guard ONE_COIN)
    )

    (write history-table address {"tx-time": curr-time})

    ;;insert transaction-history in history table
    ;(insert history address {"fundCount":0})
    ;(count-funds address)
    )


  (defun limit-max (address curr-time:time)
    (if (< (last-transaction address)  (add-time curr-time (hours -1))) true false))

  (defun last-transaction (address)
    (at 'tx-time (read history-table address))
  )

  ;; Limit coins / person a day

  ;; Limit transfers
)

(create-table history-table)
