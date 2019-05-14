(module coin-faucet FAUCET-GOVERNANCE

  "'coin-faucet' represents Kadena's Coin Faucet Contract."

  (defcap FAUCET-GOVERNANCE () true)

  (use coin)

  ; --------------------------------------------------------------------------
  ; Schemas and Tables
  ; --------------------------------------------------------------------------

  (defschema history
    @doc "Table to record of the behavior of addresses. Last transaction time,   \
    \ total coins earned, and total coins returned are recorded per transaction. "
    last-tx-time:time
    total-coins-earned:decimal
    total-coins-returned:decimal
    )

  (deftable history-table: {history})

  ; --------------------------------------------------------------------------
  ; Capabilities
  ; --------------------------------------------------------------------------

  (defcap HISTORY ()
    "Enforces faucet-guard when looking up history"
    (enforce-guard (faucet-guard)))

  ; --------------------------------------------------------------------------
  ; Constants
  ; --------------------------------------------------------------------------

  (defconst FAUCET_ACCOUNT 'faucet-account)
  (defconst COIN_PER_REQUEST 1.0)
  (defconst WAIT_TIME_PER_REQUEST 3600.0)
  (defconst EPOCH  (time "1970-01-01T00:00:00Z"))

  ; --------------------------------------------------------------------------
  ; Coin Faucet Contract
  ; --------------------------------------------------------------------------

  (defun faucet-guard:guard () (create-module-guard 'faucet-admin))

  (defun request-coin:string (address address-guard)
    @doc "Transfers COIN_PER_REQUEST coins from faucet account to requester.  \
    \ Inserts or updates the record of the requester in history table to keep \
    \ track of the record. Limits the number of coin giveout by time,         \
    \ WAIT_TIME_PER_REQUEST."

    (with-capability (HISTORY)
      (with-default-read history-table address
        {"last-tx-time": EPOCH, "total-coins-earned": 0.0, "total-coins-returned": 0.0}
        {"last-tx-time":= last-tx-time,
         "total-coins-earned":= total-coins-earned,
         "total-coins-returned":= total-coins-returned}
        (enforce (> (diff-time (curr-time) last-tx-time) WAIT_TIME_PER_REQUEST) "Has reached maximum giveout")
        (transfer FAUCET_ACCOUNT address address-guard COIN_PER_REQUEST)
        (write history-table address {
          "last-tx-time": (curr-time),
          "total-coins-earned": (+ COIN_PER_REQUEST total-coins-earned),
          "total-coins-returned": total-coins-returned }))))

  (defun return-coin:string (address amount)
    @doc "Returns the coin back to the faucet account after use. Updates the    \
    \ history table to keep track of behavior. "

    @model [(property (> amount 0.0))]
    (with-capability (HISTORY)
      (with-read history-table address {"total-coins-returned":= coins-returned}
        (transfer address FAUCET_ACCOUNT (faucet-guard) amount)
        (update history-table address {"total-coins-returned": (+ amount coins-returned)}))))

  (defun describe-behavior:string (address)
    @doc "Describes past coin request and returns of the account"

    (with-capability (HISTORY)
        (with-read history-table address {
          "total-coins-earned":= total-coins-earned,
          "total-coins-returned":= total-coins-returned
          }
          (format "Address {} has earned {} and returned {}"
            [address, total-coins-earned, total-coins-returned]))))

  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"
    (add-time EPOCH (at 'block-time (chain-data)))
  )

)

(create-table history-table)
(coin.coinbase FAUCET_ACCOUNT (faucet-guard) 1000000000000.0)
