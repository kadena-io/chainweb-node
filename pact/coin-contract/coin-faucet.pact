(module coin-faucet FAUCET-GOVERNANCE

  "'coin-faucet' represents Kadena's Coin Faucet Contract."

  ;;Governance is TBD
  (defcap FAUCET-GOVERNANCE () true)

  ;; TODO - use hashed import
  (use coin)

  ; --------------------------------------------------------------------------
  ; Schemas and Tables
  ; --------------------------------------------------------------------------

  (defschema history
    @doc "Table to record the behavior of addresses. Last transaction time,       \
    \ total coins earned, and total coins returned are inserted or updated at     \
    \ transaction. "
    last-tx-time:time
    total-coins-earned:decimal
    total-coins-returned:decimal
    )

  (deftable history-table: {history})

  ; --------------------------------------------------------------------------
  ; Constants
  ; --------------------------------------------------------------------------

  (defconst FAUCET_ACCOUNT:string 'faucet-account)
  (defconst MAX_COIN_PER_REQUEST:decimal 20.0)
  (defconst WAIT_TIME_PER_REQUEST:decimal 3600.0)
  (defconst EPOCH:time  (time "1970-01-01T00:00:00Z"))

  ; --------------------------------------------------------------------------
  ; Coin Faucet Contract
  ; --------------------------------------------------------------------------

  (defun faucet-guard:guard () (create-module-guard 'faucet-admin))

  (defun request-coin:string (address:string address-guard:guard amount:decimal)
    @doc "Transfers AMOUNT of coins up to MAX_COIN_PER_REQUEST from the faucet    \
    \ account to the requester account at ADDRESS. Inserts or updates the         \
    \ transaction of the account at ADDRESS in history-table. Limits the number   \
    \ of coin requests by time, WAIT_TIME_PER_REQUEST "
    @model [(property (<= amount 20.0))]

    (enforce (<= amount MAX_COIN_PER_REQUEST)
      "Has reached maximum coin amount per request")

    (with-default-read history-table address {
      "last-tx-time": EPOCH,
      "total-coins-earned": 0.0,
      "total-coins-returned": 0.0 }
      {
      "last-tx-time":= last-tx-time,
      "total-coins-earned":= total-coins-earned,
      "total-coins-returned":= total-coins-returned }

      (enforce (> (diff-time (curr-time) last-tx-time) WAIT_TIME_PER_REQUEST)
        "Has reached maximum requests per wait time")

      (transfer FAUCET_ACCOUNT address address-guard amount)
      (write history-table address {
        "last-tx-time": (curr-time),
        "total-coins-earned": (+ amount total-coins-earned),
        "total-coins-returned": total-coins-returned })))

  (defun return-coin:string (address:string amount:decimal)
    @doc "Returns the AMOUNT of coin from account at ADDRESS back to the faucet   \
    \ account after use. Updates the transaction of the account at ADDRESS in     \
    \ history-table keep track of behavior. "
    @model [(property (> amount 0.0))]

    (with-read history-table address
      {"total-coins-returned":= coins-returned}
      (transfer address FAUCET_ACCOUNT (faucet-guard) amount)
      (update history-table address
        {"total-coins-returned": (+ amount coins-returned)})))

  (defun read-history:object{history} (address:string)
    @doc "Returns history of the account at ADDRESS"
    (read history-table address))

  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"
    (add-time EPOCH (at 'block-time (chain-data)))
  )
)

(create-table history-table)
(coin.coinbase FAUCET_ACCOUNT (faucet-guard) 100000000000000.0)
