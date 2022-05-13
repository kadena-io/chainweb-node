(interface fungible-xchain-v1

  " This interface offers a standard capability for cross-chain \
  \ transfers and associated events. "

  (defcap TRANSFER_XCHAIN:bool
    ( sender:string
      receiver:string
      amount:decimal
      target-chain:string
    )
    @doc " Managed capability sealing AMOUNT for transfer \
         \ from SENDER to RECEIVER on TARGET-CHAIN. Permits \
         \ any number of cross-chain transfers up to AMOUNT."

    @managed amount TRANSFER_XCHAIN-mgr
    )

  (defun TRANSFER_XCHAIN-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    @doc " Allows TRANSFER-XCHAIN AMOUNT to be less than or \
         \ equal managed quantity as a one-shot, returning 0.0."
  )

  (defcap TRANSFER_XCHAIN_RECD:bool
    ( sender:string
      receiver:string
      amount:decimal
      source-chain:string
    )
    @doc "Event emitted on receipt of cross-chain transfer."
    @event
  )
)
