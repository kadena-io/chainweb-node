(interface fungible-xchain-v1

  " This interface offers a standard capability for cross-chain \
  \ transfers and associated events. "

  (defcap TRANSFER-XCHAIN:bool
    ( sender:string
      receiver:string
      amount:decimal
      target-chain:string
    )
    @doc " Managed capability sealing AMOUNT for transfer \
         \ from SENDER to RECEIVER on TARGET-CHAIN. Permits \
         \ any number of cross-chain transfers up to AMOUNT."

    @managed amount TRANSFER-XCHAIN-mgr
    )

  (defun TRANSFER-XCHAIN-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    @doc " Manages TRANSFER-XCHAIN AMOUNT linearly, \
         \ such that a request for 1.0 amount on a 3.0 \
         \ managed quantity emits updated amount 2.0."
  )
)
