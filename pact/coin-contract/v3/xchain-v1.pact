(interface xchain-v1

  @doc " Minimal interface to specify cross-chain tracking events."

  (defcap XCHAIN_SEND:bool
    ( sender:string
      recipient:string
      recipient-chain:string )
    @doc " Notify of cross-chain send from SENDER \
         \ to RECIPIENT on RECIPIENT_CHAIN."
    @event)

  (defcap XCHAIN_RECEIVE:bool
    ( sender:string
      sender-chain:string
      recipient:string )
    @doc " Notify of cross-chain receive continuation from SENDER \
         \ to RECIPIENT on RECIPIENT_CHAIN. Pact ID of continuation \
         \ is used to directly link transactions."
    @event)
    
)
