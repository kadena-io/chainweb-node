{-# LANGUAGE OverloadedStrings #-}

-- This module is auto-generated. DO NOT EDIT IT MANUALLY.

module Chainweb.Pact.Transactions.MainnetKADTransactions ( transactions ) where

import Data.Bifunctor (first)
import System.IO.Unsafe

import qualified Chainweb.Pact4.Transaction as Pact4
import Chainweb.Utils

transactions :: [Pact4.Transaction]
transactions =
  let decodeTx t =
        fromEitherM . (first (userError . show)) . codecDecode (Pact4.payloadCodec maxBound) =<< decodeB64UrlNoPaddingText t
  in unsafePerformIO $ mapM decodeTx [
    "eyJoYXNoIjoieThkd0Rkc0RZdmM5alg2WHZxVG1CSndEX2xlRlJUTWlJTXJMcjhKODlVOCIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6bnVsbCxcImNvZGVcIjpcIlxcbihpZlxcbiAgKDw9IDEwMC4wIChjb2luLmdldC1iYWxhbmNlIFxcXCJlN2Y3NjM0ZTkyNTU0MWYzNjhiODI3YWQ1YzcyNDIxOTA1MTAwZjYyMDUyODVhNzhjMTlkN2I0YTM4NzExODA1XFxcIikpXFxuXFxuICAoY29pbi5yZW1lZGlhdGUgXFxcImU3Zjc2MzRlOTI1NTQxZjM2OGI4MjdhZDVjNzI0MjE5MDUxMDBmNjIwNTI4NWE3OGMxOWQ3YjRhMzg3MTE4MDVcXFwiIDEwMC4wKVxcbiAgXFxcIldhcm5pbmc6IGluc3VmZmljaWVudCBmdW5kcyBmb3IgcmVtZWRpYXRpb24sIHNvbGRpZXJpbmcgb25cXFwiKVwifX0sXCJzaWduZXJzXCI6W10sXCJtZXRhXCI6e1wiY3JlYXRpb25UaW1lXCI6MCxcInR0bFwiOjE3MjgwMCxcImdhc0xpbWl0XCI6MCxcImNoYWluSWRcIjpcIlwiLFwiZ2FzUHJpY2VcIjowLFwic2VuZGVyXCI6XCJcIn0sXCJub25jZVwiOlwibWFpbm5ldC1yZW1lZGlhdGlvbnMta2FkLW9wc1wifSJ9"
    ]
