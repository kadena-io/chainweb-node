{-# LANGUAGE OverloadedStrings #-}

-- This module is auto-generated. DO NOT EDIT IT MANUALLY.

module Chainweb.Pact.Transactions.MainnetKADTransactions ( transactions ) where

import Data.Bifunctor (first)

import Chainweb.Transaction
import Chainweb.Utils

transactions :: IO [ChainwebTransaction]
transactions =
  let decodeTx t =
        fromEitherM . (first (userError . show)) . codecDecode chainwebPayloadCodec =<< decodeB64UrlNoPaddingText t
  in mapM decodeTx [
    "eyJoYXNoIjoiUEZfMUtLMmJPZnlqSDU5ZUo0aGUzcmpMWlBTbzFzeGFWRHUzaGxRVDZmdyIsInNpZ3MiOltdLCJjbWQiOiJ7XCJuZXR3b3JrSWRcIjpudWxsLFwicGF5bG9hZFwiOntcImV4ZWNcIjp7XCJkYXRhXCI6bnVsbCxcImNvZGVcIjpcIihjb2luLnJlbWVkaWF0ZSBcXFwiZTdmNzYzNGU5MjU1NDFmMzY4YjgyN2FkNWM3MjQyMTkwNTEwMGY2MjA1Mjg1YTc4YzE5ZDdiNGEzODcxMTgwNVxcXCIgMTAwLjApXCJ9fSxcInNpZ25lcnNcIjpbXSxcIm1ldGFcIjp7XCJjcmVhdGlvblRpbWVcIjowLFwidHRsXCI6MTcyODAwLFwiZ2FzTGltaXRcIjowLFwiY2hhaW5JZFwiOlwiXCIsXCJnYXNQcmljZVwiOjAsXCJzZW5kZXJcIjpcIlwifSxcIm5vbmNlXCI6XCJtYWlubmV0LXJlbWVkaWF0aW9ucy1rYWQtb3BzXCJ9In0"
    ]
