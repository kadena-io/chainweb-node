(let (
  (spv (verify-spv 'TXOUT (read-msg 'proof)))
  (expected
    { "continuation": {}
    , "events":
      [ { "module": "coin"
        , "module-hash": "ut_J_ZNkoyaPUEJhiwVeWnkSQn9JT9sQCWKdjjVVrWo"
        , "name": "TRANSFER"
        , "params": ["sender00" "sender01" 1.0]
        }
      ]
    , "gas": 547
    , "logs": "KlSdh0LqHlhmSd0iHzukADGkI-IzkYdNdD_FDrF6uIA"
    , "meta": {}
    , "req-key": "LCLlbFalZdnCr7ax2Cqntuj2PCUq0pVGhmgAE-oabKs"
    , "result": "Write succeeded"
    , "txid": "12"
    }))
  (enforce
   (= spv expected)
   (format "Failure, result={} expected={}" [spv, expected]))
  "TXOUT Success")
