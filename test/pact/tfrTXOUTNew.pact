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
    , "gas": 5
    , "logs": "0aw7N48LexZxuIDtmGf1-WLwmGndE3VkmFM50jxwFwo"
    , "meta": {}
    , "req-key": "0IEgLi4hZ7YMZ1yg1SOzfSHUEifH8gwTBdEC7MJ9BQQ"
    , "result": "Write succeeded"
    , "txid": "12"
    }))
  (enforce
   (= spv expected)
   (format "Failure, result={} expected={}" [spv, expected]))
  "TXOUT Success")
