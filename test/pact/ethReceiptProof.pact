(let (
  (spv (verify-spv 'ETH (read-msg 'proof)))
  )

  ; TODO:
  ; (expected
  ;   { "continuation": {}
  ;   , "events":
  ;     [ { "module": "coin"
  ;       , "module-hash": "ut_J_ZNkoyaPUEJhiwVeWnkSQn9JT9sQCWKdjjVVrWo"
  ;       , "name": "TRANSFER"
  ;       , "params": ["sender00" "sender01" 1.0]
  ;       }
  ;     ]
  ;   , "gas": 5
  ;   , "logs": "oJkGFAj99kRU1Qo-Hydz-uY3acJ1je8QeSDtCLiCCDE"
  ;   , "meta": {}
  ;   , "req-key": "0IEgLi4hZ7YMZ1yg1SOzfSHUEifH8gwTBdEC7MJ9BQQ"
  ;   , "result": "Write succeeded"
  ;   , "txid": "12"
  ;   }))
  ; (enforce
  ;  (= spv expected)
  ;  (format "Failure, result={} expected={}" [spv, expected]))
  "ETH Success")

