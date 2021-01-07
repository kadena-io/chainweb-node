(let
  ( (spv (verify-spv 'TXOUT (read-msg 'proof)))
    (expected
      { 'amount: 1.0
      , 'receiver: 'sender01
      , 'receiver-guard: (read-keyset 'data)
      })
  )
  (enforce
    (= spv expected)
    (format "Failure, result={} expected={}" [spv, expected]))
  "TXOUT Success")
