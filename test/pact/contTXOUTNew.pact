(let (
  (spv (verify-spv 'TXOUT (read-msg 'proof)))
  (expected
    { "continuation":
      { "cont":
        { "args": ["sender00" "sender01" (read-keyset 'data) "1" 1.0]
        , "name": "coin.transfer-crosschain"
        }
      , "executed": ""
      , "pact-id": "rwFh_qoxumclpxZj6QIo5GOM0CmE5AztlKukhFCRTVA"
      , "step": 0
      , "step-count": 2
      , "step-has-rollback": false
      , "yield":
        { "data":
          { "amount": 1.0
          , "receiver": "sender01"
          , "receiver-guard": (read-keyset 'data)
          }
        , "provenance":
          { "module-hash": "ut_J_ZNkoyaPUEJhiwVeWnkSQn9JT9sQCWKdjjVVrWo"
          , "target-chain": "1"
          }
        }
      }
    , "events": []
    , "gas": 6
    , "logs": "RMxEPTM7AhZX3veWT1I2AGrJXvIR2rTZW0p90f8Y-Q4"
    , "meta": {}
    , "req-key": "rwFh_qoxumclpxZj6QIo5GOM0CmE5AztlKukhFCRTVA"
    , "result":
      { "amount": 1.0
      , "receiver": "sender01"
      , "receiver-guard": (read-keyset 'data)
      }
    , "txid": "12"
    }))
  (enforce
   (= spv expected)
   (format "Failure, result={} expected={}" [spv, expected]))
  "TXOUT Success")
