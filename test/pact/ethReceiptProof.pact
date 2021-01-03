(let* (
  (spv (verify-spv 'ETH (read-msg)))
  (expected
    { "depth": 0
    , "header":
       { "difficulty": "0xbfabcdbd93dda"
        , "extra-data": "0x737061726b706f6f6c2d636e2d6e6f64652d3132"
        , "gas-limit": 7992222
        , "gas-used": 7982291
        , "hash": "0xb3b20624f8f0f86eb50dd04688409e5cea4bd02d700bf6e79e9384d47d6a5a35"
        , "miner": "0x5a0b54d5dc17e0aadc383d2db43b0a0d3e029c4c"
        , "mix-hash": "0x3d1fdd16f15aeab72e7db1013b9f034ee33641d92f71c0736beab4e67d34c7a7"
        , "nonce": "0x4db7a1c01d8a8072"
        , "number": 6008149
        , "parent-hash": "0x61a8ad530a8a43e3583f8ec163f773ad370329b2375d66433eb82f005e1d6202"
        , "receipts-root": "0x5eced534b3d84d3d732ddbc714f5fd51d98a941b28182b6efe6df3a0fe90004b"
        , "sha3-uncles": "0x8a562e7634774d3e3a36698ac4915e37fc84a2cd0044cb84fa5d80263d2af4f6"
        , "state-root": "0xf5208fffa2ba5a3f3a2f64ebd5ca3d098978bedd75f335f56b705d8715ee2305"
        , "timestamp": 1532236873
        , "transactions-root": "0xf98631e290e88f58a46b7032f025969039aa9b5696498efc76baf436fa69b262"
       }
    , "index": 2
    , "receipt":
       { "cumulative-gas-used": 93125
        , "logs":
          [ { "address": "0x818fc6c2ec5986bc6e2cbf00939d90556ab12ce5"
             , "data": "0x00000000000000000000000000000000000000000007bcadb57b861109080000" ;; ERC-20 amount
             , "topics":
               [ "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef" ;; ERC-20 transfer
                 "0x000000000000000000000000532a2bae845abe7e5115808b832d34f9c3d41eed" ;; ERC-20 from
                 "0x000000000000000000000000398a58b2e3790431fdac1ea56017e65401fa9988" ;; ERC-20 to
               ]
             }
          ]
        , "status": true
      }
      , "root": "0xb3b20624f8f0f86eb50dd04688409e5cea4bd02d700bf6e79e9384d47d6a5a35"
      , "weight": 0
    })
    (erc-20-amount (at 'data (at 0 (at 'logs (at 'receipt spv)))))
    (amount (str-to-int 16 (drop 2 erc-20-amount)))
    (expected-amount 9353490000000000000000000)
  )
  (enforce
    (= spv expected)
    (format "Failure, result={} expected={}"
            [spv, expected]))

  (enforce (= amount expected-amount)
    (format "Failure, expected {}, got {}, {}"
            [expected-amount, erc-20-amount, amount]))

  "ETH Success")
