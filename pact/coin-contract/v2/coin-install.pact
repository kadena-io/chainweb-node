(create-table coin-table)
(create-table allocation-table)
(enforce
  (=
    "ut_J_ZNkoyaPUEJhiwVeWnkSQn9JT9sQCWKdjjVVrWo"
    (at 'hash (describe-module 'coin)))
  "hash mismatch")
