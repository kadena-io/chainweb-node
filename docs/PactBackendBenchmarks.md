# Batch 1: checkpointer only

## Experiment 1: Flags in master

```
fastNoJournalPragmas :: [Pragma]
fastNoJournalPragmas = [
  "synchronous = NORMAL",
  "journal_mode = WAL",
  "locking_mode = EXCLUSIVE",
  "temp_store = MEMORY",
  "auto_vacuum = FULL",
  "page_size = 8192"
  ]
```

```
open_v2 (fromString file) (collapseFlags [sqlite_open_readwrite , sqlite_open_create , sqlite_open_fullmutex]) Nothing
```

Results:

```
benchmarking checkpointer/usertable
time                 2.031 ms   (1.754 ms .. 2.204 ms)
                     0.919 R²   (0.895 R² .. 0.942 R²)
mean                 1.292 ms   (1.113 ms .. 1.467 ms)
std dev              510.7 μs   (435.4 μs .. 629.4 μs)
variance introduced by outliers: 98% (severely inflated)
```

## Experiment 2: normal open

No improvement

```
benchmarking checkpointer/usertable
time                 2.037 ms   (1.690 ms .. 2.204 ms)
                     0.917 R²   (0.887 R² .. 0.940 R²)
mean                 1.332 ms   (1.163 ms .. 1.578 ms)
std dev              511.4 μs   (421.9 μs .. 624.1 μs)
variance introduced by outliers: 98% (severely inflated)

```

## Experiment 3: normal open, pact pragmas

No improvement

```
benchmarking checkpointer/usertable
time                 2.040 ms   (1.774 ms .. 2.286 ms)
                     0.914 R²   (0.885 R² .. 0.940 R²)
mean                 1.296 ms   (1.126 ms .. 1.497 ms)
std dev              523.4 μs   (428.0 μs .. 637.5 μs)
variance introduced by outliers: 98% (severely inflated)
```

## Experiment 4: normal open, no pragmas

 No improvement! Adding "normal" pact backend to test

# Phase two: comparing to Pact backends

## Pact Pragmas

```
fastNoJournalPragmas :: [Pragma]
fastNoJournalPragmas = [
  "synchronous = OFF",
  "journal_mode = MEMORY",
  "locking_mode = EXCLUSIVE",
  "temp_store = MEMORY"
  ]
```

As shown below, these outperform the current Chainweb pragmas.

`synchronous = OFF` is a big one I would imagine vs the Chainweb pragmas; I tried
`journal_mode = WAL` a while ago and didn't see a big difference so left that alone.
`auto_vacuum = FULL` and `page_size = 8192`, dunno.


## Experiment 5: pact persist backend, pact pragmas

MUCH faster than checkpointer

```
benchmarking pact-backend/checkpointer/usertable
time                 2.092 ms   (1.845 ms .. 2.308 ms)
                     0.915 R²   (0.890 R² .. 0.939 R²)
mean                 1.332 ms   (1.146 ms .. 1.525 ms)
std dev              545.1 μs   (453.3 μs .. 644.2 μs)
variance introduced by outliers: 98% (severely inflated)

benchmarking pact-backend/pact-sqlite/usertable
time                 55.65 μs   (55.08 μs .. 56.69 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 55.40 μs   (54.94 μs .. 56.16 μs)
std dev              1.757 μs   (1.067 μs .. 2.605 μs)
variance introduced by outliers: 32% (moderately inflated)
```

## Experiment 6: pact persist backend, no pragmas

FINALLY pragmas actually mattering

```
benchmarking pact-backend/checkpointer/usertable
time                 2.028 ms   (1.670 ms .. 2.207 ms)
                     0.919 R²   (0.891 R² .. 0.942 R²)
mean                 1.322 ms   (1.137 ms .. 1.542 ms)
std dev              510.6 μs   (419.8 μs .. 614.7 μs)
variance introduced by outliers: 98% (severely inflated)

benchmarking pact-backend/pact-sqlite/usertable
time                 824.0 μs   (803.9 μs .. 842.9 μs)
                     0.988 R²   (0.974 R² .. 0.995 R²)
mean                 847.4 μs   (817.3 μs .. 900.5 μs)
std dev              133.4 μs   (86.95 μs .. 184.3 μs)
variance introduced by outliers: 89% (severely inflated)
```

## Experiment 7: pact persist backend, chainweb pragmas

Definitely slower

```
benchmarking pact-backend/checkpointer/usertable
time                 2.048 ms   (1.738 ms .. 2.229 ms)
                     0.918 R²   (0.891 R² .. 0.943 R²)
mean                 1.330 ms   (1.151 ms .. 1.527 ms)
std dev              520.4 μs   (441.7 μs .. 640.6 μs)
variance introduced by outliers: 98% (severely inflated)

benchmarking pact-backend/pact-sqlite/usertable
time                 98.95 μs   (96.97 μs .. 102.1 μs)
                     0.992 R²   (0.987 R² .. 0.998 R²)
mean                 98.88 μs   (96.63 μs .. 102.4 μs)
std dev              9.005 μs   (5.596 μs .. 13.71 μs)
variance introduced by outliers: 78% (severely inflated)
```

## Notes on Phase 2

Would like to see if open_v2 hurts or helps pact but can't crack open `initSQLite` ...

# Index problems and nested savepoints

## Fixing index on versioned tables

HUGE improvement: index on (rowkey, blockheight, txid), pact pragmas

```
benchmarking pact-backend/checkpointer/usertable
time                 80.96 μs   (80.46 μs .. 81.97 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 81.36 μs   (80.50 μs .. 82.52 μs)
std dev              3.181 μs   (2.226 μs .. 4.413 μs)
variance introduced by outliers: 41% (moderately inflated)
```

## Nested savepoints

Bracketing the benchmark in an outer savepoint actually IMPROVES performance:

```
benchmarking pact-backend/checkpointer/usertable
time                 68.97 μs   (68.39 μs .. 69.49 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 70.07 μs   (69.10 μs .. 71.65 μs)
std dev              4.127 μs   (1.893 μs .. 6.809 μs)
variance introduced by outliers: 61% (severely inflated)
```
