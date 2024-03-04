Pact Database ("Checkpointer") Schema and Overview
==========

Overview of Versioned Table Maintenance and Schema documentation for Checkpointer Pact DB implementation in SQLite.

Checkpointer Operations
===

See table schemas below for tables referenced in this overview.

## Restore/Rewind

Restore positions the checkpointer to be able to accept a new block at a particular
height. If this height is not exactly 1 higher than the highest block stored in
the `BlockHistory` table, then a rewind occurs to delete any data after the rewind point.

If no rewind occurs, there are no modifications to the database state for restore.

The `TxId` counter is reset to the ending `TxId` for rewind height - 1 from `BlockHistory`.

Rewind invokes the following db operations:

 - Get ending `TxId` for rewind height - 1 from `BlockHistory`.
 - Delete all rows from versioned system tables higher than ending tx id.
 - Drop all tables in `VersionedTableCreation` at rewind height or higher.
 - Delete all rows higher than ending tx id from versioned user tables in
   `VersionedTableMutation` at rewind height or higher.
 - Delete all rows from `VersionedTableCreation`, `VersionedTableMutation`,
   `BlockHistory`, `TransactionIndex` at rewind height or higher.


## Save

Save reads the `TxId` counter and inserts a new row into `BlockHistory` with the
current block height, block hash and `TxId` counter value.

## Transaction execution

During transaction execution the following db operations occur:

 - Table creation inserts a row in `VersionedTableCreation` at the current block height.
 - Writes to a table row inserts a row in `VersionedTableMutation` at the current block height.
 - All writes record the txid at which they occurred.
 - Transaction completion during block validation results in the transaction hash being recorded
   at the current block height in `TransactionIndex`.


Checkpointer System Tables
===

## BlockHistory table

Tracks the latest block (by hash) and ending `TxId` for a given block height.
Rewinds delete from rewind block height upward.

```sql
CREATE TABLE BlockHistory

  ( blockheight UNSIGNED BIGINT NOT NULL
  , hash BLOB NOT NULL
  , endingtxid UNSIGNED BIGINT NOT NULL

  , CONSTRAINT blockHashConstraint UNIQUE (blockheight)

  );
```

## VersionedTableCreation table

Records any user tables created in a block. This allows rewinds to drop
tables that were created after the rewind point.

```sql
CREATE TABLE VersionedTableCreation

  ( tablename TEXT NOT NULL
  , createBlockheight UNSIGNED BIGINT NOT NULL

  , CONSTRAINT creation_unique UNIQUE(createBlockheight, tablename)

  );
```

## VersionedTableMutation table

Records any user tables that were modified in a block. This optimizes
row deletion during a rewind to just those tables that were actually
touched after the rewind point.

```sql
CREATE TABLE VersionedTableMutation

  ( tablename TEXT NOT NULL
  , blockheight UNSIGNED BIGINT NOT NULL

  , CONSTRAINT mutation_unique UNIQUE(blockheight, tablename)

  );
```

## TransactionIndex table

Records transactions executed in a block by block height.
Rewinds delete from rewind block height upward.
Used by mempool to track duplicate transactions.

```sql
CREATE TABLE TransactionIndex
  ( txhash BLOB NOT NULL
  , blockheight UNSIGNED BIGINT NOT NULL

  , CONSTRAINT transactionIndexConstraint UNIQUE(txhash)

  );

CREATE INDEX transactionIndexByBH
  ON TransactionIndex(blockheight);

```


Versioned Pact DB User and System tables
===

All other tables are specified by the `PactDB` service provider
interface in the Pact runtime and are considered "Versioned Tables",
meaning they are subject to data management by the checkpointer.

## Versioned Table Schema

All versioned tables have an identical schema, indexed to facilitate
rewind deletions on `txid`, and queries on `rowkey` at a particular
`txid`. The "current value" of a key is the `rowdata` at the maximum
value for `txid` for that key.

```sql
CREATE TABLE IF NOT EXISTS $table

  ( rowkey TEXT
  , txid UNSIGNED BIGINT NOT NULL
  , rowdata BLOB NOT NULL

  , UNIQUE (rowkey, txid)
  );

  CREATE INDEX IF NOT EXISTS $table_ix
    ON $table (txid DESC);

```

## Pact System Tables

The following tables are created at system initialization and thus
effectively at "genesis", and as such are never deleted/rewound:

```
SYS:KeySets
SYS:Modules
SYS:Namespaces
SYS:Pacts
```

## Pact User Table naming

User tables are named incorporating the fully-qualified module name
and the tablename in the format `$MODULE_$TABLE`. Thus the KDA coin
ledger is stored as `coin_coin-table`, while a namespaced module `somens.foo`
with a table `tbl` would be stored as `somens.foo_tbl`.
