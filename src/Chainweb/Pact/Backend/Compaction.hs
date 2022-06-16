{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: Chainweb.Pact.Backend.Compaction
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: see LICENSE.md
-- Maintainer: stuart@kadena.io
-- Stability: experimental
--

module Chainweb.Pact.Backend.Compaction

  where

import Control.Monad

import Database.SQLite3.Direct

import Chainweb.BlockHeight
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Utils

import Pact.Types.Logger
import Pact.Types.SQLite


compact :: Logger -> BlockHeight -> Database -> IO ()
compact log' blockheight' db = do


    txid <- initialize

    vtables <- collectVersionedTables

    mapM_ (computeTableHash txid) vtables

    computeGlobalHash

    forM_ vtables $ \t -> do
      compactTable t
      verifyTable t

    return ()
  where
    blockheight = SInt $ fromIntegral blockheight'
    debug = logDebug_ log'

    initialize = do
      debug "initialize"
      exec_ db $
          "CREATE TABLE IF NOT EXISTS VersionedTableChecksum " <>
          "( tablename TEXT " <> -- NULL is global checksum
          ", blockheight UNSIGNED BIGINT NOT NULL " <>
          ", hash BLOB " <>
          ", UNIQUE (tablename) );"

      exec_ db $ "DELETE FROM VersionedTableChecksum"

      exec_ db $
          "CREATE TABLE IF NOT EXISTS ActiveVersion " <>
          "( tablename TEXT NOT NULL " <>
          ", rowkey TEXT NOT NULL " <>
          ", vrowid INTEGER NOT NULL " <>
          ", hash BLOB " <>
          ", UNIQUE (tablename,rowkey) );"

      exec_ db $ "DELETE FROM ActiveVersion"

      qry db "SELECT endingtxid FROM BlockHistory WHERE blockheight=?"
         [blockheight] [RInt] >>= \r ->
        case r of
          [] -> internalError "initialize: invalid target block height"
          [[SInt txid]] -> return txid
          _ -> internalError "initialize: expected single-row int"


    collectVersionedTables = do
      debug "collectVersionedTables"
      rs <- qry db
          ("SELECT DISTINCT tablename FROM VersionedTableMutation " <>
           "WHERE blockheight <= ? ORDER BY tablename")
          [blockheight]
          [RText]
      forM rs $ \r -> case r of
        [SText n] -> return n
        _ -> internalError "collectVersionedTables: expected text"

    computeTableHash txid vtable = do
      debug $ "computeTableHash:insert " ++ show vtable
      let stmt1 =
            "INSERT INTO ActiveVersion " <>
            "SELECT ?1,rowkey,rowid,hash FROM " <> tbl vtable <>
            " t1 WHERE txid=(select max(txid) from " <> tbl vtable <>
            "  t2 where t2.rowkey=t1.rowkey and t2.txid<?2)" <>
            " GROUP BY rowkey"
      exec' db stmt1 [SText vtable,SInt txid]
      debug $ "computeTableHash:checksum " ++ show vtable

      let stmt2 =
            "INSERT INTO VersionedTableChecksum VALUES (?1, ?2, " <>
            " (SELECT sha3a_256(hash) FROM ActiveVersion " <>
            "  WHERE tablename=?1 ORDER BY rowkey))"
      exec' db stmt2
          [SText vtable,blockheight]

    computeGlobalHash = do
      debug "computeGlobalHash"
      exec' db
          ("INSERT INTO VersionedTableChecksum VALUES (NULL,?1," <>
           "(SELECT sha3a_256(hash) FROM VersionedTableChecksum " <>
           " WHERE tablename IS NOT NULL ORDER BY tablename))")
          [blockheight]

    compactTable vtable = do
      debug $ "compactTable: " ++ show vtable
      let stmt =
            "DELETE FROM " <> tbl vtable <>
            " WHERE rowid NOT IN (SELECT t.rowid FROM " <> tbl vtable <>
            " t LEFT JOIN ActiveVersion v WHERE t.rowid = v.vrowid AND v.tablename=?1)"
      exec' db stmt [SText vtable]

    verifyTable vtable = do
      debug $ "verifyTable: " ++ show vtable
      rs <- qry db
          ("SELECT hash FROM VersionedTableChecksum WHERE tablename=?1 " <>
           "UNION ALL " <>
           "SELECT sha3a_256(hash) FROM (SELECT hash from " <> tbl vtable <>
           " t1 WHERE txid=(select max(txid) from " <> tbl vtable <>
           " t2 where t2.rowkey=t1.rowkey) GROUP BY rowkey)" )
          [SText vtable]
          [RBlob]
      case rs of
        [[SBlob prev],[SBlob curr]] | prev == curr -> return ()
        _ -> internalError $ "Table verification failed! " <> sshow (vtable,rs)
