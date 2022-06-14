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

import Pact.Types.Logger
import Pact.Types.SQLite


compact :: Logger -> BlockHeight -> Database -> IO ()
compact log' blockheight db = do


    initialize

    vtables <- collectVersionedTables

    mapM_ computeTableHash vtables

    return ()
  where
    initialize = do
      debug "initialize"
      exec_ db $ "CREATE TABLE IF NOT EXISTS VersionedTableChecksum " <>
          "( tablename TEXT NOT NULL " <>
          ", blockheight UNSIGNED BIGINT NOT NULL " <>
          ", hash BLOB " <>
          ", UNIQUE (tablename) );"
      exec_ db $ "DELETE FROM VersionedTableChecksum"

    collectVersionedTables = do
      debug "collectVersionedTables"
      rs <- qry db
          ("SELECT DISTINCT tablename FROM VersionedTableMutation " <>
           "WHERE blockheight <= ? ORDER BY tablename")
          [SInt $ fromIntegral blockheight]
          [RText]
      forM rs $ \r -> case r of
        [SText n] -> return n
        _ -> internalError "collectVersionedTables: expected text"

    computeTableHash vtable = do
      debug $ "computeTableHash: " ++ show vtable
      let stmt = "INSERT INTO VersionedTableChecksum VALUES (?1, ?2, " <>
           "(SELECT sha3a_256(hash) FROM " <>
           " (SELECT hash FROM " <> tbl vtable <>
           "  t1 where txid=(select max(txid) from " <> tbl vtable <>
           "  t2 where t2.rowkey=t1.rowkey and t2.txid<?2))))"
      exec' db stmt
          [SText vtable,SInt $ fromIntegral blockheight]

    debug = logDebug_ log'
