{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Chainweb.Pact.Utils
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact service for Chainweb

module Chainweb.Pact.Utils
    ( toEnv'
    , toEnvPersist'
    , closePactDb
    , withSQLiteDb
    ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad

import System.Directory

import Pact.Interpreter
import Pact.PersistPactDb
import Pact.Persist.SQLite (SQLite(..), closeSQLite, initSQLite, persister)
import Pact.Types.SQLite
import Pact.Types.Logger

import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Types

toEnv' :: EnvPersist' -> IO Env'
toEnv' (EnvPersist' ep') = do
    let thePactDb = _pdepPactDb $! ep'
    let theDbEnv = _pdepEnv $! ep'
    env <- mkPactDbEnv thePactDb theDbEnv
    return $! Env' env

toEnvPersist' :: Env' -> IO EnvPersist'
toEnvPersist' (Env' pactDbEnv) = do
    let mVar = pdPactDbVar $! pactDbEnv -- :: MVar (P.DbEnv a)
    !dbEnv <- readMVar $! mVar           -- :: P.DbEnv a
    let pDbEnvPersist = PactDbEnvPersist
          { _pdepPactDb = pdPactDb pactDbEnv -- :: P.PactDb (P.DbEnv a)
          , _pdepEnv = dbEnv
          }
    return $! EnvPersist' pDbEnvPersist

-- This is a band-aid solution; We're just going to close the
-- database connection here to be safe.
closePactDb :: PactDbState -> IO ()
closePactDb =  go . _pdbsDbEnv
  where
    go (EnvPersist' (PactDbEnvPersist _ dbEnv)) =
      either fail return =<< closeDb (_db dbEnv)

withSQLiteDb :: Logger -> Bool -> Maybe SQLiteConfig -> Loggers -> (PactDbEnv (DbEnv SQLite) -> IO a) -> IO a
withSQLiteDb _ _ Nothing _ = error "withSQLitedb: No SQLite config!"
withSQLiteDb initLog deleteOldFile (Just sqlc) loggers = bracket open close
  where
    open = do
      when deleteOldFile $ do
        dbExists <- doesFileExist (_dbFile sqlc)
        when dbExists $ do
          logLog initLog "INIT" "Deleting existing Pact DB File"
          removeFile (_dbFile sqlc)
      dbe <- initDbEnv loggers persister <$> initSQLite sqlc loggers
      mkPactDbEnv chainwebpactdb dbe
    -- We would want asynchronous exceptions to bubble up if any should exist.
    close env = withMVar (pdPactDbVar env) (closeSQLite . _db)
