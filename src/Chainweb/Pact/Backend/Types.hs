{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# LANGUAGE StandaloneDeriving        #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

-- |
-- Module: Chainweb.Pact.Backend.Types
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Chainweb / Pact Types module for various database backends
module Chainweb.Pact.Backend.Types
    ( CheckpointEnv(..)
    , cpeCommandConfig
    , cpeCheckpointer
    , cpeLogger
    , cpeGasEnv
    , Checkpointer(..)
    , Env'(..)
    , EnvPersist'(..)
    , PactDbBackend(..)
    , PactDbConfig(..)
    , pdbcGasLimit
    , pdbcGasRate
    , pdbcLogDir
    , pdbcPersistDir
    , pdbcPragmas
    , PactDbEnvPersist(..)
    , pdepEnv
    , pdepPactDb
    , PactDbState'(..)
    , PactDbState(..)
    , pdbsDbEnv
    , pdbsTxId
    , pdbsState
    , SaveData(..)
    , saveDataVersion
    , sTxRecord
    , sTxId
    , sSQLiteConfig
    , sCommandState
    , sPactTxId
    , usage
    ) where

import Control.Concurrent.MVar (MVar)
import Control.Lens

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Serialize

import GHC.Generics

import qualified Pact.Interpreter as P
import qualified Pact.Persist as P
import qualified Pact.Persist.Pure as P
import qualified Pact.Persist.SQLite as P
import qualified Pact.PersistPactDb as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Persistence as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Orphans ()

import System.IO.Extra

class PactDbBackend e where
    closeDb :: e -> IO (Either String ())
    prepSerialization :: PactDbEnvPersist e -> P.CommandState -> P.TxId -> IO (Maybe String, SaveData e)

instance PactDbBackend P.PureDb where
    closeDb = const $ return $ Right ()
    prepSerialization PactDbEnvPersist {..} commandState txId =
      case _pdepEnv of
        P.DbEnv {..} -> do
          let _sTxRecord = _txRecord
              _sTxId = _txId
              _sSQLiteConfig = Nothing
              _sCommandState = commandState
              _sVersion = saveDataVersion
              _sPactTxId = txId
          return (Nothing, SaveData {..})

instance PactDbBackend P.SQLite where
    closeDb = P.closeSQLite
    prepSerialization = prepSerializationSQLite

prepSerializationSQLite
    :: PactDbEnvPersist P.SQLite
    -> P.CommandState
    -> P.TxId
    -> IO (Maybe String, SaveData P.SQLite)
prepSerializationSQLite PactDbEnvPersist {..} commandState txId = do
    case _pdepEnv of
      P.DbEnv {..} -> do
        let _sTxRecord = _txRecord
            _sTxId = _txId
            _sSQLiteConfig = Just $ P.config _db
            _sCommandState = commandState
            _sPactTxId = txId
            prefix = makeFileNamePrefix
        return (Just prefix, SaveData {..})
  where
    makeFileNamePrefix = "chainweb_pact_serialize_version=" ++ saveDataVersion ++ "_"

saveDataVersion :: String
saveDataVersion = "0.0.0"

data SaveData p = SaveData
    { _sTxRecord :: M.Map P.TxTable [P.TxLog A.Value]
    , _sTxId :: Maybe P.TxId -- TODO: is this needed:
    , _sSQLiteConfig :: Maybe P.SQLiteConfig
    , _sCommandState :: P.CommandState
    , _sPactTxId :: P.TxId
    } deriving (Generic, Show)

instance Eq (SaveData p) where
  (SaveData a1 a2 a3 _ a5) == (SaveData b1 b2 b3 _ b5) =
    a1 == b1 && a2 == b2 && a3 == b3 && a5 == b5

instance Serialize (SaveData p) where
    put SaveData {..} = do
        put _sTxRecord
        put _sTxId
        put _sSQLiteConfig
        put _sCommandState
        put _sPactTxId
    get = do
        _sTxRecord <- get
        _sTxId <- get
        _sSQLiteConfig <- get
        _sCommandState <- get
        _sPactTxId <- get
        return $ SaveData {..}

data Env' =
    forall a. PactDbBackend a =>
              Env' (P.PactDbEnv (P.DbEnv a))

data PactDbEnvPersist p = PactDbEnvPersist
    { _pdepPactDb :: P.PactDb (P.DbEnv p)
    , _pdepEnv :: P.DbEnv p
    }

makeLenses ''PactDbEnvPersist

makeLenses ''SaveData

data EnvPersist' =
    forall a. PactDbBackend a =>
              EnvPersist' (PactDbEnvPersist a)

newtype PactDbState = PactDbState (MVar PactDbState')
-- data PactDbState = InMem PactDbState' | OnDisk (MVar PactDbState')

data PactDbState' = PactDbState'
    { _pdbsDbEnv :: EnvPersist'
    , _pdbsState :: P.CommandState
    , _pdbsTxId :: P.TxId   }

makeLenses ''PactDbState'

data PactDbConfig = PactDbConfig
    { _pdbcPersistDir :: Maybe FilePath
    , _pdbcLogDir :: FilePath
    , _pdbcPragmas :: [P.Pragma]
    , _pdbcGasLimit :: Maybe Int
    , _pdbcGasRate :: Maybe Int
    } deriving (Eq, Show, Generic)

instance A.FromJSON PactDbConfig

makeLenses ''PactDbConfig

usage :: String
usage =
    "Config file is YAML format with the following properties: \n\
  \persistDir - Directory for database files. \n\
  \logDir     - Directory for HTTP logs \n\
  \pragmas    - SQLite pragmas to use with persistence DBs \n\
  \gasLimit   - Gas limit for each transaction, defaults to 0 \n\
  \gasRate    - Gas price per action, defaults to 0 \n\
  \\n"

data Checkpointer = Checkpointer
    { restore :: BlockHeight -> BlockHash -> IO (Either String PactDbState)
    , restoreInitial ::IO (Either String PactDbState)
    , save :: BlockHeight -> BlockHash -> PactDbState -> IO (Either String ())
    , saveInitial :: PactDbState -> IO (Either String ())
    , discard :: BlockHeight -> BlockHash -> PactDbState -> IO (Either String ())
    }

-- functions like the ones below need to be implemented internally
-- , prepareForValidBlock :: BlockHeight -> BlockHash -> IO (Either String PactDbState)
-- , prepareForNewBlock :: BlockHeight -> BlockHash -> IO (Either String PactDbState)
data CheckpointEnv = CheckpointEnv
    { _cpeCheckpointer :: Checkpointer
    , _cpeCommandConfig :: P.CommandConfig
    , _cpeLogger :: P.Logger
    , _cpeGasEnv :: P.GasEnv
    }

makeLenses ''CheckpointEnv

_serializationTest :: IO ()
_serializationTest = do
  let savedata =
        SaveData
          M.empty
          (Just $ P.TxId 0)
          Nothing
          (P.CommandState P.initRefStore M.empty)
          (P.TxId 0) :: SaveData P.SQLite
  let test = decode $ encode savedata
  either (const $ print "error") (print . (== savedata)) test

_serializationTestFromFile :: IO ()
_serializationTestFromFile = do
  contents <- B.readFile "savedatadump.txt"
  let test :: Either String (SaveData P.SQLite)
      test = decode contents
  either (const $ print "can't decode") (const $ putStrLn "success") test

_serializationSQLConfigTest :: IO ()
_serializationSQLConfigTest = do
  let thedata =
        P.SQLiteConfig
          { P._dbFile = "/tmp/extra-dir-36571033905417/test.sqlite"
          , P._pragmas = []
          }
  case decode (encode thedata) of
    Left _ -> putStrLn "fuck"
    Right r -> print $ r == thedata

deriving instance Eq P.CommandState
deriving instance Eq P.CommandPact

-- This test works.
_serializationSaveDataFromFile :: IO ()
_serializationSaveDataFromFile = do
  let thedata =
        SaveData
          M.empty
          Nothing
          (Just
            (P.SQLiteConfig
             { P._dbFile = "/tmp/extra-dir-36571033905417/test.sqlite"
             , P._pragmas = []
             }))
          (P.CommandState P.initRefStore M.empty)
          (P.TxId 0)
      bytes = encode thedata
  withTempFile $ \file -> do
    B.writeFile file bytes
    e <- decode <$> B.readFile file
    case e of
      Left _ -> putStrLn "_serialization of \"SaveData\" test fails"
      Right r ->
        print (r == thedata)


-- _serializationCommandStateTestFromFile :: IO ()
-- _serializationCommandStateTestFromFile = do
--   contents <- B.readFile "savedatadumpcommandstate.txt"
--   let test :: Either String (P.CommandState)
--       test = decode contents

--   either (const $ print "can't decode") (const $ putStrLn "success") test

-- deriving instance Show P.SQLite
-- deriving instance Show P.TableStmts
