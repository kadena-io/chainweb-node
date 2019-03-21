{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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
    ) where

import Control.Lens

import qualified Data.Aeson as A
import qualified Data.ByteString as B (readFile, writeFile)
import Data.Bytes.Serial
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.Map as M

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

import System.IO.Extra

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Orphans ()

class PactDbBackend e where
    closeDb :: e -> IO (Either String ())
    saveDb :: PactDbEnvPersist e -> P.CommandState -> P.TxId -> IO (Maybe String, SaveData e)
    -- TODO: saveDb needs a better name

instance PactDbBackend P.PureDb where
    closeDb = const $ return $ Right ()
    saveDb PactDbEnvPersist {..} commandState txId =
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
    saveDb = saveSQLite

saveSQLite
    :: PactDbEnvPersist P.SQLite
    -> P.CommandState
    -> P.TxId
    -> IO (Maybe String, SaveData P.SQLite)
saveSQLite PactDbEnvPersist {..} commandState txId = do
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
    makeFileNamePrefix = "chainweb_pact_serialize_version=" ++ map go saveDataVersion ++ "_"
      where
        go x
           | x == '.' = '-'
           | otherwise = x

saveDataVersion :: String
saveDataVersion = "0.0.0"

data SaveData p = SaveData
    { _sTxRecord :: M.Map P.TxTable [P.TxLog A.Value]
    , _sTxId :: Maybe P.TxId -- TODO: is this needed:
    , _sSQLiteConfig :: Maybe P.SQLiteConfig
    , _sCommandState :: P.CommandState
    , _sPactTxId :: P.TxId
    } deriving (Generic)

instance Serial (SaveData p)

-- instance Serialize (SaveData p) where
--     put SaveData {..} = do
--         put _sTxRecord
--         put _sTxId
--         put _sSQLiteConfig
--         put _sCommandState
--         put _sPactTxId
--     get = do
--         _sTxRecord <- get
--         _sTxId <- get
--         _sSQLiteConfig <- get
--         _sCommandState <- get
--         _sPactTxId <- get
--         return $ SaveData {..}

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

data PactDbState = PactDbState
    { _pdbsDbEnv :: EnvPersist'
    , _pdbsState :: P.CommandState
    , _pdbsTxId :: P.TxId   }

makeLenses ''PactDbState

data PactDbConfig = PactDbConfig
    { _pdbcPersistDir :: Maybe FilePath
    , _pdbcLogDir :: FilePath
    , _pdbcPragmas :: [P.Pragma]
    , _pdbcGasLimit :: Maybe Int
    , _pdbcGasRate :: Maybe Int
    } deriving (Eq, Show, Generic)

instance A.FromJSON PactDbConfig

makeLenses ''PactDbConfig

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

_serializationSaveDataFromFile :: IO ()
_serializationSaveDataFromFile = do
  let thedata = SaveData
                 M.empty
                 Nothing
                 (Just
                     (P.SQLiteConfig
                         { P._dbFile = "/tmp/extra-dir-36571033905417/test.sqlite"
                         , P._pragmas = []
                         }))
                 (P.CommandState P.initRefStore M.empty)
                 (P.TxId 0)
      bytes = runPutS (serialize thedata)
  withTempFile $ \file -> do
    B.writeFile file bytes
    e <- runGetS deserialize <$> B.readFile file
    case e of
        Left _ -> putStrLn "_serializaiton of \"SaveData\" test fails"
        Right r ->
            print (checkEq r thedata)
  where
    checkEq :: SaveData p -> SaveData p -> Bool
    checkEq a b = _sTxRecord a == _sTxRecord b
                && _sTxId a  == _sTxId b
                && _sSQLiteConfig a == _sSQLiteConfig b
                && eqCommandState (_sCommandState a) (_sCommandState b)
                && _sPactTxId a == _sPactTxId b
    eqCommandState a b = P._csRefStore a == P._csRefStore b && P._csPacts a == P._csPacts b
