{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.Pact.Backend.RocksDbCheckpointer
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Pact.Backend.RocksDbCheckpointer
( initRocksDbCheckpointEnv

-- * Utils
, encodeDbEnv
) where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Catch

import Data.Aeson hiding (encode, decode)
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as M
import Data.Serialize (Serialize(..), encode, decode)
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Putter)
import Data.Typeable
import Data.Word

import GHC.Generics

import Pact.Persist
import Pact.Persist.Pure
import Pact.PersistPactDb
import qualified Pact.Types.Logger as P
import qualified Pact.Types.PactValue as P
import qualified Pact.Types.Runtime as P

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Utils hiding (Codec)

import Data.CAS.RocksDB
import Data.DedupStore

-- -------------------------------------------------------------------------- --
-- Ranked Block Hash

data RankedBlockHash = RankedBlockHash
    { _rankedBlockHashHeight :: !BlockHeight
    , _rankedBlockHash :: !BlockHash
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

encodeRankedBlockHash :: MonadPut m => RankedBlockHash -> m ()
encodeRankedBlockHash (RankedBlockHash r bh) = do
    encodeBlockHeightBe r
    encodeBlockHash bh
{-# INLINE encodeRankedBlockHash #-}

decodeRankedBlockHash :: MonadGet m => m RankedBlockHash
decodeRankedBlockHash = RankedBlockHash
    <$!> decodeBlockHeightBe
    <*> decodeBlockHash
{-# INLINE decodeRankedBlockHash #-}

-- -------------------------------------------------------------------------- --
-- Pact DB Tables

checkpointerTable
    :: RocksDb
    -> P.Logger
    -> DedupStore RankedBlockHash PactDbState
checkpointerTable rdb logger = newDedupStore rdb
    (Codec encodePactDbState (decodePactDbState logger))
    (Codec (runPut . encodeRankedBlockHash) (runGet decodeRankedBlockHash))
    ["PactCheckpointer"]

-- -------------------------------------------------------------------------- --
-- PactDb

data CheckpointerStore = CheckpointerStore
    { _checkpointerStorePersisted :: !(DedupStore RankedBlockHash PactDbState)
    , _checkpointerStoreCache :: !(MVar (M.Map RankedBlockHash PactDbState))
    }

initRocksDbCheckpointEnv :: RocksDb -> P.Logger -> P.GasEnv -> IO CheckpointEnv
initRocksDbCheckpointEnv rdb logger gasEnv = do
    store <- CheckpointerStore (checkpointerTable rdb logger)
        <$> newMVar mempty
    return $!
        CheckpointEnv
            { _cpeCheckpointer =
                  Checkpointer
                      { restore = restore' store
                      , restoreInitial = restoreInitial' store
                      , save = save' store
                      , saveInitial = saveInitial' store
                      , discard = discard' store
                      }
            , _cpeLogger = logger
            , _cpeGasEnv = gasEnv
            }

-- -------------------------------------------------------------------------- --
-- Checkpoint

data EnvData = EnvData
    { _edb :: !PureDb -- TODO do we have to store the pure db?
    , _etxRecord :: !(M.Map TxTable [P.TxLog Value])
    , _etxId :: !P.TxId
    , _emode :: !(Maybe P.ExecutionMode)
    }
    deriving (Generic, ToJSON, FromJSON, Serialize)

dbEnvData :: DbEnv PureDb -> EnvData
dbEnvData e = EnvData
    { _edb = _db e
    , _etxRecord = _txRecord e
    , _etxId = _txId e
    , _emode = _mode e
    }

encodeDbEnv :: DbEnv PureDb -> B.ByteString
-- encodeDbEnv = encodeToByteString . dbEnvData
encodeDbEnv = encode . dbEnvData
{-# INLINE encodeDbEnv #-}

decodeDbEnv :: MonadThrow m => P.Logger -> B.ByteString -> m (DbEnv PureDb)
decodeDbEnv logger bytes = do
    -- d <- decodeStrictOrThrow' bytes
    d <- case decode bytes of
        Right x -> return x
        Left e -> error $ "failed to decode: " <> e
    return $ DbEnv
        { _db = _edb d
        , _persist = persister
        , _logger = logger
        , _txRecord = _etxRecord d
        , _txId = _etxId d
        , _mode = _emode d
        }

{-
data DbEnv p = DbEnv
  { _db :: p
  , _persist :: Persister p
  , _logger :: Logger
  , _txRecord :: M.Map TxTable [TxLog Value]
  , _txId :: TxId
  , _mode :: Maybe ExecutionMode
  }
-}

-- -------------------------------------------------------------------------- --
--

restore' :: CheckpointerStore -> BlockHeight -> BlockHash -> IO (Either String PactDbState)
restore' (CheckpointerStore store cache) height hash = do
    readMVar cache >>= \m -> case M.lookup key m of
        Just x -> return $ Right x
        Nothing -> dedupLookup store key >>= \case
            Just dbstate -> return $! Right $! dbstate
            Nothing -> return $! Left $!
                "InMemoryCheckpointer: Restore not found: height=" <> show height
                <> ", hash=" <> show hash
                -- <> ", known=" <> show (HMS.keys store)
  where
    key = RankedBlockHash height hash

restoreInitial' :: CheckpointerStore -> IO (Either String PactDbState)
restoreInitial' store = restore' store (BlockHeight 0) nullBlockHash

saveInitial' :: CheckpointerStore -> PactDbState -> IO (Either String ())
saveInitial' store p = save' store (BlockHeight 0) nullBlockHash p

save' :: CheckpointerStore -> BlockHeight -> BlockHash -> PactDbState -> IO (Either String ())
save' (CheckpointerStore store cache) height hash p = do
    -- B8.putStrLn $ encodePactDbState p
    readMVar cache >>= \m -> case M.member key m of
        True -> return $ Right ()
        False -> do
            dedupInsert store key p
            modifyMVar_ cache $ \m' -> evaluate (M.drop (M.size m - maxSize) $ M.insert key p m')
            return $ Right ()
  where
    key = RankedBlockHash height hash
    maxSize = 10

discard' :: CheckpointerStore -> PactDbState -> IO (Either String ())
discard' _ _ = return (Right ())

-- -------------------------------------------------------------------------- --
-- Encode Pact DB State

pactDbStatePureEnv :: PactDbState -> DbEnv PureDb
pactDbStatePureEnv (PactDbState (EnvPersist' p)) = case cast p of
    Just ((p' :: PactDbEnvPersist PureDb)) -> _pdepEnv p'
    Nothing -> error "only PureDb persistence is supported by the rocksdb checkpointer"

encodePactDbState :: PactDbState -> B.ByteString
encodePactDbState = encodeDbEnv . pactDbStatePureEnv
{-# INLINE encodePactDbState #-}

decodePactDbState :: MonadThrow m => P.Logger -> B.ByteString -> m PactDbState
decodePactDbState logger bytes = PactDbState . EnvPersist' <$> do
    e <- decodeDbEnv logger bytes
    return $ PactDbEnvPersist
        { _pdepPactDb = pactdb
        , _pdepEnv = e
        }

{-
data PactDbState = PactDbState
    { _pdbsDbEnv :: EnvPersist' }

data EnvPersist' = forall a. EnvPersist' (PactDbEnvPersist a)

data PactDbEnvPersist p = PactDbEnvPersist
    { _pdepPactDb :: P.PactDb (P.DbEnv p)
    , _pdepEnv :: P.DbEnv p
    }
-}

-- -------------------------------------------------------------------------- --
-- Orphans

deriving instance Generic Db
deriving instance ToJSON Db
deriving instance FromJSON Db
deriving instance Serialize Db

instance ToJSON P.ExecutionMode where
    toJSON P.Transactional = "transactional"
    toJSON P.Local = "local"

instance FromJSON P.ExecutionMode where
    parseJSON = withText "ExecutionMode:" $ \case
        "transaction" -> return P.Transactional
        "local" -> return P.Local
        e -> fail $ "unknown execution mode " <> show e

instance Serialize P.ExecutionMode where
    put P.Transactional = putWord8 0x0
    put P.Local = putWord8 0x1

    get = getWord8 >>= \case
        0x0 -> return P.Transactional
        _ -> return P.Local

deriving instance Generic PureDb
deriving instance ToJSON PureDb
deriving instance FromJSON PureDb
deriving instance Serialize PureDb

deriving newtype instance ToJSON TxKey
deriving newtype instance ToJSON DataKey
deriving newtype instance ToJSONKey TxKey
deriving newtype instance ToJSONKey DataKey
deriving newtype instance ToJSON (Tables TxKey)
deriving newtype instance ToJSON (Tables DataKey)
deriving newtype instance ToJSON (Tbl TxKey)
deriving newtype instance ToJSON (Tbl DataKey)

deriving newtype instance FromJSON TxKey
deriving newtype instance FromJSON DataKey
deriving newtype instance FromJSONKey TxKey
deriving newtype instance FromJSONKey DataKey
deriving newtype instance FromJSON (Tables TxKey)
deriving newtype instance FromJSON (Tables DataKey)
deriving newtype instance FromJSON (Tbl TxKey)
deriving newtype instance FromJSON (Tbl DataKey)

deriving newtype instance Serialize TxKey
deriving newtype instance Serialize DataKey
deriving newtype instance Serialize (Tables TxKey)
deriving newtype instance Serialize (Tables DataKey)
deriving newtype instance Serialize (Tbl TxKey)
deriving newtype instance Serialize (Tbl DataKey)

instance Serialize (P.TxLog Value)
instance Serialize P.TxId

-- instance ToJSON PValue where
--     toJSON (PValue a) = toJSON a

instance ToJSON PValue where
    toJSON (PValue a)
        | t == typeRep (Proxy @Value) = v "value" a
        | t == typeRep (Proxy @P.Namespace) = v "namespace" a
        | t == typeRep (Proxy @P.KeySet) = v "keyset" a
        | t == typeRep (Proxy @P.PersistModuleData) = v "moduledata" a
        | t == typeRep (Proxy @(P.ObjectMap P.PactValue)) = v "pactvalue" a
        | t == typeRep (Proxy @UserTableInfo) = v "usertableinfo" a
        | t == typeRep (Proxy @[P.TxLog Value]) = v "txlogvalue" a
        | t == typeRep (Proxy @(Maybe P.PactExec)) = v "pactexec" a
        | otherwise = error $ "unrecognized pact type: " <> show t
      where
        t = typeOf a

        v :: ToJSON a => String -> a -> Value
        v ty x = object [ "type" .= ty , "value" .= x]

instance FromJSON PValue where
    parseJSON = withObject "PValue" $ \o -> (o .: "type") >>= \case
        ("value" :: String) -> PValue @Value <$> (o .: "value")
        ("namespace" :: String) -> PValue @P.Namespace <$> (o .: "value")
        ("keyset" :: String) -> PValue @P.KeySet <$> (o .: "value")
        ("moduledata" :: String) -> PValue @P.PersistModuleData <$> (o .: "value")
        ("pactvalue" :: String) -> PValue @(P.ObjectMap P.PactValue) <$> (o .: "value")
        ("usertableinfo" :: String) -> PValue @UserTableInfo <$> (o .: "value")
        ("txlogvalue" :: String) -> PValue @[P.TxLog Value] <$> (o .: "value")
        ("pactexec" :: String) -> PValue @(Maybe P.PactExec) <$> (o .: "value")
        e -> fail $ "unrecognized pact type: " <> sshow e

instance Serialize PValue where

    put (PValue a)
        | t == typeRep (Proxy @Value) = v 0 a
        | t == typeRep (Proxy @P.Namespace) = v 1 a
        | t == typeRep (Proxy @P.KeySet) = v 2 a
        | t == typeRep (Proxy @P.PersistModuleData) = v 3 a
        | t == typeRep (Proxy @(P.ObjectMap P.PactValue)) = v 4 a
        | t == typeRep (Proxy @UserTableInfo) = v 5 a
        | t == typeRep (Proxy @[P.TxLog Value]) = v 6 a
        | t == typeRep (Proxy @(Maybe P.PactExec)) = v 7 a
        | otherwise = error $ "unrecognized pact type: " <> show t
      where
        t = typeOf a
        v :: ToJSON a => Word8 -> Putter a
        v ty x = put ty >> put (encodeToByteString x)

    get = getWord8 >>= \case
        0 -> PValue @Value <$> v
        1 -> PValue @P.Namespace <$> v
        2 -> PValue @P.KeySet <$> v
        3 -> PValue @P.PersistModuleData <$> v
        4 -> PValue @(P.ObjectMap P.PactValue) <$> v
        5 -> PValue @UserTableInfo <$> v
        6 -> PValue @[P.TxLog Value] <$> v
        7 -> PValue @(Maybe P.PactExec) <$> v
        e -> fail $ "unrecognized pact type: " <> sshow e
      where
        v :: FromJSON a => Get a
        v = get >>= \x -> case eitherDecodeStrict' x of
            Right r -> return r
            Left e -> fail e

{-
data Table k where
  DataTable :: !TableId -> DataTable
  TxTable :: !TableId -> TxTable
  deriving (Generic)

data PValue = forall a . (PactDbValue a) => PValue a

instance PactDbValue v => PactDbValue (TxLog v) where
instance PactDbValue (ObjectMap PactValue) where
instance PactDbValue a => PactDbValue [a] where
instance PactDbValue PersistModuleData where
instance PactDbValue KeySet where
instance PactDbValue Value where
instance PactDbValue Namespace where
instance PactDbValue (Maybe PactExec) where
-}

instance ToJSON (Table DataKey) where
    toJSON (DataTable (TableId k)) = toJSON k

instance ToJSON (Table TxKey) where
    toJSON (TxTable (TableId k)) = toJSON k

instance FromJSON (Table DataKey) where
    parseJSON = withText "DataTable" $ return . DataTable . TableId

instance FromJSON (Table TxKey) where
    parseJSON = withText "TxTable" $ return . TxTable . TableId

instance Serialize (Table TxKey) where
    put (TxTable (TableId k)) = put k
    get = TxTable . TableId <$> get

instance Serialize (Table DataKey) where
    put (DataTable (TableId k)) = put k
    get = DataTable . TableId <$> get

instance ToJSONKey DataTable
instance ToJSONKey TxTable
instance FromJSONKey DataTable
instance FromJSONKey TxTable

