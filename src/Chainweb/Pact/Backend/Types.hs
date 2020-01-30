{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
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
    , cpeCheckpointer
    , cpeLogger
    , Checkpointer(..)
    , PactDbEnv'(..)
    , SQLiteRowDelta(..)
    , SQLiteDeltaKey(..)
    , SQLitePendingTableCreations
    , SQLitePendingWrites
    , SQLitePendingData(..)
    , pendingTableCreation
    , pendingWrites
    , pendingTxLogMap
    , pendingSuccessfulTxs
    , emptySQLitePendingData
    , BlockImage(..)
    , BlockMap(..)
    , blockMapInsert
    , blockMapEmpty
    , blockMapLookup
    , BlockState(..)
    , initBlockState
    , bsBlockHeight
    , bsMode
    , bsTxId
    , bsPendingBlock
    , bsPendingTx
    , bsBlockMap
    , bsParent
    , BlockEnv(..)
    , benvBlockState
    , benvDb
    , SQLiteEnv(..)
    , sConn
    , sConfig
    , BlockHandler(..)
    , ParentHash
    , BlockDbEnv(..)
    , bdbenvDb
    , SQLiteFlag(..)

      -- * mempool
    , MemPoolAccess(..)

    , PactServiceException(..)

      -- * PactDbValue
    , PactDbValue
    , PValue(..)
    , castData
    ) where

import Control.Arrow
import Control.Exception
import Control.Exception.Safe hiding (bracket)
import Control.Lens
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Aeson
import Data.Bits
import Data.ByteString (ByteString)
import Data.DList (DList)
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Tuple.Strict
import Data.Typeable (cast)
import Data.Vector (Vector)

import Database.SQLite3.Direct as SQ3

import Foreign.C.Types (CInt(..))

import GHC.Generics

import Pact.Interpreter (PactDbEnv(..))
import Pact.Persist (PactDbValue(..))
import Pact.Persist.Pure (PValue(..))
import Pact.Persist.SQLite (SQLiteConfig(..))
import qualified Pact.Types.Hash as P
import Pact.Types.Logger (Logger(..), Logging(..))
import Pact.Types.Runtime

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Mempool.Mempool (MempoolPreBlockCheck)
import Chainweb.Transaction


-- | Within a @restore .. save@ block, mutations to the pact database are held
-- in RAM to be written to the DB in batches at @save@ time. For any given db
-- write, we need to record the table name, the current tx id, the row key, and
-- the row value.
--

castData :: PactDbValue o => PValue -> Maybe o
castData (PValue o) = cast o


data SQLiteRowDelta = SQLiteRowDelta
    { _deltaTableName :: !ByteString -- utf8?
    , _deltaTxId :: {-# UNPACK #-} !TxId
    , _deltaRowKey :: !ByteString
    , _deltaData :: !PValue
    } -- deriving (Show, Generic, Eq)
instance Show SQLiteRowDelta where
  show SQLiteRowDelta{..} =
    "SQLiteRowDelta {_deltaTableName=" ++ show _deltaTableName
    ++ ", _deltaTxId=" ++ show _deltaTxId
    ++ ", _deltaRowKey=" ++ show _deltaRowKey
instance Eq SQLiteRowDelta where
  a == b = _deltaTableName a == _deltaTableName b &&
    _deltaTxId a == _deltaTxId b &&
    _deltaRowKey a == _deltaRowKey b

instance Ord SQLiteRowDelta where
    compare a b = compare aa bb
      where
        aa = (_deltaTableName a, _deltaRowKey a, _deltaTxId a)
        bb = (_deltaTableName b, _deltaRowKey b, _deltaTxId b)
    {-# INLINE compare #-}

-- | When we index 'SQLiteRowDelta' values, we need a lookup key.
data SQLiteDeltaKey = SQLiteDeltaKey
    { _dkTable :: !ByteString
    , _dkRowKey :: !ByteString
    }
  deriving (Show, Generic, Eq, Ord)
  deriving anyclass Hashable

-- | A map from table name to a list of 'TxLog' entries. This is maintained in
-- 'BlockState' and is cleared upon pact transaction commit.
type TxLogMap = Map TableName (DList (TxLog Value))

-- | Between a @restore..save@ bracket, we also need to record which tables
-- were created during this block (so the necessary @CREATE TABLE@ statements
-- can be performed upon block save).
type SQLitePendingTableCreations = HashSet ByteString

-- | Pact transaction hashes resolved during this block.
type SQLitePendingSuccessfulTxs = HashSet ByteString

-- | Pending writes to the pact db during a block, to be recorded in 'BlockState'.
type SQLitePendingWrites = HashMap SQLiteDeltaKey (DList SQLiteRowDelta)

-- | A collection of pending mutations to the pact db. We maintain two of
-- these; one for the block as a whole, and one for any pending pact
-- transaction. Upon pact transaction commit, the two 'SQLitePendingData'
-- values are merged together.
data SQLitePendingData = SQLitePendingData
    { _pendingTableCreation :: !SQLitePendingTableCreations
    , _pendingWrites :: !SQLitePendingWrites
    , _pendingTxLogMap :: !TxLogMap
    , _pendingSuccessfulTxs :: !SQLitePendingSuccessfulTxs
    }
    deriving (Show)

makeLenses ''SQLitePendingData

data SQLiteEnv = SQLiteEnv
    { _sConn :: !Database
    , _sConfig :: !SQLiteConfig
    }

makeLenses ''SQLiteEnv

data BlockImage = BlockImage
  { _biBlockHeight :: !BlockHeight
  , _biBlockHash :: !BlockHash
  , _biParent :: !(Maybe BlockImage)
  , _biData :: !SQLitePendingData
  , _biTxId :: !TxId
  } deriving (Show)

instance Eq BlockImage where
  (==) = (==) `on` (_biBlockHeight &&& _biBlockHash)

instance Ord BlockImage where
  compare = compare `on` (_biBlockHeight &&& _biBlockHash)

newtype BlockMap = BlockMap { _blockMap :: Map BlockHeight (Map BlockHash BlockImage) }
  deriving (Show)

blockMapInsert :: BlockImage -> BlockMap -> BlockMap
blockMapInsert bi@BlockImage{..} = BlockMap . M.alter upd _biBlockHeight . _blockMap
  where upd = maybe (Just $ M.singleton _biBlockHash bi) $
              Just . M.insert _biBlockHash bi

blockMapEmpty :: BlockMap
blockMapEmpty = BlockMap mempty

blockMapLookup :: BlockHeight -> BlockHash -> BlockMap -> Maybe BlockImage
blockMapLookup bhi bha = join . fmap (M.lookup bha) . M.lookup bhi . _blockMap


-- | Monad state for 'BlockHandler.
data BlockState = BlockState
    { _bsTxId :: !TxId
    , _bsMode :: !(Maybe ExecutionMode)
    , _bsBlockHeight :: !BlockHeight
    , _bsPendingBlock :: !SQLitePendingData
    , _bsPendingTx :: !(Maybe SQLitePendingData)
    , _bsBlockMap :: !BlockMap
    , _bsParent :: Maybe BlockImage
    }
    deriving Show

emptySQLitePendingData :: SQLitePendingData
emptySQLitePendingData = SQLitePendingData mempty mempty mempty mempty

initBlockState :: BlockState
initBlockState = BlockState 0 Nothing 0 emptySQLitePendingData Nothing blockMapEmpty Nothing

makeLenses ''BlockState

data BlockDbEnv p = BlockDbEnv
    { _bdbenvDb :: !p
    , _logger :: !Logger
    }

makeLenses ''BlockDbEnv

data BlockEnv p = BlockEnv
    { _benvDb :: !(BlockDbEnv p)
    , _benvBlockState :: !BlockState -- ^ The current block state.
    }

makeLenses ''BlockEnv

newtype BlockHandler p a = BlockHandler
    { runBlockHandler :: ReaderT (BlockDbEnv p) (StateT BlockState IO) a
    } deriving newtype ( Functor
                       , Applicative
                       , Monad
                       , MonadState BlockState
                       , MonadThrow
                       , MonadCatch
                       , MonadMask
                       , MonadIO
                       , MonadReader (BlockDbEnv p)
                       , MonadFail
                       )




data PactDbEnv' = forall e. PactDbEnv' (PactDbEnv e)

instance Logging (BlockHandler p) where
    log c s = view logger >>= \l -> liftIO $ logLog l c s

type ParentHash = BlockHash

data Checkpointer = Checkpointer
    {
      _cpRestore :: !(Maybe (BlockHeight, ParentHash) -> IO PactDbEnv')
      -- ^ prerequisite: (BlockHeight - 1, ParentHash) is a direct ancestor of
      -- the "latest block"
    , _cpSave :: !(BlockHash -> IO ())
      -- ^ commits pending modifications to block, with the given blockhash
    , _cpDiscard :: IO ()
      -- ^ discard pending block changes
    , _cpGetLatestBlock :: IO (Maybe (BlockHeight, BlockHash))
      -- ^ get the checkpointer's idea of the latest block
    , _cpBeginCheckpointerBatch :: IO ()
    , _cpCommitCheckpointerBatch :: IO ()
    , _cpDiscardCheckpointerBatch :: IO ()
    , _cpLookupBlockInCheckpointer :: !((BlockHeight, BlockHash) -> IO Bool)
      -- ^ is the checkpointer aware of the given block?
    , _cpGetBlockParent :: !((BlockHeight, BlockHash) -> IO (Maybe BlockHash))
    , _cpRegisterProcessedTx :: !(P.PactHash -> IO ())

      -- TODO: this would be nicer as a batch lookup :(
    , _cpLookupProcessedTx :: !(P.PactHash -> IO (Maybe (T2 BlockHeight BlockHash)))
    }

data CheckpointEnv = CheckpointEnv
    { _cpeCheckpointer :: !Checkpointer
    , _cpeLogger :: !Logger
    }

makeLenses ''CheckpointEnv

newtype SQLiteFlag = SQLiteFlag { getFlag :: CInt }
  deriving newtype (Eq, Ord, Bits, Num)

-- TODO: get rid of this shim, it's probably not necessary
data MemPoolAccess = MemPoolAccess
  { mpaGetBlock
        :: MempoolPreBlockCheck ChainwebTransaction
        -> BlockHeight
        -> BlockHash
        -> BlockHeader
        -> IO (Vector ChainwebTransaction)
  , mpaSetLastHeader :: BlockHeader -> IO ()
  , mpaProcessFork :: BlockHeader -> IO ()
  , mpaBadlistTx :: P.PactHash -> IO ()
  }

instance Semigroup MemPoolAccess where
  MemPoolAccess f g h i <> MemPoolAccess t u v w =
      MemPoolAccess (f <> t) (g <> u) (h <> v) (i <> w)

instance Monoid MemPoolAccess where
  mempty = MemPoolAccess (\_ _ _ -> mempty) (const mempty) (const mempty) (const mempty)

data PactServiceException = PactServiceIllegalRewind
    { _attemptedRewindTo :: Maybe (BlockHeight, BlockHash)
    , _latestBlock :: Maybe (BlockHeight, BlockHash)
    } deriving (Generic)

instance Show PactServiceException where
  show (PactServiceIllegalRewind att l)
    = concat [ "illegal rewind attempt to block "
             , show att
             , ", latest was "
             , show l
             ]

instance Exception PactServiceException
