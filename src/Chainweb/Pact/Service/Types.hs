{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module: Chainweb.Pact.Service.Types
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Types module for Pact execution API

module Chainweb.Pact.Service.Types where

import Control.Concurrent.MVar.Strict
import Control.Monad.Catch

import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Tuple.Strict
import Data.Vector (Vector)

import GHC.Generics
import Numeric.Natural (Natural)

-- internal pact modules

import qualified Pact.Types.ChainId as Pact
import Pact.Types.Command
import Pact.Types.Hash

-- internal chainweb modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Mempool.Mempool (InsertError(..))
import Chainweb.Miner.Pact
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils (encodeToText)
import Chainweb.Version


-- | Externally-injected PactService properties.
data PactServiceConfig = PactServiceConfig
  { _pactReorgLimit :: Natural
    -- ^ Maximum allowed reorg depth, implemented as a rewind limit in validate. New block
    -- hardcodes this to 8 currently.
  , _pactRevalidate :: Bool
    -- ^ Re-validate payload hashes during transaction replay
  , _pactAllowReadsInLocal :: Bool
    -- ^ Allow direct database reads in local mode
  , _pactQueueSize :: Natural
    -- ^ max size of pact internal queue.
  , _pactResetDb :: Bool
    -- ^ blow away pact dbs
  } deriving (Eq,Show)



data PactException
  = BlockValidationFailure Value
  | PactInternalError Text
  | PactTransactionExecError PactHash Text
  | CoinbaseFailure Text
  | NoBlockValidatedYet
  | TransactionValidationException [(PactHash, Text)]
  | PactDuplicateTableError Text
  | TransactionDecodeFailure Text
  | RewindLimitExceeded Text BlockHeight BlockHeight
  | BlockHeaderLookupFailure Text
  deriving (Eq,Generic)

instance Show PactException where
    show = unpack . encodeToText

instance ToJSON PactException
instance FromJSON PactException

instance Exception PactException


internalError :: MonadThrow m => Text -> m a
internalError = throwM . PactInternalError

internalError' :: MonadThrow m => String -> m a
internalError' = internalError . pack

data RequestMsg = NewBlockMsg NewBlockReq
                | ValidateBlockMsg ValidateBlockReq
                | LocalMsg LocalReq
                | LookupPactTxsMsg LookupPactTxsReq
                | PreInsertCheckMsg PreInsertCheckReq
                | CloseMsg
                deriving (Show)

type PactExMVar t = MVar (Either PactException t)

data NewBlockReq = NewBlockReq
    { _newBlockHeader :: !ParentHeader
    , _newMiner :: !Miner
    , _newCreationTime :: !BlockCreationTime
    , _newResultVar :: !(PactExMVar PayloadWithOutputs)
    }
instance Show NewBlockReq where show NewBlockReq{..} = show (_newBlockHeader, _newMiner)

data ValidateBlockReq = ValidateBlockReq
    { _valBlockHeader :: BlockHeader
    , _valPayloadData :: PayloadData
    , _valResultVar :: PactExMVar PayloadWithOutputs
    }
instance Show ValidateBlockReq where show ValidateBlockReq{..} = show (_valBlockHeader, _valPayloadData)

data LocalReq = LocalReq
    { _localRequest :: ChainwebTransaction
    , _localResultVar :: PactExMVar (CommandResult Hash)
    }
instance Show LocalReq where show LocalReq{..} = show _localRequest

data LookupPactTxsReq = LookupPactTxsReq
    { _lookupRestorePoint :: !Rewind
        -- here if the restore point is "Nothing" it means "we don't care"
    , _lookupKeys :: !(Vector PactHash)
    , _lookupResultVar :: !(PactExMVar (Vector (Maybe (T2 BlockHeight BlockHash))))
    }
instance Show LookupPactTxsReq where
    show (LookupPactTxsReq m _ _) =
        "LookupPactTxsReq@" ++ show m

data PreInsertCheckReq = PreInsertCheckReq
    { _preInsCheckTxs :: !(Vector ChainwebTransaction)
    , _preInsCheckResult :: !(PactExMVar (Vector (Either InsertError ())))
    }
instance Show PreInsertCheckReq where
    show (PreInsertCheckReq v _) =
        "PreInsertCheckReq@" ++ show v

data SpvRequest = SpvRequest
    { _spvRequestKey :: RequestKey
    , _spvTargetChainId :: Pact.ChainId
    } deriving (Eq, Show, Generic)

instance ToJSON SpvRequest where
  toJSON (SpvRequest k tid) = object
    [ "requestKey" .= k
    , "targetChainId" .= tid
    ]

instance FromJSON SpvRequest where
  parseJSON = withObject "SpvRequest" $ \o -> SpvRequest
    <$> o .: "requestKey"
    <*> o .: "targetChainId"

newtype TransactionOutputProofB64 = TransactionOutputProofB64 Text
    deriving stock (Eq, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | This data type marks whether or not a particular header is
-- expected to rewind or not. In the case of 'NoRewind', no
-- header data is given, and a chain id is given instead for
-- routing purposes
--
data Rewind
    = DoRewind !BlockHeader
    | NoRewind {-# UNPACK #-} !ChainId
    deriving (Eq, Show)

instance HasChainId Rewind where
    _chainId = \case
      DoRewind !bh -> _chainId bh
      NoRewind !cid -> cid
    {-# INLINE _chainId #-}
