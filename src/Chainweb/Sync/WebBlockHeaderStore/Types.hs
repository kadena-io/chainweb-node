{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Sync.WebBlockHeaderStore.Types
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Sync.WebBlockHeaderStore.Types
( WebBlockHeaderStore(..)

-- *
, WebBlockPayloadStore(..)

-- * Utils
, PactExecutionService(..)

, ChainValue(..)

, WebBlockHeaderCas(..)
) where

import Control.DeepSeq
import Control.Lens

import Data.Hashable

import GHC.Generics (Generic)

import qualified Network.HTTP.Client as HTTP

import Servant.Client

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Version
import Chainweb.WebBlockHeaderDB.Types
import Chainweb.WebPactExecutionService.Types

import Data.CAS
import Data.LogMessage
import Data.PQueue
import Data.TaskMap

import P2P.TaskQueue


-- -------------------------------------------------------------------------- --
-- Tag Values With a ChainId

data ChainValue a = ChainValue !ChainId !a
    deriving (Show, Eq, Ord, Generic)
    deriving (Functor, Foldable, Traversable)
    deriving anyclass (NFData, Hashable)

instance TraversableWithIndex ChainId ChainValue where
  itraverse f (ChainValue cid v) = ChainValue cid <$> f cid v
  {-# INLINE itraverse #-}

instance FoldableWithIndex ChainId ChainValue
instance FunctorWithIndex ChainId ChainValue

instance IsCasValue a => IsCasValue (ChainValue a) where
    type CasKeyType (ChainValue a) = ChainValue (CasKeyType a)
    casKey (ChainValue cid a) = ChainValue cid (casKey a)
    {-# INLINE casKey #-}

instance HasChainId (ChainValue a) where
    _chainId (ChainValue cid _) = cid
    {-# INLINE _chainId #-}

-- -------------------------------------------------------------------------- --
-- Append Only CAS for WebBlockHeaderDb

newtype WebBlockHeaderCas = WebBlockHeaderCas WebBlockHeaderDb

instance HasChainwebVersion WebBlockHeaderCas where
    _chainwebVersion (WebBlockHeaderCas db) = _chainwebVersion db
    {-# INLINE _chainwebVersion #-}


-- -------------------------------------------------------------------------- --
-- Obtain and Validate Block Payloads

data WebBlockPayloadStore cas = WebBlockPayloadStore
    { _webBlockPayloadStoreCas :: !(PayloadDb cas)
        -- ^ Cas for storing complete payload data including outputs.
    , _webBlockPayloadStoreMemo :: !(TaskMap BlockPayloadHash PayloadData)
        -- ^ Internal memo table for active tasks
    , _webBlockPayloadStoreQueue :: !(PQueue (Task ClientEnv PayloadData))
        -- ^ task queue for scheduling tasks with the task server
    , _webBlockPayloadStoreLogFunction :: !LogFunction
        -- ^ LogFunction
    , _webBlockPayloadStoreMgr :: !HTTP.Manager
        -- ^ Manager object for making HTTP requests
    , _webBlockPayloadStorePact :: !WebPactExecutionService
        -- ^ handle to the pact execution service for validating transactions
        -- and computing outputs.
    }


-- -------------------------------------------------------------------------- --
-- WebBlockHeaderStore

-- | In order to use this a processor for the queue is needed.
--
-- The module P2P.TaskQueue provides a P2P session that serves the queue.
--
-- TODO
--
-- * Find a better name
-- * Parameterize in cas
-- * This is currently based on TreeDB (for API) and BlockHeaderDB, it
--   would be possible to run this on top of any CAS and API that offers
--   a simple GET.
--
data WebBlockHeaderStore = WebBlockHeaderStore
    { _webBlockHeaderStoreCas :: !WebBlockHeaderDb
    , _webBlockHeaderStoreMemo :: !(TaskMap (ChainValue BlockHash) (ChainValue BlockHeader))
    , _webBlockHeaderStoreQueue :: !(PQueue (Task ClientEnv (ChainValue BlockHeader)))
    , _webBlockHeaderStoreLogFunction :: !LogFunction
    , _webBlockHeaderStoreMgr :: !HTTP.Manager
    }

instance HasChainwebVersion WebBlockHeaderStore where
    _chainwebVersion = _chainwebVersion . _webBlockHeaderStoreCas
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph WebBlockHeaderStore where
    _chainGraph = _chainGraph . _webBlockHeaderStoreCas
    {-# INLINE _chainGraph #-}

