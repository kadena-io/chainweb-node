{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.CutDB.Types
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.CutDB.Types
( CutDb(..)
) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TVar

import Data.Function
import Data.LogMessage
import Data.Ord
import qualified Data.Text as T

import Numeric.Natural

import Prelude hiding (lookup)

-- internal modules

import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.Graph
import Chainweb.Sync.WebBlockHeaderStore.Types
import qualified Chainweb.Utils.TokenLimiting as Limiter
import Chainweb.Version

import Control.Concurrent.FixedThreadPool (ThreadPool)

import Data.CAS.RocksDB
import Data.PQueue

-- -------------------------------------------------------------------------- --
-- Cut DB

-- | This is a singleton DB that contains the latest chainweb cut as only entry.
--
data CutDb cas = CutDb
    { _cutDbCut :: !(TVar Cut)
    , _cutDbQueue :: !(PQueue (Down CutHashes))
    , _cutDbAsync :: !(Async ())
    , _cutDbLogFunction :: !LogFunction
    , _cutDbHeaderStore :: !WebBlockHeaderStore
    , _cutDbPayloadStore :: !(WebBlockPayloadStore cas)
    , _cutDbQueueSize :: !Natural
    , _cutDbStore :: !(RocksDbCas CutHashes)
    , _cutDbRateLimiter :: !(Limiter.TokenLimitMap T.Text)
    , _cutDbFetchThreadPool :: !ThreadPool
    }

instance HasChainGraph (CutDb cas) where
    _chainGraph = _chainGraph . _cutDbHeaderStore
    {-# INLINE _chainGraph #-}

instance HasChainwebVersion (CutDb cas) where
    _chainwebVersion = _chainwebVersion . _cutDbHeaderStore
    {-# INLINE _chainwebVersion #-}
