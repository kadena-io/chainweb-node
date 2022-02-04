{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | An endpoint for making RocksDB checkpoints. Used for backups and 
-- to synchronize databases between nodes.

module Chainweb.RestAPI.Checkpoint
    ( CheckpointApi
    , someCheckpointApi
    , someCheckpointServer
    ) where

import Control.Monad.IO.Class
import Data.Proxy
import Data.Text (Text)
import Servant

import Chainweb.RestAPI.Utils

type CheckpointApi = "make-checkpoint" :> Get '[PlainText] Text

someCheckpointApi :: SomeApi
someCheckpointApi = SomeApi (Proxy @CheckpointApi)

someCheckpointServer :: IO Text -> SomeServer
someCheckpointServer makeCheckpoint = 
    SomeServer (Proxy @CheckpointApi) handler
  where
    handler = liftIO makeCheckpoint 

