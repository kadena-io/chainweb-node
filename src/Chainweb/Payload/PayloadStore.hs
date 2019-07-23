{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.Payload.PayloadStore
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Payload.PayloadStore
(
  module Chainweb.Payload.PayloadStore.Types

-- ** Initialize Payload Database with Genesis Payloads
, initializePayloadDb
) where

import Data.Foldable

-- internal modules

import Chainweb.BlockHeader.Genesis (genesisBlockPayload)
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Initialize a PayloadDb with Genesis Payloads

-- | Initialize a PayloadDb with genesis payloads for the given chainweb
-- version.
--
initializePayloadDb
    :: PayloadCas cas
    => ChainwebVersion
    -> PayloadDb cas
    -> IO ()
initializePayloadDb v db = traverse_ initForChain $ chainIds v
  where
    initForChain cid = do
        addNewPayload db $ genesisBlockPayload v cid

